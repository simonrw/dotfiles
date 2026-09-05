"""Serve Markdown from the working directory using render-markdown convert."""

import argparse
import html
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from pathlib import Path
import subprocess
import sys
from urllib.parse import quote, unquote, urlsplit
import webbrowser


MARKDOWN_SUFFIXES = {".md", ".markdown"}


def handler_for(root, renderer, no_mermaid=False):
    class Handler(BaseHTTPRequestHandler):
        def do_GET(self):
            self.respond()

        def do_HEAD(self):
            self.respond(head=True)

        def respond(self, head=False):
            url_path = unquote(urlsplit(self.path).path)
            if url_path == "/":
                documents = sorted(
                    (path for path in root.rglob("*")
                     if path.suffix.lower() in MARKDOWN_SUFFIXES
                     and path.is_file() and path.resolve().is_relative_to(root)),
                    key=lambda path: str(path.relative_to(root)).casefold(),
                )
                links = "".join(
                    '<li><a href="/{}">{}</a></li>'.format(
                        quote(path.relative_to(root).as_posix()),
                        html.escape(path.relative_to(root).as_posix()),
                    )
                    for path in documents
                )
                body = (
                    '<!doctype html><html lang="en"><meta charset="utf-8">'
                    '<meta name="viewport" content="width=device-width, initial-scale=1">'
                    '<title>Markdown documents</title><style>'
                    'body{{font:18px system-ui;max-width:60rem;margin:3rem auto;padding:0 1rem;'
                    'color-scheme:light dark}}li{{margin:.6rem 0}}</style>'
                    '<h1>Markdown documents</h1><p>{}</p>{}</html>'.format(
                        html.escape(str(root)),
                        "<ul>" + links + "</ul>" if links else "<p>No Markdown documents found.</p>",
                    )
                ).encode()
            else:
                try:
                    path = (root / url_path.lstrip("/")).resolve()
                    if (not path.is_relative_to(root) or not path.is_file()
                            or path.suffix.lower() not in MARKDOWN_SUFFIXES):
                        self.send_error(404, "Markdown document not found")
                        return
                    command = [str(renderer), "convert", str(path), "-o", "-",
                               "--no-open", "-M", "server-mode=true"]
                    if no_mermaid:
                        command.append("--no-mermaid")
                    result = subprocess.run(command, capture_output=True)
                except (OSError, ValueError):
                    self.send_error(404, "Markdown document not found")
                    return
                if result.returncode:
                    print(result.stderr.decode(errors="replace"), file=sys.stderr)
                    self.send_error(500, "Could not render Markdown document")
                    return
                body = result.stdout
            self.send_response(200)
            self.send_header("Content-Type", "text/html; charset=utf-8")
            self.send_header("Content-Length", str(len(body)))
            self.send_header("Cache-Control", "no-store")
            self.end_headers()
            if not head:
                self.wfile.write(body)

    return Handler


def main():
    parser = argparse.ArgumentParser(
        prog="render-markdown server",
        description="Render Markdown under the current directory on each request.",
    )
    parser.add_argument("--renderer", required=True, help=argparse.SUPPRESS)
    parser.add_argument("--host", default="0.0.0.0", help="bind address (default: 0.0.0.0)")
    parser.add_argument("--port", type=int, default=8000, help="local port (default: 8000)")
    parser.add_argument("--no-open", action="store_true", help="do not open the browser")
    parser.add_argument("--no-mermaid", action="store_true", help="leave diagrams as source")
    args = parser.parse_args()
    if not 0 <= args.port <= 65535:
        parser.error("port must be between 0 and 65535")
    root = Path.cwd().resolve()
    try:
        server = ThreadingHTTPServer(
            (args.host, args.port),
            handler_for(root, Path(args.renderer).resolve(), args.no_mermaid),
        )
    except OSError as error:
        parser.exit(1, f"render-markdown: {error}\n")
    with server:
        browser_host = "127.0.0.1" if args.host == "0.0.0.0" else args.host
        url = f"http://{browser_host}:{server.server_port}/"
        print(f"Serving {root} at {url} (listening on {args.host}:{server.server_port}; Ctrl-C to stop)", flush=True)
        if not args.no_open:
            webbrowser.open(url)
        try:
            server.serve_forever()
        except KeyboardInterrupt:
            pass


if __name__ == "__main__":
    main()
