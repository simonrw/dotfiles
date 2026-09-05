"""Integration checks using the installed Pandoc configuration."""

from pathlib import Path
import subprocess
import tempfile
import unittest
from urllib.error import HTTPError
from urllib.request import Request, urlopen


RENDERER = Path(__file__).resolve().parents[2] / ".bin" / "render-markdown"


class ServerTest(unittest.TestCase):
    def test_browsing_and_conversion(self):
        with tempfile.TemporaryDirectory() as directory:
            root = Path(directory)
            (root / "nested").mkdir()
            doc = root / "nested" / "A & B.md"
            doc.write_text("# First title\n\nOriginal content\n\n[Other](../other.markdown)\n\n![Dot](dot.svg)\n")
            (root / "nested" / "dot.svg").write_text(
                '<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">'
                '<circle cx="5" cy="5" r="4"/></svg>'
            )
            (root / "other.markdown").write_text("# Other\n")
            (root / "secret.txt").write_text("not a document")
            (root / "escape.md").symlink_to("/etc/hosts")
            process = subprocess.Popen(
                [str(RENDERER), "server", "--host", "127.0.0.1", "--port", "0", "--no-open", "--no-mermaid"],
                cwd=root, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True,
            )
            try:
                startup = process.stdout.readline()
                if not startup:
                    self.fail(process.stderr.read())
                url = startup.split(" at ")[1].split(" ")[0].rstrip("/")

                def get(path):
                    with urlopen(url + path, timeout=10) as response:
                        self.assertEqual(response.headers["Cache-Control"], "no-store")
                        return response.read().decode()

                index = get("/")
                self.assertIn("nested/A%20%26%20B.md", index)
                self.assertIn("A &amp; B.md", index)
                self.assertIn("other.markdown", index)
                self.assertNotIn("escape.md", index)
                page = get("/nested/A%20%26%20B.md")
                self.assertIn("Original content", page)
                self.assertIn("All documents", page)
                self.assertNotIn("window.__planReview", page)
                self.assertIn("../other.markdown", page)
                self.assertIn("data:image/svg+xml", page)
                doc.write_text("# Changed title\n\nUpdated content\n")
                self.assertIn("Updated content", get("/nested/A%20%26%20B.md"))
                (root / "new.MD").write_text("# New\n")
                self.assertIn("new.MD", get("/"))
                for path in ["/escape.md", "/secret.txt", "/../etc/hosts", "/missing.md"]:
                    with self.assertRaises(HTTPError) as caught:
                        get(path)
                    self.assertEqual(caught.exception.code, 404)
                with urlopen(Request(url + "/other.markdown", method="HEAD"), timeout=10) as response:
                    self.assertEqual(response.status, 200)
                    self.assertEqual(response.read(), b"")
                converted = subprocess.check_output(
                    [str(RENDERER), "convert", str(doc), "-o", "-", "--no-mermaid"], text=True,
                )
                self.assertIn("Updated content", converted)
                self.assertIn("window.__planReview", converted)
                self.assertNotIn("All documents", converted)
            finally:
                process.terminate()
                process.communicate(timeout=10)


if __name__ == "__main__":
    unittest.main()
