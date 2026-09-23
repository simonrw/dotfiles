import http from 'node:http';
import https from 'node:https';
import { isIP } from 'node:net';
import { pipeline } from 'node:stream';
import { brotliDecompressSync, gunzipSync, inflateSync, zstdDecompressSync } from 'node:zlib';
import { Redactor } from './redact.mts';

export type LogEvent = Record<string, unknown>;
export const DEFAULT_MAX_BODY = 16 * 1024 * 1024;
type ProxyOptions = {
  upstream: URL;
  log: (event: LogEvent) => void;
  maxBody?: number;
  redact?: boolean;
};

const hopHeaders = new Set([
  'connection', 'keep-alive', 'proxy-authenticate', 'proxy-authorization',
  'te', 'trailer', 'transfer-encoding', 'upgrade',
]);

export function isLoopback(host: string): boolean {
  host = host.replace(/^\[|\]$/g, '');
  return host === 'localhost' || host === '::1' || (isIP(host) === 4 && host.startsWith('127.'));
}

export function parseUpstream(raw: string): URL {
  let url: URL;
  try { url = new URL(raw); } catch { throw new Error('Upstream must be an HTTP(S) URL'); }
  if (url.username || url.password || url.search || url.hash || raw.includes('?') || raw.includes('#')) {
    throw new Error('Upstream must not contain credentials, a query, or a fragment');
  }
  if (url.protocol !== 'https:' && !(url.protocol === 'http:' && isLoopback(url.hostname))) {
    throw new Error('Upstream requires HTTPS, except for loopback test servers');
  }
  return url;
}

function forwardingHeaders(headers: http.IncomingHttpHeaders): http.OutgoingHttpHeaders {
  const blocked = new Set(hopHeaders);
  for (const name of (headers.connection ?? '').split(',')) blocked.add(name.trim().toLowerCase());
  return Object.fromEntries(Object.entries(headers).filter(([name]) => !blocked.has(name)));
}

function inspectBody(data: Buffer, encoding: string, limit: number): unknown {
  const options = { maxOutputLength: limit + 1 };
  switch (encoding.toLowerCase()) {
    case '': case 'identity': break;
    case 'gzip': data = gunzipSync(data, options); break;
    case 'deflate': data = inflateSync(data, options); break;
    case 'br': data = brotliDecompressSync(data, options); break;
    case 'zstd': data = zstdDecompressSync(data, options); break;
    default: throw new Error('Unsupported encoding');
  }
  if (data.length > limit) throw new Error('Decoded body exceeds inspection limit');
  return JSON.parse(data.toString('utf8'));
}

/** A fixed-upstream, loopback-only HTTP/SSE inspector. No credential storage. */
export function createLoggingProxy({ upstream, log, maxBody = DEFAULT_MAX_BODY, redact = true }: ProxyOptions): http.Server {
  upstream = parseUpstream(upstream.href);
  if (!Number.isSafeInteger(maxBody) || maxBody < 1 || maxBody > 64 * 1024 * 1024) {
    throw new Error('max-body must be an integer between 1 and 67108864');
  }
  let nextID = 0;
  const server = http.createServer((request, response) => {
    let host = '';
    try { host = new URL(`http://${request.headers.host}`).hostname; } catch { /* Reject below. */ }
    const reject = (status: number, message: string) => {
      response.writeHead(status, { 'content-type': 'text/plain', connection: 'close' });
      response.end(`${message}\n`);
    };
    if (!isLoopback(host) || request.headers.origin !== undefined
      || (request.headers['sec-fetch-site'] && request.headers['sec-fetch-site'] !== 'none')) {
      reject(403, 'Only local CLI clients are allowed');
      return;
    }
    if (!request.url?.startsWith('/') || request.url.startsWith('//')) {
      reject(400, 'Use a provider base URL, not HTTP_PROXY or HTTPS_PROXY');
      return;
    }
    const id = ++nextID;
    const started = performance.now();
    const redactor = redact ? new Redactor(request) : undefined;
    const emit = (event: LogEvent) => log({ time: new Date().toISOString(), id, ...event });
    let chunks: Buffer[] = [];
    let requestBytes = 0;
    let responseBytes = 0;
    let requestLogged = false;
    let responseLogged = false;
    let failure: string | undefined;

    const logRequest = (interrupted = false) => {
      if (requestLogged) return;
      requestLogged = true;
      const event: LogEvent = { event: 'request', method: redactor ? redactor.text(request.method ?? '') : request.method ?? '', bytes: requestBytes };
      if (interrupted) event.body_note = 'Request interrupted; body omitted';
      else if (requestBytes === 0) event.body_note = 'Empty body';
      else if (requestBytes > maxBody) event.body_note = 'Body exceeds inspection limit; increase --max-body to see messages. Forwarded without logging';
      else {
        try {
          const body = inspectBody(Buffer.concat(chunks), String(request.headers['content-encoding'] ?? ''), maxBody);
          redactor?.collect(body);
          event.body = redactor ? redactor.value(body) : body;
        } catch {
          // Never fall back to raw bytes or error text: either can expose secrets.
          event.body_note = 'Body omitted: invalid/non-JSON, unsupported encoding, or inspection limit. Increase --max-body for larger prompts';
        }
      }
      chunks = [];
      event.path = redactor ? redactor.path(request.url!) : request.url;
      event.headers = redactor ? redactor.headers(request) : request.headers;
      emit(event);
    };
    const logResponse = (complete: boolean) => {
      if (responseLogged) return;
      responseLogged = true;
      logRequest(!request.complete);
      emit({ event: 'response', status: response.headersSent ? response.statusCode : null,
        bytes: responseBytes, duration_ms: Math.round(performance.now() - started),
        complete, ...(failure ? { error: failure } : {}) });
    };
    const fail = (status: number, reason: string) => {
      failure = reason;
      if (response.headersSent) response.destroy();
      else reject(status, reason);
    };

    const headers = forwardingHeaders(request.headers);
    headers.host = upstream.host;
    // Don't send caller-supplied forwarding metadata to the provider.
    delete headers.forwarded;
    for (const name of Object.keys(headers)) if (name.startsWith('x-forwarded-')) delete headers[name];
    const outgoing = (upstream.protocol === 'https:' ? https : http).request(upstream, {
      method: request.method,
      // Concatenation keeps the raw request path/query and never changes origin.
      path: upstream.pathname.replace(/\/$/, '') + request.url,
      headers,
    }, incoming => {
      incoming.on('data', chunk => { responseBytes += chunk.length; });
      incoming.on('error', () => { failure = 'Upstream response interrupted'; });
      response.writeHead(incoming.statusCode ?? 502, forwardingHeaders(incoming.headers));
      response.flushHeaders();
      pipeline(incoming, response, () => { /* Errors close the stream; logResponse records completion. */ });
    });
    outgoing.setTimeout(300_000, () => {
      fail(504, 'Upstream idle timeout');
      outgoing.destroy();
    });
    outgoing.on('error', () => {
      if (!response.destroyed && !response.writableEnded) fail(502, 'Upstream connection failed');
    });
    request.on('data', (chunk: Buffer) => {
      requestBytes += chunk.length;
      if (requestBytes <= maxBody) chunks.push(chunk);
      else chunks = [];
    });
    request.on('end', () => logRequest());
    request.on('error', () => {
      failure = 'Client request interrupted';
      logRequest(true);
      outgoing.destroy();
    });
    response.on('finish', () => logResponse(true));
    response.on('close', () => {
      outgoing.destroy();
      logResponse(response.writableFinished);
    });
    request.pipe(outgoing);
  });
  // Explicit refusal avoids silently hiding model requests in WebSocket frames.
  server.on('upgrade', (_request, socket) => {
    const body = 'WebSocket logging unsupported; use HTTP/SSE transport.\n';
    socket.end(`HTTP/1.1 501 Not Implemented\r\nConnection: close\r\nContent-Length: ${Buffer.byteLength(body)}\r\n\r\n${body}`);
  });
  server.on('connect', (_request, socket) => {
    socket.end('HTTP/1.1 405 Method Not Allowed\r\nConnection: close\r\nContent-Length: 0\r\n\r\n');
  });
  server.headersTimeout = 10_000;
  server.requestTimeout = 120_000;
  return server;
}
