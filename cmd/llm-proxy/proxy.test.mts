import assert from 'node:assert/strict';
import http from 'node:http';
import { once } from 'node:events';
import { connect } from 'node:net';
import { test } from 'node:test';
import type { TestContext } from 'node:test';
import { gzipSync, zstdCompressSync } from 'node:zlib';
import { createLoggingProxy, parseUpstream } from './proxy.mts';
import type { LogEvent } from './proxy.mts';
import { formatEvent } from './format.mts';

async function listen(t: TestContext, server: http.Server): Promise<string> {
  server.listen(0, '127.0.0.1');
  await once(server, 'listening');
  t.after(() => { server.closeAllConnections(); server.close(); });
  const address = server.address();
  assert.ok(address && typeof address !== 'string');
  return `http://127.0.0.1:${address.port}`;
}

async function readBody(request: http.IncomingMessage): Promise<Buffer> {
  const chunks: Buffer[] = [];
  for await (const chunk of request) chunks.push(chunk);
  return Buffer.concat(chunks);
}

async function fixture(t: TestContext, options: { maxBody?: number; prefix?: string; redact?: boolean } = {}) {
  const received: { body: Buffer; headers: http.IncomingHttpHeaders; path: string }[] = [];
  const upstream = await listen(t, http.createServer(async (req, res) => {
    received.push({ body: await readBody(req), headers: req.headers, path: req.url! });
    res.writeHead(201, { 'content-type': 'text/event-stream', 'set-cookie': 'session=response-secret' });
    res.end('data: {"message":"ok"}\n\n');
  }));
  const events: LogEvent[] = [];
  const url = await listen(t, createLoggingProxy({
    upstream: new URL(upstream + (options.prefix ?? '')),
    maxBody: options.maxBody,
    redact: options.redact,
    log: event => events.push(event),
  }));
  return { url, upstream, events, received };
}

test('forwards original JSON, credentials and signed query; logs redacted request and response metadata', async t => {
  const f = await fixture(t, { prefix: '/backend-api' });
  const body = JSON.stringify({
    model: 'test-model', max_output_tokens: 500,
    input: [{ role: 'user', content: 'Use auth-value, cookie-value, vendor-value, query-value, and nested-value.' }],
    settings: { client_secret: 'nested-value', accessToken: 'access-value', password: 'password-value' },
    arguments: JSON.stringify({ api_key: 'argument-value', visible: 'argument-value' }),
    instructions: 'OPENAI_API_KEY=assignment-value sk-proj-patternValue Bearer inline-value',
    url: 'https://user:pass@example.com/path?opaque=query-secret',
    private_key: '-----BEGIN PRIVATE KEY-----\nprivate-value\n-----END PRIVATE KEY-----',
  });
  const response = await fetch(`${f.url}/codex/responses?opaque=query-value&signed=a%2Fb+%20`, {
    method: 'POST', body,
    headers: { authorization: 'Bearer auth-value', cookie: 'session=cookie-value',
      'x-vendor-credential': 'vendor-value', 'content-type': 'application/json', 'x-forwarded-for': 'spoofed' },
  });
  assert.equal(response.status, 201);
  assert.equal(await response.text(), 'data: {"message":"ok"}\n\n');
  assert.equal(response.headers.get('set-cookie'), 'session=response-secret');
  assert.equal(f.received[0].body.toString(), body);
  assert.equal(f.received[0].headers.authorization, 'Bearer auth-value');
  assert.equal(f.received[0].headers.cookie, 'session=cookie-value');
  assert.equal(f.received[0].headers['x-vendor-credential'], 'vendor-value');
  assert.equal(f.received[0].headers['x-forwarded-for'], undefined);
  assert.equal(f.received[0].headers.host, new URL(f.upstream).host);
  assert.equal(f.received[0].path, '/backend-api/codex/responses?opaque=query-value&signed=a%2Fb+%20');
  const logs = JSON.stringify(f.events) + f.events.map(event => formatEvent(event)).join('');
  for (const secret of ['auth-value', 'cookie-value', 'vendor-value', 'query-value', 'nested-value',
    'access-value', 'password-value', 'argument-value', 'assignment-value', 'patternValue',
    'inline-value', 'query-secret', 'private-value', 'response-secret', 'user:pass']) {
    assert.ok(!logs.includes(secret), `logs leaked ${secret}`);
  }
  assert.ok(logs.includes('test-model'));
  assert.ok(logs.includes('max_output_tokens'));
  assert.ok(logs.includes('[REDACTED]'));
  assert.equal(f.events[1].status, 201);
  assert.equal(f.events[1].complete, true);
});

test('disabling redaction preserves credentials in all log formats without changing forwarding', async t => {
  const f = await fixture(t, { redact: false });
  const original = {
    input: 'Use auth-value, cookie-value, query-value and sk-proj-patternValue.\u001b[31m',
    settings: { api_key: 'nested-value' },
    arguments: ' { "password": "argument-value" } ',
    url: 'https://user:pass@example.com/path?token=url-value',
  };
  const body = gzipSync(Buffer.from(JSON.stringify(original)));
  const path = '/sk-proj-pathValue/responses?token=query-value&signed=a%2Fb+%20';
  const response = await fetch(f.url + path, {
    method: 'POST', body,
    headers: { authorization: 'Bearer auth-value', cookie: 'session=cookie-value',
      'content-type': 'application/json', 'content-encoding': 'gzip' },
  });
  assert.equal(response.status, 201);
  assert.equal(await response.text(), 'data: {"message":"ok"}\n\n');
  assert.deepEqual(f.received[0].body, body);
  assert.equal(f.received[0].path, path);
  assert.equal(f.received[0].headers.authorization, 'Bearer auth-value');
  assert.deepEqual(f.events[0].body, original);
  assert.equal(f.events[0].path, path);
  const headers = f.events[0].headers as http.IncomingHttpHeaders;
  assert.equal(headers.authorization, 'Bearer auth-value');
  assert.equal(headers.cookie, 'session=cookie-value');
  for (const format of ['pretty', 'json', 'jsonl']) {
    const output = formatEvent(f.events[0], format);
    for (const value of ['Bearer auth-value', 'session=cookie-value', path, 'nested-value',
      'argument-value', 'sk-proj-patternValue', original.url]) {
      assert.ok(output.includes(value), `${format} omitted ${value}`);
    }
    assert.ok(!output.includes('[REDACTED]'));
    assert.ok(!output.includes('\u001b'), 'terminal escapes must remain sanitized');
  }
});

test('shows full compressed system and harness prompts beyond the old 1 MiB inspection limit', async t => {
  const f = await fixture(t);
  const instructions = 'System instructions\n' + 'Full harness context.\n'.repeat(60_000) + 'End of system prompt';
  const original = {
    instructions,
    input: [{ role: 'developer', content: [{ type: 'input_text', text: 'Harness rules\nNever print prompt-credential.' }] }],
  };
  const body = zstdCompressSync(Buffer.from(JSON.stringify(original)));
  const response = await fetch(f.url, { method: 'POST', body,
    headers: { 'content-encoding': 'zstd', authorization: 'Bearer prompt-credential' } });
  await response.text();
  assert.deepEqual(f.received[0].body, body);
  const output = formatEvent(f.events[0]);
  assert.ok(output.includes(instructions.split('\n').join('\n  ')));
  assert.ok(output.includes('DEVELOPER [input[0]]'));
  assert.ok(output.includes('Harness rules\n  Never print [REDACTED].'));
  assert.ok(!output.includes('prompt-credential'));
});

test('redacts secrets collected from nested tool arguments before earlier prompt text', async t => {
  const f = await fixture(t);
  const response = await fetch(f.url, { method: 'POST', body: JSON.stringify({
    input: 'Do not print embedded-secret', arguments: ' {"apiKey":"embedded-secret"}',
    list: [{ refresh_token: 'refresh-secret' }],
  }) });
  await response.text();
  assert.ok(!JSON.stringify(f.events).includes('embedded-secret'));
  assert.ok(!JSON.stringify(f.events).includes('refresh-secret'));
});

for (const [encoding, compress] of [['gzip', gzipSync], ['zstd', zstdCompressSync]] as const) {
  test(`inspects ${encoding} JSON but forwards original compressed bytes`, async t => {
    const f = await fixture(t);
    const body = compress(Buffer.from('{"model":"compressed-model","api_key":"compressed-secret"}'));
    const response = await fetch(f.url, { method: 'POST', body, headers: { 'content-encoding': encoding } });
    await response.text();
    assert.deepEqual(f.received[0].body, body);
    assert.equal(f.received[0].headers['content-encoding'], encoding);
    assert.ok(JSON.stringify(f.events).includes('compressed-model'));
    assert.ok(!JSON.stringify(f.events).includes('compressed-secret'));
  });
}

test('omits oversized, malformed, unsupported, and decompression-bomb bodies without modifying forwarding', async t => {
  const f = await fixture(t, { maxBody: 256 });
  for (const [body, encoding] of [
    [Buffer.from('raw-sensitive-content'), ''],
    [Buffer.from('{"secret":"unfinished'), ''],
    [Buffer.from('{"input":"' + 'oversized-secret'.repeat(100) + '"}'), ''],
    [gzipSync(Buffer.from('decompressed-secret'.repeat(100))), 'gzip'],
    [Buffer.from('{"input":"unknown-secret"}'), 'unknown'],
  ] as const) {
    const response = await fetch(f.url, { method: 'POST', body, headers: { 'content-encoding': encoding } });
    await response.text();
    assert.deepEqual(f.received.at(-1)!.body, body);
    assert.ok(f.events.at(-2)!.body_note);
    assert.equal(f.events.at(-2)!.body, undefined);
  }
  assert.ok(!JSON.stringify(f.events).includes('secret'));
  assert.ok(!JSON.stringify(f.events).includes('raw-sensitive-content'));
});

test('SSE chunks reach client before upstream closes; cancellation closes upstream', async t => {
  let release: () => void;
  const closed = new Promise<void>(resolve => { release = resolve; });
  const upstream = await listen(t, http.createServer(async (req, res) => {
    await readBody(req);
    res.on('close', () => release());
    res.writeHead(200, { 'content-type': 'text/event-stream' });
    res.write('data: first\n\n');
    // Remains open until the client cancels. Buffering would hang the test.
  }));
  const events: LogEvent[] = [];
  const url = await listen(t, createLoggingProxy({ upstream: new URL(upstream), log: event => events.push(event) }));
  const response = await fetch(url, { signal: AbortSignal.timeout(3000) });
  const reader = response.body!.getReader();
  const first = await reader.read();
  assert.equal(new TextDecoder().decode(first.value), 'data: first\n\n');
  await reader.cancel();
  await Promise.race([closed, new Promise((_, reject) => {
    const timer = setTimeout(() => reject(new Error('Upstream not cancelled')), 3000);
    timer.unref();
  })]);
  assert.equal(events.at(-1)!.complete, false);
});

test('passes upstream HTTP errors without logging their response bodies', async t => {
  const upstream = await listen(t, http.createServer((_req, res) => {
    res.writeHead(429, { 'retry-after': '2' });
    res.end('upstream-error-secret');
  }));
  const events: LogEvent[] = [];
  const url = await listen(t, createLoggingProxy({ upstream: new URL(upstream), log: event => events.push(event) }));
  const response = await fetch(url);
  assert.equal(response.status, 429);
  assert.equal(response.headers.get('retry-after'), '2');
  assert.equal(await response.text(), 'upstream-error-secret');
  assert.ok(!JSON.stringify(events).includes('upstream-error-secret'));
});

test('connection errors produce a generic 502 without leaking query credentials', async t => {
  const unavailable = http.createServer();
  const upstream = await listen(t, unavailable);
  unavailable.close();
  await once(unavailable, 'close');
  const events: LogEvent[] = [];
  const url = await listen(t, createLoggingProxy({ upstream: new URL(upstream), log: event => events.push(event) }));
  const response = await fetch(`${url}/responses?token=error-secret`);
  assert.equal(response.status, 502);
  assert.equal(await response.text(), 'Upstream connection failed\n');
  assert.ok(!JSON.stringify(events).includes('error-secret'));
});

test('rejects browser origins and DNS rebinding hosts', async t => {
  const f = await fixture(t);
  for (const headers of [{ origin: 'https://example.com' }, { host: 'attacker.example' }, { 'sec-fetch-site': 'cross-site' }]) {
    // Node fetch owns the Host header, so use the lower-level client here.
    const response = await new Promise<http.IncomingMessage>((resolve, reject) => {
      http.get(f.url, { headers }, resolve).on('error', reject);
    });
    assert.equal(response.statusCode, 403);
    await readBody(response);
  }
  assert.equal(f.received.length, 0);
  assert.equal(f.events.length, 0);
});

test('refuses WebSocket upgrades instead of silently missing their requests', async t => {
  const f = await fixture(t);
  const url = new URL(f.url);
  const socket = connect(Number(url.port), url.hostname);
  t.after(() => socket.destroy());
  await once(socket, 'connect');
  socket.write(`GET /responses HTTP/1.1\r\nHost: ${url.host}\r\nConnection: Upgrade\r\nUpgrade: websocket\r\n\r\n`);
  let reply = '';
  for await (const chunk of socket) reply += chunk;
  assert.match(reply, /501 Not Implemented/);
  assert.match(reply, /HTTP\/SSE/);
  assert.equal(f.received.length, 0);
});

test('validates upstream without echoing supplied credentials', () => {
  for (const raw of ['https://user:secret@example.com', 'https://example.com?key=secret',
    'https://example.com#secret', 'http://example.com', 'file:///etc/passwd', 'invalid']) {
    assert.throws(() => parseUpstream(raw), error => error instanceof Error && !error.message.includes('secret'));
  }
  assert.equal(parseUpstream('https://api.openai.com').hostname, 'api.openai.com');
  assert.equal(parseUpstream('http://127.0.0.1:1234').port, '1234');
});

test('concurrent requests retain separate IDs and redaction state', async t => {
  const f = await fixture(t);
  await Promise.all(Array.from({ length: 8 }, async (_, n) => {
    const response = await fetch(f.url, { method: 'POST', body: JSON.stringify({ model: `model-${n}`, input: `credential-${n}` }),
      headers: { authorization: `Bearer credential-${n}` } });
    await response.text();
  }));
  const requests = f.events.filter(event => event.event === 'request');
  assert.equal(new Set(requests.map(event => event.id)).size, 8);
  assert.ok(!JSON.stringify(f.events).includes('credential-'));
  assert.equal(f.events.filter(event => event.event === 'response').length, 8);
});
