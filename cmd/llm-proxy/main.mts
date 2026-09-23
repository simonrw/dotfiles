#!/usr/bin/env node
import { parseArgs } from 'node:util';
import { createLoggingProxy, DEFAULT_MAX_BODY, isLoopback, parseUpstream } from './proxy.mts';
import { formatEvent } from './format.mts';

function main(): void {
  const { values } = parseArgs({ options: {
    upstream: { type: 'string', default: 'https://api.openai.com' },
    host: { type: 'string', default: '127.0.0.1' },
    port: { type: 'string', default: '8765' },
    format: { type: 'string', default: 'pretty' },
    'max-body': { type: 'string', default: String(DEFAULT_MAX_BODY) },
    'no-redact': { type: 'boolean', default: false },
    help: { type: 'boolean', short: 'h' },
  } });
  if (values.help) {
    console.log(`Usage: node cmd/llm-proxy/main.mts [options]

  --upstream URL    Fixed upstream origin/prefix (default https://api.openai.com)
  --host ADDRESS    Loopback only (default 127.0.0.1)
  --port NUMBER     Listening port (default 8765)
  --format FORMAT   pretty transcript, json, or jsonl (default pretty)
  --max-body BYTES  Inspection limit, not a forwarding limit (default ${DEFAULT_MAX_BODY})
  --no-redact       Disable log redaction, including credentials

Requires Node 24+. Logs go to stdout. Requests use HTTP/SSE, not WebSockets.
Pretty output shows system instructions, developer/harness prompts and all sent messages.
Credentials are redacted unless --no-redact is set; prompts and tool content remain sensitive.`);
    return;
  }
  if (!isLoopback(values.host)) throw new Error('Host must be a loopback address');
  const port = Number(values.port);
  if (!Number.isInteger(port) || port < 1 || port > 65535) throw new Error('Port must be between 1 and 65535');
  if (!['pretty', 'json', 'jsonl'].includes(values.format)) throw new Error('Format must be pretty, json or jsonl');
  const server = createLoggingProxy({
    upstream: parseUpstream(values.upstream),
    maxBody: Number(values['max-body']),
    redact: !values['no-redact'],
    log: event => {
      process.stdout.write(formatEvent(event, values.format));
    },
  });
  const stop = () => {
    server.close();
    server.closeAllConnections();
  };
  process.once('SIGINT', stop);
  process.once('SIGTERM', stop);
  process.stdout.on('error', () => {
    console.error('llm-proxy: cannot write logs');
    process.exitCode = 1;
    stop();
  });
  server.on('error', () => {
    console.error('llm-proxy: cannot open loopback listener');
    process.exitCode = 1;
  });
  server.listen(port, values.host, () => {
    if (values['no-redact']) console.error('WARNING: redaction disabled; logs may contain credentials.');
    console.error(`Listening on port ${port}. Logs go to stdout; prompts and tool content remain sensitive.`);
  });
}

try { main(); } catch {
  // Argument parser errors may echo credentials accidentally put on the command line.
  console.error('llm-proxy: invalid configuration. Run with --help; use a loopback listener and an upstream URL without credentials.');
  process.exitCode = 1;
}
