import type { LogEvent } from './proxy.mts';

type Fields = Record<string, unknown>;

function object(value: unknown): value is Fields {
  return value !== null && typeof value === 'object' && !Array.isArray(value);
}

// Preserve real newlines and tabs, but never execute terminal escape sequences.
function terminalText(value: string): string {
  return value.replace(/[\x00-\x08\x0b-\x1f\x7f-\x9f\u202a-\u202e\u2066-\u2069]/g,
    character => `\\u${character.charCodeAt(0).toString(16).padStart(4, '0')}`);
}

function content(value: unknown): string {
  if (typeof value === 'string') return value;
  if (Array.isArray(value)) return value.map(content).join('\n\n');
  if (object(value) && typeof value.text === 'string') {
    const { text, type, ...metadata } = value;
    const suffix = Object.keys(metadata).length ? `\n${JSON.stringify(metadata, null, 2)}` : '';
    return text + suffix;
  }
  return JSON.stringify(value, null, 2) ?? '';
}

function section(label: string, value: unknown): string {
  // Indenting payloads distinguishes prompt text from the logger's headings.
  return `\n--- ${label} ---\n${content(value).split('\n').map(line => `  ${line}`).join('\n')}\n`;
}

function messages(value: unknown, source: string): string {
  if (typeof value === 'string') return section(`USER [${source}]`, value);
  if (!Array.isArray(value)) return section(`INPUT [${source}]`, value);
  if (value.length === 0) return section(`INPUT [${source}]`, []);
  return value.map((message, index) => {
    if (!object(message)) return section(`INPUT [${source}[${index}]]`, message);
    const label = typeof message.role === 'string' ? message.role.toUpperCase()
      : typeof message.type === 'string' ? message.type.toUpperCase() : 'INPUT';
    // Keep tool calls, tool results, multimodal content and unknown fields visible.
    if (!('content' in message)) return section(`${label} [${source}[${index}]]`, message);
    const { role, content: body, ...metadata } = message;
    let output = section(`${label} [${source}[${index}]]`, body);
    if (Object.keys(metadata).length) output += section(`MESSAGE FIELDS [${source}[${index}]]`, metadata);
    return output;
  }).join('');
}

/** Receives only already-redacted events from the proxy. */
export function formatEvent(event: LogEvent, format = 'pretty'): string {
  if (format === 'jsonl' || format === 'json') {
    return JSON.stringify(event, null, format === 'json' ? 2 : undefined) + '\n';
  }
  if (event.event === 'response') {
    return terminalText(`\n=== Response #${event.id}: HTTP ${event.status}, ${event.duration_ms} ms, ${event.bytes} bytes, ${event.complete ? 'complete' : 'interrupted'}${event.error ? `, ${event.error}` : ''} ===\n`);
  }
  if (event.event !== 'request') return terminalText(JSON.stringify(event, null, 2)) + '\n';

  let output = `\n=== Request #${event.id}: ${event.method} ${event.path} ===\n${event.time}\n`;
  if (event.body_note) output += section('BODY NOT SHOWN', event.body_note);
  if (object(event.body)) {
    const { instructions, system, input, messages: chat, tools, ...settings } = event.body;
    if (instructions !== undefined) output += section('SYSTEM / DEVELOPER INSTRUCTIONS [instructions]', instructions);
    if (system !== undefined) output += section('SYSTEM [system]', system);
    if (input !== undefined) output += messages(input, 'input');
    if (chat !== undefined) output += messages(chat, 'messages');
    if (tools !== undefined) output += section('TOOL DEFINITIONS [tools]', tools);
    if (Object.keys(settings).length) output += section('REQUEST SETTINGS / OTHER FIELDS', settings);
  } else if ('body' in event) {
    output += section('BODY', event.body);
  }
  if (event.headers) output += section('HEADERS', event.headers);
  return terminalText(output);
}
