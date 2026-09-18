import type { IncomingMessage } from 'node:http';

const REDACTED = '[REDACTED]';
const credentialPattern = /\b(?:bearer|basic)\s+[a-z0-9._~+/=-]+|\b(?:sk-|gh[pousr]_|github_pat_|xox[baprs]-)[a-z0-9_-]+|\beyJ[a-z0-9_-]+\.[a-z0-9_-]+\.[a-z0-9_-]+/gi;
const privateKeyPattern = /-----BEGIN [A-Z ]*PRIVATE KEY-----[\s\S]*?-----END [A-Z ]*PRIVATE KEY-----/g;
const assignmentPattern = /\b([a-z0-9_-]*(?:api[_-]?key|token|secret|password|passwd|credential|authorization)[a-z0-9_-]*\s*[=:]\s*)(?:"[^"\r\n]*"|'[^'\r\n]*'|[^\s,;&]+)/gi;
const visibleHeaders = new Set(['content-type', 'content-length', 'content-encoding', 'accept']);

function sensitiveName(name: string): boolean {
  const normalized = name.toLowerCase().replace(/[-_.]/g, '');
  return /^(key|token|auth|signature)$/.test(normalized)
    || /(authorization|apikey|password|passwd|secret|credential|cookie|privatekey)/.test(normalized)
    || normalized.endsWith('token');
}

function embeddedJSON(value: string): unknown {
  if (/^\s*[{[]/.test(value)) {
    try { return JSON.parse(value); } catch { /* Not JSON: redact it as text. */ }
  }
  return undefined;
}

/** Redact an inspection copy only. Never use these values for forwarding. */
export class Redactor {
  private secrets = new Set<string>();

  constructor(request: IncomingMessage) {
    for (const [name, value] of Object.entries(request.headers)) {
      if (visibleHeaders.has(name)) continue;
      for (const item of Array.isArray(value) ? value : [value]) {
        if (!item) continue;
        this.remember(item);
        if (name.includes('authorization')) this.remember(item.split(/\s+/, 2)[1]);
      }
    }
    for (const cookie of (request.headers.cookie ?? '').split(';')) {
      const equals = cookie.indexOf('=');
      if (equals >= 0) this.remember(cookie.slice(equals + 1).trim());
    }
    const query = (request.url ?? '').split('?').slice(1).join('?');
    for (const value of new URLSearchParams(query).values()) this.remember(value);
  }

  private remember(value: unknown): void {
    if (typeof value !== 'string' || value.length === 0) return;
    this.secrets.add(value);
    this.secrets.add(encodeURIComponent(value));
  }

  // Collect first, so a secret repeated in an earlier prompt is also removed.
  collect(value: unknown, sensitive = false, depth = 0): void {
    if (depth > 64) throw new Error('Inspection nesting limit');
    if (typeof value === 'string') {
      if (sensitive) this.remember(value);
      const nested = embeddedJSON(value);
      if (nested !== undefined) this.collect(nested, sensitive, depth + 1);
    } else if (Array.isArray(value)) {
      for (const item of value) this.collect(item, sensitive, depth + 1);
    } else if (value && typeof value === 'object') {
      for (const [key, item] of Object.entries(value)) {
        this.collect(item, sensitive || sensitiveName(key), depth + 1);
      }
    }
  }

  text(value: string): string {
    for (const secret of [...this.secrets].sort((a, b) => b.length - a.length)) {
      value = value.split(secret).join(REDACTED);
    }
    value = value.replace(privateKeyPattern, REDACTED)
      .replace(credentialPattern, REDACTED)
      .replace(assignmentPattern, `$1${REDACTED}`);
    return value.replace(/https?:\/\/[^\s<>"']+/g, raw => {
      try {
        const url = new URL(raw);
        if (url.username || url.password) {
          url.username = REDACTED;
          url.password = '';
        }
        if (url.search) url.search = '?[query redacted]';
        if (url.hash) url.hash = REDACTED;
        return url.toString();
      } catch {
        return '[URL omitted]';
      }
    });
  }

  value(value: unknown, depth = 0): unknown {
    if (depth > 64) throw new Error('Inspection nesting limit');
    if (typeof value === 'string') {
      const nested = embeddedJSON(value);
      return nested === undefined ? this.text(value) : JSON.stringify(this.value(nested, depth + 1));
    }
    if (Array.isArray(value)) return value.map(item => this.value(item, depth + 1));
    if (value && typeof value === 'object') {
      return Object.fromEntries(Object.entries(value).map(([key, item]) => [
        this.text(key), sensitiveName(key) ? REDACTED : this.value(item, depth + 1),
      ]));
    }
    return value;
  }

  headers(request: IncomingMessage): Record<string, unknown> {
    return Object.fromEntries(Object.entries(request.headers).map(([name, value]) => [
      this.text(name), visibleHeaders.has(name) ? this.value(value) : REDACTED,
    ]));
  }

  path(raw: string): string {
    const [path] = raw.split('?');
    return this.text(path) + (raw.includes('?') ? '?[query redacted]' : '');
  }
}
