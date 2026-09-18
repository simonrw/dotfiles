import assert from 'node:assert/strict';
import { test } from 'node:test';
import { formatEvent } from './format.mts';

function request(body: unknown) {
  return { event: 'request', id: 1, method: 'POST', path: '/responses', time: '2026-09-18', body };
}

test('prints Responses system instructions and harness messages as readable text in sent order', () => {
  const output = formatEvent(request({
    instructions: 'You are a coding assistant.\nRead files before editing.',
    input: [
      { role: 'system', content: 'System message' },
      { role: 'developer', content: [{ type: 'input_text', text: '<environment_context>\nUse the sandbox.\n</environment_context>' }] },
      { role: 'user', content: [{ type: 'input_text', text: '# AGENTS.md\nUse TypeScript.' }] },
      { role: 'user', content: 'Build a proxy.' },
      { role: 'assistant', content: [{ type: 'output_text', text: 'I will inspect the files.' }] },
      { type: 'function_call', name: 'read_file', call_id: 'call_1', arguments: '{"path":"README.md"}' },
      { type: 'function_call_output', call_id: 'call_1', output: 'File contents' },
    ],
    tools: [{ type: 'function', name: 'read_file', description: 'Read a file' }],
    model: 'test-model', stream: true,
  }));
  for (const heading of ['SYSTEM / DEVELOPER INSTRUCTIONS [instructions]', 'SYSTEM [input[0]]',
    'DEVELOPER [input[1]]', 'USER [input[2]]', 'ASSISTANT [input[4]]',
    'FUNCTION_CALL [input[5]]', 'FUNCTION_CALL_OUTPUT [input[6]]', 'TOOL DEFINITIONS [tools]']) {
    assert.ok(output.includes(heading), heading);
  }
  assert.ok(output.includes('You are a coding assistant.\n  Read files before editing.'));
  assert.ok(output.includes('<environment_context>\n  Use the sandbox.\n  </environment_context>'));
  assert.ok(output.includes('# AGENTS.md\n  Use TypeScript.'));
  assert.ok(output.indexOf('System message') < output.indexOf('Use the sandbox.'));
  assert.ok(output.indexOf('Use TypeScript.') < output.indexOf('Build a proxy.'));
  assert.ok(output.includes('File contents'));
  assert.ok(output.includes('test-model'));
});

test('prints Chat Completions roles and tool metadata without losing multimodal blocks', () => {
  const output = formatEvent(request({ messages: [
    { role: 'system', content: 'System prompt\nSecond line' },
    { role: 'developer', content: 'Harness prompt' },
    { role: 'user', content: [{ type: 'text', text: 'Describe this' }, { type: 'image_url', image_url: { url: 'test-image' } }] },
    { role: 'assistant', content: null, tool_calls: [{ id: 'tool_1', function: { name: 'read_file' } }] },
    { role: 'tool', content: 'Tool output', tool_call_id: 'tool_1' },
  ] }));
  assert.ok(output.includes('SYSTEM [messages[0]]'));
  assert.ok(output.includes('DEVELOPER [messages[1]]'));
  assert.ok(output.includes('System prompt\n  Second line'));
  assert.ok(output.includes('image_url'));
  assert.ok(output.includes('test-image'));
  assert.ok(output.includes('tool_calls'));
  assert.ok(output.includes('tool_call_id'));
  assert.ok(output.includes('TOOL [messages[4]]'));
});

test('prints Anthropic system text blocks and tool use/result messages', () => {
  const output = formatEvent(request({
    system: [{ type: 'text', text: 'Base system prompt', cache_control: { type: 'ephemeral' } },
      { type: 'text', text: 'Additional harness prompt\nFollow project rules.' }],
    messages: [
      { role: 'assistant', content: [{ type: 'tool_use', id: 'tool_1', name: 'read_file', input: { path: 'README.md' } }] },
      { role: 'user', content: [{ type: 'tool_result', tool_use_id: 'tool_1', content: 'Project rules' }] },
    ],
  }));
  for (const text of ['SYSTEM [system]', 'Base system prompt', 'Additional harness prompt\n  Follow project rules.',
    'cache_control', 'ephemeral', 'tool_use', 'tool_result', 'Project rules']) assert.ok(output.includes(text), text);
});

test('supports string input and preserves unknown request fields', () => {
  const output = formatEvent(request({ input: 'Plain user input\nSecond line', custom_prompt: 'Other harness context' }));
  assert.ok(output.includes('USER [input]'));
  assert.ok(output.includes('Plain user input\n  Second line'));
  assert.ok(output.includes('Other harness context'));
});

test('escapes terminal commands and misleading control characters but preserves newlines and tabs', () => {
  const output = formatEvent(request({ instructions: 'First\n\tIndented\x1b[2J\r\x07\u009b\u202eEnd' }));
  assert.ok(output.includes('First\n  \tIndented'));
  assert.ok(output.includes('\\u001b[2J\\u000d\\u0007\\u009b\\u202eEnd'));
  assert.doesNotMatch(output, /[\x00-\x08\x0b-\x1f\x7f-\x9f\u202e]/);
});

test('prints body omissions and leaves JSON output machine-readable', () => {
  const event = { event: 'request', id: 2, body_note: 'Inspection limit exceeded; increase --max-body' };
  assert.ok(formatEvent(event).includes('BODY NOT SHOWN'));
  assert.ok(formatEvent(event).includes('increase --max-body'));
  const input = request({ instructions: 'Line one\nLine two' });
  assert.deepEqual(JSON.parse(formatEvent(input, 'json')), input);
  assert.deepEqual(JSON.parse(formatEvent(input, 'jsonl')), input);
  assert.equal(formatEvent(input, 'jsonl').trim().split('\n').length, 1);
});
