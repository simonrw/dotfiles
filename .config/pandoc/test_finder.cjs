// Run with: node --test .config/pandoc/test_finder.cjs
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { test } = require('node:test');
const vm = require('node:vm');

const template = readFileSync(__dirname + '/templates/plan.html5', 'utf8');
const source = template.match(/function fuzzyScore\(value, query\) \{[\s\S]*?^  \}/m)[0];
const fuzzyScore = vm.runInNewContext('(' + source + ')');

test('matches non-contiguous characters without regard to case', () => {
  assert.notEqual(fuzzyScore('nested/A & B.md', 'NAB'), null);
  assert.equal(fuzzyScore('nested/A & B.md', 'NZB'), null);
});

test('ranks compact matches ahead of scattered matches', () => {
  assert.ok(fuzzyScore('notes.md', 'notes') < fuzzyScore('nested/other-notes.md', 'notes'));
});
