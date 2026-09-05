// Run with: node --test .config/pandoc/test_comments.cjs
const assert = require('node:assert/strict');
const { readFileSync } = require('node:fs');
const { test } = require('node:test');
const vm = require('node:vm');

const template = readFileSync(__dirname + '/templates/plan.html5', 'utf8');
const script = template.match(/<script>\s*(\(function \(\) \{\s*\/\* ---- review comments[\s\S]*?)<\/script>/)[1];

function indexReview() {
  const elements = [];
  const listeners = {};
  const storage = new Map();
  const documents = ['first', 'nested/second'].map(name => ({
    source: '/docs/' + name + '.md', name: name + '.md', url: '/' + name + '.md',
  }));
  const key = name => 'plan-comments:/docs/' + name + '.md';
  for (const name of ['first', 'nested/second', 'outside']) {
    storage.set(key(name), JSON.stringify([{
      id: name, quote: 'Quoted text', text: 'Feedback for ' + name,
      sectionTitle: 'Details', anchored: true,
    }]));
  }
  function element() {
    const el = {
      style: {}, children: [], events: {}, classList: { add() {}, remove() {}, toggle() {} },
      appendChild(child) { this.children.push(child); },
      addEventListener(name, fn) { this.events[name] = fn; },
      querySelectorAll() { return []; },
    };
    elements.push(el);
    return el;
  }
  const main = element();
  const window = {
    confirm: () => true,
    alert(message) { assert.fail(message); },
    addEventListener(name, fn) { listeners[name] = fn; },
  };
  vm.runInNewContext(script, {
    window, document: {
      title: 'Documents', body: element(), createElement: element,
      addEventListener() {},
      querySelector(selector) {
        if (selector === 'main') return main;
        if (selector.includes('plan-directory')) return { content: JSON.stringify(documents) };
        return null;
      },
    },
    localStorage: { getItem: key => storage.get(key), setItem: (key, value) => storage.set(key, value) },
  });
  return { review: window.__planReview, storage, key, elements, listeners };
}

test('directory feedback names each source and excludes unrelated documents', () => {
  const { review } = indexReview();
  assert.equal(review.comments().length, 2);
  const markdown = review.buildMarkdown();
  assert.match(markdown, /File: \/docs\/first.md/);
  assert.match(markdown, /File: \/docs\/nested\/second.md/);
  assert.match(markdown, /Section: Details/);
  assert.match(markdown, /Quoted text/);
  assert.match(markdown, /Feedback for nested\/second/);
  assert.doesNotMatch(markdown, /outside/);
});

test('clear removes directory comments, preserves unrelated feedback, and survives reload', () => {
  const { review, storage, key, elements, listeners } = indexReview();
  elements.find(el => el.textContent === 'Clear all').events.click();
  assert.equal(review.comments().length, 0);
  assert.deepEqual(JSON.parse(storage.get(key('first'))), []);
  assert.deepEqual(JSON.parse(storage.get(key('nested/second'))), []);
  assert.equal(JSON.parse(storage.get(key('outside'))).length, 1);
  listeners.storage({ key: null });
  assert.equal(review.comments().length, 0);
});

test('directory refreshes when another tab changes comments', () => {
  const { review, storage, key, listeners } = indexReview();
  storage.set(key('first'), '[]');
  listeners.storage({ key: key('first') });
  assert.equal(review.comments().length, 1);
  assert.equal(review.comments()[0].document.name, 'nested/second.md');
});
