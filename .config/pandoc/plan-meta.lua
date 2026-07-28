-- Agent plans start with "# Title" and often a "Status: proposed" line, not
-- YAML frontmatter. Lift both into metadata so the template can render a
-- proper masthead instead of an h1 floating in the body.
--
-- Explicit YAML frontmatter always wins. If there is no title anywhere, fall
-- back to the `fallback-title` metadata the caller supplies (render-plan passes
-- the filename). That fallback must NOT be passed as `title` directly - doing
-- so looks like explicit frontmatter and suppresses lifting the "# Title".

-- Mermaid fences are handed to the browser as-is: pandoc must not syntax
-- highlight them, and the template's code-block chrome must not wrap them.
-- Emitting raw HTML here (rather than a plain CodeBlock the JS picks up later)
-- keeps the source out of the highlighter and marks the block unambiguously.
-- render-plan sets `no-mermaid` when it has no bundle to inline (or was told
-- not to); the fences then stay ordinary code blocks rather than becoming
-- diagram shells nothing will ever draw into.
local function html_escape(s)
  return s:gsub("&", "&amp;"):gsub("<", "&lt;"):gsub(">", "&gt;")
end

local function mermaid_block(block)
  if block.classes[1] ~= "mermaid" then return nil end
  return pandoc.RawBlock("html",
    '<figure class="diagram"><pre class="mermaid">'
      .. html_escape(block.text)
      .. "</pre></figure>")
end

local LIFTABLE = {
  status = true, owner = true, author = true, date = true,
  repo = true, ticket = true, issue = true, ["last-updated"] = true,
}

function Pandoc(doc)
  local blocks = doc.blocks

  -- Done here rather than in a CodeBlock filter because the decision depends
  -- on metadata, which is only available once the whole document is in hand.
  if not doc.meta["no-mermaid"] then
    blocks = doc:walk({ CodeBlock = mermaid_block }).blocks
  end
  doc.meta["no-mermaid"] = nil

  if not doc.meta.title and blocks[1] and blocks[1].t == "Header" and blocks[1].level == 1 then
    doc.meta.title = pandoc.MetaInlines(blocks[1].content)
    table.remove(blocks, 1)
  end

  -- Consume leading "Key: value" paragraphs, but only ones we recognise, so a
  -- plan opening with a normal sentence containing a colon is left alone.
  while blocks[1] and blocks[1].t == "Para" do
    local text = pandoc.utils.stringify(blocks[1])
    local key, value = text:match("^([%w%-]+):%s*(.+)$")
    if not key or value:find("\n") then break end
    key = key:lower()
    if not LIFTABLE[key] then break end
    if not doc.meta[key] then doc.meta[key] = pandoc.MetaString(value) end
    table.remove(blocks, 1)
  end

  if not doc.meta.title then
    doc.meta.title = doc.meta["fallback-title"]
  end
  doc.meta["fallback-title"] = nil

  -- Keep the browser tab label in step with the masthead.
  if not doc.meta.pagetitle then
    doc.meta.pagetitle = doc.meta.title
  end

  return pandoc.Pandoc(blocks, doc.meta)
end
