-- Agent plans start with "# Title" and often a "Status: proposed" line, not
-- YAML frontmatter. Lift both into metadata so the template can render a
-- proper masthead instead of an h1 floating in the body.
--
-- Explicit YAML frontmatter always wins. If there is no title anywhere, fall
-- back to the `fallback-title` metadata the caller supplies (render-plan passes
-- the filename). That fallback must NOT be passed as `title` directly - doing
-- so looks like explicit frontmatter and suppresses lifting the "# Title".

local LIFTABLE = {
  status = true, owner = true, author = true, date = true,
  repo = true, ticket = true, issue = true, ["last-updated"] = true,
}

function Pandoc(doc)
  local blocks = doc.blocks

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
