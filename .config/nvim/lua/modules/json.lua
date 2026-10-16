local M = {}

function M.path_at_cursor()
    local node = vim.treesitter.get_node()
    if not node then
        return nil
    end

    local segments = {}
    while node do
        local parent = node:parent()
        if not parent then
            break
        end

        if node:type() == 'pair' then
            local key = node:field('key')[1]
            table.insert(segments, '[' .. vim.treesitter.get_node_text(key, 0) .. ']')
        elseif parent:type() == 'array' then
            local index = 0
            for child in parent:iter_children() do
                if child:named() then
                    if child:id() == node:id() then
                        break
                    end
                    index = index + 1
                end
            end
            table.insert(segments, '[' .. index .. ']')
        end
        node = parent
    end

    local path = '.'
    for i = #segments, 1, -1 do
        path = path .. segments[i]
    end
    return path
end

function M.copy_path()
    local path = M.path_at_cursor()
    if not path then
        vim.notify('No JSON value under the cursor', vim.log.levels.WARN)
        return
    end
    vim.fn.setreg('+', path)
    vim.notify('Copied jq path: ' .. path)
end

return M
