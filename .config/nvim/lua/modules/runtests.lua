local M = {}

local get_nearest_test = function()
    local query = vim.treesitter.query.parse('python', [[
    (function_definition
    name: (identifier) @name
    (#match? @name "^test_.*$")) @test
]])
    local tree = vim.treesitter.get_parser():parse()[1]


    local cursor_pos = vim.fn.getpos(".")

    local cursor_in_range = function(cursor, range)
        local line = cursor[2] - 1
        -- local col = cursor[3] - 1

        if range.start_row >= line then
            return false
        end

        if range.end_row <= line then
            return false
        end


        return true
    end

    for id, node in query:iter_captures(tree:root(), 0) do
        local capture_name = query.captures[id]
        if capture_name == "test" then
            local row1, col1, row2, col2 = node:range()
            if cursor_in_range(cursor_pos, {
                    start_row = row1,
                    start_col = col1,
                    end_row = row2,
                    end_col = col2,
                }) then
                local test_name_node = node:field('name')[1]
                local test_name = vim.treesitter.get_node_text(test_name_node, vim.api.nvim_get_current_buf())
                return test_name
            end
        end
    end
end

local spawn_terminal = function(cmd)
    vim.cmd.tabnew()
    local term_buf = vim.api.nvim_create_buf(false, true)
    vim.api.nvim_set_current_buf(term_buf)
    vim.fn.jobstart(cmd, { term = true })
end

M.setup = function()
    vim.api.nvim_create_autocmd('FileType', {
        pattern = { 'python' },
        callback = function(ev)
            vim.keymap.set('n', 'tn', function()
                local test_name = get_nearest_test()
                local test_path = vim.api.nvim_buf_get_name(0)
                local command = { "pytest", test_path, "-k", test_name }
                spawn_terminal(command)
            end, { buffer = ev.buf })
        end,
    })
end

return M
--
