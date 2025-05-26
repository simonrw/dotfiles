local event_log = {}

local buffer_text = {}

function get_text_from_bytes(buf_id, start, length)
end

function setup()
    vim.api.nvim_create_autocmd({ "InsertEnter" }, {
        callback = function(event)
            local line_count = vim.api.nvim_buf_line_count(event.buf)
            buffer_text[event.buf] = vim.api.nvim_buf_get_lines(event.buf, 0, line_count, true)
        end,
    })

    vim.api.nvim_create_autocmd({ "BufNew", "BufEnter" }, {
        callback = function(evt)
            vim.api.nvim_buf_attach(evt.buf, false, {
                -- - the string "bytes"
                -- - buffer id
                -- - b:changedtick
                -- - start row of the changed text (zero-indexed)
                -- - start column of the changed text
                -- - byte offset of the changed text (from the start of the buffer)
                -- - old end row of the changed text (offset from start row)
                -- - old end column of the changed text (if old end row = 0, offset from start column)
                -- - old end byte length of the changed text
                -- - new end row of the changed text (offset from start row)
                -- - new end column of the changed text (if new end row = 0, offset from start column)
                -- - new end byte length of the changed text
                on_bytes = function(
                    _,
                    buf_id,
                    _,
                    _,
                    _,
                    changed_byte_offset,
                    _,
                    _,
                    old_end_byte_length,
                    _,
                    _,
                    new_end_byte_length
                )
                    local old_text = get_text_from_bytes(buf_id, changed_byte_offset, old_end_byte_length)
                    table.insert(event_log, {
                        changed_byte_offset = changed_byte_offset,
                        old_end_byte_length = old_end_byte_length,
                        new_end_byte_length = new_end_byte_length,
                        old_text = old_text,
                    })
                end,
            })
        end,
    })


    vim.api.nvim_create_user_command("PrintEvents", function()
        vim.print(event_log)
    end, {})
end
