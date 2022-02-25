local null_ls = require("null-ls")

null_ls.setup({
    sources = {
        null_ls.builtins.formatting.black,
        null_ls.builtins.formatting.djhtml,
        null_ls.builtins.formatting.elm_format,
        null_ls.builtins.formatting.goimports,
        null_ls.builtins.diagnostics.flake8,
    },
    on_attach = function(client, bufnr)
        if client.resolved_capabilities.document_formatting then
            vim.cmd([[
            augroup LspFormatting
            autocmd! * <buffer>
            autocmd BufWritePre <buffer> lua vim.lsp.buf.formatting_sync()
            augroup END
            ]])
        end
    end,
})
