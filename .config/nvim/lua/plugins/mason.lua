return {
    {
        "mason-org/mason-lspconfig.nvim",
        dependencies = {
            { "mason-org/mason.nvim", opts = {} },
            "neovim/nvim-lspconfig",
        },
        config = function()
            require("mason-lspconfig").setup({
                ensure_installed = {
                    -- Don't include rust_analyzer as it's usually installed by rustup
                    "basedpyright",
                    "lua_ls",
                },
            })

            -- install additional requirements that are not allowed with the ensure_installed key
            local a = require("plenary.async")
            local reg = require("mason-registry")

            local packages = { "debugpy", "stylua", "shellcheck", "ruff" }

            for _, package_name in ipairs(packages) do
                a.run(function()
                    if not reg.is_installed(package_name) then
                        Snacks.notify('Installing ' .. package_name)
                        local package = reg.get_package(package_name)
                        package:install({}, function(success, error)
                            if not success then
                                Snacks.notify.error('Error installing ' .. package_name .. ': ' .. error)
                            end
                        end)
                    end
                end)
            end
        end,
    }
}
