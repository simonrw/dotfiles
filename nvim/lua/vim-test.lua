nnoremap('tf', [[:update\|:TestFile<Cr>]])
nnoremap('tl', [[:update\|:TestLast<Cr>]])
nnoremap('tn', [[:update\|:TestNearest<Cr>]])
nnoremap('ta', [[:update\|:TestSuite<Cr>]])
nnoremap('ts', [[:update\|:TestSuite<Cr>]])

-- Configure runners
vim.g['test#python#runner'] = 'pytest'
vim.g['test#javascript#reactscripts#options'] = '--watchAll=false'


