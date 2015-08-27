require(['notebook/js/codecell', 'notebook/js/cell'], function(code_cell, cell) {
    code_cell.CodeCell.options_default.cm_config.autoCloseBrackets = false;
    cell.Cell.options_default.cm_config.lineWrapping = true;
});
