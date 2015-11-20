require(['jquery', 'base/js/namespace', 'notebook/js/codecell', 'notebook/js/cell'], function($, IPython, code_cell, cell) {
    $([IPython.events]).on('notebook_loaded.Notebook',function () {
        /* Hide ui elements */
        $('div#maintoolbar').hide();
        $('div#header-container').hide();
    });
    code_cell.CodeCell.options_default.cm_config.autoCloseBrackets = false;
    cell.Cell.options_default.cm_config.lineWrapping = true;
});
