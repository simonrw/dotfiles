/* XXX which one of these works I wonder? */
require(['jquery', 'base/js/namespace', 'notebook/js/codecell', 'notebook/js/cell'], function($, IPython, code_cell, cell) {
    $([IPython.events]).on('notebook_loaded.Notebook',function () {
        /* Hide ui elements */
        /* This causes visual bugs which I don't like. Hence I'm
         * disabling this behaviour - SRW 2017/03/01
        $('div#maintoolbar').hide();
        $('div#header-container').hide();
        */
    });
    code_cell.CodeCell.options_default.cm_config.autoCloseBrackets = false;
    cell.Cell.options_default.cm_config.lineWrapping = true;
});

// require(['base/js/namespace', 'jquery', 'base/js/events'], function(IPython, $, events){
//     events.on('app_initialized.NotebookApp', function(){
//         $('#header-container').hide();
//         $('.header-bar').hide();
//         $('div#maintoolbar').hide();
//         IPython.menubar._size_header();
//     })
//     console.log('Header and toolbar should be hidden by default now')
// })
