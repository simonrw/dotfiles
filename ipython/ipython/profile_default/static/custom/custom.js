// Hide the toolbar by default
$([IPython.events]).on('notebook_loaded.Notebook', function(){
    $('div#maintoolbar').hide();
    IPython.layout_manager.do_resize();
});
