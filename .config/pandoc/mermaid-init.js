/* Mermaid bootstrap for plan.html5.
 *
 * render-plan concatenates the mermaid bundle and this file into one
 * <script> appended to the page, so a rendered plan stays a single
 * self-contained file with no network access at view time.
 *
 * Every diagram is rendered twice - once light, once dark - and CSS picks
 * which copy is visible. The alternative (render once for the current
 * theme) breaks printing: the page always prints light, and an SVG whose
 * colours were baked while the screen theme was dark comes out unreadable.
 */
(function () {
  var mermaid = window.mermaid;
  if (!mermaid) return;

  // Catppuccin Latte / Macchiato, mirroring the palette in plan.html5.
  var LIGHT = {
    bg: '#eff1f5', surface: '#e6e9ef', border: '#bcc0cc', text: '#4c4f69',
    blue: '#1e66f5', mauve: '#8839ef', teal: '#179299', peach: '#fe640b',
  };
  var DARK = {
    bg: '#1e2030', surface: '#363a4f', border: '#5b6078', text: '#cad3f5',
    blue: '#8aadf4', mauve: '#c6a0f6', teal: '#8bd5ca', peach: '#f5a97f',
  };

  function config(p) {
    return {
      startOnLoad: false,
      securityLevel: 'strict',
      // We report failures ourselves next to the offending source; mermaid's
      // own error graphic would otherwise be injected into the page body.
      suppressErrorRendering: true,
      fontFamily: 'ui-sans-serif, -apple-system, "Segoe UI", sans-serif',
      theme: 'base',
      themeVariables: {
        darkMode: p === DARK,
        background: p.bg,
        primaryColor: p.surface,
        primaryTextColor: p.text,
        primaryBorderColor: p.border,
        secondaryColor: p.bg,
        secondaryTextColor: p.text,
        secondaryBorderColor: p.border,
        tertiaryColor: p.bg,
        tertiaryTextColor: p.text,
        tertiaryBorderColor: p.border,
        lineColor: p.border,
        textColor: p.text,
        mainBkg: p.surface,
        nodeBorder: p.blue,
        clusterBkg: p.bg,
        clusterBorder: p.border,
        titleColor: p.text,
        edgeLabelBackground: p.bg,
        actorBkg: p.surface,
        actorBorder: p.blue,
        actorTextColor: p.text,
        signalColor: p.text,
        signalTextColor: p.text,
        labelBoxBkgColor: p.surface,
        labelBoxBorderColor: p.border,
        labelTextColor: p.text,
        loopTextColor: p.text,
        noteBkgColor: p.surface,
        noteBorderColor: p.peach,
        noteTextColor: p.text,
        activationBkgColor: p.surface,
        sectionBkgColor: p.bg,
        sectionBkgColor2: p.surface,
        altSectionBkgColor: p.surface,
        taskBkgColor: p.surface,
        taskTextColor: p.text,
        taskTextOutsideColor: p.text,
        taskBorderColor: p.blue,
        gridColor: p.border,
        todayLineColor: p.peach,
        pie1: p.blue, pie2: p.mauve, pie3: p.teal, pie4: p.peach,
        git0: p.blue, git1: p.mauve, git2: p.teal, git3: p.peach,
      },
    };
  }

  var blocks = Array.prototype.slice.call(document.querySelectorAll('pre.mermaid'));
  if (!blocks.length) return;

  // Snapshot the source before anything replaces it, and give the reader a
  // way back to it - a diagram you cannot copy is no use in an agent prompt.
  blocks.forEach(function (pre, i) {
    pre.dataset.src = pre.textContent;
    pre.dataset.idx = String(i);
  });

  function pass(variant, palette) {
    mermaid.initialize(config(palette));
    return Promise.all(blocks.map(function (pre) {
      var id = 'mmd-' + variant + '-' + pre.dataset.idx;
      return mermaid.render(id, pre.dataset.src)
        .then(function (out) { return { pre: pre, svg: out.svg, bind: out.bindFunctions }; })
        .catch(function (err) { return { pre: pre, error: err }; });
    }));
  }

  function mount(results, variant) {
    results.forEach(function (r) {
      var fig = r.pre.parentNode;
      if (r.error) {
        if (fig.querySelector('.diagram-error')) return;
        fig.classList.add('is-error');
        var msg = document.createElement('p');
        msg.className = 'diagram-error';
        msg.textContent = 'mermaid: ' + (r.error && r.error.message ? r.error.message : r.error);
        fig.insertBefore(msg, r.pre);
        r.pre.hidden = false;
        return;
      }
      var holder = document.createElement('div');
      holder.className = 'diagram-svg is-' + variant;
      holder.innerHTML = r.svg;
      if (r.bind) r.bind(holder);
      fig.appendChild(holder);
      r.pre.hidden = true;
    });
  }

  function addSourceToggle(fig, pre) {
    if (fig.classList.contains('is-error')) return;
    var bar = document.createElement('div');
    bar.className = 'diagram-bar';
    var btn = document.createElement('button');
    btn.type = 'button';
    btn.textContent = 'source';
    btn.addEventListener('click', function () {
      pre.hidden = !pre.hidden;
      btn.textContent = pre.hidden ? 'source' : 'hide';
      btn.classList.toggle('done', !pre.hidden);
    });
    var copy = document.createElement('button');
    copy.type = 'button';
    copy.textContent = 'copy';
    copy.addEventListener('click', function () {
      navigator.clipboard.writeText(pre.dataset.src).then(function () {
        copy.textContent = 'copied';
        copy.classList.add('done');
        setTimeout(function () { copy.textContent = 'copy'; copy.classList.remove('done'); }, 1400);
      });
    });
    bar.appendChild(btn);
    bar.appendChild(copy);
    fig.appendChild(bar);
  }

  pass('light', LIGHT)
    .then(function (r) { mount(r, 'light'); return pass('dark', DARK); })
    .then(function (r) { mount(r, 'dark'); })
    .then(function () {
      blocks.forEach(function (pre) { addSourceToggle(pre.parentNode, pre); });
    });
})();
