;;; init.el -*- lexical-binding: t; -*-

(doom! :completion
       corfu
       vertico

       :ui
       doom
       doom-dashboard
       hl-todo
       modeline
       ophints
       (popup +defaults)
       (vc-gutter +pretty)
       vi-tilde-fringe
       zen

       :editor
       (evil +everywhere)
       file-templates
       fold
       snippets
       word-wrap

       :emacs
       dired
       electric
       undo
       vc

       :term
       vterm

       :checkers
       syntax
       (spell +flyspell)

       :tools
       editorconfig
       (eval +overlay)
       (lsp +eglot)
       lookup
       (magit +forge)
       tree-sitter

       :os
       (:if (featurep :system 'macos) macos)

       :lang
       emacs-lisp
       (go +lsp +tree-sitter)
       (json +lsp +tree-sitter)
       (javascript +lsp +tree-sitter)
       (markdown +grip)
       (org +pretty)
       (python +lsp +tree-sitter)
       (rust +lsp +tree-sitter)
       sh
       (web +lsp +tree-sitter)
       (yaml +lsp +tree-sitter)

       :config
       (default +bindings +smartparens))
