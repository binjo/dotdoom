;;; private/binjo/init.el -*- lexical-binding: t; -*-
;; 2018.07.18 @ FEYE

;; * Modules
(doom! :tools
       ;; debugger
       eval
       ;; snippets
       lookup
       pdf

       :completion
       (company +auto +childframe)
       (ivy +childframe +icons +prescient)
       ;; ivy

       :ui
       ;; vc-gutter
       doom
       doom-dashboard
       ;; (doom-modeline +new)
       modeline
       workspaces
       hl-todo
       ;; fci
       ;; nav-flash
       ;; neotree
       treemacs
       ;; pretty-code
       (popup
        +all
        +defaults)
       window-select
       :editor
       (evil
        +everywhere)
       file-templates
       rotate-text
       :emacs
       vc
       dired
       ;; electric
       ;; ediff
       ;; imenu
       :term
       eshell
       term

       :os
        macos

       :tools
       ;; editorconfig
       ;; ein
       gist
       ;; make
       magit
       ;; rgb
       ;; reference
       ;; upload
       ;; tmux
       ;; password-store
       lsp

       :lang
       ;; lsp
       data
       (python +conda +lsp +pyenv +pipenv +pyright)
       ;; ess
       ;; (latex
       ;;  +latexmk
       ;;  +skim)
       (org
        +attach
        +babel
        +capture
        +roam
        +noter
        +present
        +pretty)
       emacs-lisp
       ;; javascript
       markdown
       ;; sh
       (web +html)
       (go +lsp)
       lua
       (cc +lsp)
       (rust +lsp)
       (java +lsp)
       (csharp +lsp)

       ;; :app
       ;; sx
       ;; rss
       ;; ;; twitter
       :email
       (mu4e
        +gmail)
       ;; (write
       ;;  +wordnut
       ;;  +osxdict
       ;;  +synosaurus
       ;;  +langtool)

       :checkers
       ;; spellcheck
       ;; (syntax +childframe)

       :config
       (default +evil +bindings)
       ;; literate
       )
