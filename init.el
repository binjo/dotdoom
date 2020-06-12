;;; private/binjo/init.el -*- lexical-binding: t; -*-
;; 2018.07.18 @ FEYE

;; * Modules
(doom! :tools
       ;; debugger
       eval
       ;; snippets
       lookup

       :completion
       (company +auto +childframe)
       (ivy +childframe +icons)
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
       ;; treemacs
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

       :tools
       ;; editorconfig
       ;; ein
       gist
       macos
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
       (python +lpy +conda +lsp +pyenv +pipenv)
       ;; ess
       ;; (latex
       ;;  +latexmk
       ;;  +skim)
       (org
        +attach
        +babel
        +capture
        +roam
        +present)
       emacs-lisp
       ;; javascript
       markdown
       ;; sh
       (web +html)
       (go +lsp)
       lua
       (cc +lsp)

       ;; :app
       ;; sx
       ;; rss
       ;; ;; twitter
       ;; email
       ;; (write
       ;;  +wordnut
       ;;  +osxdict
       ;;  +synosaurus
       ;;  +langtool)

       :checkers
       ;; spellcheck
       (syntax +childframe)

       :config
       (default +snippets +evil-commands +bindings))
