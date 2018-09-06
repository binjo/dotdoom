;; -*- no-byte-compile: t; -*-
;;; private/default/packages.el

;; (package! template
;;   :recipe (:fetcher github
;;            :repo "binjo/template"
;;            :files ("lisp/template.el" "templates")))

(package! yara-mode
  :recipe (:fetcher github
           :repo "binjo/yara-mode"))

(package! osx-dictionary)
