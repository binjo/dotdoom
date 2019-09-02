;; -*- no-byte-compile: t; -*-
;;; private/default/packages.el

;; (package! template
;;   :recipe (:fetcher github
;;            :repo "binjo/template"
;;            :files ("lisp/template.el" "templates")))

(package! yara-mode
  :recipe (:host github
           :repo "binjo/yara-mode"))

(package! osx-dictionary)

(package! org-gcal)
