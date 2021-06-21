;; -*- no-byte-compile: t; -*-
;;; private/default/packages.el

;; (package! template
;;   :recipe (:fetcher github
;;            :repo "binjo/template"
;;            :files ("lisp/template.el" "templates")))

;; (package! yara-mode
;;   :recipe (:host github
;;            :repo "binjo/yara-mode"))

(package! osx-dictionary)

(package! org-gcal)

(package! org-super-agenda)

(package! rime)

(when (featurep! :email mu4e)
  (package! mu4e-thread-folding
    :recipe (:host github
             :repo "rougier/mu4e-thread-folding")))

(package! valign)
