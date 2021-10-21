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

(package! valign)

(package! vertico-posframe
  :recipe (:host github
           :repo "tumashu/vertico-posframe"))
