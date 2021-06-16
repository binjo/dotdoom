;;; private/default/+bindings.el -*- lexical-binding: t; -*-

;; This files defines a Spacemacs-esque keybinding scheme

(map!
 (:leader
   :nv "X" nil
   ;; :desc "org-capture" :nv "X" #'counsel-org-capture
   :desc "ivy-resume" :nv "$" #'ivy-resume
   :desc "recenter" :nv "l" #'recenter
   (:desc "code" :prefix "c"
     :desc "Clear flycheck warnings" :n "C" #'flycheck-clear
     :desc "Comment DWIM" :n "l" #'comment-dwim)
   (:desc "search" :prefix "s"
    :desc "Search via rg" :n "g" #'counsel-rg)))


(provide '+bindings)
;;; +bindings.el ends here
