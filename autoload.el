;;; autoload.el --- autoload file of my private module

;; Copyright 2018 Binjo
;;
;; Author: Binjo
;; Version: $Id: autoload.el,v 0.0 2018/07/18 05:49:25 binjo Exp $
;; Keywords:
;; X-URL: not distributed yet

;;; Commentary:

;;

;;;###autoload
(defun binjo/org-capture-todo ()
  (interactive)
  (org-capture nil "t"))

;;;###autoload
(defun binjo/org-capture-remember ()
  (interactive)
  (org-capture nil "c"))

;;;###autoload
(defun binjo-unify-line (begin end)
  "wipe out the same line of specified region"
  (interactive "r")
  (goto-char begin)
  (let ((line "")
        (unified ()))
    (save-excursion
      (while (not (eobp))
        (setq line (downcase (buffer-substring-no-properties
                              (line-beginning-position) (line-end-position))))
        (unless (assoc-string line unified)
          (if (null unified)
              (setq unified (list (cons line 1)))
            (setq unified (append  (list (cons line 1)) unified))))
        (forward-line))
      (pop-to-buffer (get-buffer-create "*unified-line*"))
      (erase-buffer)
      (mapc (lambda (x)
              (insert (car x))
              (insert "\n")) (reverse unified)))))

;;
;; eshell utils
;;
;;;###autoload
(defun eshell/cls()
  "Clearing EShell Buffer."
  (interactive)
  (let ((inhibit-read-only t))
    (erase-buffer)))

;;;###autoload
(defun eshell/url (lru)
  "Decrypt LRU(url) from %u7468%u7074%u2f3a to 'http:/'."
  (interactive)
  (let ((a_url (split-string lru "%u"))
        (a ""))
    (mapc (lambda (x)
              (unless (string= x "")
                (unless (string= (substring x 2 4) "00")
                  (setq a (concat a (format "%c" (string-to-number (substring x 2 4) 16)))))
                (unless (string= (substring x 0 2) "00")
                  (setq a (concat a (format "%c" (string-to-number (substring x 0 2) 16)))))))
          a_url)
    (kill-new a t)
    a))

;;;###autoload
(defun eshell/unhex (hex-string)
  "Convert hexlified HEX-STRING to normal."
  (interactive)
  (let ((a "")
        (i 0)
        (len (length hex-string)))
    (while (< i len)
      (setq a (concat a (format "%c" (string-to-number (substring hex-string i (+ i 2)) 16))))
      (setq i (+ i 2)))
    (kill-new a t)
    a))

;;;###autoload
(defun eshell/hex (string)
  "Convert STRING to hex, a.k.a hexlify."
  (interactive)
  (let ((hexlified "")
        (a_s (string-to-list string)))
    (mapc (lambda (x)
            (setq hexlified (concat hexlified
                                    (format "%02x" x))))
          a_s)
    (kill-new hexlified t)
    hexlified))

;;;###autoload
(defun eshell/h2b (file-name hex)
  "Convert encoded HEX to binary, save as FILE-NAME."
  (interactive)
  (let ((a_h (split-string hex "%u"))
        (coding-system-for-write 'raw-text-unix))
    (with-temp-file file-name
      (mapc (lambda (x)
              (unless (string= x "")
                (insert-byte (string-to-number (substring x 2 4) 16) 1)
                (insert-byte (string-to-number (substring x 0 2) 16) 1)))
            a_h))
    "[*] Done...Check it out..."))

;;;###autoload
(defun eshell/a2b (file-name asciis)
  "Convert encoded ASCIIS to binary, save as FILE-NAME."
  (interactive)
  (let ((len (length asciis))
        (coding-system-for-write 'raw-text-unix)
        (i 0))
    (if (eq 0 (% len 2))
        (progn
         (with-temp-file file-name
           (while (< i len)
             (insert-byte (string-to-number (substring asciis i (+ i 2)) 16) 1)
             (setq i (+ i 2))))
         "[+] Done, check it out...")
    "[-] shit, length even? ...")))

;;;###autoload
(defun binjo/refile-to-roam-file (arg file)
  "Refile current heading to a particular org roam file."
  (interactive
   (list current-prefix-arg
         (ivy-completing-read "Select file to refile to: "
                              'read-file-name-internal
                              'file-exists-p
                              nil org-directory)))
  (+org/refile-to-current-file arg file))

;;;###autoload
(defun binjo/refile-to-roam-today-file (arg file)
  "Refile current heading to org roam today file."
  (interactive
   (list current-prefix-arg
         (expand-file-name (format-time-string "%Y-%m-%d.org")
                           (concat
                            (file-name-as-directory org-roam-directory)
                            org-roam-dailies-directory))))
  (+org/refile-to-current-file arg file))

;; copy&renamed from https://github.com/telotortium/doom.d/blob/f0295cc510d31c813e2a61ab2fc4aad3ae531e49/org-config.el#L1250-L1271
;; let user decide if certain part should be stripped
;;;###autoload
(defun binjo/refile-from-headline ()
  "Create an Org-roam note from the current headline and jump to it.
Normally, insert the headline’s title using the ’#title:’ file-level property
and delete the Org-mode headline. However, if the current headline has a
Org-mode properties drawer already, keep the headline and don’t insert
‘#+title:'. Org-roam can extract the title from both kinds of notes, but using
‘#+title:’ is a bit cleaner for a short note, which Org-roam encourages."
  (interactive)
  (let ((title (nth 4 (org-heading-components)))
        (has-properties (org-get-property-block)))
    (org-cut-subtree)
    (org-roam-find-file title nil nil 'no-confirm)
    (org-paste-subtree)
    (unless has-properties
      ;; (kill-line)
      (while (outline-next-heading)
        (org-promote)))
    (goto-char (point-min))
    (outline-next-heading)))

;;; autoload.el ends here
