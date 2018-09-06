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



;;; autoload.el ends here
