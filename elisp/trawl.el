;;;; Emacs functions for trawl

(require 'haskell-mode)
(require 'intero)

(defun trawl-browse-module () (interactive)
  "Browse module haddock"
  (let ((module (read-string "Module: " (haskell-ident-at-point))))
    (trawl-browse-output "--stack" "-m" module)))

(defun trawl-browse-member () (interactive)
  "Browse haddock for member of module"
  (let* ((member (read-string "Member: " (trawl-member-at-point))))
    (trawl-browse-output "--stack" "-v" member)))

(defun trawl-browse-package () (interactive)
  "Browse package haddock"
   (let ((package (read-from-minibuffer "Package: ")))
     (trawl-browse-output "--stack" "-p" package)))

(defun trawl-browse-output (&rest args)
  "Invoke trawl with ARGS and browse output path"
  (with-temp-buffer
    (let ((exit (apply 'call-process "trawl" nil (current-buffer) nil args))
	  (output (buffer-string)))
      (if (> exit 0)
	  (message (concat "trawl process failed: " output))
	  (browse-url output)))))

(defun trawl-member-at-point ()
  "Get the fully qualified name of the module member at point"
  (letrec ((ident (intero-ident-at-point))
           (result (intero-get-info-of ident)))
    (string-match "-- Defined in .\\(.+\\).$" result)
    (concat
      (match-string 1 result)
      "."
      (trawl-unqualify-ident ident))))

(defun trawl-unqualify-ident (ident)
  "Return the unqualified version of a qualified identifier"
  (string-match "^\\([a-zA-Z0-9_]+[.]\\)*\\(.+\\)$" ident)
  (match-string 2 ident))
