;;;; Emacs functions for trawl

(defun trawl-browse-module () (interactive)
  "Browse module haddock in w3m"
  (let ((module (read-from-minibuffer "Module: ")))
    (trawl-browse-output "--stack" "-m" module)))

(defun trawl-browse-member () (interactive)
  "Browse haddock for member of module in w3m"
  (let* ((member (read-from-minibuffer "Member: ")))
    (trawl-browse-output "--stack" "-v" member)))

(defun trawl-browse-package () (interactive)
  "Browse package haddock in w3m"
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
