;;;; Emacs functions for trawl

(defun trawl-browse-module () (interactive)
  "Browse module haddock in w3m"
  (let ((module (read-from-minibuffer "Module: ")))
    (trawl-browse-output "--stack" "-m" module)))

(defun trawl-browse-package () (interactive)
  "Browse package haddock in w3m"
   (let ((package (read-from-minibuffer "Package: ")))
     (trawl-browse-output "--stack" "-p" package)))

(defun trawl-browse-output (&rest args)
  "Invoke trawl with ARGS and browse output path in w3m"
  (with-temp-buffer
    (let ((exit (apply 'call-process "trawl" nil (current-buffer) nil args))
	  (output (buffer-string)))
      (if (> exit 0)
	  (message (concat "trawl process failed: " output))
	  (w3m-browse-url output)))))
