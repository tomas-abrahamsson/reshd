;; This insures that (require 'artist) will work correctly while
;; byte-compiling.

(setq load-path (cons nil load-path))

;; Make sure user is

(if (and (not (string-match "^19\\." emacs-version))
	 (not (string-match "^20\\." emacs-version)))
    (message
     (concat "\nWARNING - Artist requires version 19 or 20 of GNU Emacs.\n"
	     "Your version is:\n"
	     (emacs-version)
	     "\n")))
