;;; resh.el -- emacs support for connecting to a reshd
;;;            (remote erlang shell daemon)
;;
;; $Id: resh.el,v 1.5 2001-05-04 09:57:34 tab Exp $
;;
;; Author: Tomas Abrahamsson <tab@lysator.liu.se>

;;; COPYRIGHT

;; These programs are released into the public domain.  You may do
;; anything you like with them, including modifying them and selling
;; the binaries without source for ridiculous amounts of money without
;; saying who made them originally.
;; 
;; However, I would be happy if you release your works with complete
;; source for free use.

;;; Installation:

;; Either:
;;
;;   (autoload 'resh "resh" "Start a connection to an erlang reshd" t)
;; 
;; or:
;;
;;   (load-library "resh")
;;   (resh-install)		; installs keymaps
;;
;; The difference is that in the second case, the resh is bound to
;; C-c c immediately, while in the first case no key bindings are
;; installed until you have typed M-x resh for the first time.

;;; Code

(require 'erlang)

;; User definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
(defvar resh-default-host "localhost"
  "*Default hostname for `resh'.")

(defvar resh-default-port nil
  "*Default port (an integer) for `resh'.")
;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; End of user definable variables


(defvar resh-host-history nil
  "Host history for `resh'")

(defvar resh-port-history nil
  "Port history for `resh'")

(defvar resh-buff-history nil
  "Buffer name history for `resh'")

(defvar resh-current-host nil
  "Buffer-local variable, used for reconnection.")

(defvar resh-current-port nil
  "Buffer-local variable, used for reconnection.")

(defvar resh-is-installed nil
  "Whether resh is installed or not")

(defvar resh-auto-install-enabled t
  "Whether resh should autoinstall upon call to resh")


;;;###autoload
(defun resh (host port &optional reconnecting wanted-buffer-name)
  "Run an inferior remote Erlang shell.

The command line history can be accessed with  M-p  and  M-n.
The history is saved between sessions.

Entry to this mode calls the functions in the variables
`comint-mode-hook' and `erlang-shell-mode-hook' with no arguments.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{erlang-shell-mode-map}"
  (interactive
   ;; Handling of interactive calling
   (let* ((init-prompt "Remote erlang shell to")
	  (host-hist 'resh-host-history)
	  (port-hist 'resh-port-history)
	  (buff-hist 'resh-buff-history)
	  (host-prompt (concat init-prompt ": "))
	  (remote-host (read-string host-prompt resh-default-host host-hist))
	  (port-prompt (concat init-prompt " " remote-host " port: "))
	  (default-port (cond ((null resh-default-port) nil)
			      ((stringp resh-default-port) resh-default-port)
			      ((numberp resh-default-port)
			       (int-to-string resh-default-port))
			      (t nil)))
	  (remote-port-str (read-string port-prompt default-port port-hist))
	  (remote-port (cond ((string= "" remote-port-str)
			      (error "Not port number \"%s\"" remote-port-str))
			     (t (string-to-int remote-port-str))))
	  (buffer-prompt (concat init-prompt " "
				 remote-host ":" remote-port-str
				 ", buffer name: "))
	  (buffer-name (if current-prefix-arg
			   (read-string buffer-prompt nil buff-hist)
			 nil)))
     (list remote-host remote-port nil buffer-name)))

  (if (and (not resh-is-installed) resh-auto-install-enabled)
      (resh-install))

  (require 'comint)

  (let* ((proc-name (resh-buffer-name inferior-erlang-process-name host port))
	 (erl-buffer (make-comint proc-name (cons host port)))
	 (erl-process (get-buffer-process erl-buffer))
	 (erl-buffer-name (if wanted-buffer-name
			      wanted-buffer-name
			    (resh-buffer-name inferior-erlang-buffer-name
					      host port))))

    ;; Say no query needed if erl-process is running when Emacs is exited.
    (process-kill-without-query erl-process)

    ;; Switch to buffer in other or this window
    ;; the `erlang-inferior-shell-split-window' is a local extension
    ;; to the erlang mode.
    (if (and (boundp 'erlang-inferior-shell-split-window)
	     erlang-inferior-shell-split-window)
	(switch-to-buffer-other-window erl-buffer)
      (switch-to-buffer erl-buffer))

    ;; comint settings
    (if (and (not (eq system-type 'windows-nt))
	     (eq inferior-erlang-shell-type 'newshell))
	(setq comint-process-echoes nil))

    ;; Set buffer name and run erlang-shell-mode unless we are reconnecting
    (if reconnecting
	nil
      (condition-case nil
	  ;; `rename-buffer' takes only one argument in Emacs 18.
	  (rename-buffer erl-buffer-name t)
	(error (rename-buffer erl-buffer-name)))
      ;; remember the host/port so we can reconnect.
      (make-variable-buffer-local 'resh-current-host)
      (make-variable-buffer-local 'resh-current-port)
      (setq resh-current-host host)
      (setq resh-current-port port)
      (erlang-shell-mode))))

(defun resh-buffer-name (base host port)
  (let* ((host-port (concat host ":" (int-to-string port))))
    (if (string= (substring base -1) "*")
	(concat (substring base 0 -1) "-" host-port "*")
      (concat base "-" host-port))))

(defun resh-reconnect ()
  "Try to reconnect to a remote Erlang shell daemon."
  (interactive)
  (resh resh-current-host resh-current-port t))

(defun resh-set-inferior-erlang-buffer ()
  "Set current buffer to the inferior erlang buffer."
  (interactive)
  (setq inferior-erlang-buffer (current-buffer))
  (message "This buffer is now set to the current inferior erlang buffer"))


;;;###autoload
(defun resh-install ()
  (interactive)
  (if (not resh-is-installed)
      (progn
	(if (not (member 'resh-install-erl-keys erlang-mode-hook))
	    (add-hook 'erlang-mode-hook 'resh-install-erl-keys))
	(if (not (member 'resh-install-erl-shell-keys erlang-shell-mode-hook))
	    (add-hook 'erlang-shell-mode-hook 'resh-install-erl-shell-keys))
	(setq resh-is-installed t))))

(defun resh-install-erl-keys ()
  (local-set-key "\C-cc" 'resh-erlang))

(defun resh-install-erl-shell-keys ()
  (local-set-key "\C-cc" 'resh-erlang)
  (local-set-key "\C-cs" 'resh-set-inferior-erlang-buffer)
  ;; The reconnection is not fully working...
  ;;(local-set-key "\C-cr" 'resh-reconnect)
  )


(provide 'resh)
