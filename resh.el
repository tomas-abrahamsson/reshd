;;; resh.el -- emacs support for connecting to an reshd
;;;            (remote erlang shell daemon)
;;
;; $Id: resh.el,v 1.3 2001-04-23 17:57:53 tab Exp $
;;
;; by Tomas Abrahamsson <epktoab@lmera.ericsson.se>
;;

;;; Installation:

;; (load-library "resh")
;; (resh-install)

;;; Code

(require 'erlang)

;; User definable variables
;; vvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvvv
(defvar resh-default-host "localhost"
  "*Default hostname for `resh-inferior-erlang'.")

(defvar resh-default-port nil
  "*Default port (an integer) for `resh-inferior-erlang'.")
;; ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
;; End of user definable variables


(defvar resh-host-history nil
  "Host history `resh-inferior-erlang'")

(defvar resh-port-history nil
  "Port history `resh-inferior-erlang'")

(defvar resh-current-host nil
  "Buffer-local variable, used for reconnection.")

(defvar resh-current-port nil
  "Buffer-local variable, used for reconnection.")


(defun resh (host port &optional reconnecting)
  "Run an inferior remote Erlang shell.

The command line history can be accessed with  M-p  and  M-n.
The history is saved between sessions.

Entry to this mode calls the functions in the variables
`comint-mode-hook' and `erlang-shell-mode-hook' with no arguments.

The following commands imitate the usual Unix interrupt and
editing control characters:
\\{erlang-shell-mode-map}"
  (interactive
   (let* ((init-prompt "Remote erlang shell to")
	  (host-hist 'resh-host-history)
	  (port-hist 'resh-port-history)
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
			     (t (string-to-int remote-port-str)))))
     (list remote-host remote-port)))
  (require 'comint)

  (let* ((portstr (int-to-string port))
	 (proc-name (concat inferior-erlang-process-name "-" host ":" portstr))
	 (erl-buffer (make-comint proc-name (cons host port)))
	 (erl-process (get-buffer-process erl-buffer))
	 (erl-buffer-name (concat inferior-erlang-buffer-name
				  "-" host ":" portstr)))
    (process-kill-without-query erl-process)
    (switch-to-buffer erl-buffer)
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

(defun resh-reconnect ()
  "Try to reconnect to a remote Erlang shell daemon."
  (interactive)
  (resh-inferior-erlang resh-current-host resh-current-port t))

(defun resh-install ()
  (interactive)
  (if (not (member 'resh-install-erl-keys erlang-mode-hook))
      (add-hook 'erlang-mode-hook 'resh-install-erl-keys))
  (if (not (member 'resh-install-erl-shell-keys erlang-shell-mode-hook))
      (add-hook 'erlang-shell-mode-hook 'resh-install-erl-shell-keys)))

(defun resh-install-erl-keys ()
  (local-set-key "\C-cc" 'resh-inferior-erlang))

(defun resh-install-erl-shell-keys ()
  (local-set-key "\C-cc" 'resh-inferior-erlang)
  (local-set-key "\C-cr" 'resh-reconnect))


(provide 'resh)
