;;; resh.el -- emacs support for connecting to an reshd
;;;            (remote erlang shell daemon)
;;
;; $Id: resh.el,v 1.1 2001-04-18 17:40:39 tab Exp $
;;
;; by Tomas Abrahamsson <epktoab@lmera.ericsson.se>
;;

;;; Code

(requre 'erlang)

(defvar resh-default-host "localhost"
  "*Default hostname for `resh-inferior-erlang'.")

(defvar resh-default-port nil
  "*Default port (an integer) for `resh-inferior-erlang'.")


(defvar resh-host-history nil
  "Host history `resh-inferior-erlang'")

(defvar resh-port-history nil
  "Port history `resh-inferior-erlang'")

(defun resh-inferior-erlang (host port)
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
    ;; `rename-buffer' takes only one argument in Emacs 18.
    (condition-case nil
	(rename-buffer erl-buffer-name t)
      (error (rename-buffer erl-buffer-name)))
    (erlang-shell-mode)))

(provide 'resh)
