(require 'load-relative)
(require-relative-list
 '("helper") "realgud-")
(require-relative-list
 '("command") "realgud-buffer-")

(defun realgud-run-command-get-output (cmd &rest args)
  "Run debugger command and split output.

First line (with command itself) is excluded.
CMD - command to be executed
ARGS - arguments for command"
  (with-current-buffer-safe (realgud-get-cmdbuf)
    (let ((sleep-count 0)
	  (process (get-buffer-process (current-buffer)))
	  (realgud-track-divert-string nil))
      (realgud-cmdbuf-info-divert-output?= t)
      (if args
	  (apply cmd args)
	(funcall cmd))
      (while (and (eq 'run (process-status process))
		  (null realgud-track-divert-string)
		  (> 1000 (setq sleep-count (1+ sleep-count))))
	(sleep-for 0.001)
	)
      (if (>= sleep-count 1000)
	  (progn
	    (message "Timeout on running debugger command") ; TODO error handlig
	    nil)
	(cdr (split-string realgud-track-divert-string "\n" t)) )) ))

(defun realgud-locals-init ()
  (with-current-buffer-safe (realgud-get-cmdbuf)
    (let ((locals-buffer (get-buffer-create
			  (format "*locals %s*"
				  (realgud-get-buffer-base-name
				   (buffer-name))))))
      (realgud-cmdbuf-info-locals-buf= locals-buffer)
      (with-current-buffer locals-buffer
	(realgud-locals-mode))
      (realgud-locals-register-reload)
      (realgud-locals-insert)
      )
    )
  )

(defun realgud-locals-register-reload ()
  "Get list of local variables and load values selected by user."
  (let* ((locals-names-list (realgud-run-command-get-output 'realgud:cmd-info-locals-name-list))
	 (locals-values (mapcar
			 (lambda (var-name) ; It adds list of lists
			   (realgud-run-command-get-output 'realgud:cmd-info-value var-name))
			 locals-names-list)) )
    (puthash 'frame_id_placeholder locals-values
	     (realgud-get-info 'locals-data) )
    ))

(defun realgud-locals-insert ()
  "Serialize and format locales data"
  (let ((locals-data-hash (gethash 'frame_id_placeholder (realgud-get-info 'locals-data))))
    (with-current-buffer (realgud-get-locals-buf)
      (setq buffer-read-only nil)
      (delete-region (point-min) (point-max))
      (insert
       (mapconcat 'car locals-data-hash "\n"))
      (setq buffer-read-only t) )))

(provide-me "realgud-")
