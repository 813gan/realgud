(require 'load-relative)
(require-relative-list
 '("helper") "realgud-")
(require-relative-list
 '("command") "realgud-buffer-")

(defun realgud-run-command-get-output (cmd)
  (with-current-buffer-safe (realgud-get-cmdbuf)
    (let ((sleep-count 0)
	  (process (get-buffer-process (current-buffer)))
	  (realgud-track-divert-string nil))
      (realgud-cmdbuf-info-divert-output?= t)
      (cmd)
      (while (and (eq 'run (process-status process))
		  (null realgud-track-divert-string)
		  (> 1000 (setq sleep-count (1+ sleep-count))))
	(sleep-for 0.001)
	)
      (if (>= sleep-count 1000)
	  (progn
	    (message "Timeout on running debugger command") ; TODO error handlig
	    nil)
	(realgud-track-divert-string)) )))

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
  (with-current-buffer (realgud-get-locals-buf)
    (let* ((raw-out (realgud-run-command-get-output 'realgud:cmd-info-locals-name-list))
	   (locals-names-list (split-string raw-out "\n"))
	   (locals-values nil) )
      (setq locals-values (map (realgud:cmd-info-value) locals-names-list))
      (puthash "frame_id_placeholder" locals-values
	       (realgud-get-info 'locals-data) )
    )))

(defun realgud-locales-insert ()
  "Serialize and format locales data"
  (with-current-buffer (realgud-get-locals-buf)
    (setq buffer-read-only nil)
    (delete-region (point-min) (point-max))
    (insert
     (mapconcat string
      (gethash "frame_id_placeholder" (realgud-get-info 'locals-data))
      "\n"))
    (setq buffer-read-only t) ))

(provide-me "realgud-")
