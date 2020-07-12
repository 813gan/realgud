(require 'load-relative)
(require-relative-list
 '("helper") "realgud-")

(defun realgud-locals-init ()
  (with-current-buffer-safe (realgud-get-cmdbuf)
      (let ((sleep-count 0))
      (setq process (get-buffer-process (current-buffer)))
      (setq realgud-track-divert-string nil)
      (realgud:cmd-info-locals)
      (realgud-cmdbuf-info-divert-output?= t)
    (while (and (eq 'run (process-status process))
		(null realgud-track-divert-string)
		(> 1000 (setq sleep-count (1+ sleep-count))))
      (sleep-for 0.001)
      )
    (if (>= sleep-count 1000)
	(message "Timeout on running debugger command")
      (let ((divert-string realgud-track-divert-string)
	    (locals-buffer (get-buffer-create
			   (format "*locals %s*"
				   (realgud-get-buffer-base-name
				    (buffer-name)))))
	    )
	(with-current-buffer locals-buffer
	  (realgud-locals-mode)
	  (setq buffer-read-only nil)
	  (delete-region (point-min) (point-max))
	  (insert divert-string)
	  (setq buffer-read-only t)
	  )
	)
      )

    )
  ) )

(provide-me "realgud-")
