;;; Copyright (C) 2014 Rocky Bernstein <rocky@gnu.org>
;;; Regular expressions for nodejs Javascript debugger.

(eval-when-compile (require 'cl))

(require 'load-relative)
(require-relative-list '("../../common/regexp"
			 "../../common/loc"
			 "../../common/init")
		       "realgud-")
(require-relative-list '("../../lang/posix-shell") "realgud-lang-")

(defvar realgud-nodejs-pat-hash)
(declare-function make-realgud-loc-pat (realgud-loc))

(defvar realgud-nodejs-pat-hash (make-hash-table :test 'equal)
  "Hash key is the what kind of pattern we want to match:
backtrace, prompt, etc.  The values of a hash entry is a
realgud-loc-pat struct")

(defvar realgud-nodejs-term-escape "[\\[0-9\\]+\\[GKJ\\]"
  "Escape sequence regular expression pattern nodejs often puts in around prompts")

;; Regular expression that describes a nodejs location generally shown
;; before a command prompt.
;; For example:
;;   break in /home/indutny/Code/git/indutny/myscript.js:1
(setf (gethash "loc" realgud-nodejs-pat-hash)
      (make-realgud-loc-pat
       :regexp (format
		"\\(?:^\\|\n\\)\\(?:%s\\)*break in \\([^:]+\\):\\([0-9]*\\)"
		realgud-nodejs-term-escape)
       :file-group 1
       :line-group 2))

;; Regular expression that describes a nodejs command prompt
;; For example:
;;   debug>
(setf (gethash "prompt" realgud-nodejs-pat-hash)
      (make-realgud-loc-pat
       :regexp (format "^\\(?:%s\\)*debug> " realgud-nodejs-term-escape)
       ))

;;  Regular expression that describes a "breakpoint set" line
(setf (gethash "brkpt-set" realgud-nodejs-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Breakpoint \\([0-9]+\\) set in file \\(.+\\), line \\([0-9]+\\).\n"
       :num 1
       :file-group 2
       :line-group 3))

;; Regular expression that describes a debugger "delete" (breakpoint) response.
;; For example:
;;   Removed 1 breakpoint(s).
(setf (gethash "brkpt-del" realgud-nodejs-pat-hash)
      (make-realgud-loc-pat
       :regexp "^Removed \\([0-9]+\\) breakpoint(s).\n"
       :num 1))

;; Regular expression that describes a debugger "backtrace" command line.
;; For example:
;;   ->0 in file `../nodejs/test/example/subshell.sh' at line 6
;;   ##1 source("../nodejs/shell.sh") called from file `/bin/nodejs' at line 140
;;   ##2 main() called from file `/bin/nodejs' at line 0
(setf (gethash "debugger-backtrace" realgud-nodejs-pat-hash)
      (make-realgud-loc-pat
       :regexp 	(concat realgud-shell-frame-start-regexp
			realgud-shell-frame-num-regexp "[ ]?"
			"\\(.*\\)"
			realgud-shell-frame-file-regexp
			"\\(?:" realgud-shell-frame-line-regexp "\\)?"
			)
       :num 2
       :file-group 4
       :line-group 5)
      )

;; Regular expression that for a termination message.
(setf (gethash "termination" realgud-nodejs-pat-hash)
       "^nodejs: That's all, folks...\n")

(setf (gethash "font-lock-keywords" realgud-nodejs-pat-hash)
      '(
	;; The frame number and first type name, if present.
	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
	;;      --^-
	("^\\(->\\|##\\)\\([0-9]+\\) "
	 (2 realgud-backtrace-number-face))

	;; File name.
	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
	;;          ---------^^^^^^^^^^^^^^^^^^^^-
	("[ \t]+\\(in\\|from\\) file `\\(.+\\)'"
	 (2 realgud-file-name-face))

	;; File name.
	;; E.g. ->0 in file `/etc/init.d/apparmor' at line 35
	;;                                         --------^^
	;; Line number.
	("[ \t]+at line \\([0-9]+\\)$"
	 (1 realgud-line-number-face))
	))

(setf (gethash "nodejs" realgud-pat-hash) realgud-nodejs-pat-hash)

(defvar realgud-nodejs-command-hash (make-hash-table :test 'equal)
  "Hash key is command name like 'quit' and the value is
  the nodejs command to use, like 'quit!'")

(setf (gethash "quit" realgud-nodejs-command-hash) "quit!")
(setf (gethash "nodejs" realgud-command-hash realgud-nodejs-command-hash))

(provide-me "realgud-nodejs-")
