;;; org-linear.el --- Integration of Linear issues with Org mode -*- lexical-binding: t -*-
;;; Commentary:
;; This package is responsible for parsing the Linear issues JSON file and converting them to Org mode format.

;;; Code:
(require 'exec-path-from-shell)
(require 'f)
(require 'dash)
(require 'xterm-color)
(require 'org)

(setenv "LINEAR_API_KEY" (exec-path-from-shell-getenv "LINEAR_API_KEY"))

(defconst *linear-output-buffer-name* "*linear-output*"
  "Buffer name for Linear output.")

(defmacro with-default-dir (directory &rest body)
  "Evaluate BODY with `default-dir' as DIRECTORY.  Keymap is \\{typescript-ts-mode-map}."
  (declare (indent 1) (debug t))
  `(let ((default-directory ,directory))
     ,@body))

(defun read-file (file-name)
  "Return the contents of FILE-NAME as a Lisp data type."
  (when (file-exists-p file-name)
    (with-temp-buffer
      (insert-file-contents file-name)
      (buffer-string))))

(defun linear/json-to-org-ast (linear-issues)
  "Convert LINEAR-ISSUES JSON to org AST."
  (mapcar (lambda (issue)
            (linear/issue-to-org issue))
          linear-issues))

(defun linear/export-to-org-file ()
  "Export the linear-output.json file to a linear.org."
  (progn
    (-> (cl-coerce (json-parse-string (read-file "./linear-output.json")
                                      :object-type 'alist)
                   'list)
        linear/json-to-org-ast
        org-element-interpret-data
        (f-write-text 'utf-8 (format "%slinear.org" (if (boundp 'org-directory) org-directory "~/.emacs.d/org/"))))

    (message "Done exporting Linear issues to Org file")))

(defun linear/format-description (desc)
  "Format DESC as a description string."
  (format "Description:\n%s" desc))

(defun linear/decode-date (time-string)
  "Parse the TIME-STRING into an `org-mode' element `time-stamp'(year month day)."
  (let ((time (parse-time-string time-string)))
    `( :type active
       :range-type nil
       :year-start ,(decoded-time-year time)
       :month-start ,(decoded-time-month time)
       :day-start ,(decoded-time-day time))))

(defun linear/issue-to-org (issue)
  "Convert ISSUE to org-element structure."
  (let ((title (alist-get 'title issue))
        (linear-id (alist-get 'identifier issue))
        (issue-state (alist-get 'state issue))
        (description (alist-get 'description issue))
        (deadline (alist-get 'deadline issue))
        (scheduled (alist-get 'scheduled issue))
        (url (alist-get 'url issue)))
    `(headline ( :level 1
                 :title ,(format "[%s] - %s" linear-id title)
                 :todo-keyword ,(linear/state-to-todo issue-state)
                 :todo-type todo)
               (planning ( :deadline (timestamp
                                      ,(linear/decode-date deadline))
                           :scheduled (timestamp
                                       ,(linear/decode-date scheduled))))
               (section nil
                        (paragraph nil
                                   (link ( :type "https"
                                           :path ,(replace-regexp-in-string "^https:" "" url)
                                           :format bracket
                                           :raw-link ,(replace-regexp-in-string "^https:" "" url)
                                           :application nil
                                           :search-option nil
                                           :begin nil
                                           :end nil
                                           :contents-begin nil
                                           :contents-end nil
                                           :post-blank nil)
                                         "Linear Issue Link")
                                   "\n"
                                   ,(format "linear-id: %s" linear-id)
                                   "\n"
                                   ,(when description
                                      `(headline (:level 2 :title ,(linear/format-description description))
                                                 (section nil ()))))))))
(defun linear/bun--process-filter (process output)
  "Filter the bun PROCESS OUTPUT and apply ANSI color."
  (when (buffer-live-p (process-buffer process))
    (with-current-buffer (process-buffer process)
      (let ((moving (= (point) (process-mark process))))
        (save-excursion
          ;; Insert the colorized text
          (goto-char (process-mark process))
          (insert (xterm-color-filter output))
          (set-marker (process-mark process) (point)))
        ;; If point was at the end, move it to the new end
        (if moving (goto-char (process-mark process)))))))

(defun linear/bun--process-sentinel (process _event)
  "Handle PROCESS exit."
  (let ((status (process-status process))
        (exit-code (process-exit-status process)))
    (when (eq status 'exit)
      (if (= exit-code 0)
          (with-default-dir (file-name-directory (locate-library "org-linear"))
            (linear/export-to-org-file))
          (message "Linear Process completed successfully")
        (error "Linear Process failed with exit code %d" exit-code)))))

;;;###autoload
(defun linear/update-linear-issues ()
  "Update the Linear Org agenda file."
  (interactive)
  (with-default-dir (file-name-directory (locate-library "org-linear"))
    (make-process
     :name "bun-process"
     :buffer *linear-output-buffer-name*
     :command '("bun" "run" "index.ts")
     :filter #'linear/bun--process-filter
     :sentinel #'linear/bun--process-sentinel)))

(defun linear/state-to-todo (state)
  "Map Linear issue STATE to Org TODO item."
  (pcase state
    ("PR" "WAITING")
    ("In Progress" "NEXT")
    ("Merged" "DONE")
    (_ "TODO")))

(provide 'org-linear)
;;; org-linear.el ends here
