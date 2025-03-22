;;; org-linear.el --- summary -*- lexical-binding: t -*-
;;; Commentary:
;; This package is responsible for parsing the linear issues json file
;;; Code:

(setenv "LINEAR_API_KEY" (exec-path-from-shell-getenv "LINEAR_API_KEY"))

(defun linear/to-org-mode-ast (linear-issues)
  "LINEAR-ISSUES are a alist representation of the linear issues."
  (mapcar (lambda (issue)
            ())
          linear-issues))

(defun linear/export-to-org-file ()
  "Export the linear-output.json file to a linear.org."
  (progn
    (-> (cl-coerce (json-parse-string (read-file "./linear-output.json")
                                      :object-type 'alist)
                   'list)
        linear-json-to-org-ast
        org-element-interpret-data
        (f-write-text 'utf-8 (format "%slinear.org" org-directory)))

    (message "Done exporting linear issues to org file")))

(defun linear-json-to-org-ast (linear-issues)
  "Convert LINEAR-ISSUES JSON to org AST."
  (mapcar (lambda (issue)
            (linear/issue-to-org issue))
          linear-issues))

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
                 :title ,title
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
                                      `(headline (:level 2 :title ,description)
                                                 (section nil ()))))))))
;;;###autoload
(defun linear/update-linear-issues ()
  "Update the linear org agenda file."
  (interactive)
  (call-process "bun" nil "*linear-output*" t "index.ts" "run"))

(defun linear/state-to-todo (state)
  "Map linear issue STATE to org toto item."
  (pcase state
    ("PR" "WAITING")
    ("In Progress" "NEXT")
    ("Merged" "DONE")
    (_ "TODO")))

(provide 'org-linear)
;;; org-linear.el ends here
