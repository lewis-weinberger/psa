;;;; Copyright 2020 Lewis Weinberger
;;;; 
;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files (the
;;;; "Software"), to deal in the Software without restriction, including
;;;; without limitation the rights to use, copy, modify, merge, publish,
;;;; distribute, sublicense, and/or sell copies of the Software, and to permit
;;;; persons to whom the Software is furnished to do so, subject to the
;;;; following conditions:
;;;; 
;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.
;;;; 
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
;;;; FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;; DEALINGS IN THE SOFTWARE.
;;;;
;;;; --------------------------------------------------------------------------
;;;; A simple scheduled alerts program using the Discord API.

(in-package :cl-user)

(defpackage :psa
  (:use :common-lisp)
  (:export #:alert
	   #:parse-event
	   #:string-to-date
	   #:check-date
	   #:main-loop
	   #:main
	   #:*version*
	   #:*sleep*))

(in-package :psa)

;;; Configuration -------------------------------------------------------------

(defvar *version* 0.1)            ; version number
(setf deploy:*status-output* nil) ; turn off Deploy's status output

;;; Webhook interaction -------------------------------------------------------

(setf drakma:*drakma-default-external-format* ':utf-8)

(defun alert (webhook message)
  "Send an alert to the WEBHOOK, posting the given MESSAGE."
  (drakma:http-request webhook
                       :method :post
		       :parameters `(("content" . ,message))))

;;; Event parsing -------------------------------------------------------------

(defmacro handle-error (msg)
  `(progn (format t ,msg) nil))

(defmacro get-alist (obj alist)
  `(cdr (assoc ,obj ,alist)))

(defun parse-event (filepath)
  "Reads the event file at FILEPATH."
  (handler-case 
      (let* ((event (json:decode-json-from-source filepath))
	     (date (get-alist :date event))
	     (description (get-alist :description event))
	     (style (get-alist :style event)))
	(values date description style))
    (end-of-file () (handle-error "Reached EOF while parsing JSON.~%"))
    (json:json-syntax-error () (handle-error "JSON syntax error.~%"))))

(defun string-to-date (string)
  "Converts STRING in format YYYY-MM-DD to a timestamp."
  (let* ((split (uiop:split-string string :separator "-"))
	 (year (parse-integer (first split)))
	 (month (parse-integer (second split)))
	 (day (parse-integer (third split))))
    (local-time:encode-timestamp 0 0 0 0 day month year)))

(defun check-date (date window)
  "Check if DATE is going to occur in WINDOW days."
  (handler-case 
      (if date
	  (let* ((timestamp (string-to-date date))
		 (now (local-time:now))
		 (min-time (local-time:timestamp+ now (1- window) :day))
		 (max-time (local-time:timestamp+ now window :day)))
	    (and
	     (local-time:timestamp> timestamp min-time)
	     (local-time:timestamp<= timestamp max-time)))
	  nil)
    (sb-int:simple-parse-error () nil)
    (local-time::invalid-time-specification () nil)))

;;; Entry point ---------------------------------------------------------------

(defun usage ()
  "Print a helpful usage message and exit."
  (format t "_______________
psa version ~a

Usage:
    psa [WEBHOOK-URL] [EVENTS-DIRECTORY] [TIME-FRAME]

where:
    WEBHOOK-URL -- Discord webhook URL
    EVENTS-DIRECTORY -- path to directory containing event files
    TIME-FRAME -- number of days before events to issue notifications~%"
	  *version*)
  (sb-ext:exit))

(defun parse-args ()
  "Read and check command-line arguments."
  (let ((args sb-ext:*posix-argv*))
    (when (/= (length args) 4)
      (usage))
    (values (second args)
	    (directory (concatenate 'string (third args) "/*.json"))
	    (parse-integer (fourth args)))))

(defun main-loop ()
  "Process the event files in EVENTS-DIRECTORY, sending an alert to WEBHOOK
for each event that is scheduled to happen within WINDOW days."
  (multiple-value-bind (webhook dir window)
      (parse-args)
    (format t
	    "~%Webhook: ~a~%Events: ~a~%Window: ~a day(s)~%~%"
	    webhook
	    dir
	    window)
    (loop :for f :in dir :do
	 (multiple-value-bind (date description style)
	     (parse-event f)
	   (format t
		   "Date: ~a, Description: ~a, Style: ~a~%"
		   date
		   description
		   style)
	   (when (check-date date window)
	     (format t "Sending notification!~%")
	     (alert webhook
		    (format nil "[~a] ~a: ~a" date style description)))))))

(defun main ()
  "Run the main-loop, catching interrupts."
  (handler-case (main-loop)
    (sb-sys:interactive-interrupt () (sb-ext:exit))))

