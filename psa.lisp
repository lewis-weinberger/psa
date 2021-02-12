;;;; Copyright 2021 Lewis Weinberger
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
           #:*version*))

(in-package :psa)

;;; Configuration -------------------------------------------------------------

(defvar *major-version* 0)
(defvar *minor-version* 1)
(defvar *patch-version* 2)

;;; Macros --------------------------------------------------------------------

(defmacro panic-on-condition (&body body)
  "Run BODY, panicking if a condition is caught."
  (let ((err (gensym)))
    `(handler-case
         ,@body
       (error (,err) (progn
                       (format t "Panicking due to condition: ~A~%" ,err)
                       (usage))))))

(defmacro continue-on-condition (&body body)
  "Run BODY, continuing if a condition is caught."
  (let ((err (gensym)))
    `(handler-case
         ,@body
       (error (,err) (format t "Continuing despite condition: ~A~%" ,err)))))

(defmacro get-alist (key alist)
  "Get val from ALIST of (KEY . val) pairs."
  `(cdr (assoc ,key ,alist)))

;;; Webhook interaction -------------------------------------------------------

(setf drakma:*drakma-default-external-format* ':utf-8)

(defun alert (webhook message)
  "Send an alert to the WEBHOOK, posting the given MESSAGE."
  (panic-on-condition
    (drakma:http-request webhook
                         :method :post
                         :parameters `(("content" . ,message)))))

;;; Event parsing -------------------------------------------------------------

(defun parse-event (filepath)
  "Reads the event file at FILEPATH."
  (continue-on-condition
    (let* ((event (json:decode-json-from-source filepath))
           (date (get-alist :date event))
           (description (get-alist :description event))
           (style (get-alist :style event)))
      (values date description style))))

(defun string-to-date (string)
  "Converts STRING in format YYYY-MM-DD to a timestamp."
  (let* ((split (uiop:split-string string :separator "-"))
         (year (parse-integer (first split)))
         (month (parse-integer (second split)))
         (day (parse-integer (third split))))
    (local-time:encode-timestamp 0 0 0 0 day month year)))

(defun check-date (date window)
  "Check if DATE is going to occur in WINDOW days."
  (continue-on-condition
    (if date
        (let* ((timestamp (string-to-date date))
               (now (local-time:now))
               (min-time (local-time:timestamp+ now (1- window) :day))
               (max-time (local-time:timestamp+ now window :day)))
          (and
           (local-time:timestamp> timestamp min-time)
           (local-time:timestamp<= timestamp max-time)))
        nil)))

;;; Entry point ---------------------------------------------------------------

(defun usage ()
  "Print a helpful usage message and exit."
  (format t "_______________
psa version ~a.~a.~a

Usage:
    psa [WEBHOOK-URL] [EVENTS-DIRECTORY] [TIME-FRAME]

where:
    WEBHOOK-URL -- Discord webhook URL
    EVENTS-DIRECTORY -- path to directory containing event files
    TIME-FRAME -- number of days before events to issue notifications~%"
      *major-version* *minor-version* *patch-version*)
  (uiop:quit))

(defun parse-args ()
  "Read and check command-line arguments."
  (panic-on-condition
    (let ((args uiop:*command-line-arguments*))
      (when (/= (length args) 3)
        (usage))
      (values (first args)
              (directory (concatenate 'string (second args) "/*.json"))
              (parse-integer (third args))))))

(defun main-loop ()
  "Process the event files in EVENTS-DIRECTORY, sending an alert to WEBHOOK
for each event that is scheduled to happen within WINDOW days."
  (multiple-value-bind (webhook dir window)
      (parse-args)
    (format t
            "~%Webhook: ~a~%Events: ~a~%Window: ~a day(s)~%"
            webhook
            dir
            window)
    (loop :for f :in dir :do
      (multiple-value-bind (date description style)
          (parse-event f)
        (format t
                "~%Date: ~a, Description: ~a, Style: ~a~%"
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
    (#+sbcl sb-sys:interactive-interrupt
     #+ccl ccl:interrupt-signal-condition
     #+clisp system::simple-interrupt-condition
     #+ecl ext:interactive-interrupt
     #+allegro excl:interrupt-signal
     (sig)
      (progn
        (format t "~%Exited due to: ~A~%" sig)
        (uiop:quit)))))
