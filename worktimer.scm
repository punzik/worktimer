#!/usr/bin/guile
!#

;; Copyright (c) 2014 Nikolay Puzanov <punzik@gmail.com>
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(import (rnrs io ports (6))
        (srfi srfi-19)
        (srfi srfi-11))

;;; Use srfi-48 in other scheme implementation
(use-modules (ice-9 format))

;;; Record format:
;;; PROJECT/PATH: [START_TIME] - [STOP_TIME] - DURATION

;;; Find substring separated by ch-start and ch-end.
;; (substring/find str ch-start ch-end [begin end]) 
;; If ch-start is #f, select substring from beginning of string.
;; If ch-end is #f, select substring from ch-start to end string.
(define (substring/find str ch-start ch-end . args)
  (let ((start (if (null? args) 0 (car args)))
        (end (if (< (length args) 2) (string-length str) (cadr args))))
    (let loop ((strings '()) (start start))
      (let ((ce
             (if ch-end (string-index str ch-end start end) end)))
        (if (not ce) (reverse strings)
            (let ((cs (if ch-start (string-index str ch-start start ce) (- start 1))))
              (if (not cs) (reverse strings)
                  (loop (cons (substring str (1+ cs) ce) strings) (1+ ce)))))))))

(define (time-difference->h:m:s td)
  (let ((sec (time-second td)))
    (let* ((h (quotient sec 3600))
           (m (quotient (- sec (* h 3600)) 60))
           (s (- sec (* h 3600) (* m 60))))
      (string-append
       (format #f "~2,'0d" h) ":"
       (format #f "~2,'0d" m) ":"
       (format #f "~2,'0d" s)))))

(define (path->string path)
  (let loop ((path path)
             (str ""))
    (if (null? path) str
        (loop (cdr path)
              (string-append
               str
               (car path)
               (if (null? (cdr path)) "" "/"))))))

(define (parse-path str . args)
  (let ((start (if (null? args) 0 (car args)))
        (end (if (or (null? args) (null? (cdr args))) (string-length str) (cadr args))))
    (let loop ((path '()) (path-start start))
      (let ((item-end (string-index str #\/ path-start end)))
        (if (not item-end)
            (reverse (cons (substring str path-start end) path))
            (loop
             (cons (substring str path-start item-end) path) (1+ item-end)))))))

(define (parse-task-string str)
  (let ((path
         (let ((path-end (string-index str #\:)))
           (if (not path-end) '()
               (parse-path str 0 path-end))))
        (dates (substring/find str #\[ #\])))
    (if (null? dates) '()
        (let ((date-start (string->date (car dates) "~Y-~m-~d ~H:~M:~S")))
          (let-values (((date-end duration)
                        (if (null? (cdr dates))
                            (values #f #f)
                            (let* ((date-end (string->date (cadr dates) "~Y-~m-~d ~H:~M:~S"))
                                   (duration (time-difference
                                              (date->time-utc date-end)
                                              (date->time-utc date-start))))
                              (values date-end duration)))))
            (list path date-start date-end duration))))))

(define (parse-timesheet filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((recs '()))
        (let ((line (get-line port)))
          (if (eof-object? line)
              recs
              (loop (parse-task-string line))))))))

(define (timesheet-get-last filename)
  (let ((last-line
         (call-with-input-file filename
           (lambda (port)
             (let loop ((last-line ""))
               (let ((line (get-line port)))
                 (if (eof-object? line) last-line
                     (loop line))))))))
    (parse-task-string last-line)))

(define (call-with-append-file filename thunk)
  (let ((port (open-file filename
                         (if (file-exists? filename) "a" "w"))))
    (thunk port)
    (close-port port)))

(define (write-stop-and-duration port start-date stop-date)
  (format port " - [~a] - ~a\n"
          (date->string stop-date "~Y-~m-~d ~H:~M:~S")
          (time-difference->h:m:s
           (time-difference
            (date->time-utc stop-date)
            (date->time-utc start-date)))))

;; MAIN

(define ts-file "/home/np/timesheet.txt")

(let* ((cmdl (command-line))
       (params (cdr cmdl)))
  (if (null? params) 
      ;; No command. Show last record and duration if running
      (let ((last (timesheet-get-last ts-file)))
        (if (null? last)
            (format #t "Not any tasks\n")
            (let ((path (car last))
                  (start-date (cadr last))
                  (stop-date (caddr last))
                  (duration (cadddr last))
                  (now (current-date)))
              (format #t "~a: [~a] - [~a] - ~a\n"
                      (path->string path)
                      (date->string start-date "~Y-~m-~d ~H:~M:~S")
                      (if (not stop-date) "NOW"
                          (date->string stop-date "~Y-~m-~d ~H:~M:~S"))
                      (time-difference->h:m:s
                       (if duration duration
                           (time-difference
                            (date->time-utc (if stop-date stop-date now))
                            (date->time-utc start-date))))))))

      ;; Commands
      (let ((command (car params))
            (date (current-date)))
        (cond

         ((equal? command "start")
          ;; ----------------------- Start timer ------------------------- ;;
          (call-with-append-file
           ts-file
           (lambda (port)
             (let* ((last (timesheet-get-last ts-file))
                    (path
                     (if (null? (cdr params))
                         (if (null? last) '() (car last))
                         (parse-path (cadr params)))))
               (if (null? path)
                   (format #t "ERROR: Not specified the task path\n")
                   (begin
                     (when (and (not (null? last)) (not (caddr last)))
                       (write-stop-and-duration port (cadr last) date))
                     (format port "~a: [~a]" (path->string path) (date->string date "~Y-~m-~d ~H:~M:~S"))))))))

         ((equal? command "stop")
          ;; ----------------------- Stop timer ------------------------- ;;
          (call-with-append-file
           ts-file
           (lambda (port)
             (let ((last (timesheet-get-last ts-file)))
               (if (or (null? last) (caddr last))
                   (format #t "WARNING: Not running any tasks\n")
                   (write-stop-and-duration port (cadr last) date))))))))))
