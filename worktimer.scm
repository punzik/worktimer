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
        (srfi srfi-1)
        (srfi srfi-19)
        (srfi srfi-11))

;;; Use srfi-48 in other scheme implementation
(use-modules (ice-9 format))

(use-modules (ice-9 pretty-print))

;;; Record format:
;;; PROJECT/TASK/SUBTASK/ETC: [START_TIME] - [STOP_TIME] - DURATION

(define date-format "~Y-~m-~d ~H:~M:~S")

;;; Find substring separated by ch-start and ch-end.
;;; (substring/find str ch-start ch-end [begin end]) 
;;; If ch-start is #f, select substring from beginning of string.
;;; If ch-end is #f, select substring from ch-start to end string.
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

;;; Convert time-difference to string "hh:mm:ss"
(define (time-difference->h:m:s td)
  (let ((sec (time-second td)))
    (let* ((h (quotient sec 3600))
           (m (quotient (- sec (* h 3600)) 60))
           (s (- sec (* h 3600) (* m 60))))
      (string-append
       (format #f "~2,'0d" h) ":"
       (format #f "~2,'0d" m) ":"
       (format #f "~2,'0d" s)))))

;;; Convert path to string
(define (path->string path)
  (let loop ((path path)
             (str ""))
    (if (null? path) str
        (loop (cdr path)
              (string-append
               str
               (car path)
               (if (null? (cdr path)) "" "/"))))))

;;; Split path to separate elements
;;; (path-split str [start end])
(define (path-split str . args)
  (let ((start (if (null? args) 0 (car args)))
        (end (if (or (null? args) (null? (cdr args))) (string-length str) (cadr args))))
    (let loop ((path '()) (path-start start))
      (let ((item-end (string-index str #\/ path-start end)))
        (if (not item-end)
            (reverse (cons (substring str path-start end) path))
            (loop
             (cons (substring str path-start item-end) path) (1+ item-end)))))))

;;; Parse task string and return list:
;;; '((list of path elements) start-date stop-date duration)
(define (parse-task-string str)
  (let ((path
         (let ((path-end (string-index str #\:)))
           (if (not path-end) '()
               (path-split str 0 path-end))))
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

;;; Parse timesheet file and return list of tasks
(define (parse-timesheet filename)
  (call-with-input-file filename
    (lambda (port)
      (let loop ((recs '()))
        (let ((line (get-line port)))
          (if (eof-object? line)
              (reverse recs)
              (loop (cons (parse-task-string line) recs))))))))

;;; Return difference of two dates
(define (date-difference d1 d2)
  (time-difference
   (date->time-utc d1)
   (date->time-utc d2)))

;;; Print timesheet
(define (print-timesheet timesheet)
  (for-each
   (lambda (task)
     (let ((path (car task))
           (sdate (cadr task))
           (edate (caddr task))
           (duration (cadddr task)))
       (format #t "~a: [~a]"
               (path->string path)
               (date->string sdate date-format))
       (if edate
           (format #t " - [~a] - ~a\n"
                   (date->string edate date-format)
                   (time-difference->h:m:s duration))
           (newline))))
   timesheet))

;;; Report is a tree of projects items.
;;; Root of the tree is super-project with name "ROOT":
;;; ("ROOT" d
;;;  ("PROJ1" d
;;;   ("P1-TASK1" d
;;;    ("P1-T1-SUBTASK1" d)
;;;    ("P1-T1-SUBTASK2" d))
;;;   ("P1-TASK2" d)))
(define (make-report timesheet)
  ;; Add task duration to project branch
  (define (tree-add-duration project-tree path duration)
    (let tree-walk ((tree project-tree)
                    (path path))
      ;; Add task duration to tree leaf
      (set-car! (cdr tree) (add-duration (cadr tree) duration))

      ;; Search next leaf corresponding with path item
      (if (null? path)
          project-tree
          (let ((item (find
                       (lambda (i) (string= (car i) (car path)))
                       (cddr tree))))
            (if item
                (tree-walk item (cdr path))

                ;; Add new branch (or leaf) to tree
                (let ((add-items
                       (let add-item-loop ((add-items '())
                                           (path (reverse path)))
                         (if (null? path)
                             add-items
                             (add-item-loop
                              (if (null? add-items)
                                  (list (car path) duration)
                                  (list (car path) duration add-items))
                              (cdr path))))))
                  (set-cdr! (cdr tree) (cons add-items (cddr tree)))
                  project-tree))))))
  ;; END tree-add-duration
  
  (let loop ((projects (list "Overall" (make-time 'time-duration 0 0)))
             (timesheet timesheet))
    (if (null? timesheet)
        projects                        ; TODO Sort projects by path
        (let* ((task (car timesheet))
               (path (car task))
               (duration (cadddr task)))
          (loop (if duration
                    (tree-add-duration projects path duration)
                    projects)
                (cdr timesheet))))))

(define (print-report report)
  (let walk ((tree report)
             (level 0))
    (when (not (null? tree))
      (format #t "~v_~a: ~a\n" level (car tree)
              (time-difference->h:m:s (cadr tree)))
      (for-each (lambda (l) (walk l (+ level 2))) (cddr tree)))))

;; MAIN

(define ts-file "/home/np/timesheet.txt")

;; (let* ((cmdl (command-line))
;;        (params (cdr cmdl)))
;;   (if (null? params) 
;;       ;; No command. Show last record and duration if running
;;       (let ((last (timesheet-get-last ts-file)))
;;         (if (null? last)
;;             (format #t "Not any tasks\n")
;;             (let ((path (car last))
;;                   (start-date (cadr last))
;;                   (stop-date (caddr last))
;;                   (duration (cadddr last))
;;                   (now (current-date)))
;;               (format #t "~a: [~a] - [~a] - ~a\n"
;;                       (path->string path)
;;                       (date->string start-date "~Y-~m-~d ~H:~M:~S")
;;                       (if (not stop-date) "NOW"
;;                           (date->string stop-date "~Y-~m-~d ~H:~M:~S"))
;;                       (time-difference->h:m:s
;;                        (if duration duration
;;                            (time-difference
;;                             (date->time-utc (if stop-date stop-date now))
;;                             (date->time-utc start-date))))))))

;;       ;; Commands
;;       (let ((command (car params))
;;             (date (current-date)))
;;         (cond

;;          ((equal? command "start")
;;           ;; ----------------------- Start timer ------------------------- ;;
;;           (call-with-append-file
;;            ts-file
;;            (lambda (port)
;;              (let* ((last (timesheet-get-last ts-file))
;;                     (path
;;                      (if (null? (cdr params))
;;                          (if (null? last) '() (car last))
;;                          (path-split (cadr params)))))
;;                (if (null? path)
;;                    (format #t "ERROR: Not specified the task path\n")
;;                    (begin
;;                      (when (and (not (null? last)) (not (caddr last)))
;;                        (write-stop-and-duration port (cadr last) date))
;;                      (format port "~a: [~a]" (path->string path) (date->string date "~Y-~m-~d ~H:~M:~S"))))))))

;;          ((equal? command "stop")
;;           ;; ----------------------- Stop timer ------------------------- ;;
;;           (call-with-append-file
;;            ts-file
;;            (lambda (port)
;;              (let ((last (timesheet-get-last ts-file)))
;;                (if (or (null? last) (caddr last))
;;                    (format #t "WARNING: Not running any tasks\n")
;;                    (write-stop-and-duration port (cadr last) date))))))))))

(print-report (make-report (parse-timesheet ts-file)))
