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

;; -*- geiser-scheme-implementation: guile -*-

(import (rnrs io ports (6))
        (srfi srfi-1)
        (srfi srfi-19)
        (srfi srfi-11))

;;; Set locale according to environment settings
(setlocale LC_ALL "")

;;; Use srfi-48 in other scheme implementation
(use-modules (ice-9 format))

;;; Record format:
;;; PROJECT/TASK/SUBTASK/ETC: [START_TIME] - [STOP_TIME] - DURATION

;;; Hardcoded timesheet filename
(define ts-file (string-append (getenv "HOME") "/.timesheet"))
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
  (let ((str (string-trim-both str)))
    (if (or (zero? (string-length str))
            (equal? (string-ref str 0) #\#))
        #f
        (let ((path
               (let ((path-end (string-index str #\:)))
                 (if (not path-end) #f
                     (path-split str 0 path-end))))
              (dates (substring/find str #\[ #\])))
          (if (or (not path) (not dates))
              #f
              (let ((date-start (string->date (car dates) date-format)))
                (let-values (((date-end duration)
                              (if (null? (cdr dates))
                                  (values #f #f)
                                  (let* ((date-end (string->date (cadr dates) date-format))
                                         (duration (time-difference
                                                    (date->time-utc date-end)
                                                    (date->time-utc date-start))))
                                    (values date-end duration)))))
                  (list path date-start date-end duration))))))))

;;; Parse timesheet file and return list of tasks
(define (read-timesheet filename)
  (if (file-exists? filename)
      (call-with-input-file filename
        (lambda (port)
          (let loop ((recs '()))
            (let ((line (get-line port)))
              (if (eof-object? line)
                  (reverse recs)
                  (loop (let ((item (parse-task-string line)))
                          (if item (cons item recs) recs))))))))
      '()))

;;; Return difference of two dates
(define (date-difference d1 d2)
  (time-difference
   (date->time-utc d1)
   (date->time-utc d2)))

;;; Print timesheet item
(define (print-task task . now)
  (let ((path (car task))
        (sdate (cadr task))
        (edate (caddr task))
        (duration (cadddr task))
        (now (if (null? now) #f (car now))))
    (let-values (((edate duration)
                  (if edate
                      (values edate duration)
                      (if now
                          (values now (date-difference now sdate))
                          (values #f #f)))))
      (format #t "~a: [~a]"
              (path->string path)
              (date->string sdate date-format))
      (if edate
          (format #t " - [~a] - ~a\n"
                  (date->string edate date-format)
                  (time-difference->h:m:s duration))
          (newline)))))

;;; Print timesheet
(define (print-timesheet timesheet)
  (for-each print-task timesheet))

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
      ;; Add task duration
      (set-car! (cdr tree) (add-duration (cadr tree) duration))

      ;; Search next leaf corresponding to path item
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


;;; Date is today?
(define (today? date)
  (let ((now (current-date)))
    (and
     (= (date-day date) (date-day now))
     (= (date-month date) (date-month now))
     (= (date-year date) (date-year now)))))

;;; Date is in current month?
(define (current-month? date)
  (let ((now (current-date)))
    (and
     (= (date-month date) (date-month now))
     (= (date-year date) (date-year now)))))

;;; Check date for occurence between d-past and d-future
(define (date-in-range? d d-past d-future)
  (let ((dt (date->time-utc d))
        (dpt (date->time-utc d-past))
        (dft (date->time-utc d-future)))
    (and (time>=? dt dpt)
         (time<=? dt dft))))

;;; Truncate hours, minutes and seconds
(define (date-round-day date)
  (let ((day (date-day date))
        (month (date-month date))
        (year (date-year date))
        (zf (date-zone-offset date)))
    (make-date 0 0 0 0 day month year zf)))

;;; Returns today date with time 00:00
;; (define (today)
;;   (date-round-day (current-date)))

;;; Returns tomorrow date with time 00:00
;; (define (tomorrow)
;;   (date-round-day
;;    (julian-day->date
;;     (+ (date->julian-day (current-date)) 1))))

;;; Returns remainder plus fractional part of
;;; truncating x.
;;; For example: (remainder-and-rest 5.1 3) -> 2.1
(define (remainder-and-rest x y)
  (let ((xi (truncate x)))
    (+ (- x xi)
       (remainder xi y))))

;;; Returns monday of current week
(define (monday)
  (date-round-day
   (julian-day->date
    (let ((jd (date->julian-day (current-date))))
      (- jd (remainder-and-rest jd 7))))))

;;; Returns monday of next week
(define (next-monday)
  (date-round-day
   (julian-day->date
    (let ((jd (date->julian-day (current-date))))
      (+ jd (- 7 (remainder-and-rest jd 7)))))))

;;; Print report
(define (print-report report)
  (let walk ((tree report)
             (level 0))
    (when (not (null? tree))
      (format #t "~v_~a: ~a\n" level (car tree)
              (time-difference->h:m:s (cadr tree)))
      (for-each (lambda (l) (walk l (+ level 2))) (cddr tree)))))

;;; Main function
(define (main cmdl)
  (let ((command (cdr cmdl))
        (sheet (read-timesheet ts-file))
        (now (current-date)))
    (let* ((last (last-pair sheet))
           (last (if (null? last) #f (car last))))
      (if (null? command)
          ;; Show report and running task
          (if (and last (not (caddr last)))
              (format #t "~a: ~a\n"
                      (path->string (car last))
                      (time-difference->h:m:s
                       (date-difference now (cadr last))))
              (format #t "NO TASKS\n"))
          ;; Run command
          (let ((param (cdr command))
                (command (car command)))
            (let ((new-sheet
                   (cond
                    ;; ----------------------- Start timer ------------------------- ;;
                    ((string= command "start")
                     (let ((path (if (null? param)
                                     (if last (car last) #f)
                                     (path-split (car param)))))
                       (if (not path)
                           (begin
                             (format
                              (current-error-port)
                              "Not specified task path. No tasks in the sheet.\n")
                             sheet)
                           (begin
                             (when (and last (not (caddr last)))
                               ;; Stop last path if running
                               (set-car! (cddr last) now)
                               (set-car! (cdddr last) (date-difference now (cadr last))))
                             ;; Add new task
                             (let ((new-task (list path now #f #f)))
                               (format #t "--- NEW TASK RUN\n")
                               (print-task new-task)
                               (append sheet (list new-task)))))))
                    ;; ----------------------- Stop timer ------------------------- ;;
                    ((string= command "stop")
                     ;; Stop last path if running
                     (if (and last (not (caddr last)))
                         (begin
                           (set-car! (cddr last) now)
                           (set-car! (cdddr last) (date-difference now (cadr last)))
                           (format #t "--- STOP TASK\n")
                           (print-task last))
                         (format
                          (current-error-port)
                          "Noting to stop, no runnig task\n"))
                     sheet)
                    ;; ----------------------- Show report ------------------------- ;;
                    ((string= command "report")
                     (format #t "--- REPORT")
                     (let ((sheet
                            (if (null? param) sheet
                                (let ((interval (car param)))
                                  (filter (cond
                                           ((string-ci= interval "today")
                                            (format #t " FOR TODAY")
                                            (lambda (x) (today? (cadr x))))
                                           ((string-ci= interval "month")
                                            (format #t " FOR CURRENT MONTH")
                                            (lambda (x) (current-month? (cadr x))))
                                           ((string-ci= interval "week")
                                            (format #t " FOR CURRENT WEEK")
                                            (lambda (x) (date-in-range? (cadr x)
                                                                       (monday)
                                                                       (next-monday))))
                                           (else
                                            (format #t " FOR PROJECT ~a" interval)
                                            (let ((rep-path (path-split interval)))
                                              (lambda (x)
                                                (let loop ((path (car x))
                                                           (rep-path rep-path))
                                                  (if (or
                                                       (null? path)
                                                       (null? rep-path)) #t
                                                      (if (string-ci= (car path) (car rep-path))
                                                          (loop (cdr path) (cdr rep-path))
                                                          #f)))))))
                                          sheet)))))
                       (newline)
                       (print-report
                        (make-report sheet)))
                     (let* ((last (last-pair sheet))
                            (last (if (null? last) #f (car last))))
                       (when last
                         (format #t "\n--- ~a TASK\n"
                                 (if (caddr last) "LAST STOPPED" "RUNNING"))
                         (print-task last now)))
                     #f)
                    ;; ----------------------- Show usage ------------------------- ;;
                    (else
                     (with-output-to-port (current-error-port)
                       (lambda ()
                         (format #t "Usage: ~a [command]\n" (car cmdl))
                         (format #t "Commands:\n")
                         (format #t "    start [TASK]    Start new task. If no task, use last runned task\n")
                         (format #t "    stop            Stop task\n")
                         (format #t "    report          Show report\n")
                         (format #t "    report today    Show report for today\n")
                         (format #t "    report week     Show report for current week\n")
                         (format #t "    report month    Show report for current month\n")
                         (format #t "    report PATH     Show report for project\n")
                         (format #t "                    Show running task and timer\n")
                         (newline)))
                     #f))))
              
              ;; ----------------------- Save new sheet ------------------------- ;;
              (when (and
                     (list? new-sheet)
                     (not (null? new-sheet)))
                (with-output-to-file ts-file
                  (lambda ()
                    (print-timesheet new-sheet))))))))))

;;; JUST DO IT!
(main (command-line))
