#!/usr/bin/env guile
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
        (srfi srfi-11)
        (srfi srfi-43))

;;; Set locale according to environment settings
(setlocale LC_ALL "")

;;; Use srfi-48 in other scheme implementation
(use-modules (ice-9 format))

;;; Record format:
;;; PROJECT/TASK/SUBTASK/ETC: [START_TIME] - [STOP_TIME] - DURATION

;;; Hardcoded timesheet filename
(define ts-file (string-append (getenv "HOME") "/.timesheet"))
(define date-time-format "~Y-~m-~d ~H:~M:~S")
(define date-format "~Y-~m-~d")

;;; ========================= COMMON HELPER FUNCTIONS ==========================

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

;;; Remove duplicates in list
(define (remove-dup list)
  (reverse
   (fold (lambda (x l)
           (if (or (null? l)
                   (not (equal? (car l) x)))
               (cons x l) l))
         '() list)))

;;; Convert time to string "hh:mm:ss"
(define (time->string td)
  (let ((sec (time-second td)))
    (let* ((h (quotient sec 3600))
           (m (quotient (- sec (* h 3600)) 60))
           (s (- sec (* h 3600) (* m 60))))
      (string-append
       (format #f "~2,'0d" h) ":"
       (format #f "~2,'0d" m) ":"
       (format #f "~2,'0d" s)))))

;;; Convert string "hh:mm:ss" to time
(define (string->time str)
  (let ((splitted (string-split str #\:)))
    (if (not (= (length splitted) 3))
        (throw 'bad-time-string "Bad time format (expected hh:mm:ss)" str)
        (let ((h (string->number (car splitted)))
              (m (string->number (cadr splitted)))
              (s (string->number (caddr splitted))))
          (if (and (and h m s) (< m 60) (< s 60))
              (make-time 'time-duration 0
                         (+ (* h 3600) (* m 60) s))
              (throw 'bad-time-string "Bad time string" str))))))

;;; Convert string to date or time
;;; Time is "hh:mm:ss", date is date-format
(define (string->date/time str)
  (if (string-any #\: str)
      (string->time str)
      (string->date str date-format)))

;;; Convert date or time to string
(define (date/time->string time)
  (if (date? time)
      (date->string time date-format)
      (time->string time)))

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

;;; Compare paths
(define (path<? a b)
  (let loop ((a a) (b b))
    (if (and (null? a) (null? b)) #f
        (if (null? a) #t
            (if (null? b) #f
                (if (string=? (car a) (car b))
                    (loop (cdr a) (cdr b))
                    (string< (car a) (car b))))))))

;;; Path prefixes with prefix?
(define (path-prefix? path prefix)
  (let loop ((path path)
             (prefix prefix))
    (if (null? prefix) #t
        (if (null? path) #f
            (if (string=? (car path) (car prefix))
                (loop (cdr path) (cdr prefix))
                #f)))))

;;; Compare dates
(define (date<? a b)
  (let ((ta (date->time-utc a))
        (tb (date->time-utc b)))
    (time<? ta tb)))

;;; Return difference of two dates
(define (date-difference d1 d2)
  (time-difference
   (date->time-utc d1)
   (date->time-utc d2)))

;;; Test dates for equality
(define (date= a b)
  (and (= (date-year a) (date-year b))
       (= (date-month a) (date-month b))
       (= (date-day a) (date-day b))))

;;; Dates is in same day?
(define (same-month? d1 d2)
  (and
   (= (date-month d1) (date-month d2))
   (= (date-year d1) (date-year d2))))

;;; Dates is in same month?
(define (same-day? d1 d2)
  (and
   (same-month? d1 d2)
   (= (date-day d1) (date-day d2))))

;;; Check date for occurence between d-past and d-future
(define (date-in-range? d d-past d-future)
  (let ((dt (date->time-utc d)))
    (and (time>=? dt (date->time-utc d-past))
         (time<=? dt (date->time-utc d-future)))))

;;; Truncate hours, minutes and seconds
(define (date-round-day date)
  (make-date 0 0 0 0
             (date-day date)
             (date-month date)
             (date-year date)
             (date-zone-offset date)))

;;; Returns remainder plus fractional part of
;;; truncating x.
;;; For example: (remainder-and-rest 5.1 3) -> 2.1
(define (remainder-and-rest x y)
  (let ((xi (truncate x)))
    (+ (- x xi)
       (remainder xi y))))

;;; Returns monday of current week
(define (monday-of-week date)
  (date-round-day
   (julian-day->date
    (let ((jd (date->julian-day date)))
      (- jd (remainder-and-rest jd 7))))))

;;; Returns monday of next week
(define (monday-of-next-week date)
  (date-round-day
   (julian-day->date
    (let ((jd (date->julian-day date)))
      (+ jd (- 7 (remainder-and-rest jd 7)))))))

;;; Returns n-th element of list l, or NUL if list is shorter than n
(define (nth-maybe n l)
  (if (null? l) '()
      (if (zero? n) (car l)
          (nth-maybe (- n 1) (cdr l)))))

;;; Add time duration to date
(define (date-add-duration date duration)
  (time-monotonic->date
   (add-duration
    (date->time-monotonic date) duration)))

;;; ========================= PROJECT SPECIFIC HELPERS =========================

;;; Find task or deadline by path
(define (find-by-path sheet path)
  (find (lambda (x) (equal? (car x) path)) sheet))

;;; Compare deadlines by path
(define (deadline<? a b)
  (path<? (car a) (car b)))

;;; Compare timerecords
(define (timerecord<? a b)
  (let ((a-start-time (cadr a))
        (a-stop-time (caddr a))
        (b-start-time (cadr b))
        (b-stop-time (caddr b)))
    (if (and (not a-stop-time)
             (not b-stop-time))
        #f
        (if (not a-stop-time) #f
            (date<? a-start-time b-start-time)))))

;;; Returns last record of the sheet or #f if sheet is empty.
(define (last-task sheet)
  (let ((last (last-pair sheet)))
    (if (null? last) #f (car last))))

;;; Returns running record
(define (running sheet)
  (let ((last (last-task sheet)))
    (if (and last (not (caddr last))) last #f)))

;;; Stop task. Returns #t if stopped, #f if no running task.
(define (stop-task task)
  (if (not (caddr task))
      (let ((now (current-date)))
        (set-car! (cddr task) now)
        (set-car! (cdddr task) (date-difference now (cadr task)))
        #t)
      #f))

;;; Start new task and append its to the sheet.
;;; Returns new sheet and new task.
(define (new-task sheet path)
  (let ((task (list path (current-date) #f #f)))
    (values (append sheet (list task)) task)))

;;; Returns unique path names of record lists
(define (record-path-list records . more)
  (remove-dup
   (sort
    (let loop ((recs (apply append (cons records more)))
               (tasklist '()))
      (if (null? recs) tasklist
          (loop (cdr recs)
                (let fold-path ((path (caar recs))
                                (spath "")
                                (tasklist tasklist))
                  (if (null? path) tasklist
                      (let ((spath (string-append
                                    spath
                                    (if (zero? (string-length spath)) "" "/")
                                    (car path))))
                        (fold-path (cdr path) spath
                                   (cons spath tasklist))))))))
    string<?)))

;;; Returns unique task names
(define (record-name-list records . more)
  (remove-dup
   (sort
    (map (lambda (x) (path->string (car x)))
         (apply append (cons records more)))
    string<?)))

;;; Check the path it belongs to archive
(define (path-in-archive? path archive)
  (let loop ((a archive))
    (if (null? a) #f
        (if (path-prefix? path (car a)) #t
            (loop (cdr a))))))

;;; ========================= PROJECT MAIN FUNCTIONS  ==========================

;;; Parse task string and return list:
;;; '((list of path elements) start-date stop-date|#f duration|#f)
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
          (if (or (not path) (not dates) (null? dates))
              #f
              (let ((date-start (string->date (car dates) date-time-format)))
                (let-values (((date-end duration)
                              (if (null? (cdr dates))
                                  (values #f #f)
                                  (let* ((date-end (string->date (cadr dates) date-time-format))
                                         (duration (time-difference
                                                    (date->time-utc date-end)
                                                    (date->time-utc date-start))))
                                    (values date-end duration)))))
                  (list path date-start date-end duration))))))))

;;; Deadline file format
;;; PROJECT/TASK/ETC: DATE|TIME
;;; Returns list '((list of path elements) date|time)
(define (parse-deadline-string str)
  (let ((colon (string-index str #\:)))
    (if (not colon) #f
        (let ((path (path-split
                     (string-trim
                      (substring str 0 colon))))
              (time (string-trim-both
                     (substring str
                                (1+ colon)
                                (string-length str)))))
          (if (string-null? time) #f
              (let ((time (string->date/time time)))
                (list path time)))))))


;;; Read timesheet and deadlines
(define (read-timesheet filename)
  (if (file-exists? filename)
      (call-with-input-file filename
        (lambda (port)
          (let loop ((record-type 'unknown)
                     (timerecords '())
                     (deadlines '())
                     (archives '()))
            (let ((line (get-line port)))
              (if (eof-object? line)
                  (values (remove-dup
                           (sort timerecords timerecord<?))
                          (remove-dup
                           (sort deadlines deadline<?))
                          (remove-dup
                           (sort archives path<?)))
                  (let ((line (string-trim-both line)))
                    (cond
                     ((or (string-null? line)
                          (eq? (string-ref line 0) #\#))
                      (loop record-type timerecords deadlines archives))
                     ((string-ci=? line "--- DEADLINES")
                      (loop 'deadline timerecords deadlines archives))
                     ((string-ci=? line "--- TIMESHEET")
                      (loop 'timerecord timerecords deadlines archives))
                     ((string-ci=? line "--- ARCHIVE")
                      (loop 'archive timerecords deadlines archives))
                     (else
                      (cond
                       ((eq? record-type 'timerecord)
                        (loop record-type
                              (let ((item (parse-task-string line)))
                                (if item
                                    (cons item timerecords)
                                    (begin
                                      (format #t "Warning: Line '~a' is not a timerecord. Skip.\n" line)
                                      timerecords)))
                              deadlines
                              archives))
                       ((eq? record-type 'deadline)
                        (loop record-type
                              timerecords
                              (let ((item (parse-deadline-string line)))
                                (if item
                                    (cons item deadlines)
                                    (begin
                                      (format #t "Warning: Line '~a' is not a deadline. Skip.\n" line)
                                      deadlines)))
                              archives))
                       ((eq? record-type 'archive)
                        (loop record-type
                              timerecords
                              deadlines
                              (let ((item (path-split line)))
                                (if item
                                    (cons item archives)
                                    (begin
                                      (format #t "Warning: Line '~a' is not a archive path. Skip.\n" line)
                                      archives)))))
                       (else (loop record-type timerecords deadlines archives)))))))))))
      (values '() '() '())))

;;; Print deadline record
(define (print-deadline dline)
  (format #t "~a: ~a\n"
          (path->string (car dline))
          (date/time->string (cadr dline))))

;;; Print timesheet record
(define (print-timerecord task . now)
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
              (date->string sdate date-time-format))
      (if edate
          (format #t " - [~a] - ~a\n"
                  (date->string edate date-time-format)
                  (time->string duration))
          (newline)))))

;;; Print timerecords
(define (print-timerecords timesheet)
  (for-each print-timerecord (sort timesheet timerecord<?)))

;;; Print deadlines
(define (print-deadlines deadlines)
  (for-each print-deadline (sort deadlines deadline<?)))

;;; Print archives
(define (print-archives archives)
  (for-each
   (lambda (a) (format #t "~a\n" (path->string a)))
   archives))

;;; Print timesheet
(define (print-timesheet timesheet deadlines archives)
  (unless (null? deadlines)
    (format #t "--- DEADLINES\n")
    (print-deadlines deadlines))
  (unless (null? archives)
    (format #t "--- ARCHIVE\n")
    (print-archives archives))
  (unless (null? timesheet)
    (format #t "--- TIMESHEET\n")
    (print-timerecords timesheet)))

;;; Make report
;;; Report is a tree of projects items.
;;; Root of the tree is super-project with name "ROOT":
;;; ("ROOT" d dl
;;;  ("PROJ1" d dl
;;;   ("P1-TASK1" d dl
;;;    ("P1-T1-SUBTASK1" d dl)
;;;    ("P1-T1-SUBTASK2" d dl))
;;;   ("P1-TASK2" d dl)))
(define (make-report timesheet)
  ;; Add task duration to project branch
  (define (tree-add-duration! project-tree path duration)
    (let tree-walk ((tree project-tree)
                    (path path))
      ;; Add task duration
      (set-car! (cdr tree) (add-duration (cadr tree) duration))

      ;; Search next leaf corresponding to path item
      (if (or (null? path))
          project-tree
          (let ((item (find
                       (lambda (i) (string= (car i) (car path)))
                       (cdddr tree))))
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
                                  (list (car path) duration #f)
                                  (list (car path) duration #f add-items))
                              (cdr path))))))
                  (set-cdr! (cddr tree) (cons add-items (cdddr tree)))
                  project-tree))))))
  ;; END tree-add-duration!

  (let loop ((projects (list "Overall" (make-time 'time-duration 0 0) #f))
             (timesheet timesheet))
    (if (null? timesheet)
        projects
        (let* ((task (car timesheet))
               (path (car task))
               (start (cadr task))
               (duration (cadddr task)))
          (loop (tree-add-duration! projects path
                                    (if duration duration
                                        (date-difference (current-date) start)))
                (cdr timesheet))))))

;;; Add deadlines to report tree
(define (add-deadlines-to-report! report deadlines)
  (let walk ((tree report)
             (path '()))
    (unless (null? tree)
      (let ((deadline (find-by-path deadlines path)))
        (when deadline
          (set-car! (cddr tree) (cadr deadline))))
      (for-each
       (lambda (l)
         (walk l (append path (list (car l)))))
       (cdddr tree))))
  report)

;;; Print report
(define (print-report report)
  (let walk ((tree report)
             (level 0))
    (unless (null? tree)
      (format #t "~v_~a: ~a~a\n"
              level (car tree)
              (time->string (cadr tree))
              (let ((deadline (caddr tree)))
                (cond
                 ;; deadline is date
                 ((date? deadline)
                  (string-append
                   " -- deadline at "
                   (date/time->string deadline)
                   (if (date<? (date-round-day deadline)
                               (date-round-day (current-date)))
                       " (expired)" "")))

                 ;; deadline is time
                 ((time? deadline)
                  (string-append
                   " -- deadline time "
                   (date/time->string deadline)
                   (if (time<? deadline (cadr tree))
                       " (expired)"
                       (string-append " (expires in "
                                      (time->string
                                       (time-difference deadline (cadr tree))) ")"))))

                 ;; no deadline
                 (else ""))))

      (for-each
       (lambda (l)
         (walk l (+ level 2)))
       (cdddr tree)))))

;;; Filter sheet by qualis and range.
;;; Qualis is string "day", "week", "month" or task name.
;;; Range is the date string. Both qualis and range may be nil.
;;;
;;; Function returns values of filtered sheet and string
;;; with description of filtering range.
(define (filter-sheet sheet qualis range)
  (if (null? qualis)
      (values sheet '())
      (let ((date (catch #t
                    (lambda () (string->date range date-format))
                    (lambda (key . args)
                      (let ((n (if (null? range) #f (string->number range))))
                        (if (and n (string-ci= qualis "day"))
                            (date-add-duration
                             (current-date)
                             (make-time time-duration 0
                                        (* n 24 60 60)))
                            (current-date)))))))
        (let-values (((description filter-lambda)
                      (cond
                       ;; Filter records by day
                       ((string-ci= qualis "day")
                        (values
                         (format #f "DAY ~a" (date->string date "~Y-~m-~d"))
                         (lambda (x) (same-day? date (cadr x)))))

                       ;; Filter records by month
                       ((string-ci= qualis "month")
                        (values
                         (format #f "MONTH ~a" (date->string date "~Y-~m"))
                         (lambda (x) (same-month? date (cadr x)))))

                       ;; Filter records by week
                       ((string-ci= qualis "week")
                        (let ((beg (monday-of-week date))
                              (end (monday-of-next-week date)))
                          (values
                           (format #f "WEEK [~a - ~a)"
                                   (date->string beg date-format)
                                   (date->string end date-format))
                           (lambda (x) (date-in-range? (cadr x) beg end)))))

                       ;; Filter records by path
                       (else
                        (values
                         (format #f "PROJECT ~a" qualis)
                         (let ((rep-path (path-split qualis)))
                           (lambda (x)
                             (let loop ((path (car x))
                                        (rep-path rep-path))
                               (if (or
                                    (null? path)
                                    (null? rep-path)) #t
                                    (if (string-ci= (car path) (car rep-path))
                                        (loop (cdr path) (cdr rep-path))
                                        #f))))))))))
          (values
           (filter filter-lambda sheet)
           description)))))

;;; Remove archived tasks from sheet
(define (not-archived sheet archives)
  (filter (lambda (x)
            (not (path-in-archive?
                  (car x)
                  archives))) sheet))

;;; ================================ COMMANDS ==================================

;;; Start new task. Returns new sheet with started task or #f if nothing started.
(define (cmd-start-task sheet deadlines archives . params)
  (let* ((last (last-task sheet))
         (path (if (null? params)
                   (if last (car last) #f)
                   (path-split (car params))))
         (now (current-date)))
    (if (not path)
        (begin (format
                (current-error-port)
                "Not specified task path. No tasks in the sheet.\n")
               (values #f #f #f))
        (begin
          (when last (stop-task last))
          (let-values (((sheet task) (new-task sheet path)))
            (format #t "--- NEW TASK RUN\n")
            (print-timerecord task)
            (values sheet deadlines archives))))))

;;; Stop a running task. Returns new sheet or #f if nothing to stop.
(define (cmd-stop-task sheet deadlines archives . params)
  (let ((last (last-task sheet)))
    (if (and last (stop-task last))
        (begin
          (format #t "--- STOP TASK\n")
          (print-timerecord last)
          (values sheet deadlines archives))
        (begin
          (format (current-error-port) "Nothing to stop\n")
          (values #f #f #f)))))

;;; Print report
(define (cmd-report sheet deadlines archives . params)
  (format #t "--- REPORT")
  (let ((qualis (nth-maybe 0 params))
        (range (nth-maybe 1 params)))
    (let-values (((sheet description)
                  (filter-sheet
                   (not-archived sheet archives)
                   qualis range)))
      (when (not (null? description))
        (display ". ")
        (display description))
      (newline)
      (print-report
       (add-deadlines-to-report!
        (make-report sheet) deadlines))))

  (let ((last (last-task sheet)))
    (when last
      (format #t "\n--- ~a TASK\n"
              (if (caddr last) "LAST STOPPED" "RUNNING"))
      (print-timerecord last (current-date))))
  (values #f #f #f))

;;; Print all tasks
(define (cmd-tasklist sheet deadlines archives . unused)
  (format #t "~{~a ~}\n" (record-path-list
                          (not-archived sheet archives)
                          (not-archived deadlines archives)))
  (values #f #f #f))

;;; Print dmenu items
(define (cmd-dmenu sheet deadlines archives . unused)
  (let ((runrec (running sheet)))
    (if runrec
        (format #t "-- STOP ~a\n" (path->string (car runrec)))
        (let ((last (last-task sheet)))
          (when last
            (format #t "-- START ~a\n" (path->string (car last)))))))

  (format #t "~{~a\n~}" (record-path-list
                          (not-archived sheet archives)
                          (not-archived deadlines archives)))
  (values #f #f #f))

;;; Print deadlines
(define (cmd-deadlist sheet deadlines archives . unused)
  (format #t "~{~a ~}\n" (record-name-list deadlines))
  (values #f #f #f))

;;; Print archive
(define (cmd-archlist sheet deadlines archives . unused)
  (format #t "~{~a ~}\n"
          (remove-dup
           (sort
            (map (lambda (x) (path->string x)) archives)
            string<?)))
  (values #f #f #f))

;;; Print last task
(define (cmd-lasttask sheet deadlines archives . unused)
  (let ((last (last-task sheet)))
    (format #t "~a\n" (if last (path->string (car last)) "")))
  (values #f #f #f))

;;; Deadlines
(define (cmd-deadline sheet deadlines archives . args)
  (let ((deadlines
         (let* ((arg0 (if (null? args) #f (car args)))
                (arg1 (if (and arg0 (not (null? (cdr args)))) (cadr args) #f))
                (arg2 (if (and arg1 (not (null? (cddr args)))) (caddr args) #f))
                (last (last-task sheet)))
           (cond
            ;; Add deadline
            ((equal? arg0 "set")
             (if (not arg1)
                 (begin (format #t "Not specified date/time of deadline\n") #f)
                 (let-values (((task time)
                               (if arg2
                                   (values (path-split arg1)
                                           (string->date/time arg2))
                                   (values (if last (car last) #f)
                                           (string->date/time arg1)))))
                   (if (not task)
                       (begin
                         (format #t "Not specified task path.\n")
                         #f)
                       (let ((dl (find-by-path deadlines task)))
                         (if dl
                             (begin (set-car! (cdr dl) time) deadlines)
                             (append deadlines (list (list task time)))))))))

            ;; Delete deadline
            ((equal? arg0 "clear")
             (call/cc
              (lambda (break-del)
                (let ((task (if arg1
                                (path-split arg1)
                                (if (not last)
                                    (begin (format #t "Not specified task path.\n") (break-del #f))
                                    (car last)))))
                  (fold-right
                   (lambda (x l)
                     (if (equal? (car x) task)
                         (begin
                           (format #t "Deleted ~a: ~a\n"
                                   (path->string (car x))
                                   (date/time->string (cadr x)))
                           l)
                         (cons x l)))
                   '() deadlines)))))

            ;; Show all deadlines
            ((equal? arg0 "all")
             (print-deadlines deadlines)
             #f)

            ;; Show deadline for task
            (else
             (let ((task (if arg0
                             (path-split arg0)
                             (if (not last)
                                 (begin (format #t "--- ALL DEADLINES\n") `())
                                 (car last)))))
               (for-each
                (lambda (dl)
                  (when (let loop ((p1 task)
                                   (p2 (car dl)))
                          (if (null? p1) #t
                              (if (null? p2) #f
                                  (if (not (string=? (car p1) (car p2))) #f
                                      (loop (cdr p1) (cdr p2))))))
                    (print-deadline dl)))
                deadlines))
             #f)))))
    (if deadlines
        (values sheet deadlines archives)
        (values #f #f #f))))

;;; Events
(define (cmd-timesheet sheet deadlines archives . params)
  (format #t "--- TIMESHEET")
  (let ((qualis (nth-maybe 0 params))
        (range (nth-maybe 1 params)))
    (let-values (((sheet description)
                  (filter-sheet
                   (not-archived sheet archives)
                   qualis range)))
      (when (not (null? description))
        (display ". ")
        (display description))
      (newline)
      (print-timerecords sheet)))
  (values #f #f #f))

;;; Archive
(define (cmd-archive sheet deadlines archives . params)
  (let ((task-str (nth-maybe 0 params)))
    (if (null? task-str)
        (begin
          (format #t "--- ARCHIVE\n")
          (print-archives archives)
          (values #f #f #f))
        (values sheet
                deadlines
                (cons (path-split task-str) archives)))))

;;; Unarchive
(define (cmd-unarch sheet deadlines archives . params)
  (let ((task-str (nth-maybe 0 params)))
    (if (null? task-str)
        (values #f #f #f)
        (let ((task (path-split task-str)))
          (values sheet
                  deadlines
                  (fold (lambda (p a)
                          (if (equal? p task) a
                              (cons p a))) '() archives))))))

;;; Show current running task
(define (cmd-current sheet deadlines archives . params)
  (let ((runrec (running sheet)))
    (if runrec
        (let* ((path (car runrec))
               (timer (date-difference (current-date) (cadr runrec))))
          (format #t "~a: ~a ~a\n"
                  (path->string path)
                  (time->string timer)
                  ;; Print deadline
                  (let ((deadline (find-by-path deadlines path)))
                    (if (and deadline (cadr deadline))
                        (let ((deadtime (cadr deadline)))
                          (string-append
                           "(" (if (or (and (date? deadtime)
                                            (date<? (date-round-day deadtime)
                                                    (date-round-day (current-date))))
                                       (and (time? deadtime)
                                            (time<? deadtime timer)))
                                   "expired"
                                   (date/time->string deadtime))
                           ")"))
                        ""))))
        (begin
          (format #t "NO TASKS\n"))))
  (values #f #f #f))

;; stat is a list of lists: ((<index> <count-of-adds> <duration>)...)
(define (stat-add-duration stat idx duration idx-equality)
  (define (idx-eq item) (idx-equality (car item) idx))
  (if (any idx-eq stat)
      (map (lambda (i) (if (idx-eq i)
                      (list idx
                            (+ (cadr i) 1)
                            (add-duration (caddr i) duration))
                      i)) stat)
      (cons (list idx 1 duration) stat)))

(define week-day-names
  '((0 (7 "SUNDAY" "SUN"))
    (1 (1 "MONDAY" "MON"))
    (2 (2 "TUESDAY" "TUE"))
    (3 (3 "WEDNESDAY" "WED"))
    (4 (4 "THURSDAY" "THU"))
    (5 (5 "FRIDAY" "FRI"))
    (6 (6 "SATURDAY" "SAT"))))

;;; Show statistics
(define (cmd-stats sheet deadlines archives . params)
  (let* ((task-str (nth-maybe 0 params))
         (req-path (if (null? task-str) #f (path-split task-str))))

    ;; Make list with (by-date by-path) duration
    (let ((by-date
           (fold (lambda (rec by-date-stat)
                   (let ((path (car rec))
                         (start (cadr rec))
                         (dur (cadddr rec)))
                     (stat-add-duration by-date-stat (list path start) dur
                                        (lambda (i1 i2)
                                          (and (equal? (car i1) (car i2))
                                               (date= (cadr i1) (cadr i2)))))))
                 '() sheet)))

      ;; Make statistics by day of the week
      (let ((by-day
             (fold (lambda (rec by-day-stat)
                     (let ((path (caar rec))
                           (start (cadar rec))
                           (dur (caddr rec)))
                       (if (or (not req-path)
                               (path-prefix? path req-path))
                           (stat-add-duration by-day-stat (date-week-day start) dur =)
                           by-day-stat)))
                   '() by-date)))

        ;; Print
        (format #t "-- STATISTICS\n")
        (when (not (null? by-day))
          (format #t "By day-of-week statistics for ~a:\n" (if req-path (path->string req-path) "all projects"))
          (for-each (lambda (r)
                      (let ((day-name (cadr (assq (car r) week-day-names))))
                        (format #t "  ~a ~a: ~a\n"
                                (car day-name)
                                (caddr day-name)
                                (time->string (make-time 'time-duration 0
                                                         (round
                                                          (/ (time-second (caddr r)) (cadr r))))))))
                    (sort by-day (lambda (a b)
                                   ;; Set MONDAY as first day of week
                                   (cond
                                    ((= (car a) 0) #f)
                                    ((= (car b) 0) #t)
                                    (else (< (car a) (car b)))))))))))
  (values #f #f #f))

;;; ================================ MAIN FUNCTION ==================================

(define (main cmdl)
  (let ((command (cdr cmdl)))
    (let-values (((sheet deadlines archives) (read-timesheet ts-file)))
      (if (null? command)
          (begin
            (cmd-current sheet deadlines archives)
            (if (not (running sheet))
                (begin
                  (display "Last task: ")
                  (cmd-lasttask sheet deadlines archives))))

          ;; Else run command
          (let ((param (cdr command))
                (command (car command)))
            (let-values
                (((sheet' deadlines' archives')
                  (apply
                   (cond
                    ((string= command "start") cmd-start-task)
                    ((string= command "stop") cmd-stop-task)
                    ((string= command "report") cmd-report)
                    ((string= command "refresh") (lambda (s d a . p) (values s d a)))
                    ((string= command "deadline") cmd-deadline)
                    ((string= command "timesheet") cmd-timesheet)
                    ((string= command "archive") cmd-archive)
                    ((string= command "unarch") cmd-unarch)
                    ((string= command "current") cmd-current)
                    ((string= command "stats") cmd-stats)
                    ;; Service commands
                    ((string= command "tasklist") cmd-tasklist)
                    ((string= command "deadlist") cmd-deadlist)
                    ((string= command "archlist") cmd-archlist)
                    ((string= command "lasttask") cmd-lasttask)
                    ((string= command "dmenu") cmd-dmenu)

                    ;; ----------------------- Show usage ------------------------- ;;
                    (else
                     (with-output-to-port (current-error-port)
                       (lambda ()
                         (format #t "Usage: ~a [command]\n" (car cmdl))
                         (format #t "Commands:\n")
                         (format #t "    start [TASK]                    Start new task. If no task, use last runned task\n")
                         (format #t "    stop                            Stop task\n")
                         (format #t "    current                         Show current running task\n")
                         (format #t "    report                          Show report\n")
                         (format #t "    report day [DATE/DELTA]         Show report for today or DATE or DELTA days ago (e.g. -1)\n")
                         (format #t "    report week [DATE]              Show report for current week or week of DATE\n")
                         (format #t "    report month [DATE]             Show report for current month or month of DATE\n")
                         (format #t "    report TASK                     Show report for project\n")
                         (format #t "    deadline set [TASK] DATE|TIME   Add deadline for project (or for last task)\n")
                         (format #t "    deadline clear [TASK]           Remove deadline for project (or for last task)\n")
                         (format #t "    deadline [TASK]                 Show deadline for project\n")
                         (format #t "    deadline all                    Show all deadlines\n")
                         (format #t "    timesheet                       Show all raw events\n")
                         (format #t "    timesheet day [DATE]            Show raw events for today or DATE\n")
                         (format #t "    timesheet week [DATE]           Show raw events for current week or week of DATE\n")
                         (format #t "    timesheet month [DATE]          Show raw events for current month or month of DATE\n")
                         (format #t "    timesheet TASK                  Show raw events\n")
                         (format #t "    archive                         Show archive tasks\n")
                         (format #t "    archive TASK                    Add task to archive\n")
                         (format #t "    unarch TASK                     Remove task from archive\n")
                         (format #t "    refresh                         Refresh worksheet file after manual edit\n")
                         (format #t "    stats                           Show statistics\n")
                         (format #t "    (no command)                    Show running task and timer\n\n")
                         (format #t "DATE format: YYYY-mm-dd\n")
                         (format #t "TIME format: HH:MM:SS\n")
                         (newline)))
                     (lambda (s d a . p) (values #f #f #f))))
                   (cons* sheet deadlines archives param))))

              ;; ----------------------- Save new sheet ------------------------- ;;
              (when (and
                     (list? sheet')
                     (not (null? sheet')))
                (when (access? ts-file W_OK)
                  (copy-file ts-file (string-append ts-file ".bak")))
                (with-output-to-file ts-file
                  (lambda ()
                    (print-timesheet sheet' deadlines' archives'))))))))))

;;; JUST DO IT!
(main (command-line))
