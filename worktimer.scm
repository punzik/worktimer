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
(define dl-file (string-append (getenv "HOME") "/.deadlines"))
(define date-time-format "~Y-~m-~d ~H:~M:~S")
(define date-format "~Y-~m-~d")

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
(define (time->string td)
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

;;; Read file and parse it line by line.
;;; Returns list of parsed lines.
(define (read-and-parse-file filename lineparser)
  (if (file-exists? filename)
      (call-with-input-file filename
        (lambda (port)
          (let loop ((recs '()))
            (let ((line (get-line port)))
              (if (eof-object? line)
                  (reverse recs)
                  (loop (let ((item (lineparser line)))
                          (if item (cons item recs) recs))))))))
      '()))

;;; Parse timesheet file and return list of tasks
(define (read-timesheet filename)
  (read-and-parse-file filename parse-task-string))

;;; Compare paths
(define (path<? a b)
  (let loop ((a a) (b b))
    (if (and (null? a) (null? b)) #f
        (if (null? a) #t
            (if (null? b) #f
                (if (string=? (car a) (car b))
                    (loop (cdr a) (cdr b))
                    (string< (car a) (car b))))))))

;;; Compare deadlines by path
(define (deadline<? a b)
  (path<? (car a) (car b)))

;;; Read timesheet and deadlines
(define (read-timesheet1 filename)
  (if (file-exists? filename)
      (call-with-input-file filename
        (lambda (port)
          (let loop ((record-type 'unknown)
                     (timerecords '())
                     (deadlines '()))
            (let ((line (get-line port)))
              (if (eof-object? line)
                  (values (reverse timerecords)
                          (sort deadlines deadline<?))
                  (let ((line (string-trim-both line)))
                    (cond
                     ((or (string-null? line)
                          (eq? (string-ref line 0) #\#))
                      (loop record-type timerecords deadlines))
                     ((string-ci=? line "--- DEADLINES")
                      (loop 'deadline timerecords deadlines))
                     ((string-ci=? line "--- TIMESHEET")
                      (loop 'timerecord timerecords deadlines))
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
                              deadlines))
                       ((eq? record-type 'deadline)
                        (loop record-type
                              timerecords
                              (let ((item (parse-deadline-string line)))
                                (if item
                                    (cons item deadlines)
                                    (begin
                                      (format #t "Warning: Line '~a' is not a deadline. Skip.\n" line)
                                      deadlines))))))))))))))
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
              (date->string sdate date-time-format))
      (if edate
          (format #t " - [~a] - ~a\n"
                  (date->string edate date-time-format)
                  (time->string duration))
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
               (start (cadr task))
               (duration (cadddr task)))
          (loop (tree-add-duration projects path
                                   (if duration duration
                                       (date-difference (current-date) start)))
                (cdr timesheet))))))


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

;;; Print report
(define (print-report report)
  (let ((deadlines (read-deadlines dl-file)))
    (let walk ((tree report)
               (level 0)
               (path '()))
      (when (not (null? tree))
        (format #t "~v_~a: ~a" level (car tree)
                (time->string (cadr tree)))

        (let ((dl (find-by-path deadlines path)))
          (when dl
            (let ((time (cadr dl)))
              (if (date? time)
                  (begin
                    (format #t " -- DEADLINE AT ~a" (date/time->string time))
                    (when (time<? (date->time-utc time)
                                  (date->time-utc (current-date)))
                      (format #t " (expired)")))
                  (begin
                    (format #t " -- DEADLINE TIME ~a" (date/time->string time))
                    (if (time<? time (cadr tree))
                        (format #t " (expired)")
                        (format #t " (expiration at ~a)"
                                (time->string (time-difference time (cadr tree))))))))))
        (newline)
        
        (for-each
         (lambda (l)
           (walk l (+ level 2) (append path (list (car l)))))
         (cddr tree))))))

;;; Returns last record of the sheet or #f if sheet is empty.
(define (last-task sheet)
  (let ((last (last-pair sheet)))
    (if (null? last) #f (car last))))

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

;;; Convert string HH:MM:SS to time
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
;;; Time is HH:MM:SS, date is date-format
(define (string->date/time str)
  (if (string-any #\: str)
      (string->time str)
      (string->date str date-format)))

;;; Convert date or time to string
(define (date/time->string time)
  (if (date? time)
      (date->string time date-format)
      (time->string time)))

;;; Deadline file format
;;; PROJECT/TASK/ETC: DATE|TIME
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

;;; Print deadline record
(define (print-deadline dline)
  (format #t "~a: ~a\n"
          (path->string (car dline))
          (date/time->string (cadr dline))))

;;; Read deadline records from file
(define (read-deadlines filename)
  (sort
   (read-and-parse-file filename parse-deadline-string)
   (lambda (a b)
     (let loop ((pa (car a))
                (pb (car b)))
       (if (and (null? pa) (null? pb)) #f
           (if (null? pa) #t
               (if (null? pb) #f
                   (if (string=? (car pa) (car pb))
                       (loop (cdr pa) (cdr pb))
                       (string< (car pa) (car pb))))))))))

;;; Print deadline records
(define (print-deadlines dlines)
  (for-each (lambda (x) (print-deadline x)) dlines))

;;; Find task or deadline by path
(define (find-by-path sheet path)
  (find (lambda (x) (equal? (car x) path)) sheet))

;;; ================================ COMMANDS ==================================

;;; Start new task. Returns new sheet with started task or #f if nothing started.
(define (cmd-start-task sheet . params)
  (let* ((last (last-task sheet))
         (path (if (null? params)
                   (if last (car last) #f)
                   (path-split (car params))))
         (now (current-date)))
    (if (not path)
        (begin (format
                (current-error-port)
                "Not specified task path. No tasks in the sheet.\n")
               #f)
        (begin
          (stop-task last)
          (let-values (((sheet task) (new-task sheet path)))
            (format #t "--- NEW TASK RUN\n")
            (print-task task)
            sheet)))))

;;; Stop a running task. Returns new sheet or #f if nothing to stop.
(define (cmd-stop-task sheet . params)
  (let ((last (last-task sheet)))
    (if (stop-task last)
        (begin
          (format #t "--- STOP TASK\n")
          (print-task last)
          sheet)
        (begin
          (format (current-error-port) "Nothing to stop\n")
          #f))))

;;; Print report
(define (cmd-report sheet . params)
  (format #t "--- REPORT")
  (let ((sheet
         (if (null? params) sheet
             (let* ((interval (car params))
                    (report-date (catch #t
                                   (lambda () (string->date (cadr params) date-format))
                                   (lambda (key . args) (current-date)))))
               (filter (cond
                        ;; Filter records by day
                        ((string-ci= interval "day")
                         (format #t ". DAY ~a" (date->string report-date "~Y-~m-~d"))
                         (lambda (x) (same-day? report-date (cadr x))))

                        ;; Filter records by month
                        ((string-ci= interval "month")
                         (format #t ". MONTH ~a" (date->string report-date "~Y-~m"))
                         (lambda (x) (same-month? report-date (cadr x))))

                        ;; Filter records by week
                        ((string-ci= interval "week")
                         (let ((beg (monday-of-week report-date))
                               (end (monday-of-next-week report-date)))
                           (format #t ". WEEK [~a - ~a)"
                                   (date->string beg date-format)
                                   (date->string end date-format))
                           (lambda (x) (date-in-range? (cadr x) beg end))))

                        ;; Filter records by path
                        (else
                         (format #t ". PROJECT ~a" interval)
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
  (let ((last (last-task sheet)))
    (when last
      (format #t "\n--- ~a TASK\n"
              (if (caddr last) "LAST STOPPED" "RUNNING"))
      (print-task last (current-date))))
  #f)

;;; Print all tasks
(define (cmd-tasklist sheet . unused)
  (format
   #t "~{~a ~}\n"
   (let loop ((sheet sheet)
              (tasklist '()))
     (if (null? sheet) tasklist
         (loop (cdr sheet)
               (let fold-path ((path (caar sheet))
                               (spath "")
                               (tasklist tasklist))
                 (if (null? path) tasklist 
                     (let ((spath (string-append
                                   spath
                                   (if (zero? (string-length spath)) "" "/")
                                   (car path))))
                       (fold-path (cdr path) spath
                                  (if (find (lambda (x) (string=? x spath)) tasklist)
                                      tasklist
                                      (cons spath tasklist))))))))))
  
  #f)

;;; Deadlines
(define (cmd-deadline sheet . args)
  (let ((dlines (read-deadlines dl-file)))
    (let ((new-dlines
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
                         (let ((dl (find-by-path dlines task)))
                           (if dl
                               (begin (set-car! (cdr dl) time) dlines)
                               (append dlines (list (list task time)))))))))

              ;; Delete deadline
              ((equal? arg0 "clear")
               (call/cc
                (lambda (break-del)
                  (let ((task (if arg1
                                  (path-split arg1)
                                  (if (null? last)
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
                     '() dlines)))))

              ;; Show all deadlines
              ((equal? arg0 "all")
               (print-deadlines dlines)
               #f)
              
              ;; Show deadline for task
              (else
               (let ((task (if arg0
                               (path-split arg0)
                               (if (null? last)
                                   (begin (format #t "--- SHOW ALL DEADLINES\n") `())
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
                  dlines))
               #f)))))
      
      (when new-dlines
        (with-output-to-file dl-file
          (lambda () (print-deadlines new-dlines))))))
  #f)

;;; ================================ MAIN FUNCTION ==================================
(define (main cmdl)
  (let ((command (cdr cmdl)))
    (let-values (((sheet deadlines) (read-timesheet1 ts-file)))
      (if (null? command)

          ;; Show running task
          (let ((last (last-task sheet)))
            (if (and last (not (caddr last)))
                (format #t "~a: ~a\n"
                        (path->string (car last))
                        (time->string
                         (date-difference (current-date) (cadr last))))
                (format #t "NO TASKS\n")))

          ;; Else run command
          (let ((param (cdr command))
                (command (car command)))
            (let ((new-sheet
                   (apply (cond
                           ((string= command "start") cmd-start-task)
                           ((string= command "stop") cmd-stop-task)
                           ((string= command "report") cmd-report)
                           ((string= command "refresh") (lambda (s . p) s))
                           ((string= command "tasklist") cmd-tasklist)
                           ((string= command "deadline") cmd-deadline)
                           
                           ;; ----------------------- Show usage ------------------------- ;;
                           (else
                            (with-output-to-port (current-error-port)
                              (lambda ()
                                (format #t "Usage: ~a [command]\n" (car cmdl))
                                (format #t "Commands:\n")
                                (format #t "    start [TASK]                    Start new task. If no task, use last runned task\n")
                                (format #t "    stop                            Stop task\n")
                                (format #t "    report                          Show report\n")
                                (format #t "    report day [DATE]               Show report for today or DATE\n")
                                (format #t "    report week [DATE]              Show report for current week or week of DATE\n")
                                (format #t "    report month [DATE]             Show report for current month or month of DATE\n")
                                (format #t "    report TASK                     Show report for project\n")
                                (format #t "    deadline set [TASK] DATE|+TIME  Add deadline for project (or for last task)\n")
                                (format #t "    deadline clear [TASK]           Remove deadline for project (or for last task)\n")
                                (format #t "    deadline [TASK]                 Show deadline for project\n")
                                (format #t "    deadline all                    Show all deadlines\n")
                                (format #t "    refresh                         Refresh worksheet file after manual edit\n")
                                (format #t "    (no command)                    Show running task and timer\n\n")
                                (format #t "DATE format: YYYY-mm-dd\n")
                                (format #t "TIME format: HH-MM-SS\n")
                                (newline)))
                            (lambda (s . p) #f)))
                          (cons sheet param))))
              
              ;; ----------------------- Save new sheet ------------------------- ;;
              (when (and
                     (list? new-sheet)
                     (not (null? new-sheet)))
                (with-output-to-file ts-file
                  (lambda ()
                    (print-timesheet new-sheet))))))))))

;;; JUST DO IT!
;(main (command-line))

