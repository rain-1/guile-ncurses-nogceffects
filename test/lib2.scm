;; Copyright 2009, 2010 Free Software Foundation, Inc.

;; This file is part of Guile-Ncurses.

;; Guile-Ncurses is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.

;; Guile-Ncurses is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.

;; You should have received a copy of the GNU Lesser General Public
;; License along with Guile-Ncurses.  If not, see
;; <http://www.gnu.org/licenses/>.

(define-module (test lib2)
  :use-module (srfi srfi-1)
  :use-module (test lib)
  :export (curses-test-start
           curses-test-end
           maybe-sleep
           *sleep*))

(define *sleep* #f)

(define (curses-test-start)
  (let ((count-reporter (make-count-reporter))
        (out-string (open-output-string)))
    (register-reporter (make-log-reporter out-string))
    (register-reporter (first count-reporter))
    (list (second count-reporter)
          out-string)))

(define (maybe-sleep n)
  (if (or *sleep*
          (getenv "GUCU_VERBOSE_TEST"))
      (sleep n)))

;; Exit the test.  If an error was detected, save an error
;; log into FILENAME.
(define (curses-test-end reporter-and-string filename)
  (let* ((count-reporter (first reporter-and-string))
         (out-string     (second reporter-and-string))
         (results        (count-reporter)))
    (for-each
     (lambda (tag)
       (let ((count (assq-ref results tag)))
         (if (> count 0)
             (let ((port (open-output-file filename)))
               (display (get-output-string out-string) port)
               (display (get-output-string out-string))
               (exit 1)))))
     '(fail upass error))
    (exit 0)))
