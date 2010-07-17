#!/usr/bin/guile
!#
(use-modules (gucu curses)
             (ice-9 format))

(define stdscr (initscr))

(let* ((mesg "Enter a string: ")
       (len (string-length mesg))
       (siz (getmaxyx stdscr))
       (row (car siz))
       (col (cadr siz)))
  (addstr stdscr mesg
          #:y (round (/ row 2))
          #:x 0)

  (refresh stdscr)
  (let ((str (getnstr stdscr 80)))
    (addstr stdscr
            (format #f "You entered: ~s~%" str)
            #:y (- row 2)
            #:x 0)

    (getch stdscr)))

(endwin)
