#!/usr/bin/guile
!#

(use-modules (gucu curses)
	     (srfi srfi-1))

(define stdscr (initscr))

(let* ((mesg "Just a string")
       (len (string-length mesg))
       (siz (getmaxyx stdscr))
       (row (first siz))
       (col (second siz)))

  ;; Print the message centered in the window
  (move stdscr
	(round (/ row 2))		
	(round (/ (- col len) 2)))
  (addstr stdscr mesg)

  ;; Use "format" to generate a message, and then print it
  (addstr stdscr 
	  (format #f "This screen has ~a rows and ~a columns ~%" 
		  row col)
	  #:y (- row 2)
	  #:x 0)
            
  (addstr stdscr "Try resizing your window (if possible) ")
  (addstr stdscr "and then run this program again")
  (refresh stdscr)
  
  ;; Wait for a keypress
  (getch stdscr)
  (endwin))
