#!/usr/bin/guile
!#

(use-modules (gucu curses)
             (ice-9 format))

;; A helper function that return the cursor's current row
(define (getrow win)
  (car (getyx win)))

;; The program should be passed a filename from the command line
(if (not (eqv? 2 (length (command-line))))
    (begin
      (format #t "Usage: ~a <scm file name>~%" (car (command-line)))
      (primitive-exit 1)))

(let* ((filename (cadr (command-line)))
       (fport (open-input-file filename))
       (stdscr (initscr)))

  ;; Read one char at a time from the file
  (let loop ((ch (read-char fport)))
    (if (not (eof-object? ch))
	(begin
	  ;; Wait for a key press once a page
	  ;; of text has been printed
	  (if (eqv? (getrow stdscr) (- (lines) 1))
	      (begin
		(addstr stdscr "<-Press any key->")
		(refresh stdscr)
		(getch stdscr)
		(clear stdscr)
		(move stdscr 0 0)))
	  ;; Bold all text between a semicolon
	  ;; and the end of a line
	  (cond
	   ((eqv? ch #\;)
	    (attr-on stdscr A_BOLD))
	   ((eqv? ch #\nl)
	    (attr-off stdscr A_BOLD)))
	  (addch stdscr (char->integer ch))
	  (refresh stdscr)
	  (loop (read-char fport)))
	;; Clean up on exit
	(begin
	  (endwin)
	  (close-input-port fport)))))
