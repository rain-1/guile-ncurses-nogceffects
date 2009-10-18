#!/usr/bin/guile
!#

(use-modules (gucu curses))

(define stdscr (initscr))

;; Prep the color functions
(start-color)

;; Label cyan on black as color-pair #1
(init-pair 1 COLOR_CYAN COLOR_BLACK)

(addstr stdscr "Blink Don't Blink")

(chgat   stdscr		     ; window
	 5                   ; num of chars 
         A_BLINK             ; attributes
         1                   ; use color pair #1
         #:y 0               ; start y
         #:x 0)              ; start x

;; Move the cursor out of the way
(move stdscr 1 0)

(refresh stdscr)
(getch stdscr)
(endwin)
