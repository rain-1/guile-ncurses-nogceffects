#!/usr/bin/guile
!#
(use-modules (gucu curses))

(define stdscr (initscr))	   ; Start curses mode
(raw!)				   ; Line buffering disabled
(keypad! stdscr #t)		   ; We get F1, F2, etc
(noecho!)			   ; Don't echo when we get a keypress
(addstr stdscr "Type any character to see it in bold\n")
(let ((ch (getch stdscr)))	   ; Read a key press, put it in 'ch'
  (addstr stdscr "The pressed key is ")

  (if (char? ch)		    ; If a non-function key is pressed
      (addch stdscr (bold ch))	    ; print its name

      (addchstr stdscr (bold (keyname ch)))) ; Or, print the function
					     ; key name
  
  (refresh stdscr)			; Print it on the real screen
  (getch stdscr)			; Wait for user input
  (endwin))                             ; End curses mode
