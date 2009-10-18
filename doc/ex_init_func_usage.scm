#!/usr/bin/guile
!#
(use-modules (gucu curses))

(define stdscr (initscr))	   ; Start curses mode
(raw)				   ; Line buffering disabled
(keypad! stdscr #t)		   ; We get F1, F2, etc
(noecho)			   ; Don't echo when we get a keypress
(addstr stdscr "Type any character to see it in bold\n")
(let ((ch (getch stdscr)))	    ; Read a key press, put it in 'ch'

  (if (eqv? ch (key-f 1))               ; If the key pressed is F1
      (addstr stdscr "F1 Key pressed")
      
      (begin
        (addstr stdscr "The pressed key is ")
        (addch stdscr (logior A_BOLD ch))))    ; Print the char in bold
  (refresh stdscr)			; Print it on the real screen
  (getch stdscr)			; Wait for user input
  (endwin))                             ; End curses mode
