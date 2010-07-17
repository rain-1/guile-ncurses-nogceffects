#!/usr/bin/guile
!#
(use-modules (gucu curses))

(define stdscr (initscr))
(addstr stdscr "Hello World!!!")
(refresh stdscr)
(getch stdscr)
(endwin)
