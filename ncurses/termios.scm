;; -*- Mode: scheme; -*-

;; termios.scm

;; Copyright 2010 Free Software Foundation, Inc.

;; This file is part of GNU Guile-Ncurses.

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

(define-module (ncurses termios)
  #:use-module (ncurses lib)
  #:use-module (srfi srfi-1)
  #:export (
            BS0
            BS1
            BSDLY
            CR0
            CR1
            CR2
            CR3
            CRDLY
            FF0
            FF1
            FFDLY
            IUCLC
            NL0
            NL1
            NLDLY
            OCRNL
            OFDEL
            OFILL
            OLCUC
            ONLCR
            ONLRET
            ONOCR
            OPOST
            TAB1
            TAB2
            TAB3
            TABDLY
            TCIFLUSH
            TCIOFF
            TCIOFLUSH
            TCION
            TCOFLUSH
            TCOOFF
            TCOON
            TCSADRAIN
            TCSAFLUSH
            TCSANOW
            VT0
            VT1
            VTDLY
            XCASE

            NCCS

            VDISCARD
            VEOF
            VEOL
            VEOL2
            VERASE
            VINTR
            VKILL
            VLNEXT
            VMIN
            VQUIT
            VREPRINT
            VSTART
            VSTOP
            VSUSP
            VSWTC
            VWERASE

            BRKINT
            ICRNL
            IGNBRK
            IGNCR
            IGNPAR
            IMAXBEL
            INLCR
            INPCK
            ISTRIP
            IXANY
            IXOFF
            IXON
            PARMRK

            CLOCAL
            CREAD
            CS5
            CS6
            CS7
            CS8
            CSIZE
            CSTOPB
            HUPCL
            PARENB
            PARODD
            VTIME

            ECHO
            ECHOCTL
            ECHOE
            ECHOK
            ECHOKE
            ECHONL
            ECHOPRT
            FLUSHO
            ICANON
            IEXTEN
            ISIG
            NOFLSH
            PENDIN
            TOSTOP

            B0
            B110
            B1200
            B134
            B150
            B1800
            B19200
            B200
            B2400
            B300
            B38400
            B4800
            B50
            B600
            B75
            B9600

            termios?
            new-termios

            cfgetispeed
            cfgetospeed
            cfmakeraw!
            cfsetispeed!
            cfsetspeed!
            cfsetospeed!
            tcdrain
            tcflow
            tcflush
            tcgetattr
            tcgetsid
            tcsendbreak
            tcsetattr!
            wcwidth

            termios-iflag
            termios-oflag
            termios-cflag
            termios-lflag
            termios-line
            termios-cc
            termios-iflag-set!
            termios-oflag-set!
            termios-cflag-set!
            termios-lflag-set!
            termios-cc-set!
            ))

;; Return the number of character cells that C takes
(define (wcwidth x)
  (cond
   ((char? x)
    (%strwidth (string x)))
   ((and (integer? x) (logtest x A_ALTCHARSET))
    1)
   ((xchar? x)
    (%strwidth (xchar-chars x)))
   ((string? x)
    (%strwidth x))
   ((and (list? x) (every xchar? x))
    (%strwidth (apply string (apply append (map xchar-chars x)))))
   (else
    (error (gettext "Invalid input ~s") x))))

(load-extension "libguile-ncurses" "gucu_termios_init")
