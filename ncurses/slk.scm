;; -*- Mode: scheme; -*-
;; eti.scm -- A module containing curses soft label routines

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

(define-module (ncurses slk)
  #:use-module (ice-9 optargs)
  #:export (
	    slk-attr
	    slk-attr-off!
	    slk-attr-on!
	    slk-attr-set!
	    slk-clear
	    slk-color!
	    slk-init
	    slk-label
	    slk-noutrefresh
	    slk-refresh
	    slk-restore
	    slk-set
	    slk-touch
	    ))

(load-extension "libguile-ncurses" "gucu_slk_init_function")
