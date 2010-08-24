;; -*- Mode: scheme; -*-

;; eti.scm

;; Copyright 2009, 2010 Free Software Foundation, Inc.

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

(define-module (ncurses eti)
  #:export (
	    E_OK
	    E_SYSTEM_ERROR
	    E_BAD_ARGUMENT
	    E_POSTED
	    E_CONNECTED
	    E_BAD_STATE
	    E_NO_ROOM
	    E_NOT_POSTED
	    E_UNKNOWN_COMMAND
	    E_NO_MATCH
	    E_NOT_SELECTABLE
	    E_NOT_CONNECTED
	    E_REQUEST_DENIED
	    E_INVALID_FIELD
	    E_CURRENT
	    ))

(load-extension "libguile-ncurses" "gucu_eti_init")
