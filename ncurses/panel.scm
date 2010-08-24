;; -*- Mode: scheme; -*-

;; panel.scm

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

(define-module (ncurses panel)
  #:export (
	    new-panel
	    bottom-panel
	    top-panel
	    show-panel
	    update-panels
	    hide-panel
	    panel-window
	    replace-panel!
	    move-panel
	    panel-hidden?
	    del-panel

	    ;;panel-above
	    ;;panel-below
	    ;;set-panel-userdata
	    ;;panel-userdata
	    ))

(load-extension "libguile-ncurses" "gucu_panel_init")
