;; -*- Mode: scheme; -*-

;; A module containing curses soft label routines

(define-module (gucu slk)
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

(load-extension "libguile-gucu" "gucu_slk_init_function")
