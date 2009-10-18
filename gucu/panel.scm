
;; -*- Mode: scheme; -*-
(define-module (gucu panel)
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

(load-extension "libguile-gucu" "gucu_panel_init")
