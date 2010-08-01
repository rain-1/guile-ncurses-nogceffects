;; -*- Mode: scheme; -*-
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
