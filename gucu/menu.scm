
;; -*- Mode: scheme; -*-
(define-module (gucu menu)
        #:use-module (gucu eti)
	#:export ( 
		set-menu-fore!
		menu-fore
		set-menu-back!
		menu-back
		set-menu-grey!
		menu-grey
		set-menu-pad!
		menu-pad
		pos-menu-cursor
		menu-driver
		set-menu-format!
		item-count
		set-menu-mark!
		menu-mark
		set-menu-opts!
		menu-opts-off!
		menu-opts-on!
		menu-opts
		set-menu-pattern!
		menu-pattern
		post-menu
		unpost-menu
		menu-request-name
		menu-request-by-name
		set-menu-spacing!
		set-menu-win!
		set-menu-sub!
		menu-win
		menu-sub
		set-current-item!
		current-item
		set-top-row!
		top-row
		item-index
		item-name
		item-description
		set-item-opts!
		item-opts-on!
		item-opts-off!
		item-opts
		set-item-value!
		item-value
		item-visible?
		REQ_LEFT_ITEM
		REQ_RIGHT_ITEM
		REQ_UP_ITEM
		REQ_DOWN_ITEM
		REQ_SCR_ULINE
		REQ_SCR_DLINE
		REQ_SCR_UPAGE
		REQ_SCR_DPAGE
		REQ_NEXT_ITEM
		REQ_PREV_ITEM
		REQ_FIRST_ITEM
		REQ_LAST_ITEM
		REQ_TOGGLE_ITEM
		REQ_CLEAR_PATTERN
		REQ_BACK_PATTERN
		REQ_NEXT_MATCH
		REQ_PREV_MATCH
		MAX_COMMAND
		O_ONEVALUE
		O_SHOWDESC
		O_ROWMAJOR
		O_IGNORECASE
		O_SHOWMATCH
		O_NONCYCLIC
		O_SELECTABLE
		new-item
		item?
		new-menu
		menu?
		menu-format
		set-menu-items!
		menu-items
		menu-spacing
		scale-menu
		menu-itemlen

        )
        #:re-export (

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
(load-extension "libguile-gucu" "gucu_menu_init")

