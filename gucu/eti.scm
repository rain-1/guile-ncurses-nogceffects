;; -*- Mode: scheme; -*-
(define-module (gucu eti)
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

(load-extension "libguile-gucu" "gucu_eti_init")
