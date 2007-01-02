#include <curses.h>
#include <libguile.h>
#include <form.h>
#include <menu.h>

#include "eti_const.h"
#include "compat.h"

#define _GNU_SOURCE
SCM gucu_E_OK;
SCM gucu_E_SYSTEM_ERROR;
SCM gucu_E_BAD_ARGUMENT;
SCM gucu_E_POSTED;
SCM gucu_E_CONNECTED;
SCM gucu_E_BAD_STATE;
SCM gucu_E_NO_ROOM;
SCM gucu_E_NOT_POSTED;
SCM gucu_E_UNKNOWN_COMMAND;
SCM gucu_E_NO_MATCH;
SCM gucu_E_NOT_SELECTABLE;
SCM gucu_E_NOT_CONNECTED;
SCM gucu_E_REQUEST_DENIED;
SCM gucu_E_INVALID_FIELD;
SCM gucu_E_CURRENT;

void 
gucu_eti_init_constant ()
{
	gucu_E_OK = scm_permanent_object (scm_c_define ("E_OK", scm_from_int (E_OK)));
	gucu_E_SYSTEM_ERROR = scm_permanent_object (scm_c_define ("E_SYSTEM_ERROR", scm_from_int (E_SYSTEM_ERROR)));
	gucu_E_BAD_ARGUMENT = scm_permanent_object (scm_c_define ("E_BAD_ARGUMENT", scm_from_int (E_BAD_ARGUMENT)));
	gucu_E_POSTED = scm_permanent_object (scm_c_define ("E_POSTED", scm_from_int (E_POSTED)));
	gucu_E_CONNECTED = scm_permanent_object (scm_c_define ("E_CONNECTED", scm_from_int (E_CONNECTED)));
	gucu_E_BAD_STATE = scm_permanent_object (scm_c_define ("E_BAD_STATE", scm_from_int (E_BAD_STATE)));
	gucu_E_NO_ROOM = scm_permanent_object (scm_c_define ("E_NO_ROOM", scm_from_int (E_NO_ROOM)));
	gucu_E_NOT_POSTED = scm_permanent_object (scm_c_define ("E_NOT_POSTED", scm_from_int (E_NOT_POSTED)));
	gucu_E_UNKNOWN_COMMAND = scm_permanent_object (scm_c_define ("E_UNKNOWN_COMMAND", scm_from_int (E_UNKNOWN_COMMAND)));
	gucu_E_NO_MATCH = scm_permanent_object (scm_c_define ("E_NO_MATCH", scm_from_int (E_NO_MATCH)));
	gucu_E_NOT_SELECTABLE = scm_permanent_object (scm_c_define ("E_NOT_SELECTABLE", scm_from_int (E_NOT_SELECTABLE)));
	gucu_E_NOT_CONNECTED = scm_permanent_object (scm_c_define ("E_NOT_CONNECTED", scm_from_int (E_NOT_CONNECTED)));
	gucu_E_REQUEST_DENIED = scm_permanent_object (scm_c_define ("E_REQUEST_DENIED", scm_from_int (E_REQUEST_DENIED)));
	gucu_E_INVALID_FIELD = scm_permanent_object (scm_c_define ("E_INVALID_FIELD", scm_from_int (E_INVALID_FIELD)));
	gucu_E_CURRENT = scm_permanent_object (scm_c_define ("E_CURRENT", scm_from_int (E_CURRENT)));
}
