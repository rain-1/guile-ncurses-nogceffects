#include <libguile.h>
#include <menu.h>

#include "compat.h"
#include "menu_const.h"

SCM gucu_REQ_LEFT_ITEM;
SCM gucu_REQ_RIGHT_ITEM;
SCM gucu_REQ_UP_ITEM;
SCM gucu_REQ_DOWN_ITEM;
SCM gucu_REQ_SCR_ULINE;
SCM gucu_REQ_SCR_DLINE;
SCM gucu_REQ_SCR_UPAGE;
SCM gucu_REQ_SCR_DPAGE;
SCM gucu_REQ_NEXT_ITEM;
SCM gucu_REQ_PREV_ITEM;
SCM gucu_REQ_FIRST_ITEM;
SCM gucu_REQ_LAST_ITEM;
SCM gucu_REQ_TOGGLE_ITEM;
SCM gucu_REQ_CLEAR_PATTERN;
SCM gucu_REQ_BACK_PATTERN;
SCM gucu_REQ_NEXT_MATCH;
SCM gucu_REQ_PREV_MATCH;
SCM gucu_MAX_COMMAND;
SCM gucu_O_ONEVALUE;
SCM gucu_O_SHOWDESC;
SCM gucu_O_ROWMAJOR;
SCM gucu_O_IGNORECASE;
SCM gucu_O_SHOWMATCH;
SCM gucu_O_NONCYCLIC;
SCM gucu_O_SELECTABLE;

void
gucu_menu_init_constant ()
{
  gucu_REQ_LEFT_ITEM =
    scm_permanent_object (scm_c_define
			  ("REQ_LEFT_ITEM", scm_from_int (REQ_LEFT_ITEM)));
  gucu_REQ_RIGHT_ITEM =
    scm_permanent_object (scm_c_define
			  ("REQ_RIGHT_ITEM", scm_from_int (REQ_RIGHT_ITEM)));
  gucu_REQ_UP_ITEM =
    scm_permanent_object (scm_c_define
			  ("REQ_UP_ITEM", scm_from_int (REQ_UP_ITEM)));
  gucu_REQ_DOWN_ITEM =
    scm_permanent_object (scm_c_define
			  ("REQ_DOWN_ITEM", scm_from_int (REQ_DOWN_ITEM)));
  gucu_REQ_SCR_ULINE =
    scm_permanent_object (scm_c_define
			  ("REQ_SCR_ULINE", scm_from_int (REQ_SCR_ULINE)));
  gucu_REQ_SCR_DLINE =
    scm_permanent_object (scm_c_define
			  ("REQ_SCR_DLINE", scm_from_int (REQ_SCR_DLINE)));
  gucu_REQ_SCR_UPAGE =
    scm_permanent_object (scm_c_define
			  ("REQ_SCR_UPAGE", scm_from_int (REQ_SCR_UPAGE)));
  gucu_REQ_SCR_DPAGE =
    scm_permanent_object (scm_c_define
			  ("REQ_SCR_DPAGE", scm_from_int (REQ_SCR_DPAGE)));
  gucu_REQ_NEXT_ITEM =
    scm_permanent_object (scm_c_define
			  ("REQ_NEXT_ITEM", scm_from_int (REQ_NEXT_ITEM)));
  gucu_REQ_PREV_ITEM =
    scm_permanent_object (scm_c_define
			  ("REQ_PREV_ITEM", scm_from_int (REQ_PREV_ITEM)));
  gucu_REQ_FIRST_ITEM =
    scm_permanent_object (scm_c_define
			  ("REQ_FIRST_ITEM", scm_from_int (REQ_FIRST_ITEM)));
  gucu_REQ_LAST_ITEM =
    scm_permanent_object (scm_c_define
			  ("REQ_LAST_ITEM", scm_from_int (REQ_LAST_ITEM)));
  gucu_REQ_TOGGLE_ITEM =
    scm_permanent_object (scm_c_define
			  ("REQ_TOGGLE_ITEM",
			   scm_from_int (REQ_TOGGLE_ITEM)));
  gucu_REQ_CLEAR_PATTERN =
    scm_permanent_object (scm_c_define
			  ("REQ_CLEAR_PATTERN",
			   scm_from_int (REQ_CLEAR_PATTERN)));
  gucu_REQ_BACK_PATTERN =
    scm_permanent_object (scm_c_define
			  ("REQ_BACK_PATTERN",
			   scm_from_int (REQ_BACK_PATTERN)));
  gucu_REQ_NEXT_MATCH =
    scm_permanent_object (scm_c_define
			  ("REQ_NEXT_MATCH", scm_from_int (REQ_NEXT_MATCH)));
  gucu_REQ_PREV_MATCH =
    scm_permanent_object (scm_c_define
			  ("REQ_PREV_MATCH", scm_from_int (REQ_PREV_MATCH)));
  gucu_MAX_COMMAND =
    scm_permanent_object (scm_c_define
			  ("MAX_COMMAND", scm_from_int (MAX_COMMAND)));
  gucu_O_ONEVALUE =
    scm_permanent_object (scm_c_define
			  ("O_ONEVALUE", scm_from_int (O_ONEVALUE)));
  gucu_O_SHOWDESC =
    scm_permanent_object (scm_c_define
			  ("O_SHOWDESC", scm_from_int (O_SHOWDESC)));
  gucu_O_ROWMAJOR =
    scm_permanent_object (scm_c_define
			  ("O_ROWMAJOR", scm_from_int (O_ROWMAJOR)));
  gucu_O_IGNORECASE =
    scm_permanent_object (scm_c_define
			  ("O_IGNORECASE", scm_from_int (O_IGNORECASE)));
  gucu_O_SHOWMATCH =
    scm_permanent_object (scm_c_define
			  ("O_SHOWMATCH", scm_from_int (O_SHOWMATCH)));
  gucu_O_NONCYCLIC =
    scm_permanent_object (scm_c_define
			  ("O_NONCYCLIC", scm_from_int (O_NONCYCLIC)));
  gucu_O_SELECTABLE =
    scm_permanent_object (scm_c_define
			  ("O_SELECTABLE", scm_from_int (O_SELECTABLE)));
}
