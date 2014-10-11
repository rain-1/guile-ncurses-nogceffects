/*
  form_const.c

  Copyright 2009, 2010, 2014 Free Software Foundation, Inc.

  This file is part of GNU Guile-Ncurses.

  Guile-Ncurses is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as
  published by the Free Software Foundation, either version 3 of the
  License, or (at your option) any later version.

  Guile-Ncurses is distributed in the hope that it will be useful, but
  WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
  Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public
  License along with Guile-Ncurses.  If not, see
  <http://www.gnu.org/licenses/>.
*/

#include <config.h>

#include <libguile.h>
#include <wchar.h>

#if HAVE_CURSES_H
#include <curses.h>
#include <form.h>
#elif HAVE_NCURSES_CURSES_H
#include <ncurses/curses.h>
#include <ncurses/form.h>
#elif HAVE_NCURSESW_CURSES_H
#include <ncursesw/curses.h>
#include <ncursesw/form.h>
#else
#error "No curses.h file included"
#endif

#include "compat.h"
#include "form_const.h"
#include "form_type.h"
#include "type.h"

SCM gucu_NO_JUSTIFICATION;
SCM gucu_JUSTIFY_LEFT;
SCM gucu_JUSTIFY_CENTER;
SCM gucu_JUSTIFY_RIGHT;
SCM gucu_O_VISIBLE;
SCM gucu_O_ACTIVE;
SCM gucu_O_PUBLIC;
SCM gucu_O_EDIT;
SCM gucu_O_WRAP;
SCM gucu_O_BLANK;
SCM gucu_O_AUTOSKIP;
SCM gucu_O_NULLOK;
SCM gucu_O_PASSOK;
SCM gucu_O_STATIC;
SCM gucu_O_NL_OVERLOAD;
SCM gucu_O_BS_OVERLOAD;
SCM gucu_REQ_NEXT_PAGE;
SCM gucu_REQ_PREV_PAGE;
SCM gucu_REQ_FIRST_PAGE;
SCM gucu_REQ_LAST_PAGE;
SCM gucu_REQ_NEXT_FIELD;
SCM gucu_REQ_PREV_FIELD;
SCM gucu_REQ_FIRST_FIELD;
SCM gucu_REQ_LAST_FIELD;
SCM gucu_REQ_SNEXT_FIELD;
SCM gucu_REQ_SPREV_FIELD;
SCM gucu_REQ_SFIRST_FIELD;
SCM gucu_REQ_SLAST_FIELD;
SCM gucu_REQ_LEFT_FIELD;
SCM gucu_REQ_RIGHT_FIELD;
SCM gucu_REQ_UP_FIELD;
SCM gucu_REQ_DOWN_FIELD;
SCM gucu_REQ_NEXT_CHAR;
SCM gucu_REQ_PREV_CHAR;
SCM gucu_REQ_NEXT_LINE;
SCM gucu_REQ_PREV_LINE;
SCM gucu_REQ_NEXT_WORD;
SCM gucu_REQ_PREV_WORD;
SCM gucu_REQ_BEG_FIELD;
SCM gucu_REQ_END_FIELD;
SCM gucu_REQ_BEG_LINE;
SCM gucu_REQ_END_LINE;
SCM gucu_REQ_LEFT_CHAR;
SCM gucu_REQ_RIGHT_CHAR;
SCM gucu_REQ_UP_CHAR;
SCM gucu_REQ_DOWN_CHAR;
SCM gucu_REQ_NEW_LINE;
SCM gucu_REQ_INS_CHAR;
SCM gucu_REQ_INS_LINE;
SCM gucu_REQ_DEL_CHAR;
SCM gucu_REQ_DEL_PREV;
SCM gucu_REQ_DEL_LINE;
SCM gucu_REQ_DEL_WORD;
SCM gucu_REQ_CLR_EOL;
SCM gucu_REQ_CLR_EOF;
SCM gucu_REQ_CLR_FIELD;
SCM gucu_REQ_OVL_MODE;
SCM gucu_REQ_INS_MODE;
SCM gucu_REQ_SCR_FLINE;
SCM gucu_REQ_SCR_BLINE;
SCM gucu_REQ_SCR_FPAGE;
SCM gucu_REQ_SCR_BPAGE;
SCM gucu_REQ_SCR_FHPAGE;
SCM gucu_REQ_SCR_BHPAGE;
SCM gucu_REQ_SCR_FCHAR;
SCM gucu_REQ_SCR_BCHAR;
SCM gucu_REQ_SCR_HFLINE;
SCM gucu_REQ_SCR_HBLINE;
SCM gucu_REQ_SCR_HFHALF;
SCM gucu_REQ_SCR_HBHALF;
SCM gucu_REQ_VALIDATION;
SCM gucu_REQ_NEXT_CHOICE;
SCM gucu_REQ_PREV_CHOICE;
SCM gucu_MIN_FORM_COMMAND;
SCM gucu_MAX_FORM_COMMAND;

SCM gucu_wide_form_driver;

void
gucu_form_init_constant ()
{
  gucu_NO_JUSTIFICATION =
    scm_permanent_object (scm_c_define
                          ("NO_JUSTIFICATION",
                           scm_from_int (NO_JUSTIFICATION)));
  gucu_JUSTIFY_LEFT =
    scm_permanent_object (scm_c_define
                          ("JUSTIFY_LEFT", scm_from_int (JUSTIFY_LEFT)));
  gucu_JUSTIFY_CENTER =
    scm_permanent_object (scm_c_define
                          ("JUSTIFY_CENTER", scm_from_int (JUSTIFY_CENTER)));
  gucu_JUSTIFY_RIGHT =
    scm_permanent_object (scm_c_define
                          ("JUSTIFY_RIGHT", scm_from_int (JUSTIFY_RIGHT)));
  gucu_O_VISIBLE =
    scm_permanent_object (scm_c_define
                          ("O_VISIBLE", scm_from_int (O_VISIBLE)));
  gucu_O_ACTIVE =
    scm_permanent_object (scm_c_define ("O_ACTIVE", scm_from_int (O_ACTIVE)));
  gucu_O_PUBLIC =
    scm_permanent_object (scm_c_define ("O_PUBLIC", scm_from_int (O_PUBLIC)));
  gucu_O_EDIT =
    scm_permanent_object (scm_c_define ("O_EDIT", scm_from_int (O_EDIT)));
  gucu_O_WRAP =
    scm_permanent_object (scm_c_define ("O_WRAP", scm_from_int (O_WRAP)));
  gucu_O_BLANK =
    scm_permanent_object (scm_c_define ("O_BLANK", scm_from_int (O_BLANK)));
  gucu_O_AUTOSKIP =
    scm_permanent_object (scm_c_define
                          ("O_AUTOSKIP", scm_from_int (O_AUTOSKIP)));
  gucu_O_NULLOK =
    scm_permanent_object (scm_c_define ("O_NULLOK", scm_from_int (O_NULLOK)));
  gucu_O_PASSOK =
    scm_permanent_object (scm_c_define ("O_PASSOK", scm_from_int (O_PASSOK)));
  gucu_O_STATIC =
    scm_permanent_object (scm_c_define ("O_STATIC", scm_from_int (O_STATIC)));
  gucu_O_NL_OVERLOAD =
    scm_permanent_object (scm_c_define
                          ("O_NL_OVERLOAD", scm_from_int (O_NL_OVERLOAD)));
  gucu_O_BS_OVERLOAD =
    scm_permanent_object (scm_c_define
                          ("O_BS_OVERLOAD", scm_from_int (O_BS_OVERLOAD)));
  gucu_REQ_NEXT_PAGE =
    scm_permanent_object (scm_c_define
                          ("REQ_NEXT_PAGE", scm_from_int (REQ_NEXT_PAGE)));
  gucu_REQ_PREV_PAGE =
    scm_permanent_object (scm_c_define
                          ("REQ_PREV_PAGE", scm_from_int (REQ_PREV_PAGE)));
  gucu_REQ_FIRST_PAGE =
    scm_permanent_object (scm_c_define
                          ("REQ_FIRST_PAGE", scm_from_int (REQ_FIRST_PAGE)));
  gucu_REQ_LAST_PAGE =
    scm_permanent_object (scm_c_define
                          ("REQ_LAST_PAGE", scm_from_int (REQ_LAST_PAGE)));
  gucu_REQ_NEXT_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_NEXT_FIELD", scm_from_int (REQ_NEXT_FIELD)));
  gucu_REQ_PREV_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_PREV_FIELD", scm_from_int (REQ_PREV_FIELD)));
  gucu_REQ_FIRST_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_FIRST_FIELD",
                           scm_from_int (REQ_FIRST_FIELD)));
  gucu_REQ_LAST_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_LAST_FIELD", scm_from_int (REQ_LAST_FIELD)));
  gucu_REQ_SNEXT_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_SNEXT_FIELD",
                           scm_from_int (REQ_SNEXT_FIELD)));
  gucu_REQ_SPREV_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_SPREV_FIELD",
                           scm_from_int (REQ_SPREV_FIELD)));
  gucu_REQ_SFIRST_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_SFIRST_FIELD",
                           scm_from_int (REQ_SFIRST_FIELD)));
  gucu_REQ_SLAST_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_SLAST_FIELD",
                           scm_from_int (REQ_SLAST_FIELD)));
  gucu_REQ_LEFT_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_LEFT_FIELD", scm_from_int (REQ_LEFT_FIELD)));
  gucu_REQ_RIGHT_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_RIGHT_FIELD",
                           scm_from_int (REQ_RIGHT_FIELD)));
  gucu_REQ_UP_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_UP_FIELD", scm_from_int (REQ_UP_FIELD)));
  gucu_REQ_DOWN_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_DOWN_FIELD", scm_from_int (REQ_DOWN_FIELD)));
  gucu_REQ_NEXT_CHAR =
    scm_permanent_object (scm_c_define
                          ("REQ_NEXT_CHAR", scm_from_int (REQ_NEXT_CHAR)));
  gucu_REQ_PREV_CHAR =
    scm_permanent_object (scm_c_define
                          ("REQ_PREV_CHAR", scm_from_int (REQ_PREV_CHAR)));
  gucu_REQ_NEXT_LINE =
    scm_permanent_object (scm_c_define
                          ("REQ_NEXT_LINE", scm_from_int (REQ_NEXT_LINE)));
  gucu_REQ_PREV_LINE =
    scm_permanent_object (scm_c_define
                          ("REQ_PREV_LINE", scm_from_int (REQ_PREV_LINE)));
  gucu_REQ_NEXT_WORD =
    scm_permanent_object (scm_c_define
                          ("REQ_NEXT_WORD", scm_from_int (REQ_NEXT_WORD)));
  gucu_REQ_PREV_WORD =
    scm_permanent_object (scm_c_define
                          ("REQ_PREV_WORD", scm_from_int (REQ_PREV_WORD)));
  gucu_REQ_BEG_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_BEG_FIELD", scm_from_int (REQ_BEG_FIELD)));
  gucu_REQ_END_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_END_FIELD", scm_from_int (REQ_END_FIELD)));
  gucu_REQ_BEG_LINE =
    scm_permanent_object (scm_c_define
                          ("REQ_BEG_LINE", scm_from_int (REQ_BEG_LINE)));
  gucu_REQ_END_LINE =
    scm_permanent_object (scm_c_define
                          ("REQ_END_LINE", scm_from_int (REQ_END_LINE)));
  gucu_REQ_LEFT_CHAR =
    scm_permanent_object (scm_c_define
                          ("REQ_LEFT_CHAR", scm_from_int (REQ_LEFT_CHAR)));
  gucu_REQ_RIGHT_CHAR =
    scm_permanent_object (scm_c_define
                          ("REQ_RIGHT_CHAR", scm_from_int (REQ_RIGHT_CHAR)));
  gucu_REQ_UP_CHAR =
    scm_permanent_object (scm_c_define
                          ("REQ_UP_CHAR", scm_from_int (REQ_UP_CHAR)));
  gucu_REQ_DOWN_CHAR =
    scm_permanent_object (scm_c_define
                          ("REQ_DOWN_CHAR", scm_from_int (REQ_DOWN_CHAR)));
  gucu_REQ_NEW_LINE =
    scm_permanent_object (scm_c_define
                          ("REQ_NEW_LINE", scm_from_int (REQ_NEW_LINE)));
  gucu_REQ_INS_CHAR =
    scm_permanent_object (scm_c_define
                          ("REQ_INS_CHAR", scm_from_int (REQ_INS_CHAR)));
  gucu_REQ_INS_LINE =
    scm_permanent_object (scm_c_define
                          ("REQ_INS_LINE", scm_from_int (REQ_INS_LINE)));
  gucu_REQ_DEL_CHAR =
    scm_permanent_object (scm_c_define
                          ("REQ_DEL_CHAR", scm_from_int (REQ_DEL_CHAR)));
  gucu_REQ_DEL_PREV =
    scm_permanent_object (scm_c_define
                          ("REQ_DEL_PREV", scm_from_int (REQ_DEL_PREV)));
  gucu_REQ_DEL_LINE =
    scm_permanent_object (scm_c_define
                          ("REQ_DEL_LINE", scm_from_int (REQ_DEL_LINE)));
  gucu_REQ_DEL_WORD =
    scm_permanent_object (scm_c_define
                          ("REQ_DEL_WORD", scm_from_int (REQ_DEL_WORD)));
  gucu_REQ_CLR_EOL =
    scm_permanent_object (scm_c_define
                          ("REQ_CLR_EOL", scm_from_int (REQ_CLR_EOL)));
  gucu_REQ_CLR_EOF =
    scm_permanent_object (scm_c_define
                          ("REQ_CLR_EOF", scm_from_int (REQ_CLR_EOF)));
  gucu_REQ_CLR_FIELD =
    scm_permanent_object (scm_c_define
                          ("REQ_CLR_FIELD", scm_from_int (REQ_CLR_FIELD)));
  gucu_REQ_OVL_MODE =
    scm_permanent_object (scm_c_define
                          ("REQ_OVL_MODE", scm_from_int (REQ_OVL_MODE)));
  gucu_REQ_INS_MODE =
    scm_permanent_object (scm_c_define
                          ("REQ_INS_MODE", scm_from_int (REQ_INS_MODE)));
  gucu_REQ_SCR_FLINE =
    scm_permanent_object (scm_c_define
                          ("REQ_SCR_FLINE", scm_from_int (REQ_SCR_FLINE)));
  gucu_REQ_SCR_BLINE =
    scm_permanent_object (scm_c_define
                          ("REQ_SCR_BLINE", scm_from_int (REQ_SCR_BLINE)));
  gucu_REQ_SCR_FPAGE =
    scm_permanent_object (scm_c_define
                          ("REQ_SCR_FPAGE", scm_from_int (REQ_SCR_FPAGE)));
  gucu_REQ_SCR_BPAGE =
    scm_permanent_object (scm_c_define
                          ("REQ_SCR_BPAGE", scm_from_int (REQ_SCR_BPAGE)));
  gucu_REQ_SCR_FHPAGE =
    scm_permanent_object (scm_c_define
                          ("REQ_SCR_FHPAGE", scm_from_int (REQ_SCR_FHPAGE)));
  gucu_REQ_SCR_BHPAGE =
    scm_permanent_object (scm_c_define
                          ("REQ_SCR_BHPAGE", scm_from_int (REQ_SCR_BHPAGE)));
  gucu_REQ_SCR_FCHAR =
    scm_permanent_object (scm_c_define
                          ("REQ_SCR_FCHAR", scm_from_int (REQ_SCR_FCHAR)));
  gucu_REQ_SCR_BCHAR =
    scm_permanent_object (scm_c_define
                          ("REQ_SCR_BCHAR", scm_from_int (REQ_SCR_BCHAR)));
  gucu_REQ_SCR_HFLINE =
    scm_permanent_object (scm_c_define
                          ("REQ_SCR_HFLINE", scm_from_int (REQ_SCR_HFLINE)));
  gucu_REQ_SCR_HBLINE =
    scm_permanent_object (scm_c_define
                          ("REQ_SCR_HBLINE", scm_from_int (REQ_SCR_HBLINE)));
  gucu_REQ_SCR_HFHALF =
    scm_permanent_object (scm_c_define
                          ("REQ_SCR_HFHALF", scm_from_int (REQ_SCR_HFHALF)));
  gucu_REQ_SCR_HBHALF =
    scm_permanent_object (scm_c_define
                          ("REQ_SCR_HBHALF", scm_from_int (REQ_SCR_HBHALF)));
  gucu_REQ_VALIDATION =
    scm_permanent_object (scm_c_define
                          ("REQ_VALIDATION", scm_from_int (REQ_VALIDATION)));
  gucu_REQ_NEXT_CHOICE =
    scm_permanent_object (scm_c_define
                          ("REQ_NEXT_CHOICE",
                           scm_from_int (REQ_NEXT_CHOICE)));
  gucu_REQ_PREV_CHOICE =
    scm_permanent_object (scm_c_define
                          ("REQ_PREV_CHOICE",
                           scm_from_int (REQ_PREV_CHOICE)));
  gucu_MIN_FORM_COMMAND =
    scm_permanent_object (scm_c_define
                          ("MIN_FORM_COMMAND",
                           scm_from_int (MIN_FORM_COMMAND)));
  gucu_MAX_FORM_COMMAND =
    scm_permanent_object (scm_c_define
                          ("MAX_FORM_COMMAND",
                           scm_from_int (MAX_FORM_COMMAND)));

#if (HAVE_NCURSESW == 1) && (HAVE_FORM_DRIVER_W)
  gucu_wide_form_driver =
    scm_permanent_object (scm_c_define ("%is-form-driver-wide", SCM_BOOL_T));
#else
  gucu_wide_form_driver =
    scm_permanent_object (scm_c_define ("%is-form-driver-wide", SCM_BOOL_F));
#endif

}
