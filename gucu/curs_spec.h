#ifndef CURS_SPEC_H
#define CURS_SPEC_H

#include <libguile.h>
#include "visibility.h"

GUCU_API SCM gucu_color_content (SCM s_color);
GUCU_API SCM gucu_delwin (SCM win);
GUCU_API SCM gucu_getbegyx (SCM win);
GUCU_API SCM gucu_getmaxyx (SCM win);
GUCU_API SCM gucu_getmouse (void);
GUCU_API SCM gucu_getparyx (SCM win);
GUCU_API SCM gucu_getsyx (void);
GUCU_API SCM gucu_getyx (SCM win);
GUCU_API SCM gucu_innwstr (SCM n);
GUCU_API SCM gucu_mousemask (SCM x);
GUCU_API SCM gucu_mvinnwstr (SCM y, SCM x, SCM n);
GUCU_API SCM gucu_pair_content (SCM s_color);
GUCU_API SCM gucu_ungetmouse (SCM event);
GUCU_API SCM gucu_wattr_get (SCM win);
GUCU_API SCM gucu_wattr_set_x (SCM win, SCM attrs, SCM pair);
GUCU_API SCM gucu_wgetnstr (SCM win, SCM n);
GUCU_API SCM gucu_winchnstr (SCM win, SCM n);
GUCU_API SCM gucu_winnstr (SCM win, SCM n);
GUCU_API SCM gucu_wmouse_trafo (SCM win, SCM y, SCM x, SCM to);

GUCU_API SCM gucu_ACS_BLOCK (void);
GUCU_API SCM gucu_ACS_BOARD (void);
GUCU_API SCM gucu_ACS_BTEE (void);
GUCU_API SCM gucu_ACS_BULLET (void);
GUCU_API SCM gucu_ACS_CKBOARD (void);
GUCU_API SCM gucu_ACS_DARROW (void);
GUCU_API SCM gucu_ACS_DEGREE (void);
GUCU_API SCM gucu_ACS_DIAMOND (void);
GUCU_API SCM gucu_ACS_GEQUAL (void);
GUCU_API SCM gucu_ACS_HLINE (void);
GUCU_API SCM gucu_ACS_LANTERN (void);
GUCU_API SCM gucu_ACS_LARROW (void);
GUCU_API SCM gucu_ACS_LEQUAL (void);
GUCU_API SCM gucu_ACS_LLCORNER (void);
GUCU_API SCM gucu_ACS_LRCORNER (void);
GUCU_API SCM gucu_ACS_LTEE (void);
GUCU_API SCM gucu_ACS_NEQUAL (void);
GUCU_API SCM gucu_ACS_PI (void);
GUCU_API SCM gucu_ACS_PLMINUS (void);
GUCU_API SCM gucu_ACS_PLUS (void);
GUCU_API SCM gucu_ACS_RARROW (void);
GUCU_API SCM gucu_ACS_RTEE (void);
GUCU_API SCM gucu_ACS_S1 (void);
GUCU_API SCM gucu_ACS_S3 (void);
GUCU_API SCM gucu_ACS_S7 (void);
GUCU_API SCM gucu_ACS_S9 (void);
GUCU_API SCM gucu_ACS_STERLING (void);
GUCU_API SCM gucu_ACS_TTEE (void);
GUCU_API SCM gucu_ACS_UARROW (void);
GUCU_API SCM gucu_ACS_ULCORNER (void);
GUCU_API SCM gucu_ACS_URCORNER (void);
GUCU_API SCM gucu_ACS_VLINE (void);

GUCU_API SCM gucu_LINES (void);
GUCU_API SCM gucu_COLS (void);
GUCU_API SCM gucu_COLORS (void);
GUCU_API SCM gucu_COLOR_PAIRS (void);
GUCU_API SCM gucu_TABSIZE (void);
GUCU_API SCM gucu_set_TABSIZE (SCM x);
GUCU_API SCM gucu_stdscr (void);
GUCU_API SCM gucu_curscr (void);

GUCU_LOCAL void gucu_init_special (void);

#endif /* not CURS_SPEC_H */
