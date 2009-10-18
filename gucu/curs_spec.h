#ifndef CURS_SPEC_H
#define CURS_SPEC_H 1

#include <libguile.h>
#include <config.h>

#ifdef DLL_EXPORT
#define API __attribute__ ((dllexport, cdecl))
#else
#define API
#endif

SCM gucu_color_content (SCM s_color) API;
SCM gucu_delwin (SCM win) API;
SCM gucu_getbegyx (SCM win) API;
SCM gucu_getmaxyx (SCM win) API;
SCM gucu_getmouse (void) API;
SCM gucu_getparyx (SCM win) API;
SCM gucu_getsyx (void) API;
SCM gucu_getyx (SCM win) API;
SCM gucu_innwstr (SCM n) API;
SCM gucu_mousemask (SCM x) API;
SCM gucu_mvinnwstr (SCM y, SCM x, SCM n) API;
SCM gucu_pair_content (SCM s_color) API;
SCM gucu_ungetmouse (SCM event) API;
SCM gucu_wattr_get (SCM win) API;
SCM gucu_wattr_set_x (SCM win, SCM attrs, SCM pair) API;
SCM gucu_wgetnstr (SCM win, SCM n) API;
SCM gucu_winchnstr (SCM win, SCM n) API;
SCM gucu_winnstr (SCM win, SCM n) API;
SCM gucu_wmouse_trafo (SCM win, SCM y, SCM x, SCM to) API;

SCM gucu_ACS_BLOCK (void) API;
SCM gucu_ACS_BOARD (void) API;
SCM gucu_ACS_BTEE (void) API;
SCM gucu_ACS_BULLET (void) API;
SCM gucu_ACS_CKBOARD (void) API;
SCM gucu_ACS_DARROW (void) API;
SCM gucu_ACS_DEGREE (void) API;
SCM gucu_ACS_DIAMOND (void) API;
SCM gucu_ACS_GEQUAL (void) API;
SCM gucu_ACS_HLINE (void) API;
SCM gucu_ACS_LANTERN (void) API;
SCM gucu_ACS_LARROW (void) API;
SCM gucu_ACS_LEQUAL (void) API;
SCM gucu_ACS_LLCORNER (void) API;
SCM gucu_ACS_LRCORNER (void) API;
SCM gucu_ACS_LTEE (void) API;
SCM gucu_ACS_NEQUAL (void) API;
SCM gucu_ACS_PI (void) API;
SCM gucu_ACS_PLMINUS (void) API;
SCM gucu_ACS_PLUS (void) API;
SCM gucu_ACS_RARROW (void) API;
SCM gucu_ACS_RTEE (void) API;
SCM gucu_ACS_S1 (void) API;
SCM gucu_ACS_S3 (void) API;
SCM gucu_ACS_S7 (void) API;
SCM gucu_ACS_S9 (void) API;
SCM gucu_ACS_STERLING (void) API;
SCM gucu_ACS_TTEE (void) API;
SCM gucu_ACS_UARROW (void) API;
SCM gucu_ACS_ULCORNER (void) API;
SCM gucu_ACS_URCORNER (void) API;
SCM gucu_ACS_VLINE (void) API;


SCM gucu_LINES (void) API;
SCM gucu_COLS (void) API;
SCM gucu_COLORS (void) API;
SCM gucu_COLOR_PAIRS (void) API;
SCM gucu_TABSIZE (void) API;
SCM gucu_set_TABSIZE (SCM x) API;
SCM gucu_stdscr (void) API;
SCM gucu_curscr (void) API;

void gucu_init_special (void);

#endif /* not CURS_SPEC_H */
