
#ifndef FUNCTION_H
#define FUNCTION_H

#include <libguile.h>
#include <config.h>
#ifdef DLL_EXPORT
#define API __attribute__ ((dllexport, cdecl))
#else
#define API
#endif

SCM gucu_assume_default_colors (SCM fg, SCM bg);
SCM gucu_attr_off_x (SCM arg1, SCM arg2) API;
SCM gucu_attr_on_x (SCM arg1, SCM arg2) API;
SCM gucu_baudrate (void) API;
SCM gucu_beep (void) API;
SCM gucu_bkgd (SCM win, SCM ch) API;
SCM gucu_bkgdset_x (SCM win, SCM ch) API;
SCM gucu_border (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, 
		  SCM arg7, SCM arg8, SCM arg9) API;
SCM gucu_can_change_color_p (void) API;
SCM gucu_cbreak (void) API;
SCM gucu_clear (SCM arg1) API;
SCM gucu_clearok (SCM arg1, SCM arg2) API;
SCM gucu_clrtobot (SCM arg1) API;
SCM gucu_clrtoeol (SCM arg1) API;
SCM gucu_COLOR_PAIR (SCM arg1) API;
SCM gucu_color_set (SCM arg1, SCM arg2) API;
SCM gucu_copywin (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7, SCM arg8, SCM arg9) API;
SCM gucu_curs_set (SCM arg1) API;
SCM gucu_curses_version (void) API;
SCM gucu_def_prog_mode (void) API;
SCM gucu_def_shell_mode (void) API;
SCM gucu_define_key (SCM arg1, SCM arg2) API;
SCM gucu_delay_output (SCM arg1) API;
SCM gucu_delscreen (SCM arg1) API;
SCM gucu_derwin (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5) API;
SCM gucu_doupdate (void) API;
SCM gucu_dupwin (SCM arg1) API;
SCM gucu_echo (void) API;
SCM gucu_endwin (void) API;
SCM gucu_erase (SCM arg1) API;
SCM gucu_erasechar (void) API;
SCM gucu_filter (void) API;
SCM gucu_flash (void) API;
SCM gucu_flushinp (void) API;
SCM gucu_getbkgd (SCM arg1) API;
SCM gucu_halfdelay (SCM arg1) API;
SCM gucu_has_colors_p (void) API;
SCM gucu_has_ic_p (void) API;
SCM gucu_has_il_p (void) API;
SCM gucu_has_key_p (SCM key) API;
SCM gucu_idcok_x (SCM arg1, SCM arg2) API;
SCM gucu_idlok_x (SCM arg1, SCM arg2) API;
SCM gucu_immedok_x (SCM arg1, SCM arg2) API;
SCM gucu_initscr (void) API;
SCM gucu_init_color (SCM arg1, SCM arg2, SCM arg3, SCM arg4) API;
SCM gucu_init_pair (SCM arg1, SCM arg2, SCM arg3) API;
SCM gucu_intrflush (SCM arg2) API;
SCM gucu_isendwin_p (void) API;
SCM gucu_is_linetouched_p (SCM arg1, SCM arg2) API;
SCM gucu_is_wintouched_p (SCM arg1) API;
SCM gucu_KEY_F (SCM arg1) API;
SCM gucu_key_defined (SCM str) API;
SCM gucu_keyname (SCM arg1) API;
SCM gucu_keypad_x (SCM arg1, SCM arg2) API;
SCM gucu_killchar (void) API;
SCM gucu_leaveok_x (SCM arg1, SCM arg2) API;
SCM gucu_longname (void) API;
SCM gucu_meta (SCM bf) API;
SCM gucu_mouseinterval (SCM arg1) API;
SCM gucu_mvcur (SCM arg1, SCM arg2, SCM arg3, SCM arg4) API;
SCM gucu_mvderwin (SCM arg1, SCM arg2, SCM arg3) API;
SCM gucu_mvwin (SCM arg1, SCM arg2, SCM arg3) API;
SCM gucu_napms (SCM arg1) API;
SCM gucu_newpad (SCM arg1, SCM arg2) API;
SCM gucu_newwin (SCM arg1, SCM arg2, SCM arg3, SCM arg4) API;
SCM gucu_nl (void) API;
SCM gucu_nocbreak (void) API;
SCM gucu_nodelay_x (SCM arg1, SCM arg2) API;
SCM gucu_noecho (void) API;
SCM gucu_nonl (void) API;
SCM gucu_noqiflush (void) API;
SCM gucu_noraw (void) API;
SCM gucu_notimeout_x (SCM arg1, SCM arg2) API;
SCM gucu_noutrefresh (SCM arg1) API;
SCM gucu_overlay (SCM arg1, SCM arg2) API;
SCM gucu_overwrite (SCM arg1, SCM arg2) API;
SCM gucu_PAIR_NUMBER (SCM arg1) API;
SCM gucu_pechochar (SCM arg1, SCM arg2) API;
SCM gucu_pnoutrefresh (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7) API;
SCM gucu_prefresh (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5, SCM arg6, SCM arg7) API;
SCM gucu_qiflush (void) API;
SCM gucu_raw (void) API;
SCM gucu_redrawwin (SCM arg1) API;
SCM gucu_refresh (SCM arg1) API;
SCM gucu_reset_prog_mode (void) API;
SCM gucu_reset_shell_mode (void) API;
SCM gucu_resetty (void) API;
SCM gucu_savetty (void) API;
SCM gucu_scr_dump (SCM arg1) API;
SCM gucu_scr_init (SCM arg1) API;
SCM gucu_scrl (SCM arg1, SCM arg2) API;
SCM gucu_scrollok_x (SCM arg1, SCM arg2) API;
SCM gucu_scr_restore (SCM arg1) API;
SCM gucu_scr_set (SCM arg1) API;
SCM gucu_setscrreg_x (SCM arg1, SCM arg2, SCM arg3) API;
SCM gucu_set_term (SCM arg1) API;
SCM gucu_setsyx (SCM y, SCM x) API;
SCM gucu_standend_x (SCM arg1) API;
SCM gucu_standout_x (SCM arg1) API;
SCM gucu_start_color (void) API;
SCM gucu_subpad (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5) API;
SCM gucu_subwin (SCM arg1, SCM arg2, SCM arg3, SCM arg4, SCM arg5) API;
SCM gucu_syncok_x (SCM arg1, SCM arg2) API;
SCM gucu_term_attrs (void) API;
SCM gucu_termname (void) API;
SCM gucu_timeout_x (SCM arg1, SCM arg2) API;
SCM gucu_typeahead (SCM arg1) API;
SCM gucu_ungetch (SCM arg1) API;
SCM gucu_use_default_colors (void) API;
SCM gucu_use_extended_names (SCM bf) API;
SCM gucu_use_env (SCM arg1) API;
SCM gucu_vidattr (SCM arg1) API;
SCM gucu_waddch (SCM arg1, SCM arg2) API;
SCM gucu_waddchnstr (SCM arg1, SCM arg2, SCM arg3) API;
SCM gucu_waddnstr (SCM arg1, SCM arg2, SCM arg3) API;
SCM gucu_wchgat (SCM arg1, SCM arg2, SCM arg3, SCM arg4) API;
SCM gucu_wcursyncup (SCM arg1) API;
SCM gucu_wdelch (SCM arg1) API;
SCM gucu_wechochar (SCM arg1, SCM arg2) API;
SCM gucu_wenclose_p (SCM arg1, SCM arg2, SCM arg3) API;
SCM gucu_wgetch (SCM arg1) API;
SCM gucu_whline (SCM arg1, SCM arg2, SCM arg3) API;
SCM gucu_winch (SCM arg1) API;
SCM gucu_winsch (SCM win, SCM ch) API;
SCM gucu_winsdelln (SCM arg1, SCM arg2) API;
SCM gucu_winsnstr (SCM arg1, SCM arg2, SCM arg3) API;
SCM gucu_wmove (SCM arg1, SCM arg2, SCM arg3) API;
SCM gucu_wredrawln (SCM arg1, SCM arg2, SCM arg3) API;
SCM gucu_wsyncup (SCM arg1) API;
SCM gucu_wsyncdown (SCM arg1) API;
SCM gucu_wtouchln (SCM arg1, SCM arg2, SCM arg3, SCM arg4) API;
SCM gucu_wvline (SCM arg1, SCM arg2, SCM arg3) API;

void gucu_init_function (void) API;
#endif /* not FUNCTION_H */
