
#include <libguile.h>
#include <curses.h>
#include <menu.h>
#include <config.h>

#include "menu_func.h"
#include "type.h"
#include "menu_type.h"
#include "compat.h"

#define _GNU_SOURCE
#define cstring_t cchar_t


static void menu_posted_error (const char *funcname)
{
  scm_misc_error (funcname, "Menu already posted", SCM_BOOL_F);
}

static void menu_not_connected_error (const char *funcname)
{
  scm_misc_error (funcname, "Menu has no items", SCM_BOOL_F);
}

/* Set the foreground attribute of the menu */
SCM
gucu_set_menu_fore (SCM menu, SCM attr)
{
  MENU *c_menu;
  chtype c_attr;
  int ret;

  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "set-menu-fore!");
  SCM_ASSERT (_scm_is_chtype (attr), attr, SCM_ARG2, "set-menu-fore!");
  
  c_menu = _scm_to_menu (menu);
  c_attr = _scm_to_chtype (attr);

  ret = set_menu_fore (c_menu, c_attr);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-menu-fore!", attr);
  
  return SCM_UNSPECIFIED;
}

/* Return the foreground attribute of the menu */
SCM
gucu_menu_fore (SCM menu)
{
  MENU *c_menu;
  chtype ret;
  SCM s_ret;

  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "menu-fore");

  c_menu = _scm_to_menu (menu);

  ret = menu_fore (c_menu);
  s_ret = _scm_from_chtype (ret);

  return s_ret;
}

/* Set the background attribute of the menu */
SCM
gucu_set_menu_back (SCM menu, SCM attr)
{
  MENU *c_menu;
  chtype c_attr;
  int ret;

  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "set-menu-back!");
  SCM_ASSERT (_scm_is_chtype (attr), attr, SCM_ARG2, "set-menu-back!");
  
  c_menu = _scm_to_menu (menu);
  c_attr = _scm_to_chtype (attr);

  ret = set_menu_back (c_menu, c_attr);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-menu-back!", attr);
  
  return SCM_UNSPECIFIED;
}

/* Return the background attribute of the menu */
SCM
gucu_menu_back (SCM menu)
{
  MENU *c_menu;
  chtype ret;
  SCM s_ret;

  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "menu-back");

  c_menu = _scm_to_menu (menu);

  ret = menu_back (c_menu);
  s_ret = _scm_from_chtype (ret);

  return s_ret;
}

/* Set the attributes for "unselectable" menu items */
SCM
gucu_set_menu_grey (SCM menu, SCM attr)
{
  MENU *c_menu;
  chtype c_attr;
  int ret;

  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "set-menu-grey!");
  SCM_ASSERT (_scm_is_chtype (attr), attr, SCM_ARG2, "set-menu-grey!");
  
  c_menu = _scm_to_menu (menu);
  c_attr = _scm_to_chtype (attr);

  ret = set_menu_grey (c_menu, c_attr);
  if (ret == E_BAD_ARGUMENT)
    scm_out_of_range ("set-menu-grey!", attr);
  
  return SCM_UNSPECIFIED;
}

/* Return the attributes for unselectable menu items */
SCM
gucu_menu_grey (SCM menu)
{
  MENU *c_menu;
  chtype ret;
  SCM s_ret;

  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "menu-grey");

  c_menu = _scm_to_menu (menu);

  ret = menu_grey (c_menu);
  s_ret = _scm_from_chtype (ret);

  return s_ret;
}

SCM
gucu_set_menu_pad (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "set-menu-pad!");
	SCM_ASSERT (scm_is_integer (arg2), arg2, SCM_ARG2, "set-menu-pad!");

	MENU *c_arg1 = _scm_to_menu (arg1);
	int c_arg2 = scm_to_int (arg2);

	int ret = set_menu_pad (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_menu_pad (SCM arg1)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "menu-pad");

	 MENU *c_arg1 = _scm_to_menu (arg1);

	int ret = menu_pad (c_arg1);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_pos_menu_cursor (SCM arg1)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "pos-menu-cursor");

	 MENU *c_arg1 = _scm_to_menu (arg1);

	int ret = pos_menu_cursor (c_arg1);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_menu_driver (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "menu-driver");
	SCM_ASSERT (scm_is_integer (arg2), arg2, SCM_ARG2, "menu-driver");

	MENU *c_arg1 = _scm_to_menu (arg1);
	int c_arg2 = scm_to_int (arg2);

	int ret = menu_driver (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

/* Set the maximum display size of the menu in rows and columns */
SCM
gucu_set_menu_format (SCM menu, SCM rows, SCM cols)
{
  MENU *c_menu;
  int c_rows, c_cols, ret;

  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "set-menu-format!");
  SCM_ASSERT (scm_is_integer (rows), rows, SCM_ARG2, "set-menu-format!");
  SCM_ASSERT (scm_is_integer (cols), cols, SCM_ARG3, "set-menu-format!");
  
  c_menu = _scm_to_menu (menu);
  c_rows = scm_to_int (rows);
  c_cols = scm_to_int (cols);

  if (c_rows < 0)
    scm_out_of_range ("set-menu-format!", rows);
  if (c_cols < 0)
    scm_out_of_range ("set-menu-format!", cols);

  ret = set_menu_format (c_menu, c_rows, c_cols);
  if (ret == E_POSTED)
    menu_posted_error ("set-menu-format!");
  else if (ret == E_NOT_CONNECTED)
    menu_not_connected_error ("set-menu-format!");

  return SCM_UNSPECIFIED;
}

SCM
gucu_item_count (SCM arg1)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "item-count");

	 MENU *c_arg1 = _scm_to_menu (arg1);

	int ret = item_count (c_arg1);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_set_menu_mark (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "set-menu-mark!");
	SCM_ASSERT (scm_is_string (arg2), arg2, SCM_ARG2, "set-menu-mark!");

	MENU *c_arg1 = _scm_to_menu (arg1);
	 char *c_arg2 = scm_to_locale_string (arg2);

	int ret = set_menu_mark (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	free (c_arg2);

	return s_ret;
}

SCM
gucu_menu_mark (SCM menu)
{
  MENU *c_menu;
  SCM s_ret;

  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "menu-mark");

  c_menu = _scm_to_menu (menu);
  
  s_ret = scm_from_locale_string (menu_mark (c_menu));

  return s_ret;
}

SCM
gucu_set_menu_opts (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "set-menu-opts!");
	SCM_ASSERT (scm_is_integer (arg2), arg2, SCM_ARG2, "set-menu-opts!");

	MENU *c_arg1 = _scm_to_menu (arg1);
	int c_arg2 = scm_to_int (arg2);

	int ret = set_menu_opts (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_menu_opts_off (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "menu-opts-off!");
	SCM_ASSERT (scm_is_integer (arg2), arg2, SCM_ARG2, "menu-opts-off!");

	MENU *c_arg1 = _scm_to_menu (arg1);
	int c_arg2 = scm_to_int (arg2);

	int ret = menu_opts_off (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_menu_opts_on (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "menu-opts-on!");
	SCM_ASSERT (scm_is_integer (arg2), arg2, SCM_ARG2, "menu-opts-on!");

	MENU *c_arg1 = _scm_to_menu (arg1);
	int c_arg2 = scm_to_int (arg2);

	int ret = menu_opts_on (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_menu_opts (SCM arg1)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "menu-opts");

	 MENU *c_arg1 = _scm_to_menu (arg1);

	int ret = menu_opts (c_arg1);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_set_menu_pattern (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "set-menu-pattern!");
	SCM_ASSERT (scm_is_string (arg2), arg2, SCM_ARG2, "set-menu-pattern!");

	MENU *c_arg1 = _scm_to_menu (arg1);
	 char *c_arg2 = scm_to_locale_string (arg2);

	int ret = set_menu_pattern (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	free (c_arg2);

	return s_ret;
}

SCM
gucu_menu_pattern (SCM arg1)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "menu-pattern");

	 MENU *c_arg1 = _scm_to_menu (arg1);

	char * ret = menu_pattern (c_arg1);
	SCM s_ret = scm_from_locale_string (ret);

	return s_ret;
}

/* Post the menu to its associated subwindow to make it visible */
SCM
gucu_post_menu (SCM menu)
{
  MENU *c_menu;
  int ret;
  SCM s_ret;

  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "post-menu");

  c_menu = _scm_to_menu (menu);

  ret = post_menu (c_menu);

  /* FIXME: what is the error policy?  Fail on all?  Fail on
     non-drawin errors? */
  if (ret == E_POSTED)
    menu_posted_error ("post-menu");
  else if (ret == E_NOT_CONNECTED)
    menu_not_connected_error ("post-menu");

  s_ret = scm_from_int (ret);

  return s_ret;
}

SCM
gucu_unpost_menu (SCM arg1)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "unpost-menu");

	MENU *c_arg1 = _scm_to_menu (arg1);

	int ret = unpost_menu (c_arg1);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

/* Returns the printable name of a menu request code */
SCM
gucu_menu_request_name (SCM request)
{
  SCM s_ret;
  int c_request;
  
  SCM_ASSERT (scm_is_integer (request), request, SCM_ARG1, 
	      "menu-request-name");

  c_request = scm_to_int (request);

  s_ret = scm_from_locale_string (menu_request_name (c_request));

  return s_ret;
}

SCM
gucu_menu_request_by_name (SCM arg1)
{
	SCM_ASSERT (scm_is_string (arg1), arg1, SCM_ARG1, "menu-request-by-name");

	 char *c_arg1 = scm_to_locale_string (arg1);

	int ret = menu_request_by_name (c_arg1);
	SCM s_ret = scm_from_int (ret);

	free (c_arg1);

	return s_ret;
}

SCM
gucu_set_menu_spacing (SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "set-menu-spacing!");
	SCM_ASSERT (scm_is_integer (arg2), arg2, SCM_ARG2, "set-menu-spacing!");
	SCM_ASSERT (scm_is_integer (arg3), arg3, SCM_ARG3, "set-menu-spacing!");
	SCM_ASSERT (scm_is_integer (arg4), arg4, SCM_ARG4, "set-menu-spacing!");

	MENU *c_arg1 = _scm_to_menu (arg1);
	int c_arg2 = scm_to_int (arg2);
	int c_arg3 = scm_to_int (arg3);
	int c_arg4 = scm_to_int (arg4);

	int ret = set_menu_spacing (c_arg1, c_arg2, c_arg3, c_arg4);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

/* Set the window that contains the title and border of the menu */
SCM
gucu_set_menu_win (SCM menu, SCM win)
{
  MENU *c_menu;
  WINDOW *c_win;
  int ret;
  struct gucu_menu *gm;

  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "set-menu-win!");
  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG2, "set-menu-win!");

  c_menu = _scm_to_menu (menu);
  c_win = _scm_to_window (win);

  ret = set_menu_win (c_menu, c_win);
  if (ret == E_POSTED)
    menu_posted_error ("set-menu-win!");
  
  /* Protect the window from GC */
  gm = (struct gucu_menu *) SCM_SMOB_DATA (menu);
  scm_call_1 (gm->win_guard, win);

  return SCM_UNSPECIFIED;
}

/* Set the window that contains the contents of the menu */
SCM
gucu_set_menu_sub (SCM menu, SCM win)
{
  MENU *c_menu;
  WINDOW *c_win;
  int ret;
  struct gucu_menu *gm;

  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "set-menu-sub!");
  SCM_ASSERT (_scm_is_window (win), win, SCM_ARG2, "set-menu-sub!");

  c_menu = _scm_to_menu (menu);
  c_win = _scm_to_window (win);

  ret = set_menu_sub (c_menu, c_win);
  if (ret == E_POSTED)
    menu_posted_error ("set-menu-sub!");
  
  /* Protect the window from GC */
  gm = (struct gucu_menu *) SCM_SMOB_DATA (menu);
  scm_call_1 (gm->subwin_guard, win);

  return SCM_UNSPECIFIED;
}

SCM
gucu_menu_win (SCM arg1)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "menu-win");

	 MENU *c_arg1 = _scm_to_menu (arg1);

	WINDOW * ret = menu_win (c_arg1);
	SCM s_ret = _scm_from_window (ret);

	return s_ret;
}

SCM
gucu_menu_sub (SCM arg1)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "menu-sub");

	 MENU *c_arg1 = _scm_to_menu (arg1);

	WINDOW * ret = menu_sub (c_arg1);
	SCM s_ret = _scm_from_window (ret);

	return s_ret;
}

SCM
gucu_set_current_item (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "set-current-item!");
	SCM_ASSERT (_scm_is_item (arg2), arg2, SCM_ARG2, "set-current-item!");

	MENU *c_arg1 = _scm_to_menu (arg1);
	 ITEM *c_arg2 = _scm_to_item (arg2);

	int ret = set_current_item (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_current_item (SCM arg1)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "current-item");

	 MENU *c_arg1 = _scm_to_menu (arg1);

	ITEM * ret = current_item (c_arg1);
	SCM s_ret = _scm_from_item (ret);

	return s_ret;
}

SCM
gucu_set_top_row (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "set-top-row!");
	SCM_ASSERT (scm_is_integer (arg2), arg2, SCM_ARG2, "set-top-row!");

	MENU *c_arg1 = _scm_to_menu (arg1);
	int c_arg2 = scm_to_int (arg2);

	int ret = set_top_row (c_arg1, c_arg2);
	if (ret == ERR)
	  return SCM_BOOL_F;

	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_top_row (SCM arg1)
{
	SCM_ASSERT (_scm_is_menu (arg1), arg1, SCM_ARG1, "top-row");

	 MENU *c_arg1 = _scm_to_menu (arg1);

	int ret = top_row (c_arg1);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_item_index (SCM arg1)
{
	SCM_ASSERT (_scm_is_item (arg1), arg1, SCM_ARG1, "item-index");

	 ITEM *c_arg1 = _scm_to_item (arg1);

	int ret = item_index (c_arg1);
	if (ret == ERR)
	  return SCM_BOOL_F;

	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_item_name (SCM item)
{
  
  SCM_ASSERT (_scm_is_item (item), item, SCM_ARG1, "item-name");

  {
    const ITEM *c_item = _scm_to_item (item);

    return scm_from_locale_string (item_name (c_item));
  }
}

/* Return the description part of a given item */
SCM
gucu_item_description (SCM item)
{
  SCM_ASSERT (_scm_is_item (item), item, SCM_ARG1, "item-description");

  {
    const ITEM *c_item = _scm_to_item (item);

    return scm_from_locale_string (item_description (c_item));
  }
}

SCM
gucu_set_item_opts (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_item (arg1), arg1, SCM_ARG1, "set-item-opts!");
	SCM_ASSERT (scm_is_integer (arg2), arg2, SCM_ARG2, "set-item-opts!");

	ITEM *c_arg1 = _scm_to_item (arg1);
	int c_arg2 = scm_to_int (arg2);

	int ret = set_item_opts (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_item_opts_on (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_item (arg1), arg1, SCM_ARG1, "item-opts-on!");
	SCM_ASSERT (scm_is_integer (arg2), arg2, SCM_ARG2, "item-opts-on!");

	ITEM *c_arg1 = _scm_to_item (arg1);
	int c_arg2 = scm_to_int (arg2);

	int ret = item_opts_on (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_item_opts_off (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_item (arg1), arg1, SCM_ARG1, "item-opts-off!");
	SCM_ASSERT (scm_is_integer (arg2), arg2, SCM_ARG2, "item-opts-off!");

	ITEM *c_arg1 = _scm_to_item (arg1);
	int c_arg2 = scm_to_int (arg2);

	int ret = item_opts_off (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_item_opts (SCM arg1)
{
	SCM_ASSERT (_scm_is_item (arg1), arg1, SCM_ARG1, "item-opts");

	 ITEM *c_arg1 = _scm_to_item (arg1);

	int ret = item_opts (c_arg1);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_set_item_value (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_item (arg1), arg1, SCM_ARG1, "set-item-value!");
	SCM_ASSERT (scm_is_bool (arg2), arg2, SCM_ARG2, "set-item-value!");

	ITEM *c_arg1 = _scm_to_item (arg1);
	bool c_arg2 = scm_to_bool (arg2);

	int ret = set_item_value (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_item_value (SCM arg1)
{
	SCM_ASSERT (_scm_is_item (arg1), arg1, SCM_ARG1, "item-value");

	 ITEM *c_arg1 = _scm_to_item (arg1);

	bool ret = item_value (c_arg1);
	SCM s_ret = scm_from_bool (ret);

	return s_ret;
}

SCM
gucu_item_visible_p (SCM arg1)
{
	SCM_ASSERT (_scm_is_item (arg1), arg1, SCM_ARG1, "item-visible?");

	 ITEM *c_arg1 = _scm_to_item (arg1);

	bool ret = item_visible (c_arg1);
	SCM s_ret = scm_from_bool (ret);

	return s_ret;
}


void 
gucu_menu_init_function ()
{
	scm_c_define_gsubr ("set-menu-fore!", 2, 0, 0, gucu_set_menu_fore);
	scm_c_define_gsubr ("menu-fore", 1, 0, 0, gucu_menu_fore);
	scm_c_define_gsubr ("set-menu-back!", 2, 0, 0, gucu_set_menu_back);
	scm_c_define_gsubr ("menu-back", 1, 0, 0, gucu_menu_back);
	scm_c_define_gsubr ("set-menu-grey!", 2, 0, 0, gucu_set_menu_grey);
	scm_c_define_gsubr ("menu-grey", 1, 0, 0, gucu_menu_grey);
	scm_c_define_gsubr ("set-menu-pad!", 2, 0, 0, gucu_set_menu_pad);
	scm_c_define_gsubr ("menu-pad", 1, 0, 0, gucu_menu_pad);
	scm_c_define_gsubr ("pos-menu-cursor", 1, 0, 0, gucu_pos_menu_cursor);
	scm_c_define_gsubr ("menu-driver", 2, 0, 0, gucu_menu_driver);
	scm_c_define_gsubr ("set-menu-format!", 3, 0, 0, gucu_set_menu_format);
	scm_c_define_gsubr ("item-count", 1, 0, 0, gucu_item_count);
	scm_c_define_gsubr ("set-menu-mark!", 2, 0, 0, gucu_set_menu_mark);
	scm_c_define_gsubr ("menu-mark", 1, 0, 0, gucu_menu_mark);
	scm_c_define_gsubr ("set-menu-opts!", 2, 0, 0, gucu_set_menu_opts);
	scm_c_define_gsubr ("menu-opts-off!", 2, 0, 0, gucu_menu_opts_off);
	scm_c_define_gsubr ("menu-opts-on!", 2, 0, 0, gucu_menu_opts_on);
	scm_c_define_gsubr ("menu-opts", 1, 0, 0, gucu_menu_opts);
	scm_c_define_gsubr ("set-menu-pattern!", 2, 0, 0, gucu_set_menu_pattern);
	scm_c_define_gsubr ("menu-pattern", 1, 0, 0, gucu_menu_pattern);
	scm_c_define_gsubr ("post-menu", 1, 0, 0, gucu_post_menu);
	scm_c_define_gsubr ("unpost-menu", 1, 0, 0, gucu_unpost_menu);
	scm_c_define_gsubr ("menu-request-name", 1, 0, 0, gucu_menu_request_name);
	scm_c_define_gsubr ("menu-request-by-name", 1, 0, 0, gucu_menu_request_by_name);
	scm_c_define_gsubr ("set-menu-spacing!", 4, 0, 0, 
			    gucu_set_menu_spacing);
	scm_c_define_gsubr ("set-menu-win!", 2, 0, 0, gucu_set_menu_win);
	scm_c_define_gsubr ("set-menu-sub!", 2, 0, 0, gucu_set_menu_sub);
	scm_c_define_gsubr ("menu-win", 1, 0, 0, gucu_menu_win);
	scm_c_define_gsubr ("menu-sub", 1, 0, 0, gucu_menu_sub);
	scm_c_define_gsubr ("set-current-item!", 2, 0, 0, gucu_set_current_item);
	scm_c_define_gsubr ("current-item", 1, 0, 0, gucu_current_item);
	scm_c_define_gsubr ("set-top-row!", 2, 0, 0, gucu_set_top_row);
	scm_c_define_gsubr ("top-row", 1, 0, 0, gucu_top_row);
	scm_c_define_gsubr ("item-index", 1, 0, 0, gucu_item_index);
	scm_c_define_gsubr ("item-name", 1, 0, 0, gucu_item_name);
	scm_c_define_gsubr ("item-description", 1, 0, 0, gucu_item_description);
	scm_c_define_gsubr ("set-item-opts!", 2, 0, 0, gucu_set_item_opts);
	scm_c_define_gsubr ("item-opts-on!", 2, 0, 0, gucu_item_opts_on);
	scm_c_define_gsubr ("item-opts-off!", 2, 0, 0, gucu_item_opts_off);
	scm_c_define_gsubr ("item-opts", 1, 0, 0, gucu_item_opts);
	scm_c_define_gsubr ("set-item-value!", 2, 0, 0, gucu_set_item_value);
	scm_c_define_gsubr ("item-value", 1, 0, 0, gucu_item_value);
	scm_c_define_gsubr ("item-visible?", 1, 0, 0, gucu_item_visible_p);
}
