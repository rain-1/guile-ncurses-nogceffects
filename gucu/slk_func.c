#include <libguile.h>
#include <curses.h>
#include <config.h>
#include "slk_func.h"
#include "type.h"


SCM
gucu_slk_attr_off (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_attr (arg1), arg1, SCM_ARG1, "slk-attr-off");
	SCM_ASSERT (_scm_is_voidstring (arg2), arg2, SCM_ARG2, "slk-attr-off");

	const attr_t c_arg1 = _scm_to_attr (arg1);
	void *c_arg2 = _scm_to_voidstring (arg2);

	int ret = slk_attr_off (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_slk_attroff (SCM arg1)
{
	SCM_ASSERT (_scm_is_chtype (arg1), arg1, SCM_ARG1, "slk-attroff");

	const chtype c_arg1 = _scm_to_chtype (arg1);

	int ret = slk_attroff (c_arg1);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_slk_attr_on (SCM arg1, SCM arg2)
{
	SCM_ASSERT (_scm_is_attr (arg1), arg1, SCM_ARG1, "slk-attr-on");
	SCM_ASSERT (_scm_is_voidstring (arg2), arg2, SCM_ARG2, "slk-attr-on");

	const attr_t c_arg1 = _scm_to_attr (arg1);
	void *c_arg2 = _scm_to_voidstring (arg2);

	int ret = slk_attr_on (c_arg1, c_arg2);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_slk_attron (SCM arg1)
{
	SCM_ASSERT (_scm_is_chtype (arg1), arg1, SCM_ARG1, "slk-attron");

	const chtype c_arg1 = _scm_to_chtype (arg1);

	int ret = slk_attron (c_arg1);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_slk_attr_set (SCM arg1, SCM arg2, SCM arg3)
{
	SCM_ASSERT (_scm_is_attr (arg1), arg1, SCM_ARG1, "slk-attr-set");
	SCM_ASSERT (scm_is_integer (arg2), arg2, SCM_ARG2, "slk-attr-set");
	SCM_ASSERT (_scm_is_voidstring (arg3), arg3, SCM_ARG3, "slk-attr-set");

	const attr_t c_arg1 = _scm_to_attr (arg1);
	short c_arg2 = scm_to_short (arg2);
	void *c_arg3 = _scm_to_voidstring (arg3);

	int ret = slk_attr_set (c_arg1, c_arg2, c_arg3);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_slk_attrset (SCM arg1)
{
	SCM_ASSERT (_scm_is_chtype (arg1), arg1, SCM_ARG1, "slk-attrset");

	const chtype c_arg1 = _scm_to_chtype (arg1);

	int ret = slk_attrset (c_arg1);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_slk_clear ()
{
	int ret = slk_clear ();
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_slk_color (SCM arg1)
{
	SCM_ASSERT (scm_is_integer (arg1), arg1, SCM_ARG1, "slk-color");

	short c_arg1 = scm_to_short (arg1);

	int ret = slk_color (c_arg1);
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_slk_init (SCM arg1)
{
	SCM_ASSERT (scm_is_integer (arg1), arg1, SCM_ARG1, "slk-init");

	int c_arg1 = scm_to_int (arg1);

	int ret = slk_init (c_arg1);
	if (ret != OK)
	  return SCM_BOOL_F;

	return SCM_BOOL_T;
}

SCM
gucu_slk_label (SCM arg1)
{
	SCM_ASSERT (scm_is_integer (arg1), arg1, SCM_ARG1, "slk-label");

	int c_arg1 = scm_to_int (arg1);

	char * ret = slk_label (c_arg1);
	SCM s_ret = scm_from_locale_string (ret);

	return s_ret;
}

SCM
gucu_slk_noutrefresh ()
{
	int ret = slk_noutrefresh ();
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_slk_refresh ()
{
	int ret = slk_refresh ();
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_slk_restore ()
{
	int ret = slk_restore ();
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}

SCM
gucu_slk_set (SCM arg1, SCM arg2, SCM arg3)
{
	SCM_ASSERT (scm_is_integer (arg1), arg1, SCM_ARG1, "slk-set");
	SCM_ASSERT (scm_is_string (arg2), arg2, SCM_ARG2, "slk-set");
	SCM_ASSERT (scm_is_integer (arg3), arg3, SCM_ARG3, "slk-set");

	int c_arg1 = scm_to_int (arg1);
	 char *c_arg2 = scm_to_locale_string (arg2);
	int c_arg3 = scm_to_int (arg3);

	int ret = slk_set (c_arg1, c_arg2, c_arg3);
	if (ret != OK)
	  return SCM_BOOL_F;
	
	return SCM_BOOL_T;

}

SCM
gucu_slk_touch ()
{
	int ret = slk_touch ();
	SCM s_ret = scm_from_int (ret);

	return s_ret;
}


void 
gucu_slk_init_function ()
{
  scm_c_define_gsubr ("slk-attr-off", 2, 0, 0, gucu_slk_attr_off);
  scm_c_define_gsubr ("slk-attroff", 1, 0, 0, gucu_slk_attroff);
  scm_c_define_gsubr ("slk-attr-on", 2, 0, 0, gucu_slk_attr_on);
  scm_c_define_gsubr ("slk-attron", 1, 0, 0, gucu_slk_attron);
  scm_c_define_gsubr ("slk-attr-set", 3, 0, 0, gucu_slk_attr_set);
  scm_c_define_gsubr ("slk-attrset", 1, 0, 0, gucu_slk_attrset);
  scm_c_define_gsubr ("slk-clear", 0, 0, 0, gucu_slk_clear);
  scm_c_define_gsubr ("slk-color", 1, 0, 0, gucu_slk_color);
  scm_c_define_gsubr ("slk-init", 1, 0, 0, gucu_slk_init);
  scm_c_define_gsubr ("slk-label", 1, 0, 0, gucu_slk_label);
  scm_c_define_gsubr ("slk-noutrefresh", 0, 0, 0, gucu_slk_noutrefresh);
  scm_c_define_gsubr ("slk-refresh", 0, 0, 0, gucu_slk_refresh);
  scm_c_define_gsubr ("slk-restore", 0, 0, 0, gucu_slk_restore);
  scm_c_define_gsubr ("slk-set", 3, 0, 0, gucu_slk_set);
  scm_c_define_gsubr ("slk-touch", 0, 0, 0, gucu_slk_touch);
}

