#include <config.h>

#include <assert.h>
#include <errno.h>
#include <libguile.h>
#include <libintl.h>
#include <menu.h>
#include <stdio.h>
#include <string.h>

#include "compat.h"
#include "menu_type.h"
#include "type.h"

scm_t_bits menu_tag;
scm_t_bits item_tag;

SCM equalp_item (SCM x1, SCM x2);
size_t gc_free_item (SCM x);
SCM mark_item (SCM x);
int print_item (SCM x, SCM port, scm_print_state * pstate);

SCM equalp_menu (SCM x1, SCM x2);
size_t gc_free_menu (SCM x);
SCM mark_menu (SCM x);
int print_menu (SCM x, SCM port, scm_print_state * pstate);

/* item -- in C, an ITEM *.  In Scheme, a smob that contains the
 * pointer */

SCM
gucu_new_item (SCM name, SCM description)
{
  SCM_ASSERT (scm_is_string (name), name, SCM_ARG1, "new-item");
  SCM_ASSERT (scm_is_string (description), description, SCM_ARG2, "new-item");

  char *c_name = scm_to_locale_string (name);
  char *c_description = scm_to_locale_string (description);

  ITEM *c_item = new_item (c_name, c_description);
  if (c_item == NULL)
    {
      if (errno == E_BAD_ARGUMENT)
	{
	  scm_error_scm (SCM_BOOL_F,
			 scm_from_locale_string ("new-item"),
			 scm_from_locale_string ( gettext ("bad argument")),
			 SCM_BOOL_F, SCM_BOOL_F);
	}
      else if (errno == E_SYSTEM_ERROR)
	{
	  scm_error_scm (SCM_BOOL_F,
			 scm_from_locale_string ("new-item"),
			 scm_from_locale_string ( gettext ("system error")),
			 SCM_BOOL_F, SCM_BOOL_F);
	}
      else
	abort ();
    }

  SCM ret = _scm_from_item (c_item);

  return ret;
}


int
_scm_is_item (SCM x)
{
  return SCM_SMOB_PREDICATE (item_tag, x);
}

ITEM *
_scm_to_item (SCM x)
{
  return (ITEM *) SCM_SMOB_DATA (x);
}

SCM
_scm_from_item (ITEM * x)
{
  SCM s_item;

  assert (x != NULL);

  SCM_NEWSMOB (s_item, item_tag, x);

  assert (x == (ITEM *) SCM_SMOB_DATA (s_item));

  if (0)
    {
      fprintf (stderr, gettext ("Making <#item> smob from ITEM * %p\n"), (void *) x);
    }

  return (s_item);
}

// Items are equal if they point to the same C structure
SCM
equalp_item (SCM x1, SCM x2)
{
  ITEM *item1 = (ITEM *) SCM_SMOB_DATA (x1);
  ITEM *item2 = (ITEM *) SCM_SMOB_DATA (x2);

  if ((item1 == NULL) || (item2 == NULL))
    return SCM_BOOL_F;
  else if ((item1 != item2))
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

SCM
mark_item (SCM x UNUSED)
{
  // No SCMs in the item type: nothing to do here.
  return (SCM_BOOL_F);
}

/* The name is free_item. */
size_t
gc_free_item (SCM item)
{
  SCM_ASSERT (_scm_is_item (item), item, SCM_ARG1, "free-item");

  ITEM *m = _scm_to_item (item);

  assert (m != NULL);

  free_item (m);

  return 0;
}

int
print_item (SCM x, SCM port, scm_print_state * pstate UNUSED)
{
  ITEM *frm = (ITEM *) SCM_SMOB_DATA (x);
  char *str;

  assert (frm != NULL);

  scm_puts ("#<item ", port);

  if (asprintf (&str, "%p", (void *) frm) < 0)
    scm_puts ("???", port);
  else
    scm_puts (str, port);

  scm_puts (">", port);

  // non-zero means success
  return 1;
}

SCM
gucu_is_item_p (SCM x)
{
  return scm_from_bool (_scm_is_item (x));
}



// menu -- in C, a MENU *.  In Scheme, a smob that contains the pointer
// to a form along with a list that contains the SCM of the fields

// Note the C Menu item's internal list of fields must match the SCM
// list of field to avoid garbage collection madness.

int
_scm_is_menu (SCM x)
{
  if (SCM_SMOB_PREDICATE (menu_tag, x))
    {
      if (SCM_SMOB_DATA (x) == 0)
	return 0;
      else
	return 1;
    }
  else
    return 0;
}

MENU *
_scm_to_menu (SCM x)
{
  struct gucu_menu *gm;

  scm_assert_smob_type (menu_tag, x);

  gm = (struct gucu_menu *) SCM_SMOB_DATA (x);

  return (MENU *) gm->menu;
}

// There is no _scm_from_menu since we need its items as a list to do
// that
// SCM _scm_from_menu (MENU *x)

// Menus are equal if they point to the same C structure
SCM
equalp_menu (SCM x1, SCM x2)
{
  MENU *menu1 = _scm_to_menu (x1);
  MENU *menu2 = _scm_to_menu (x2);

  if ((menu1 == NULL) || (menu2 == NULL))
    return SCM_BOOL_F;
  else if ((menu1 != menu2))
    return SCM_BOOL_F;
  else
    return SCM_BOOL_T;
}

SCM
mark_menu (SCM x)
{
  struct gucu_menu *gm;

  scm_assert_smob_type (menu_tag, x);

  gm = (struct gucu_menu *) SCM_SMOB_DATA (x);
  scm_gc_mark (gm->items_guard);
  scm_gc_mark (gm->win_guard);

  return (gm->subwin_guard);
}

/* The name is gc_free_menu because the curses primitive that frees
   memory is called free_menu. */
size_t
gc_free_menu (SCM x)
{
  struct gucu_menu *gm;
  int retval;

  scm_assert_smob_type (menu_tag, x);

  gm = (struct gucu_menu *) SCM_SMOB_DATA (x);

  assert (gm != NULL);

  retval = free_menu (gm->menu);

  if (retval == E_BAD_ARGUMENT)
    {
      scm_error_scm (SCM_BOOL_F,
		     scm_from_locale_string ( gettext ("garbage collection of menu")),
		     scm_from_locale_string ( gettext ("bad argument")),
		     SCM_BOOL_F, SCM_BOOL_F);
    }
  else if (retval == E_POSTED)
    {
      scm_error_scm (SCM_BOOL_F,
		     scm_from_locale_string ( gettext ("garbage collection of menu")),
		     scm_from_locale_string ( gettext ("posted")),
		     SCM_BOOL_F, SCM_BOOL_F);
    }
  else if (retval == E_SYSTEM_ERROR)
    {
      scm_error_scm (SCM_BOOL_F,
		     scm_from_locale_string ( gettext ("garbage collection of menu")),
		     scm_from_locale_string ( gettext ("system error")),
		     SCM_BOOL_F, SCM_BOOL_F);
    }

  /* Release scheme objects from the guardians */
  while (scm_is_true (scm_call_0 (gm->items_guard)))
    ;
  while (scm_is_true (scm_call_0 (gm->win_guard)))
    ;
  while (scm_is_true (scm_call_0 (gm->subwin_guard)))
    ;

  SCM_SET_SMOB_DATA (x, NULL);

  return 0;
}

int
print_menu (SCM x, SCM port, scm_print_state * pstate UNUSED)
{
  MENU *menu = _scm_to_menu (x);
  char *str;

  assert (menu != NULL);

  scm_puts ("#<menu ", port);

  if (asprintf (&str, "%p", (void *) menu) < 0)
    scm_puts ("???", port);
  else
    scm_puts (str, port);

  scm_puts (">", port);

  // non-zero means success
  return 1;
}

SCM
gucu_is_menu_p (SCM x)
{
  return scm_from_bool (_scm_is_menu (x));
}

SCM
gucu_new_menu (SCM items)
{
  struct gucu_menu *gm;
  size_t len;
  ITEM **c_items;
  SCM smob;
  SCM entry;
  size_t i;

  /* Step 0: Check input list */
  SCM_ASSERT (scm_is_true (scm_list_p (items)), items, SCM_ARG1, "new-menu");

  len = scm_to_size_t (scm_length (items));
  if (len == 0)
    {
      scm_wrong_type_arg ("new-menu", SCM_ARG1, items);
      return (SCM_UNSPECIFIED);
    }
  for (i = 0; i < len; i++)
    {
      entry = scm_list_ref (items, scm_from_int (i));
      if (!_scm_is_item (entry))
	scm_wrong_type_arg ("new-menu", SCM_ARG1, items);
    }

  // Step 1: allocate memory
  gm = scm_gc_malloc (sizeof (struct gucu_menu), "gucu_menu");

  c_items = scm_gc_malloc (sizeof (ITEM *) * (len + 1), "gucu_menu");

  // Step 2: initialize it with C code

  // Step 3: create the smob
  SCM_NEWSMOB (smob, menu_tag, gm);

  // Step 4: finish the initialization
  for (i = 0; i < len; i++)
    {
      entry = scm_list_ref (items, scm_from_int (i));
      c_items[i] = _scm_to_item (entry);
    }

  /* This is a null-terminated array */
  c_items[len] = NULL;

  gm->menu = new_menu (c_items);

  if (gm->menu == NULL)
    {
      free (c_items);
      if (errno == E_NOT_CONNECTED)
	{
	  scm_misc_error ("new-menu", gettext ("menu has no items"), SCM_BOOL_F);
	}
      else if (errno == E_SYSTEM_ERROR)
	{
	  scm_error_scm (SCM_BOOL_F,
			 scm_from_locale_string ("new-menu"),
			 scm_from_locale_string ( gettext ("system error")),
			 SCM_BOOL_F, SCM_BOOL_F);
	}
      else
	abort ();
    }
  scm_remember_upto_here_1 (items);

#ifndef GUILE_1_POINT_6
  gm->items_guard = scm_make_guardian ();
  gm->win_guard = scm_make_guardian ();
  gm->subwin_guard = scm_make_guardian ();
#else
  gm->items_guard = scm_make_guardian (SCM_BOOL_F);
  gm->win_guard = scm_make_guardian (SCM_BOOL_F);
  gm->subwin_guard = scm_make_guardian (SCM_BOOL_F);
#endif

  /* Guard the items list */
  scm_call_1 (gm->items_guard, items);

  return smob;
}

void
gucu_menu_init_type ()
{
  item_tag = scm_make_smob_type ("item", sizeof (ITEM *));
  scm_set_smob_mark (item_tag, mark_item);
  scm_set_smob_free (item_tag, gc_free_item);
  scm_set_smob_print (item_tag, print_item);
  scm_set_smob_equalp (item_tag, equalp_item);
  scm_c_define_gsubr ("item?", 1, 0, 0, gucu_is_item_p);
  scm_c_define_gsubr ("new-item", 2, 0, 0, gucu_new_item);

  menu_tag = scm_make_smob_type ("menu", sizeof (struct menu *));
  scm_set_smob_mark (menu_tag, mark_menu);
  scm_set_smob_free (menu_tag, gc_free_menu);
  scm_set_smob_print (menu_tag, print_menu);
  scm_set_smob_equalp (menu_tag, equalp_menu);
  scm_c_define_gsubr ("menu?", 1, 0, 0, gucu_is_menu_p);
  scm_c_define_gsubr ("new-menu", 1, 0, 0, gucu_new_menu);
}
