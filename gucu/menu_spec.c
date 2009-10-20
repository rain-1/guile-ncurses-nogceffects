#include <curses.h>
#include <errno.h>
#include <libguile.h>
#include <config.h>
#include <menu.h>
#include "type.h"
#include "menu_type.h"
#include "menu_spec.h"


// Return the minimum size of a window necessary to contain the MENU
SCM
gucu_scale_menu (SCM menu)
{
  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "scale-menu");

  MENU *m = _scm_to_menu (menu);
  int rows, columns, ret_val;
  
  ret_val = scale_menu(m, &rows, &columns);
  // printf("%p %d %d", m, rows, columns);
  if (ret_val == E_OK)
    {
      return scm_list_2 (scm_from_int (rows), scm_from_int (columns));
    }
  else
    return SCM_BOOL_F;
}


/* returns a list of 3 elements: 
1. num spaces between item name and description
2. num rows
3. num cols
or #f on failure.
*/
SCM
gucu_menu_spacing (SCM menu)
{
  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "menu-spacing");

  MENU *m = _scm_to_menu (menu);
  
  int spc_description;
  int spc_rows;
  int spc_columns;
  int ret_val;
  
  ret_val = menu_spacing (m, &spc_description, &spc_rows, &spc_columns);

  if (ret_val == E_OK)
    return scm_list_3 (scm_from_int (spc_description), 
		       scm_from_int (spc_rows),
		       scm_from_int (spc_columns));
  else
    return SCM_BOOL_F;
}

/* return the width of the menu */
SCM
gucu_menu_itemlen (SCM menu)  
{
  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "menu-itemlen");

  MENU *m = _scm_to_menu (menu);

  return scm_from_short (m->itemlen);
}

/* */
SCM
gucu_menu_format (SCM menu)
{
  SCM_ASSERT (_scm_is_menu (menu), menu, SCM_ARG1, "menu-format");
  int rows, cols;

  MENU *m = _scm_to_menu (menu);

  menu_format (m, &rows, &cols);

  return scm_list_2 (scm_from_int (rows),
		     scm_from_int (cols));
}

void
gucu_menu_init_special ()
{
   scm_c_define_gsubr ("scale-menu", 1, 0, 0, gucu_scale_menu);
   scm_c_define_gsubr ("menu-spacing", 1, 0, 0, gucu_menu_spacing);
   scm_c_define_gsubr ("menu-itemlen", 1, 0, 0, gucu_menu_itemlen);
   scm_c_define_gsubr ("menu-format", 1, 0, 0, gucu_menu_format);
}
