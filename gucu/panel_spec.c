#include <curses.h>
#include <libguile.h>
#include <config.h>
#include <panel.h>
#include "type.h"
#include "panel_type.h"
#include "panel_spec.h"

/* This won't work because of the guardians */
#if 0
// Get the panel above PAN
SCM
gucu_panel_above (SCM pan)
{
  if (_scm_is_panel (pan))
    {
      PANEL *c_pan = _scm_to_panel (pan);
      PANEL *c_pan2 = panel_above (c_pan);
      return _scm_from_panel (c_pan2);
    }
  else if (pan == SCM_BOOL_F)
    {
      PANEL *c_pan2 = panel_above (0);
      return _scm_from_panel (c_pan2);
    }
  else
    scm_wrong_type_arg ("panel-above", SCM_ARG1, pan);

  /* Never reached */
  return SCM_BOOL_F;
}

// Get the panel below PAN
SCM
gucu_panel_below (SCM pan)
{
  if (_scm_is_panel (pan))
    {
      PANEL *c_pan = _scm_to_panel (pan);
      PANEL *c_pan2 = panel_below (c_pan);
      return _scm_from_panel (c_pan2);
    }
  else if (pan == SCM_BOOL_F)
    {
      PANEL *c_pan2 = panel_below (0);
      return _scm_from_panel (c_pan2);
    }
  else
    scm_wrong_type_arg ("panel-below", SCM_ARG1, pan);

  /* Never reached */
  return SCM_BOOL_F;
}
#endif

/* FIXME: this needs to be worked in with the smob garbage collector */
#if 0
SCM
gucu_set_panel_userdata (SCM pan, SCM data)
{
  if (_scm_is_panel (pan))
    {
      set_panel_userptr (_scm_to_panel (pan), (void *)data);
    }
  else
    scm_wrong_type_arg ("set-panel-userdata", SCM_ARG1, pan);
  
  return SCM_UNDEFINED;
}

SCM
gucu_panel_userdata (SCM pan)
{
  if (_scm_is_panel (pan))
    {
      PANEL *c_panel = _scm_to_panel (pan);
      return (SCM) (panel_userptr (c_panel));
    }
  else
    scm_wrong_type_arg ("set-panel-userdata", SCM_ARG1, pan);

  /* Never reached */
  return SCM_UNDEFINED;
}
#endif

void
gucu_panel_init_special (void)
{
  #if 0
  scm_c_define_gsubr ("panel-above", 1, 0, 0, gucu_panel_above);
  scm_c_define_gsubr ("panel-below", 1, 0, 0, gucu_panel_below);
  scm_c_define_gsubr ("set-panel-userdata", 2, 0, 0, gucu_set_panel_userdata);
  scm_c_define_gsubr ("panel-userdata", 1, 0, 0, gucu_panel_userdata);
  #endif
}
