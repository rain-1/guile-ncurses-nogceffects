#include <libguile.h>

static void inner_main (void *data, int argc, char **argv);

int 
main(int argc, char **argv)
{
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0;			/* never reached */
}

static void
inner_main (void *data, int argc, char **argv)
{
  scm_c_eval_string ("(use-modules (gucu curses))");
  scm_shell (argc, argv);
}

