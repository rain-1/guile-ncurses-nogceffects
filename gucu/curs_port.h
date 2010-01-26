#ifndef CURS_PORT_H
#define CURS_PORT_H 1

#include <libguile.h>
#include <config.h>
#include "visibility.h"

#ifdef HAVE_FOPENCOOKIE
GUCU_API SCM gucu_newterm (SCM a, SCM b, SCM c);
#endif
GUCU_API SCM gucu_getwin (SCM port);
GUCU_API SCM gucu_putwin (SCM win, SCM port);

GUCU_LOCAL void gucu_init_port (void);

#endif /* not CURS_PORT_H */
