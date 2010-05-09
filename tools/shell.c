#define _GNU_SOURCE
#define _XOPEN_SOURCE
#include <config.h>

/* libc and libutil */
#include <ctype.h>
#include <locale.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <termios.h>
#include <unistd.h>
#include <sys/ioctl.h>
#include <sys/types.h>

#ifdef HAVE_UTIL_H
#include <util.h>      /* openbsd: openpty; */
#endif

#ifdef HAVE_PTY_H
#include <pty.h>       /* cygwin, fedora: openpty */
#endif

#ifdef HAVE_PROCESS_H
#include <process.h>   /* cygwin: execlp; */
#endif

/* curses and guile */
#include <curses.h>    /* all: newterm; */
#include <libguile.h>  /* all: scm_shell; */

int open_terminal (char *, int, int);
int is_unix98_pty (const char *);
int is_bsd_pty (const char *);
int is_cygwin_pty (const char *);

#ifdef HAVE_NCURSESW
const int _HAVE_NCURSESW = TRUE;
#else
const int _HAVE_NCURSESW = FALSE;
#endif

/* This function checks if NAME looks like a Unix98 pseudo-terminal
   device name, aka "/dev/pts/XXX".  Returns TRUE or FALSE. */
int
is_unix98_pty (const char *name)
{
  const char unix_pty_root[] = "/dev/pts/";
  const size_t unix_pty_root_length = strlen (unix_pty_root);
  size_t name_length = strlen (name);
  char const *slave_name;
  size_t i;


  name_length = strlen (name);

  if (name_length < unix_pty_root_length)
      return FALSE;

  if (strncmp (name, unix_pty_root, unix_pty_root_length) != 0)
    return FALSE;

  slave_name = name + unix_pty_root_length;
  if (strlen (slave_name) == 0)
    return FALSE;

  /* All the remaining characters in NAME should be numeric */
  for (i = 0; i < name_length - unix_pty_root_length; i++)
    if (!isdigit ((int) slave_name[i]))
      return FALSE;

  return TRUE;
}

/* This function checks if NAME looks like a BSD pseudo-terminal
   device name, aka "/dev/ttyXY".  Returns TRUE or FALSE. */
int
is_bsd_pty (const char *name)
{
  const char bsd_pty_root[] = "/dev/tty";
  const size_t bsd_pty_root_length = strlen (bsd_pty_root);
  size_t name_length = strlen (name);
  int X, Y;

  name_length = strlen (name);

  if (name_length != bsd_pty_root_length + 2)
      return FALSE;

  if (strncmp (name, bsd_pty_root, bsd_pty_root_length) != 0)
    return FALSE;

  X = name[bsd_pty_root_length];
  Y = name[bsd_pty_root_length + 1];

  return (isalnum (X) && isalnum (Y));
}

/* This function checks if NAME looks like a Cygwin pseudo-terminal
   device name, aka "/dev/ttyN".  Returns TRUE or FALSE. */
int
is_cygwin_pty (const char *name)
{
  const char cygwin_pty_root[] = "/dev/tty";
  const size_t cygwin_pty_root_length = strlen (cygwin_pty_root);
  size_t name_length = strlen (name);
  char const *slave_name;
  size_t i;

  name_length = strlen (name);

  if (name_length < cygwin_pty_root_length)
      return FALSE;

  if (strncmp (name, cygwin_pty_root, cygwin_pty_root_length) != 0)
    return FALSE;

  slave_name = name + cygwin_pty_root_length;
  if (strlen (slave_name) == 0)
    return FALSE;

  /* All the remaining characters in NAME should be numeric */
  for (i = 0; i < name_length - cygwin_pty_root_length; i++)
    if (!isdigit ((int) slave_name[i]))
      return FALSE;

  return TRUE;
}

/* This function forks an xterm that listens to a pseudoterminal,
   PSEUDO_TERMINAL_SLAVE_NAME is the path in /dev of the pty,
   MASTER_FILE_DESCRIPTOR is the file id of the front of the pty, and
   SLAVE_FILE_DESCRIPTOR is the back of the pty.
   It returns the process id of the xterm. */
int
open_terminal (char *pseudo_terminal_slave_name,
	       int master_file_descriptor,
	       int slave_file_descriptor)
{
  int process_id;
  int name_length;
  struct termios terminal_attributes;
  char *s_flag;
  const size_t unix98_offset = strlen("/dev/pts/");
  const size_t bsd_offset = strlen("/dev/tty");
  const size_t cygwin_offset = strlen("/dev/tty");

  name_length = strlen(pseudo_terminal_slave_name);

  process_id = fork ();

  if (process_id == -1)
    {
      perror("fork");
      return 0;
    }
  else if (process_id == 0)
    {
      /* This is the child process */
      close (slave_file_descriptor);

      if (is_unix98_pty (pseudo_terminal_slave_name))
	{
	  asprintf (&s_flag, "-S%s/%d",
		    pseudo_terminal_slave_name + unix98_offset,
		    master_file_descriptor);
	}
      else if (is_bsd_pty (pseudo_terminal_slave_name))
	{
	  asprintf(&s_flag, "-S%c%c%d",
		   pseudo_terminal_slave_name[bsd_offset],
		   pseudo_terminal_slave_name[bsd_offset+1],
		   master_file_descriptor);
	}
      else if (is_cygwin_pty (pseudo_terminal_slave_name))
	{
	  asprintf (&s_flag, "-S%s/%d",
		    pseudo_terminal_slave_name + cygwin_offset,
		    master_file_descriptor);
	}
      else
	{
	  fprintf (stderr, "Unrecognized pseudo-terminal name: %s\n",
		   pseudo_terminal_slave_name);
	  _exit (EXIT_FAILURE);
	}

      printf ("Attemping to connect an xterm to %s\n",
	      pseudo_terminal_slave_name);
      printf ("Calling 'xterm %s'\n", s_flag);
      execlp ("xterm", "xterm", s_flag, NULL);
      /* Should not return */
      return 0;
    }
  else
    {
      /* This is the parent process */
      close (master_file_descriptor);

      /* Set to raw mode */
      tcgetattr (slave_file_descriptor, &terminal_attributes);
#ifdef HAVE_CFMAKERAW
	cfmakeraw (&terminal_attributes);
#else
	terminal_attributes.c_iflag &= ~(IGNBRK|BRKINT|PARMRK|ISTRIP
					 |INLCR|IGNCR|ICRNL|IXON);
	terminal_attributes.c_oflag &= ~OPOST;
	terminal_attributes.c_lflag &= ~(ECHO|ECHONL|ICANON|ISIG|IEXTEN);
	terminal_attributes.c_cflag &= ~(CSIZE|PARENB);
	terminal_attributes.c_cflag |= CS8;
#endif

      tcsetattr (slave_file_descriptor, TCSANOW, &terminal_attributes);
      return process_id;
    }
  return 0;
}

static void
inner_main (void *data, int argc, char **argv)
{
  int master, slave;
  pid_t pid;
  char name[512];
  int slave_read, slave_write;
  FILE *fp_slave_read, *fp_slave_write;
  SCREEN *screen;
  struct termios tio;
  struct winsize win;
  int i;

  setlocale (LC_ALL, "");

  /* The command line arguments are going to be passed down to Guile,
     but, we need to check here for --version and --help. */
  for (i=1; i<argc; i++)
    {
      if (!strcmp(argv[i], "--version") || !strcmp(argv[i], "-v"))
	{
	  printf ("Gucushell 0.3\n");
	  printf ("Copyright (c) 2008,2009,2010 Michael L. Gran\n");
	  printf ("This may be freely distributed\n");
	  printf ("For details, see the file COPYING, included in the distribution\n");
	  return;
	}
      if (!strcmp(argv[i], "--help") || !strcmp(argv[i], "-h"))
	{
	  scm_shell (argc, argv);
	}
    }


  /* Create a pseudo-terminal */
  if (openpty (&master, &slave, name, &tio, &win) >= 0)
    {
      /* Connect the pseudo-terminal to an xterm */
      if ((pid = open_terminal (name, master, slave)))
	{
	  slave_read = dup (slave);
	  slave_write = dup (slave);
	  close (slave);
	  fp_slave_read = fdopen (slave_read, "r");
	  fp_slave_write = fdopen (slave_write, "w");


	  /* Need to wait for xterm to be ready before trying to initalize curses */
	  for (i = 0; i < 5; i++)
	    {
	      /* Connect curses to xterm */
	      screen = newterm ("xterm", fp_slave_write, fp_slave_read);
	      if (screen == NULL)
		{
		  printf ("Waiting for xterm...\n");
		  sleep (1);
		}
	      else
		break;
	    }
	  if (screen == NULL)
	    {
	      fprintf (stderr, "gucushell: couldn't initialize ncurses on the xterm\n");
	      exit (EXIT_FAILURE);
	    }
	  else
	    printf ("Initialized curses on xterm\n");
	  erase ();
	  refresh ();

	  scm_c_eval_string ("(set! %load-path (append %load-path (list \".\")))");
	  /* Call the curses libraries */
          printf ("Loading (gucu curses)\n");
          scm_c_eval_string ("(use-modules (gucu curses))");
	  /* scm_c_eval_string ("(define stdscr (stdscr))"); */

	  scm_shell (argc, argv);

	  if (!isendwin ())
	    endwin ();
	  delscreen (screen);
	  fclose (fp_slave_read);
	  fclose (fp_slave_write);
	  close (slave_write);
	  close (slave_read);

	  kill (pid, SIGTERM);
	}
    }
}

int
main (int argc, char **argv)
{
  scm_boot_guile (argc, argv, inner_main, 0);
  return 0;
}


