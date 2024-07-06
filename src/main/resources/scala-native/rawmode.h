#include <stdio.h>
#include <string.h>
#include <sys/ioctl.h>
#include <termios.h>
#include <unistd.h>

typedef struct winsize winsize;

int get_TIOCGWINSZ() { return TIOCGWINSZ; }
int disableRawMode(struct termios *origTermios);
int enableRawMode(struct termios *orig_termios);
