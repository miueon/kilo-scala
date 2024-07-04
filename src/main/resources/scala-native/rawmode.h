#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <string.h>

int disableRawMode(struct termios *origTermios);
int enableRawMode(struct termios *orig_termios);
