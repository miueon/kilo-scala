#include <stdio.h>
#include <termios.h>
#include <unistd.h>
#include <string.h>

void disableRawMode(struct termios *origTermios);
void enableRawMode(struct termios *orig_termios);
