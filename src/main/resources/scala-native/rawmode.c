#include "rawmode.h"
#include <termios.h>
#include <unistd.h>

void disableRawMode(struct termios origTermios) {
  tcsetattr(STDIN_FILENO, TCSAFLUSH,  &origTermios);
}

void enableRawMode(struct termios raw) {
  tcgetattr(STDIN_FILENO, &raw);
  raw.c_lflag &= ~(ECHO);
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}

