#include "rawmode.h"

void disableRawMode(struct termios *origTermios) {
  tcsetattr(STDIN_FILENO, TCSAFLUSH, origTermios);
}

void enableRawMode(struct termios *orig_termios) {
  tcgetattr(STDIN_FILENO, orig_termios);
  struct termios raw;
  memcpy(&raw, orig_termios, sizeof(struct termios));
  raw.c_lflag &= ~(ECHO);
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}
