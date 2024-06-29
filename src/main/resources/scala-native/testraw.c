#include "testraw.h"

void enableRawMode() {
  struct termios raw;
  tcgetattr(STDIN_FILENO, &raw);
  printf("%x\n", raw.c_lflag);
  raw.c_lflag &= ~(ECHO);
  printf("%x\n", raw.c_lflag);
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}

