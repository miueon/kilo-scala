#include "rawmode.h"
#include <termios.h>

void disableRawMode(struct termios *origTermios) {
  tcsetattr(STDIN_FILENO, TCSAFLUSH, origTermios);
}

void enableRawMode(struct termios *orig_termios) {
  tcgetattr(STDIN_FILENO, orig_termios);
  struct termios raw;
  memcpy(&raw, orig_termios, sizeof(struct termios));
  raw.c_iflag &= ~(IXON | ICRNL | BRKINT | INPCK | ISTRIP);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1;
  tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}
