#include "rawmode.h"

int disableRawMode(struct termios *origTermios) {
  return tcsetattr(STDIN_FILENO, TCSAFLUSH, origTermios);
}

int enableRawMode(struct termios *orig_termios) {
  int getattr_result;
  getattr_result = tcgetattr(STDIN_FILENO, orig_termios);
  if (getattr_result == -1) {
    return getattr_result;
  }
  perror("");
  struct termios raw;
  memcpy(&raw, orig_termios, sizeof(struct termios));
  raw.c_iflag &= ~(IXON | ICRNL | BRKINT | INPCK | ISTRIP);
  raw.c_oflag &= ~(OPOST);
  raw.c_cflag |= (CS8);
  raw.c_lflag &= ~(ECHO | ICANON | ISIG | IEXTEN);
  raw.c_cc[VMIN] = 0;
  raw.c_cc[VTIME] = 1;
  return tcsetattr(STDIN_FILENO, TCSAFLUSH, &raw);
}
