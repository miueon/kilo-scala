package par

import domain.Key

enum Event:
  case Key(key: domain.Key) extends Event
  case WindowResize extends Event
  case Quit extends Event