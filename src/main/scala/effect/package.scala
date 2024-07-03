package effect

import gears.async.Async

type Nothing1[A] = Nothing
infix type ~>[F[_], G[_]] = [x] => F[x] => G[x]

final class Id

