scalaVersion := "3.4.2" // A Long Term Support version.
// ThisBuild / usePipelining := true

enablePlugins(ScalaNativePlugin)

// set to Debug for compilation details (Info is default)
logLevel := Level.Info

// import to add Scala Native options
import scala.scalanative.build._

// defaults set with common options shown
nativeConfig ~= { c =>
  c.withLTO(LTO.none) // thin
    .withMode(Mode.debug) // releaseFast
    .withGC(GC.immix) // commix
    .withMultithreading(true)
}

ThisBuild / libraryDependencies += "org.typelevel" %%% "cats-core" % "2.12.0"
ThisBuild / libraryDependencies += "ch.epfl.lamp" %%% "gears" % "0.2.0"

ThisBuild / libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0" % Test
