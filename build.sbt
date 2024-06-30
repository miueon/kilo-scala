scalaVersion := "3.4.2" // A Long Term Support version.
// ThisBuild / usePipelining := true

enablePlugins(ScalaNativePlugin, BindgenPlugin)
import bindgen.interface.Binding

// set to Debug for compilation details (Info is default)
logLevel := Level.Info

bindgenBindings := Seq(
  Binding
    .apply(
      baseDirectory.value / "src" / "main" / "resources" / "scala-native" / "rawmode.h",
      "rawmode"
    )
    .withCImports(
      List(
        "rawmode.h"
      )
    )
)

// import to add Scala Native options
import scala.scalanative.build.*

// defaults set with common options shown
nativeConfig ~= { c =>
  c.withLTO(LTO.none) // thin
    .withMode(Mode.debug) // releaseFast
    .withGC(GC.immix) // commix
    .withMultithreading(true)
  // .withIncrementalCompilation(true)
  // .withTargetTriple("i686-linux-gnu")
}

ThisBuild / scalacOptions ++= List(
  "-Ykind-projector",
  "-Ypartial-unification",
  "-Xfuture",
  "-language:implicitConversions,higherKinds,existentials",
  "-feature",
  "-Xlint:type-parameter-shadow"
)
ThisBuild / libraryDependencies += "org.typelevel" %%% "cats-core" % "2.12.0"
ThisBuild / libraryDependencies += "ch.epfl.lamp" %%% "gears" % "0.2.0"
ThisBuild / libraryDependencies += "org.typelevel" %%% "alleycats-core" % "2.12.0"
ThisBuild / libraryDependencies += "org.atnos" %%% "eff" % "7.0.4"
ThisBuild / libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0" % Test
