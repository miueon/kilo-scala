import bindgen.interface.Includes
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
    .withGC(GC.boehm) // commix
    // .withMultithreading(true)
    .withIncrementalCompilation(true)
}

ThisBuild / scalacOptions ++= List(
  "-Ykind-projector",
  "-language:implicitConversions,higherKinds,existentials",
  "-feature",
  "-Xlint:type-parameter-shadow"
)
ThisBuild / libraryDependencies += "org.typelevel" %%% "cats-core" % "2.12.0"
ThisBuild / libraryDependencies += "com.lihaoyi" %%% "os-lib" % "0.10.2"
ThisBuild / libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0"
ThisBuild / libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0" % Test
