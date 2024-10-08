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
  c.withLTO(LTO.full) // thin
    .withMode(Mode.releaseSize) // releaseFast
    .withGC(GC.boehm) // commix
    .withMultithreading(true)
    .withIncrementalCompilation(true)
    .withCompileOptions(
      List(
        "-DGC_NPROCS=8",
        "-DGC_MAXIMUM_HEAP_SIZE=20 * 1024 * 1024",
        "-DGC_LOG_FILE=/tmp/gc.log",
        "-DGC_ONLY_LOG_TO_FILE",
        "-DGC_PRINT_STATS"
      )
    )
}

ThisBuild / scalacOptions ++= List(
  "-Ykind-projector",
  "-language:implicitConversions,higherKinds,existentials",
  "-feature",
  "-Wshadow:type-parameter-shadow",
  "-Wunused:all"
)
ThisBuild / libraryDependencies += "org.typelevel" %%% "cats-core" % "2.12.0"
ThisBuild / libraryDependencies += "com.lihaoyi" %%% "os-lib" % "0.10.2"
ThisBuild / libraryDependencies += "io.github.cquiroz" %%% "scala-java-time" % "2.6.0"
ThisBuild / libraryDependencies += "org.typelevel" %%% "cats-mtl" % "1.5.0"
ThisBuild / libraryDependencies += "org.scalameta" %%% "munit" % "1.0.0" % Test
