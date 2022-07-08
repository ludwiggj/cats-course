name := "scala-cats-course-rock-the-jvm"

version := "0.1"

scalaVersion := "2.13.3"

val catsVersion = "2.8.0"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
)

scalacOptions ++= Seq(
  "-language:higherKinds"
)