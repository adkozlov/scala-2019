name := "scala-2019"

version := "0.1"

scalaVersion := "2.13.1"

// https://mvnrepository.com/artifact/com.typesafe.slick/slick
libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "3.3.2",
  "org.slf4j" % "slf4j-nop" % "1.7.26"
)

libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.28.0"
