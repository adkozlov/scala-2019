name := "scala-2019"

version := "0.1"

scalaVersion := "2.13.1"

// https://mvnrepository.com/artifact/org.squeryl/squeryl
libraryDependencies += "org.squeryl" %% "squeryl" % "0.9.14"
// https://mvnrepository.com/artifact/com.h2database/h2
libraryDependencies += "com.h2database" % "h2" % "1.4.200" % Test  // TODO remove dependency?

// https://mvnrepository.com/artifact/net.sourceforge.csvjdbc/csvjdbc
libraryDependencies += "net.sourceforge.csvjdbc" % "csvjdbc" % "1.0.28"

// https://mvnrepository.com/artifact/com.typesafe.slick/slick
libraryDependencies ++= Seq(
  "com.typesafe.slick" %% "slick" % "3.3.2",
  "org.slf4j" % "slf4j-nop" % "1.7.26"
)
// https://mvnrepository.com/artifact/com.typesafe.slick/slick
//libraryDependencies += "com.typesafe.slick" %% "slick" % "3.3.2"
//"org.slf4j" % "slf4j-nop" % "1.7.26",
// https://mvnrepository.com/artifact/org.xerial/sqlite-jdbc
libraryDependencies += "org.xerial" % "sqlite-jdbc" % "3.28.0"
