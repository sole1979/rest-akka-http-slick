ThisBuild / scalaVersion     := "2.12.18"

val akkaVersion = "2.6.20"//"2.5.20"
val akkaHttpVersion = "10.2.10"//"10.1.7"
val scalaTestVersion = "3.0.5"

lazy val root = (project in file("."))
  .settings(
    name := "rest-akka-http-slick",
    version := "0.1.0",
    libraryDependencies ++= Seq(
      "com.typesafe.akka" %% "akka-http" % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-stream" % akkaVersion,
      "com.typesafe.akka" %% "akka-http-spray-json" % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-http-testkit" % akkaHttpVersion,
      "com.typesafe.akka" %% "akka-testkit" % akkaVersion,
      "org.scalatest" %% "scalatest" % scalaTestVersion,
      "com.typesafe.slick" %% "slick" % "3.3.3",
      "org.postgresql" % "postgresql" % "42.3.4",
      "com.typesafe.slick" %% "slick-hikaricp" % "3.3.3",
      "org.slf4j" % "slf4j-api" % "1.7.36",
      "ch.qos.logback" % "logback-classic" % "1.2.11",
      "de.mkammerer" % "argon2-jvm" % "2.11",
      //JWT
      "com.pauldijou" %% "jwt-spray-json" % "2.1.0"
    //  "com.github.jwt-scala" %% "jwt-spray-json" % "9.1.2"  //10.0.3
    )
  )

