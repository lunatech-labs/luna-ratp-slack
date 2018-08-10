name := "lunatech-ratp-bot"
 
version := "1.0" 
      
lazy val `lunatechratpbot` = (project in file(".")).enablePlugins(PlayScala)

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
      
resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"
      
scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  ehcache,
  ws,
  jdbc,
  "com.lunatech" % "scala-slack-client" % "0.2.3",
  "org.postgresql" % "postgresql" % "9.4.1209",
  "com.h2database" % "h2" % "1.4.+",
  "org.flywaydb" %% "flyway-play" % "4.0.+",
  "com.typesafe.play" %% "play-slick" % "3.0.3",
  "com.github.tminglei" %% "slick-pg" % "0.16.3",
  "com.github.tminglei" %% "slick-pg_play-json" % "0.16.3",
  "javax.xml.bind" % "jaxb-api" % "2.1",
  specs2 % Test,
  guice)

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )