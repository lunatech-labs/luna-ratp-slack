name := "lunatech-ratp-bot"
 
version := "1.0" 
      
lazy val `lunatechratpbot` = (project in file(".")).enablePlugins(PlayScala)

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
      
resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"
      
scalaVersion := "2.12.2"

libraryDependencies ++= Seq(
  ehcache,
  ws,
  "com.lunatech" % "scala-slack-client" % "0.2.1",
  "com.typesafe.play" %% "play-slick" % "3.0.1",
  "com.typesafe.play" %% "play-slick-evolutions" % "3.0.1",
  "com.h2database" % "h2" % "1.4.197",
  evolutions,
  specs2 % Test,
  guice)

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )