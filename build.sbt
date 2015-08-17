name := "parse-vcf"

version := "1.0"

scalaVersion := "2.11.5"

libraryDependencies ++= Seq(
  "org.scalikejdbc" %% "scalikejdbc" % "2.2.7",
  "ch.qos.logback" % "logback-classic" % "1.1.3",
  "mysql" % "mysql-connector-java" % "5.1.36"
)
