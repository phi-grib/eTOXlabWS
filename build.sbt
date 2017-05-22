name := "envoy_ws"

version := "1.0-SNAPSHOT"

libraryDependencies ++= Seq(
  jdbc,
  anorm,
  cache,
  "com.github.OriolLopezMassaguer" % "dataframe_2.10" % "1.2.1" classifier "assembly"
)     

play.Project.playScalaSettings
