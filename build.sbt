name := "hello-scalatest-scala"

version := "0.3"

scalaVersion := "3.3.3"

scalacOptions += "@.scalacOptions.txt"

coverageEnabled := true

libraryDependencies ++= Seq(
  "org.scalatest"  %% "scalatest"  % "3.2.19"  % Test,
  "org.scalacheck" %% "scalacheck" % "1.18.0"  % Test,
  "com.lihaoyi"    %% "mainargs"   % "0.4.0"   // Make sure the version is correct
)
testOptions in Test += Tests.Argument("-oDF")


enablePlugins(JavaAppPackaging)
