import sbt._, Keys._, Path._

object ProjectDefinition extends Build {
  lazy val root = Project("Serialization", file(".")) settings(publishSettings ++ Seq(
      libraryDependencies ++= Seq(
        "org.scalatest" %% "scalatest" % "1.7.1" % "test",
        "com.googlecode.json-simple" % "json-simple" % "1.1" % "optional"
      ),
      fork in test := true,
      publishArtifact in (Compile, packageDoc) := false
    ) :_*)

  lazy val publishSettings = Seq[Project.Setting[_]](
    version := "0.1-SNAPSHOT",
    publishMavenStyle := true,
    publishTo <<= (version) {
      version: String =>
      val cloudbees = "https://repository-belfry.forge.cloudbees.com/"
      if (version.trim.endsWith("SNAPSHOT")) Some("snapshot" at cloudbees + "snapshot/") 
      else                                   Some("release"  at cloudbees + "release/")
    },
    credentials += {
        val credsFile = (Path.userHome / ".credentials")
        (if (credsFile.exists) Credentials(credsFile)
        else Credentials(file("/private/belfry/.credentials/.credentials")))
    }
  )
}