//import de.heikoseeberger.sbtheader.HeaderPattern
import de.heikoseeberger.sbtheader.HeaderPlugin.autoImport._
import de.heikoseeberger.sbtheader.License._
import sbt.Keys._
import sbt._
import sbtorgpolicies._
import sbtorgpolicies.model._
import sbtorgpolicies.OrgPoliciesPlugin.autoImport._

object ProjectPlugin extends AutoPlugin {

  override def trigger: PluginTrigger = allRequirements

  override def requires: Plugins = plugins.JvmPlugin && OrgPoliciesPlugin

  object autoImport {

    lazy val V = new {
      val scala212: String = "2.12.10"
    }
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      description := "Scala Exercises: The path to enlightenment",
      startYear := Option(2016),
      orgGithubSetting := GitHubSettings(
        organization = "scala-exercises",
        project = name.value,
        organizationName = "Scala Exercises",
        groupId = "org.scala-exercises",
        organizationHomePage = url("https://www.scala-exercises.org"),
        organizationEmail = "hello@47deg.com"
      ),
      orgLicenseSetting := ApacheLicense,
      scalaVersion := V.scala212,
      scalaOrganization := "org.scala-lang",
      resolvers ++= Seq(
        Resolver.mavenLocal,
        Resolver.sonatypeRepo("snapshots"),
        Resolver.sonatypeRepo("releases")
      ),
      scalacOptions := sbtorgpolicies.model.scalacCommonOptions,
      headerLicense := Some(Custom(s"""| scala-exercises - ${name.value}
                                       | Copyright (C) 2015-2016 47 Degrees, LLC. <http://www.47deg.com>
                                       |
                                       |""".stripMargin))
    )
}
