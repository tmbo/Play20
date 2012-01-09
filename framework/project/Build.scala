import sbt._
import Keys._

object PlayBuild extends Build {

    import Resolvers._
    import Dependencies._
    import BuildSettings._
    import Generators._
    import LocalSBT._
    import Tasks._
    
    val typesafeRepo = "http://repo.typesafe.com/typesafe/releases/"

    lazy val TemplatesProject = Project(
        "Templates",
        file("src/templates"),
        settings = buildSettings ++ Seq(
            libraryDependencies := templatesDependencies,
            publishMavenStyle := false,
            publishTo := Some(playRepository),
            publishArtifact in (Compile, packageDoc) := false,
            publishArtifact in (Compile, packageSrc) := false,
            scalacOptions ++= Seq("-Xlint","-deprecation", "-unchecked","-encoding", "utf8"),
            resolvers ++= Seq(DefaultMavenRepository, typesafe)
        )
    ).settings(com.typesafe.sbtscalariform.ScalariformPlugin.defaultScalariformSettings: _*)

    lazy val AnormProject = Project(
        "Anorm",
        file("src/anorm"),
        settings = buildSettings ++ Seq(
            libraryDependencies := anormDependencies,
            publishMavenStyle := false,
            publishTo := Some(playRepository),
            scalacOptions ++= Seq("-encoding", "utf8"),
            publishArtifact in (Compile, packageDoc) := false,
            publishArtifact in (Compile, packageSrc) := false
        )
    ).settings(com.typesafe.sbtscalariform.ScalariformPlugin.defaultScalariformSettings: _*)

    lazy val PlayProject = Project(
        "Play",
        file("src/play"),
        settings = buildSettings ++ Seq(
            libraryDependencies := runtime,
            sourceGenerators in Compile <+= sourceManaged in Compile map PlayVersion,
            publishMavenStyle := false,
            publishTo := Some(playRepository),
            scalacOptions ++= Seq("-Xlint","-deprecation", "-unchecked","-encoding", "utf8"),
            publishArtifact in (Compile, packageDoc) := false,
            publishArtifact in (Compile, packageSrc) := false,
            resolvers ++= Seq(DefaultMavenRepository, typesafe),
            sourceGenerators in Compile <+= (dependencyClasspath in TemplatesProject in Runtime, packageBin in TemplatesProject in Compile, scalaSource in Compile, sourceManaged in Compile, streams) map ScalaTemplates,
            compile in (Compile) <<= PostCompile
        )
    ).settings(com.typesafe.sbtscalariform.ScalariformPlugin.defaultScalariformSettings: _*).dependsOn(TemplatesProject, AnormProject)
    
    lazy val PlayTestProject = Project(
      "Play-Test",
      file("src/play-test"),
      settings = buildSettings ++ Seq(
        libraryDependencies := testDependencies,
        publishMavenStyle := false,
        publishTo := Some(playRepository),
        scalacOptions ++= Seq("-deprecation","-Xcheckinit", "-encoding", "utf8"),
        publishArtifact in (Compile, packageDoc) := false,
        publishArtifact in (Compile, packageSrc) := false,
        resolvers ++= Seq(DefaultMavenRepository, typesafe)
      )
    ).settings(com.typesafe.sbtscalariform.ScalariformPlugin.defaultScalariformSettings: _*).dependsOn(PlayProject)

    lazy val SbtPluginProject = Project(
      "SBT-Plugin",
      file("src/sbt-plugin"),
      settings = buildSettings ++ Seq(
        sbtPlugin := true,
        libraryDependencies := sbtDependencies,
        addSbtPlugin("com.typesafe.sbteclipse" % "sbteclipse-core" % "2.0.0-M3"),
        unmanagedJars in Compile  ++=  sbtJars,
        publishMavenStyle := false,
        publishTo := Some(playRepository),
        scalacOptions ++= Seq("-Xlint", "-deprecation", "-unchecked","-encoding", "utf8"),
        publishArtifact in (Compile, packageDoc) := false,
        publishArtifact in (Compile, packageSrc) := false,
        resolvers ++= Seq(DefaultMavenRepository, typesafe),
        projectDependencies := Seq(
          "play" %% "play" % buildVersion notTransitive(),
          "play" %% "console" % buildVersion notTransitive(),
          "play" %% "templates" % buildVersion notTransitive()
        ) 
      )
    ).settings(com.typesafe.sbtscalariform.ScalariformPlugin.defaultScalariformSettings: _*).dependsOn(PlayProject, TemplatesProject, ConsoleProject)

    lazy val ConsoleProject = Project(
      "Console",
      file("src/console"),
      settings = buildSettings ++ Seq(
        libraryDependencies := consoleDependencies,
        unmanagedJars in Compile  ++=  sbtJars,
        publishMavenStyle := false,
        publishTo := Some(playRepository),
        scalacOptions ++= Seq("-deprecation","-Xcheckinit", "-encoding", "utf8"),
        publishArtifact in (Compile, packageDoc) := false,
        publishArtifact in (Compile, packageSrc) := false,
        resolvers ++= Seq(DefaultMavenRepository, typesafe),
        projectDependencies := Seq("play" %% "play" % buildVersion notTransitive()) 
      )
    ).settings(com.typesafe.sbtscalariform.ScalariformPlugin.defaultScalariformSettings: _*).dependsOn(PlayProject)

    val Root = Project(
        "Root",
        file("."),
        settings = buildSettings ++ Seq(
            cleanFiles ++= Seq(file("../dist"), file("../repository/local")),
            resetRepositoryTask,
            buildRepositoryTask,
            distTask,
            generateAPIDocsTask,
            publish <<= (publish in PlayProject, publish in TemplatesProject, publish in AnormProject, publish in SbtPluginProject, publish in ConsoleProject, publish in PlayTestProject) map { (_,_,_,_,_,_) => },
            publishLocal <<= (publishLocal in PlayProject, publishLocal in TemplatesProject, publishLocal in AnormProject, publishLocal in SbtPluginProject, publishLocal in ConsoleProject, publishLocal in PlayTestProject) map { (_,_,_,_,_,_) => }
        )
    ).settings(com.typesafe.sbtscalariform.ScalariformPlugin.defaultScalariformSettings: _*)
     .dependsOn(PlayProject).aggregate(AnormProject, TemplatesProject, PlayProject, SbtPluginProject, ConsoleProject, PlayTestProject)

    object BuildSettings {

        val buildOrganization = "play"
        val buildVersion      = "2.0-RC1-SNAPSHOT"
        val buildScalaVersion = "2.9.1"
        val buildSbtVersion   = "0.11.2"

        val buildSettings = Defaults.defaultSettings ++ Seq (
            organization   := buildOrganization,
            version        := buildVersion,
            scalaVersion   := buildScalaVersion
        )

    }

    object LocalSBT {

        import BuildSettings._

        def isJar(f:java.io.File) = f.getName.endsWith(".jar")

        val sbtJars:Seq[java.io.File] = {
            file("sbt/boot/scala-" + buildScalaVersion + "/org.scala-tools.sbt/sbt/" + buildSbtVersion).listFiles.filter(isJar) ++
            file("sbt/boot/scala-" + buildScalaVersion + "/org.scala-tools.sbt/sbt/" + buildSbtVersion + "/xsbti").listFiles.filter(isJar) ++
            Seq(file("sbt/boot/scala-" + buildScalaVersion + "/lib/jline.jar"))
        }

    }

    object Resolvers {
        val playLocalRepository = Resolver.file("Play Local Repository", file("../repository/local"))(Resolver.ivyStylePatterns)   
        val playRepository = Resolver.ssh("Play Repository", "download.playframework.org", "/srv/http/download.playframework.org/htdocs/ivy-releases/")(Resolver.ivyStylePatterns) as("root", new File(System.getProperty("user.home") + "/.ssh/id_rsa"), "") withPermissions("0644")
        val typesafe = "Typesafe Repository" at typesafeRepo
    }

    object Dependencies {

        val runtime = Seq(
            "org.jboss.netty"                   %    "netty"                    %   "3.2.6.Final",
            "org.slf4j"                         %    "slf4j-api"                %   "1.6.2",
            "org.slf4j"                         %    "jul-to-slf4j"             %   "1.6.2",
            "org.slf4j"                         %    "jcl-over-slf4j"           %   "1.6.2",
            "ch.qos.logback"                    %    "logback-core"             %   "0.9.30",
            "ch.qos.logback"                    %    "logback-classic"          %   "0.9.30",
            "com.github.scala-incubator.io"     %%   "scala-io-file"            %   "0.2.0",
            "com.typesafe.akka"                 %    "akka-actor"               %   "2.0-M2",
            "com.typesafe.akka"                 %    "akka-slf4j"               %   "2.0-M2",
            "com.typesafe.config"               %%   "config"                   %   "0.1.6",
            "org.avaje"                         %    "ebean"                    %   "2.7.3" notTransitive(),
            "org.hibernate.javax.persistence"   %    "hibernate-jpa-2.0-api"    %   "1.0.1.Final",
            "com.h2database"                    %    "h2"                       %   "1.3.158",
            "org.scala-tools"                   %%   "scala-stm"                %   "0.4",
            "com.jolbox"                        %    "bonecp"                   %   "0.7.1.RELEASE",
            "org.yaml"                          %    "snakeyaml"                %   "1.9",
            "org.hibernate"                     %    "hibernate-validator"      %   "4.2.0.Final",
            "org.springframework"               %    "spring-context"           %   "3.0.7.RELEASE"   notTransitive(),
            "org.springframework"               %    "spring-core"              %   "3.0.7.RELEASE"   notTransitive(),
            "org.springframework"               %    "spring-beans"             %   "3.0.7.RELEASE"   notTransitive(),
            "joda-time"                         %    "joda-time"                %   "2.0",
            "mysql"                             %    "mysql-connector-java"     %   "5.1.17",
            "javassist"                         %    "javassist"                %   "3.12.1.GA",
            "commons-lang"                      %    "commons-lang"             %   "2.6",
            "com.ning"                          %    "async-http-client"        %   "1.6.5",
            "oauth.signpost"                    %    "signpost-core"            %   "1.2.1.1",
            "com.codahale"                      %%   "jerkson"                  %   "0.5.0",
            "org.reflections"                   %    "reflections"              %   "0.9.5",
            "javax.servlet"                     %    "javax.servlet-api"        %   "3.0.1",
            "tyrex"                             %    "tyrex"                    %   "1.0.1",
            "org.specs2"                        %%   "specs2"                   %   "1.6.1"      %  "test",
            "com.novocode"                      %    "junit-interface"          %   "0.7"        %  "test"
        )

        val sbtDependencies = Seq(            
          "rhino"                               %    "js"                       %   "1.7R2",
          "com.google.javascript"               %    "closure-compiler"         %   "r1459",           //notTransitive(),
          "com.github.scala-incubator.io"       %%   "scala-io-file"            %   "0.2.0",
          "org.avaje"                           %    "ebean"                    %   "2.7.3",
          "com.h2database"                      %    "h2"                       %   "1.3.158",
          "javassist"                           %    "javassist"                %   "3.12.1.GA"
        )

        val consoleDependencies = Seq(
          "com.github.scala-incubator.io"       %%   "scala-io-file"            %   "0.2.0"
        )

        val templatesDependencies = Seq(
            "com.github.scala-incubator.io"     %%   "scala-io-file"            %   "0.2.0",
            "org.specs2"                        %%   "specs2"                   %   "1.6.1"    %   "test",
            "org.scala-lang"                    %    "scala-compiler"           %   buildScalaVersion
        )

        val anormDependencies = Seq(
            "org.scala-lang"                    %    "scalap"                   %   buildScalaVersion 
        )
        
        val testDependencies = Seq(
            "org.specs2"                        %%   "specs2"                   %   "1.6.1",
            "com.novocode"                      %    "junit-interface"          %   "0.7",
            "fr.javafreelance.fluentlenium"     %    "fluentlenium"             %   "0.5.3"
        )

    }

    object Generators {

        val PlayVersion = { dir:File =>
            val file = dir / "PlayVersion.scala"
            IO.write(file, 
                """|package play.core
                   |
                   |object PlayVersion {
                   |    val current = "%s"
                   |}
                """.stripMargin.format(BuildSettings.buildVersion)
            )
            Seq(file)
        }

    }

    // ----- Post compile

    lazy val PostCompile = (dependencyClasspath in Compile, compile in Compile, classDirectory in Compile) map { (deps,analysis,classes) =>

        // Ebean (really hacky sorry)

        import java.net._

        val cp = deps.map(_.data.toURL).toArray :+ classes.toURL
        val cl = new URLClassLoader(cp)

        val t = cl.loadClass("com.avaje.ebean.enhance.agent.Transformer").getConstructor(classOf[Array[URL]], classOf[String]).newInstance(cp, "debug=0").asInstanceOf[AnyRef]
        val ft = cl.loadClass("com.avaje.ebean.enhance.ant.OfflineFileTransform").getConstructor(
            t.getClass, classOf[ClassLoader], classOf[String], classOf[String]
        ).newInstance(t, ClassLoader.getSystemClassLoader, classes.getAbsolutePath, classes.getAbsolutePath).asInstanceOf[AnyRef]

        ft.getClass.getDeclaredMethod("process", classOf[String]).invoke(ft,"play/db/ebean/**")

        analysis
    }


    object Tasks {

        import BuildSettings._

        // ----- Reset repo

        val resetRepository = TaskKey[File]("reset-repository")
        val resetRepositoryTask = resetRepository := {
          val repository = file("../repository/local")
          IO.createDirectory(repository)
          repository
        }

        // ----- Generate API docs

        val generateAPIDocs = TaskKey[Unit]("api-docs")
        val generateAPIDocsTask = TaskKey[Unit]("api-docs") <<= (fullClasspath in Compile, compilers, streams) map { (classpath, cs, s) => 

          IO.delete(file("../documentation/api"))

          // Scaladoc
          val sourceFiles = (file("play/src/main/scala/play/api") ** "*.scala").get ++ (file("play/src/main/scala/views") ** "*.scala").get ++ (file("play/target/scala-2.9.1/src_managed/main/views") ** "*.scala").get
          new Scaladoc(10, cs.scalac)("Play 2.0 Scala API", sourceFiles, classpath.map(_.data), file("../documentation/api/scala"), Nil, s.log)

          // Javadoc
          val javaSources = file("play/src/main/java")
          val javaApiTarget = file("../documentation/api/java")
          val javaClasspath = classpath.map(_.data).mkString(":")
          """javadoc -windowtitle playframework -doctitle Play&nbsp;2.0&nbsp;Java&nbsp;API  -sourcepath %s -d %s -subpackages play -exclude play.api:play.core -classpath %s""".format(javaSources, javaApiTarget, javaClasspath) ! s.log

        }

        // ----- Build repo

        val buildRepository = TaskKey[Unit]("build-repository")
        val buildRepositoryTask = TaskKey[Unit]("build-repository") <<= (resetRepository, update, update in test, publishLocal, scalaVersion, streams) map { (repository, updated, testUpdated, published, scalaVersion, s) =>

            def checksum(algo:String)(bytes:Array[Byte]) = {
                import java.security.MessageDigest
                val digest = MessageDigest.getInstance(algo)
                digest.reset()
                digest.update(bytes)
                digest.digest().map(0xFF & _).map { "%02x".format(_) }.foldLeft(""){_ + _}
            }

            def copyWithChecksums(files:(File,File)) {
                IO.copyFile(files._1, files._2)
                Seq("md5","sha1").foreach { algo =>
                    IO.write(file(files._2.getAbsolutePath + "." + algo), checksum(algo)(IO.readBytes(files._2)))
                }
            }

            def writeWithChecksums(f:File, content:String) {
                IO.write(f, content)
                Seq("md5","sha1").foreach { algo =>
                    IO.write(file(f.getAbsolutePath + "." + algo), checksum(algo)(content.getBytes))
                }
            }

            // Retrieve all ivy files from cache 
            // (since we cleaned the cache and run update just before, all these dependencies are useful)
            val ivyFiles = ((repository / "../cache" * "*").filter { d => 
              d.isDirectory && d.getName != "scala_%s".format(scalaVersion) 
            } ** "ivy-*.xml").get

            // From the ivy files, deduct the dependencies
            val dependencies = ivyFiles.map { descriptor =>
              val organization = descriptor.getParentFile.getParentFile.getName
              val name = descriptor.getParentFile.getName
              val version = descriptor.getName.drop(4).dropRight(4)
              descriptor -> (organization, name, version)
            }

            // Resolve artifacts for these dependencies (only jars)
            val dependenciesWithArtifacts = dependencies.map {
              case (descriptor, (organization, name, version)) => {
                var jars = (descriptor.getParentFile ** ("*-" + version + ".jar")).get
                s.log.info("Found dependency %s::%s::%s -> %s".format(
                  organization, name, version, jars.map(_.getName).mkString(", ")
                ))
                (descriptor, jars, (organization, name, version))
              }
            }

            // Build the local repository from these informations
            dependenciesWithArtifacts.foreach { 
              case (descriptor, jars, (organization, name, version)) => {
                val dependencyDir = repository / organization / name / version
                val artifacts = jars.map(j => dependencyDir / j.getParentFile.getName / (j.getName.dropRight(5 + version.size) + ".jar"))
                val ivy = dependencyDir / "ivys/ivy.xml"

                (Seq(descriptor -> ivy) ++ jars.zip(artifacts)).foreach(copyWithChecksums)
              }
            }

        }

        // ----- Dist package

        val dist = TaskKey[File]("dist")
        val distTask = dist <<= (buildRepository, publish, generateAPIDocs) map { (_,_,_) =>

            import sbt.NameFilter._

            val root = file("..")
            val packageName = "play-" + buildVersion

            val files = {
                (root ** "*") --- 
                (root ** "dist") --- 
                (root ** "dist" ** "*") --- 
                (root ** "*.log") --- 
                (root ** "logs") --- 
                (root / "repository/cache") --- 
                (root / "repository/cache" ** "*") --- 
                (root / "framework/sbt/boot") --- 
                (root / "framework/sbt/boot" ** "*") ---
                (root ** "project/project") --- 
                (root ** "target") --- 
                (root ** "target" ** "*") --- 
                (root ** ".*") ---
                (root ** ".*") ---
                (root ** ".git" ** "*") ---
                (root ** "*.lock")
            }

            val zipFile = root / "dist" / (packageName + ".zip")

            IO.delete(root / "dist")
            IO.createDirectory(root / "dist")
            IO.zip(files x rebase(root, packageName), zipFile)

            zipFile
        }

        // ----- Compile templates

        val ScalaTemplates = { (classpath:Seq[Attributed[File]], templateEngine:File, sourceDirectory:File, generatedDir:File, streams:sbt.std.TaskStreams[sbt.Project.ScopedKey[_]]) =>
            val classloader = new java.net.URLClassLoader(classpath.map(_.data.toURI.toURL).toArray, this.getClass.getClassLoader)
            val compiler = classloader.loadClass("play.templates.ScalaTemplateCompiler")
            val generatedSource = classloader.loadClass("play.templates.GeneratedSource")

            (generatedDir ** "*.template.scala").get.foreach { source =>
                val constructor = generatedSource.getDeclaredConstructor(classOf[java.io.File])
                val sync = generatedSource.getDeclaredMethod("sync")
                val generated = constructor.newInstance(source)
                try {
                    sync.invoke(generated)
                } catch {
                    case e:java.lang.reflect.InvocationTargetException =>{
                        val t = e.getTargetException
                        t.printStackTrace()
                        throw t
                    }
                }
            }

            (sourceDirectory ** "*.scala.html").get.foreach { template =>
                val compile = compiler.getDeclaredMethod("compile", classOf[java.io.File], classOf[java.io.File], classOf[java.io.File], classOf[String], classOf[String], classOf[String])
                try {
                    compile.invoke(null, template, sourceDirectory, generatedDir, "play.api.templates.Html", "play.api.templates.HtmlFormat", "import play.api.templates._\nimport play.api.templates.PlayMagic._")
                } catch {
                    case e:java.lang.reflect.InvocationTargetException => {
                        streams.log.error("Compilation failed for %s".format(template))
                        throw e.getTargetException
                    }
                }
            }

            (generatedDir ** "*.scala").get.map(_.getAbsoluteFile)
        }

    }

}
