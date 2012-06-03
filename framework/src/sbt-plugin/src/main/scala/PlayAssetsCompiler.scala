package sbt

import Keys._
import PlayKeys._
import PlayExceptions._

// ----- Assets
trait PlayAssetsCompiler {

  // Name: name of the compiler
  // files: the function to find files to compile from the assets directory
  // naming: how to name the generated file from the original file and whether it should be minified or not
  // compile: compile the file and return the compiled sources, the minified source (if relevant) and the list of dependencies
  def AssetsCompiler(name: String,
    watch: File => PathFinder,
    filesSetting: sbt.SettingKey[PathFinder],
    naming: (String, Boolean) => String,
    compile: (File, Seq[String]) => (String, Option[String], Seq[File]),
    optionsSettings: sbt.SettingKey[Seq[String]]) =
    (sourceDirectory in Compile, resourceManaged in Compile, cacheDirectory, optionsSettings, filesSetting, incrementalAssetsCompilation) map { (src, resources, cache, options, files, incrementalAssetsCompilation) =>
 
      import java.io._
 
      val cacheFile = cache / name
      val currentInfos = watch(src).get.map(f => f -> FileInfo.lastModified(f)).toMap
      
      val (previousRelation, previousInfo) = Sync.readInfo(cacheFile)(FileInfo.lastModified.format)
       
 
      if (previousInfo != currentInfos) {
 
        //a changed file can be either a new file, a deleted file or a modified one
        lazy val changedFiles: Seq[File] = currentInfos.filter(e=> !previousInfo.get(e._1).isDefined || previousInfo(e._1).lastModified < e._2.lastModified).map(_._1).toSeq ++ previousInfo.filter(e=> !currentInfos.get(e._1).isDefined).map(_._1).toSeq
       
        previousRelation.filter((original,compiled)=> !incrementalAssetsCompilation || changedFiles.contains(original))._2s.foreach(IO.delete)
 
        val t = System.currentTimeMillis()
        val generated: Seq[(File, java.io.File)] = 
        (files x relativeTo(Seq(src / "assets"))).flatMap {
          case (sourceFile, name) => {
              if (!incrementalAssetsCompilation || changedFiles.contains(sourceFile)) {
                val (debug, min, dependencies) = compile(sourceFile, options)
                val out = new File(resources, "public/" + naming(name, false))
                val outMin = new File(resources, "public/" + naming(name, true))
                IO.write(out, debug)
                dependencies.map(_ -> out) ++ min.map { minified =>
                  IO.write(outMin, minified)
                  dependencies.map(_ -> outMin)
                }.getOrElse(Nil)
              } else {
                previousRelation.filter((original,compiled)=> original == sourceFile)._2s.map(sourceFile ->_)
              }
          }
        }
        //write object graph to cache file 
        Sync.writeInfo(cacheFile,
           Relation.empty[File, File] ++ generated,
           currentInfos)(FileInfo.lastModified.format)
 
        println("Finished compiling %s sources: %dms".format(name,System.currentTimeMillis-t))

        // Return new files
        generated.map(_._2).distinct.toList
 
      } else {
 
        // Return previously generated files
        previousRelation._2s.toSeq
 
      }
 
    }

  val LessCompiler = AssetsCompiler("less",
    (_ ** "*.less"),
    lessEntryPoints,
    { (name, min) => name.replace(".less", if (min) ".min.css" else ".css") },
    { (lessFile, options) => play.core.less.LessCompiler.compile(lessFile, options) },
    lessOptions
  )
 
  def JavascriptCompiler(fullCompilerOptions: Option[com.google.javascript.jscomp.CompilerOptions]) = AssetsCompiler("javascripts",
    (_ ** "*.js"),
    javascriptEntryPoints,
    { (name, min) => name.replace(".js", if (min) ".min.js" else ".js") },
    { (jsFile: File, simpleCompilerOptions) => play.core.jscompile.JavascriptCompiler.compile(jsFile, simpleCompilerOptions, fullCompilerOptions) },
    closureCompilerOptions
  )
 
  val CoffeescriptCompiler = AssetsCompiler("coffeescript",
    (_ ** "*.coffee"),
    coffeescriptEntryPoints,
    { (name, min) => name.replace(".coffee", if (min) ".min.js" else ".js") },
    { (coffeeFile, options) =>
      import scala.util.control.Exception._
      val jsSource = play.core.coffeescript.CoffeescriptCompiler.compile(coffeeFile, options)
      // Any error here would be because of CoffeeScript, not the developer;
      // so we don't want compilation to fail.
      val minified = 
        if(options.contains("minify"))
        catching(classOf[CompilationException])
          .opt(play.core.jscompile.JavascriptCompiler.minify(jsSource, Some(coffeeFile.getName())))
      else
        None
      (jsSource, minified, Seq(coffeeFile))
    },
    coffeescriptOptions
  )

}