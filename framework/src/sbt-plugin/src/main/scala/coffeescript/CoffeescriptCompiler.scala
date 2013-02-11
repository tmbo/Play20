package play.core.coffeescript

import java.io._
import play.api._
import sbt.PlayExceptions.AssetCompilationException

object CoffeescriptCompiler {

  import scala.collection.JavaConverters._
  import scalax.file._

  private def executeNativeCompiler(in: String, source: File, lineMapping: Map[Int, Int]): String = {
    import scala.sys.process._
    val qb = Process(in)
    var out = List[String]()
    var err = List[String]()
    val exit = qb ! ProcessLogger((s) => out ::= s, (s) => err ::= s)
    if (exit != 0) {
      val eRegex = """.*Parse error on line (\d+):.*""".r
      val errReverse = err.reverse
      val r = eRegex.unapplySeq(errReverse.mkString("")).map(_.head.toInt)
      throw AssetCompilationException(Some(source), errReverse.mkString("\n"), r.flatMap(l => lineMapping.get( l-1 )), None)
    }
    out.reverse.mkString("\n")
  }

  def createTempFile(data: String) = {
    val temp = File.createTempFile("temp", System.nanoTime().toString + ".coffee")
    val out = new PrintWriter( temp )
    try{ out.print( data ) }
    finally{ out.close }
    temp
  }

  def compile(source: File, options: Seq[String]): String = {
    try {
      val preproResult = CoffeescriptPreprocessor.process( source )
      val preprocessedFile = createTempFile(preproResult._1)
      val lineMapping = preproResult._2
      executeNativeCompiler(options.last + " " + preprocessedFile.getAbsolutePath, source, lineMapping)
    } catch {
      case e: CoffeescriptPreprocessorException => {
        throw AssetCompilationException(
            Some(source),
            "Coffeescript Preprocessor Error: " + e.error,
            Some(e.line),
            None)
      }
    }
  }

}
