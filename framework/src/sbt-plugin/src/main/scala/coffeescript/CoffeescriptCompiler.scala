package play.core.coffeescript

import java.io._
import play.api._
import scala.sys.process._
import sbt.PlayExceptions.AssetCompilationException

case class ExecLogger(var messages: List[String] = Nil,
  var error: List[String] = Nil)
  extends ProcessLogger {
  def out(s: => String) {
    messages ::= s
  }

  def err(s: => String) {
    error ::= s
  }

  def buffer[T](f: => T): T = f
}

object CoffeescriptCompiler {

  import scala.collection.JavaConverters._

  import scalax.file._

  def compile(source: File, options: Seq[String]): String = {
    val logger = new ExecLogger
    var lineMapping = Map[Int, Int]()
    try {
      val preproResult = CoffeescriptPreprocessor.process( source )
      val preprocessorOutput = preproResult._1
      lineMapping = preproResult._2
      val pipeSource = new ByteArrayInputStream(preprocessorOutput.getBytes())
      "coffee -scb" #< pipeSource !! logger
    } catch {
      case e: CoffeescriptPreprocessorException => {
        throw AssetCompilationException(
            Some(source),
            "Coffeescript Preprocessor Error: " + e.error,
            Some(e.line),
            None)
      }
      case e: java.lang.RuntimeException => {
        val error = logger.error match {
          case x if (x.size > 0) => x.last
          case _ => e.toString
        }
        val line = """.*on line ([0-9]+).*""".r

        throw error match {
          case msg @ line(sl) => 
            val l = Integer.parseInt(sl)
            AssetCompilationException(
              Some(source),
              msg,
              lineMapping.get( l-1 ) orElse ( Some( l )),
              None)
          case msg => AssetCompilationException(
            Some(source),
            msg,
            None,
            None)
        }
      }
    }
  }

}
