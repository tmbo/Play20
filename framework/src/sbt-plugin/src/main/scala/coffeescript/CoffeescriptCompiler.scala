package play.core.coffeescript

import java.io._
import play.api._
import scala.sys.process._

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
    try {
      val preprocessorOutput = CoffeescriptPreprocessor.process( source )
      val pipeSource = new ByteArrayInputStream(preprocessorOutput.getBytes())
      "coffee -scb" #< pipeSource !! logger
    } catch {
      case e: java.lang.RuntimeException => {
        val error = logger.error match {
          case x if (x.size > 0) => x.last
          case _ => e.toString
        }
        val line = """.*on line ([0-9]+).*""".r

        throw error match {
          case msg @ line(l) => CompilationException(
            msg,
            source,
            Some(Integer.parseInt(l)))
          case msg => CompilationException(
            msg,
            source,
            None)
        }
      }
    }
  }
}

case class CompilationException(message: String, coffeeFile: File, atLine: Option[Int]) extends PlayException(
  "Compilation error", message) with PlayException.ExceptionSource {
  def line = atLine

  def position = None

  def input = Some(scalax.file.Path(coffeeFile))

  def sourceName = Some(coffeeFile.getAbsolutePath)
}
