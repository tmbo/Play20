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
    var preprocessorFile: Option[File] = None
    try {
      val preprocessorOutput = CoffeescriptPreprocessor.process( source )
      preprocessorFile = Some(File.createTempFile("prepoutput", ".coffee"))
      val out = new PrintWriter( preprocessorFile.get )
      try{ 
        out.print( preprocessorOutput ) 
      } finally { out.close }
      val pipeSource = new ByteArrayInputStream(preprocessorOutput.getBytes())
      "coffee -scb" #< pipeSource !! logger
    } catch {
      case e: CoffeescriptPreprocessorException => {
        throw CompilationException(
            "Coffeescript Preprocessor Error: " + e.error,
            source,
            source,
            Some(e.line))
      }
      case e: java.lang.RuntimeException => {
        val error = logger.error match {
          case x if (x.size > 0) => x.last
          case _ => e.toString
        }
        val line = """.*on line ([0-9]+).*""".r

        throw error match {
          case msg @ line(l) => CompilationException(
            msg,
            preprocessorFile getOrElse source,
            source,
            Some(Integer.parseInt(l)))
          case msg => CompilationException(
            msg,
            preprocessorFile getOrElse source,
            source,
            None)
        }
      }
    }
  }
}

case class CompilationException(message: String, preproOutput: File, coffeeFile: File, atLine: Option[Int]) extends PlayException(
  "Compilation error", message) with PlayException.ExceptionSource {
  def line = atLine

  def position = None

  def input = Some(scalax.file.Path(preproOutput))

  def sourceName = Some(coffeeFile.getAbsolutePath)
  
  override def sourceType = Some( "coffee" )
}
