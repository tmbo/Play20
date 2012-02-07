package play.core.less

import java.io._
import play.api._
import scala.sys.process._

case class ExecLogger(var messages: List[String] = Nil,
  var error: List[String] = Nil)
  extends ProcessLogger {
  val stripColorTags = "\\[[0-9][0-9]?m".r
  def out(s: => String) {
    messages ::= s
  }

  def err(s: => String) {
    error ::= stripColorTags.replaceAllIn(s,"")
  }

  def buffer[T](f: => T): T = f
}

object LessCompiler {

  import scala.collection.JavaConverters._

  import scalax.file._

  def compile(source: File, coptions: Seq[String]) = {
    val logger = new ExecLogger();
    val minified = coptions.contains("minify");
    val options = if(minified) "-x" else "";
    try {
      val process = Process("lessc "+options+" -",Some(source.getParentFile)) #< source
      val normal = process !! logger
      (normal, None, Seq(source))
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
            Some(Integer.parseInt(l)),
            None)
          case msg => CompilationException(
            msg,
            source,
            None,
            None)
        }
      }
    }
  }
}

case class CompilationException(message: String, lessFile: File, atLine: Option[Int], atColumn: Option[Int]) extends PlayException(
  "Compilation error", message) with PlayException.ExceptionSource {
  def line = atLine
  def position = atColumn
  def input = Some(scalax.file.Path(lessFile))
  def sourceName = Some(lessFile.getAbsolutePath)
}

