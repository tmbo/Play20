package play.core.coffeescript

import java.io._
import play.api._

object CoffeescriptCompiler {

  import org.mozilla.javascript._
  import org.mozilla.javascript.tools.shell._

  import scala.collection.JavaConverters._

  import scalax.file._
  import scala.sys.process._

  private lazy val compiler = { /*val ctx = Context.enter; ctx.setOptimizationLevel(-1)
    val global = new Global; global.init(ctx)
    val scope = ctx.initStandardObjects(global)

    val wrappedCoffeescriptCompiler = Context.javaToJS(this, scope)
    ScriptableObject.putProperty(scope, "CoffeescriptCompiler", wrappedCoffeescriptCompiler)

    ctx.evaluateReader(scope, new InputStreamReader(
      this.getClass.getClassLoader.getResource("coffee-script.js").openConnection().getInputStream()),
      "coffee-script.js",
      1, null)

    val coffee = scope.get("CoffeeScript", scope).asInstanceOf[NativeObject]
    val compilerFunction = coffee.get("compile", scope).asInstanceOf[Function]

    val bareOpts = ctx.evaluateString(scope, "({bare: %b});".format(true), null, 1, null)

    Context.exit
    */ (source: File, bare: Boolean) =>
    {
      "coffee -scb" #< source !!
      /*val coffeeCode = Path(source).slurpString.replace("\r", "")
      if (bare)
        Context.call(null, compilerFunction, scope, scope, Array(coffeeCode, bareOpts)).asInstanceOf[String]
      else
        Context.call(null, compilerFunction, scope, scope, Array(coffeeCode)).asInstanceOf[String]
      */
    }

  }

  def compile(source: File, bare: Boolean): String = {
    try {
      compiler(source, bare)
    } catch {
      case e: JavaScriptException => {

        val line = """.*on line ([0-9]+).*""".r
        val error = e.getValue.asInstanceOf[Scriptable]

        throw ScriptableObject.getProperty(error, "message").asInstanceOf[String] match {
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
