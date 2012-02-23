package play.core.coffeescript

import java.io.File
import scala.util.matching.Regex

object Patterns {

  val IdentifierCharPattern = "[a-zA-Z0-9_\\-]"
  val IdentifierPattern = IdentifierCharPattern + "+"

  val ParamPattern = "\\s*" + IdentifierPattern + "\\s*"
  val GroupedParamPattern = "\\s*(" + IdentifierPattern + ")\\s*"

  val MacroPattern =
    "(" + IdentifierCharPattern + "*Macro)\\s*" + // macro name
      "=\\s*\\(" +
      "((?:" + ParamPattern + ",)*" + ParamPattern + ")" + // param list
      "\\)" +
      "\\s*\\->\\s*\\n(" + // ->
      "([ \\t]*\\n|[ \\t]+.*?\\n)+" + //empty line or intended line (body of macro)
      ")"

  val PreviousToMacroUsage = "(([ \\t]*).*?)"
}

object CoffeescriptPreprocessor {
  import Patterns._

  val MacroRx = new Regex( MacroPattern, "name", "params", "body" )

  def processOneMakro( string: String ) = {
    for {
      m <- MacroRx.findFirstMatchIn( string )
    } yield {
      val body = m.matched.split( "->" )( 1 ).trim
      val macro = Macro( m.group( "name" ), m.group( "params" ), m.group( "body" ).trim )
      var content = m.before.toString() + m.after.toString()

      macro.process( content )
    }
  }

  def process( file: File ) = {
    var result = scala.io.Source.fromFile( file ).mkString
    var foundMacro = true

    while ( foundMacro ) {
      processOneMakro( result ) match {
        case Some( r ) =>
          result = r
        case _ =>
          foundMacro = false
      }
    }

    result
  }
}

case class Macro( name: String, paramString: String, body: String ) {
  import Patterns._

  val params = paramString match {
    case s if s != "" => s.split( "," ).map( _.trim )
    case _            => Array[String]()
  }

  val multipleParams = params.map( _ => GroupedParamPattern ).mkString( "," )

  val macroRx = new Regex( PreviousToMacroUsage + name + "\\(" + multipleParams + "\\)" )

  def indentBlock( block: String, indent: String ) = {
    ( block.split( "\n" ).toList match {
      case h :: t => h :: t.map( indent + _ ) // all lines except the first one get indented
      case _      => Nil
    } ).mkString( "\n" )
  }

  def process( s: String ) = {
    macroRx.replaceAllIn( s, matchObj => {
      val paramMapping = params.zipWithIndex.map {
        case ( p, i ) =>
          val actual = matchObj.group( i + 3 )
          val paramRx = ( "(?<!" + IdentifierPattern + ")" + p + "(?!" + IdentifierPattern + ")" ).r
          paramRx -> actual
      }.toMap
      
      val indent = matchObj.group( 2 ) + "\t"
      val preMacro = matchObj.group( 1 )
      var result = preMacro + indentBlock( body, indent )
      for {
        ( param, actual ) <- paramMapping
      } {
        result = param.replaceAllIn( result, actual )
      }
      result
    } )
  }
}