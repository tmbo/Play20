package play.core.coffeescript

import java.io.File
import scala.util.matching.Regex

object Patterns {

  val IdentifierCharPattern = "[a-zA-Z0-9_\\-]"
  val IdentifierPattern = IdentifierCharPattern + "+"

  val ParamPattern = "\\s*" + IdentifierPattern + "\\s*"
  val GroupedParamPattern = "\\s*(" + IdentifierPattern + ")\\s*"

  val MacroPattern =
    "([ \\t]*)(" + IdentifierCharPattern + "*Macro)\\s*" + // macro name
      "[=:]\\s*\\(" +
      "((?:" + ParamPattern + ",)*" + ParamPattern + ")" + // param list
      "\\)" +
      "\\s*\\->[ \\t]*\\n(" + // ->
      "([ \\t]*\\n|\\1[ \\t]+.*?\\n)+" + //empty line or intended line (body of macro)
      ")"

  val PreviousToMacroUsage = "(([ \\t]*).*?)"
}

object CoffeescriptPreprocessor {
  import Patterns._

  val MacroRx = new Regex( MacroPattern, "indent", "name", "params", "body" )

  def processOneMakro( string: String ) = {
    for {
      m <- MacroRx.findFirstMatchIn( string )
    } yield {
      val macro = Macro( m.group( "name" ), m.group( "params" ), m.group( "body" ) )
      println( macro )
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

case class Macro( name: String, paramString: String, bodyString: String ) {
  import Patterns._

  val body = outdentBlock( remTrailingEmptyLines( bodyString ).split( "\n" ).toList )

  val params = paramString match {
    case s if s != "" => s.split( "," ).map( _.trim )
    case _            => Array[String]()
  }

  val multipleParams = params.map( _ => GroupedParamPattern ).mkString( "," )

  val macroRx = new Regex( PreviousToMacroUsage + name + "\\(" + multipleParams + "\\)" )

  def remTrailingEmptyLines( block: String ) =
    ( "(^\\s*\\n) | (\\n\\s*$)".r ).replaceAllIn( block, "" )

  def outdentBlock( block: List[String] ) = {
    val indentRx = "\\s*".r
    block match {
      case h :: _ =>
        val originalIndent = indentRx.findFirstIn( block.head ).get.size
        block.map { s =>
          if ( s.size >= originalIndent )
            s.substring( originalIndent )
          else
            ""
        }
      case _ =>
        block
    }
  }

  def indentBlock( block: List[String], indent: String ) = {
    block match {
      case h :: t =>
        h :: t.map( indent + _ ) // all lines except the first one get indented
      case _ =>
        Nil
    }
  }

  def process( s: String ) = {
    macroRx.replaceAllIn( s, matchObj => {
      val paramMapping = params.zipWithIndex.map {
        case ( p, i ) =>
          val actual = matchObj.group( i + 3 )
          val paramRx = ( "(?<!" + IdentifierPattern + ")" + p + "(?!" + IdentifierPattern + ")" ).r
          paramRx -> actual
      }.toMap
      val preMacro = matchObj.group( 1 )

      val additionalIndent = preMacro.trim() match {
        case "" => ""
        case _  => "\t"
      }

      val indent = matchObj.group( 2 ) + additionalIndent
      var result = preMacro + indentBlock( body, indent ).mkString( "\n" )
      for {
        ( param, actual ) <- paramMapping
      } {
        result = param.replaceAllIn( result, actual )
      }
      result
    } )
  }
}