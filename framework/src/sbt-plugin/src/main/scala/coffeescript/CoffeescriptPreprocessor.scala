package play.core.coffeescript

import java.io.File
import scala.util.matching.Regex

case class CoffeescriptPreprocessorException( error: String) extends Throwable

object Patterns {

  val IdentifierCharPattern = "[a-zA-Z0-9_\\-]"
  val NoIdentifierCharPattern = "[^a-zA-Z0-9_\\-]"
  val IdentifierPattern = IdentifierCharPattern + "+"

  val ParamPattern = "\\s*" + IdentifierPattern + "\\s*"
  val GroupedParamPattern = "\\s*(" + IdentifierPattern + ")\\s*"

  val MacroPattern =
    "([ \\t]*)(" + IdentifierCharPattern + "*Macro)\\s*" + // macro name
      "[=:]\\s*\\(" +
      "((?:" + ParamPattern + ",)*" + ParamPattern + ")" + // param list
      "\\)" +
      "\\s*\\->[ \\t]*"

  val PreviousToMacroUsage = "(([ \\t]*).*?)"

  val DependenciesPattern = "###\\s*define([^#]*)###"
  val DependenciePattern = "[^\\n\\s\\:]+"
}

object CoffeescriptPreprocessor {
  import Patterns._

  val MacroRx = new Regex( MacroPattern, "indent", "name", "params" )
  val DependenciesRx = new Regex( DependenciesPattern, "dependencies", "last" )
  val DependencieRx = new Regex( DependenciePattern, "dependencie", "name" )

  def processOneMakro( string: String ) = {
    for {
      m <- MacroRx.findFirstMatchIn( string )
    } yield {

      val indent = m.group( "indent" )

      val ( body, after ) = m.after.toString().split( "\n" ).span {
        line =>
          line.trim == "" || line.startsWith( indent + " " ) || line.startsWith( indent + "\t" )
      }

      val macro = Macro( m.group( "name" ), m.group( "params" ), body.mkString( "\n" ) )

      var content = m.before.toString() + after.mkString( "\n" )
      macro.process( content )
    }
  }

  def processMacros( fileContent: String ) = {
    var result = fileContent
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

  def process( file: File ) = {
    val fileContent = scala.io.Source.fromFile( file ).mkString
    processMacros( processDependencies( fileContent ) )
  }

  def processDependencies( fileContent: String ) = {
    ( for {
      m <- DependenciesRx.findFirstMatchIn( fileContent )
    } yield {
      val dependencieString = m.group( "dependencies" )
      val dependencieList = DependenciePattern.r.findAllIn( dependencieString ).toList
      if( dependencieList.size > 0){
        val pathList = dependencieList.sliding( 1, 2 ).flatten.map( "\t\t" + _ )
        val paths = pathList.mkString( "\n" )
        
        val parameterList = ( dependencieList.tail sliding ( 1, 2 ) flatten )
        val parameters = parameterList.mkString( ", " )
        
        if(pathList.size != parameterList.size)
          throw new CoffeescriptPreprocessorException( "Dependency list is incorrect!" )
       
        val body = m.after.toString.split( "\n" ).map( "\t\t" + _ ).mkString( "\n" )
  
        """define(
        | [
        |%s
        | ], (%s) ->
        |%s
        |)""".stripMargin.format( paths, parameters, body )
      } else {
        val body = m.after.toString.split( "\n" ).map( "\t" + _ ).mkString( "\n" )
        
        "define -> \n%s".stripMargin.format( body )
      }
    } ) getOrElse fileContent
  }
}

case class Macro( name: String, paramString: String, bodyString: String ) {
  import Patterns._

  val params = paramString match {
    case s if s != "" => s.split( "," ).map( _.trim )
    case _            => Array[String]()
  }

  val multipleParams = params.map( _ => GroupedParamPattern ).mkString( "," )

  val macroRx = new Regex( PreviousToMacroUsage + name + "\\(" + multipleParams + "\\)" )

  val paramsRx = params.map { p =>
    ( "(" + NoIdentifierCharPattern + ")" + p + "(" + NoIdentifierCharPattern + ")" ).r
  }

  val body = outdentBlock( parametryFy( remTrailingEmptyLines( bodyString ) ).split( "\n" ).toList )

  override def toString() =
    "Macro(%s, '%s', \n%s)".format( name, paramString, body.mkString( "\n" ) )

  def parametryFy( block: String ) =
    paramsRx.zipWithIndex.foldLeft( block.replaceAll( "%", "%%" ) )( ( result, el ) => {
      el._1.replaceAllIn( result, "$1%" + ( el._2 + 1 ) + "\\$s$2" )
    } )

  def remTrailingEmptyLines( block: String ) =
    ( "(^\\s*\\n)|(\\n\\s*$)".r ).replaceAllIn( block, "" )

  def outdentBlock( block: List[String] ) = {
    val indentRx = "\\s*".r
    block match {
      case h :: _ =>
        val originalIndent = indentRx.findFirstIn( h ).get.size
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
      val actual = 3 to matchObj.groupCount map ( matchObj.group )
      val preMacro = matchObj.group( 1 )
      val additionalIndent = preMacro.trim() match {
        case "" => ""
        case _  => "\t"
      }

      val indent = matchObj.group( 2 ) + additionalIndent

      var result = preMacro + indentBlock( body, indent ).mkString( "\n" )
      result.format( actual: _* ).replaceAll( "%%", "%" )
    } )
  }
}