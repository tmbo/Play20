package play.core.coffeescript

import java.io.File
import scala.util.matching.Regex
import coffeescript.CoffeeFile
import coffeescript.CoffeeFile._

case class CoffeescriptPreprocessorException( error: String, line: Int ) extends Throwable

object Patterns {

  val IdentifierCharPattern = "[a-zA-Z0-9_\\-]"
  val NoIdentifierCharPattern = "^|[^a-zA-Z0-9_\\-]|$"
  val IdentifierPattern = IdentifierCharPattern + "+"

  val ParamPattern = "\\s*" + IdentifierPattern + "\\s*"
  val GroupedParamPattern = "\\s*(" + IdentifierPattern + ")\\s*"
 
  val MacroPattern =
    "([ \\t]*)(" + IdentifierCharPattern + "*Macro)\\s*" + // macro name
      "[=:]\\s*\\(" +
      "((?:" + ParamPattern + ",)*" + ParamPattern + ")" + // param list
      "\\)" +
      "\\s*\\->[ \\t]*\\n"

  val PreviousToMacroUsage = "(([ \\t]*)[^\\n]*?)"

  val DependenciesPattern = "###\\s*define([^#]*)###"
  val DependenciePattern = "\\s*([^\"\\n\\s\\:]+)\\s*:\\s*([^\"\\n\\s\\:]+)\\s*"
}

object CoffeescriptPreprocessor {
  import Patterns._

  val MacroRx = new Regex( MacroPattern, "indent", "name", "params" )
  val DependenciesRx = new Regex( DependenciesPattern, "dependencies", "last" )
  val DependencieRx = new Regex( DependenciePattern, "dependencie", "name" )

  def processOneMakro( coffee: CoffeeFile ): Option[CoffeeFile] = {
    for {
      m <- MacroRx.findFirstMatchIn( coffee.content )
    } yield {
      
      val indent = m.group( "indent" )
      val linesBefore = m.before.toString.count( _ == '\n' )
      val bodyStart = linesBefore + m.matched.count( _ == '\n' ) + 1
      
      val ( body, after ) = m.after.toString().split( "\n" ).span {
        line =>
          line.trim == "" || line.startsWith( indent + " " ) || line.startsWith( indent + "\t" )
      }
      
      val macro = Macro( m.group( "name" ), m.group( "params" ), coffee.block( bodyStart, body.size ) )
      var content =
        coffee.blockTil( linesBefore ) ++
          coffee.blockFrom( bodyStart + body.size )
      macro.process( content )
    }
  }

  def processMacros( coffee: CoffeeFile ) = {
    var result = coffee
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

  def process( file: File ): Tuple2[String, Map[Int, Int]] = {
    val fileContent = scala.io.Source.fromFile( file ).mkString
    val coffeeFile = CoffeeFile.fromString( fileContent )
    val f = processDependencies( coffeeFile )
    val result = processMacros( f )
    result.content -> result.lineMapping
  }

  def processDependencies( coffee: CoffeeFile ): CoffeeFile = {
    ( for {
      m <- DependenciesRx.findFirstMatchIn( coffee.content )
    } yield {
      val firstLine = m.before.toString.split( "\n" ).size
      val dependencyEnd = firstLine + m.matched.split( "\n" ).size

      val dependencieStrings = m.group( "dependencies" ).split( "\n" ).filter( _.trim != "" )

      val dependencies = dependencieStrings.zipWithIndex.map { e =>
        e._1 match {
          case DependencieRx( path, parameter ) =>
            val line = coffee.lineOf( firstLine + e._2 + 1 )
            ( path, parameter, line )
          case _ =>
            val line = firstLine + e._2 + 1
            throw new CoffeescriptPreprocessorException( "Dependency list is incorrect!", line )
        }
      }
      if ( dependencies.size > 0 ) {
        val paths = dependencies.map {
          case ( path, _, line ) =>
            ( line, "\t\t\"" + path + "\"" )
        }.toList

        val parameters = dependencies.map( _._2 ).mkString( ", " )

        coffee.blockTil( firstLine - 1 ) ++
          "define(" ++
          " [" ++
          paths ++
          " ], (%s) ->".format( parameters ) ++
          coffee.blockFrom( dependencyEnd ).map( "\t\t" + _ ) ++
          ")"
      } else {
        coffee.blockTil( firstLine - 1 ) ++
          "define ->" ++
          coffee.blockFrom( dependencyEnd ).map( "\t" + _ )
      }
    } ) getOrElse coffee
  }
}

case class Macro( name: String, paramString: String, coffeeBody: CoffeeFile ) {
  import Patterns._

  val params = paramString match {
    case s if s != "" => s.split( "," ).map( _.trim )
    case _            => Array[String]()
  }

  val multipleParams = params.map( _ => GroupedParamPattern ).mkString( "," )

  val macroRx = new Regex( PreviousToMacroUsage + name + "\\(" )

  val paramsRx = params.map { p =>
    ( "(" + NoIdentifierCharPattern + ")" + p + "(" + NoIdentifierCharPattern + ")" ).r
  }

  val body = outdentBlock( parametryFy( remTrailingEmptyLines( coffeeBody ) ) )

  override def toString() =
    "Macro(%s, '%s', \n%s)".format( name, paramString, body.lines.mkString( "\n" ) )

  def parametryFy( block: CoffeeFile ) =
    paramsRx.zipWithIndex.foldLeft( block.map( _.replaceAll( "%", "%%" ) ) )( ( result, el ) => el match {
      case ( paramRx, idx ) =>
        result.map( line => paramRx.replaceAllIn( line, "$1%" + ( idx + 1 ) + "\\$s$2" ) )
    } )

  def remTrailingEmptyLines( block: CoffeeFile ) =
    CoffeeFile( block.lines.dropWhile( _.isEmpty ).reverse.dropWhile( _.isEmpty ).reverse )

  def outdentBlock( block: CoffeeFile ) = {
    val indentRx = "\\s*".r
    block.lines match {
      case h :: _ =>
        val originalIndent = indentRx.findFirstIn( h.content ).get.size
        block.map { s =>
          if ( s.size >= originalIndent ){
            s.substring( originalIndent )
          } else if( s.trim != ""){
            throw new Error( "Outdent failed" )
          } else
            ""
        }
      case _ =>
        block
    }
  }

  def indentBlock( block: CoffeeFile, indent: String ) = {
    block.lines match {
      case h :: t =>
        // all lines except the first one get indented
        block.blockTil( 1 ) ++
          block.blockFrom( 2 ).map( indent + _ )
      case _ =>
        block
    }
  }
  
  def extractActualParams( block: String): Tuple3[Seq[String], String, String] = {
    var params: List[String] = Nil
    var parenthesis = 1
    var currentPosition = 0
    var charsToProcess = block
    var totalCount = 0
    while( parenthesis>0){
      charsToProcess.charAt(currentPosition) match{
        case '(' =>
          parenthesis+=1
        case ')' =>
          parenthesis-=1
          if( parenthesis == 0){
            val (param,tail) = charsToProcess.splitAt(currentPosition)
            params ::= param
            charsToProcess = tail.drop(1)
          }
        case ',' if parenthesis == 1 =>
          val (param,tail) = charsToProcess.splitAt(currentPosition)
          params ::= param
          charsToProcess = tail.drop(1)
          currentPosition = -1
        case _ =>
          
      }
      currentPosition+=1
      totalCount+=1
    }
    (params.map(_.trim).reverse, block.take(totalCount), charsToProcess)
  }
  
  def processOneUsage( block: CoffeeFile ): Option[CoffeeFile] = {
    for {
      matchObj <- macroRx.findFirstMatchIn( block.content )
    } yield {
      val (actual, paramString, afterMatch) = extractActualParams(matchObj.after.toString)
      val preMacro = matchObj.group( 1 )
      val pastMacro = afterMatch.takeWhile( _ != '\n')
      val additionalIndent = preMacro.trim match {
        case "" => ""
        case _  => "\t"
      }
      val indent = matchObj.group( 2 ) + additionalIndent
      val result = preMacro +: indentBlock( body, indent ) :+ pastMacro

      val start = matchObj.before.toString.count( _ == '\n' )
      val size = (matchObj.matched + paramString).count( _ == '\n' )
      val replaced = result.map( _.format( actual: _* ).replaceAll( "%%", "%" ) )
      
      block.blockTil( start ) ++
          replaced ++
          block.blockFrom( start + size + 2)
    }
  }

  def process( block: CoffeeFile ) = {
    var result = block
    var foundMacro = true

    while ( foundMacro ) {

      processOneUsage( result ) match {
        case Some( r ) =>
          result = r
        case _ =>
          foundMacro = false
      }
    }
    result  
  }
}