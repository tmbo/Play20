package coffeescript

case class CoffeeLine( line: Option[Int], content: String ){
  def isEmpty = content.trim == ""
    
  def map( f: String => String) =
    CoffeeLine( line, f(content) )
}

case class CoffeeFile( lines : List[CoffeeLine]){
  
  val content = 
    lines.map( _.content).mkString("\n")
      
  def map( f: String => String ) =
    CoffeeFile( lines.map( cl => CoffeeLine( cl.line, f( cl.content ))))
    
  def block( start: Int, size: Int) = 
    CoffeeFile(lines.drop(start-1).take(size))
    
  def blockFrom( start: Int) = 
    CoffeeFile(lines.drop(start-1))
    
  def blockTil( end: Int): CoffeeFile =
    CoffeeFile(lines.take(end))
    
  def ++( other: CoffeeFile) = 
    CoffeeFile( this.lines ::: other.lines ) 
    
  def ++( other: CoffeeLine ) =
    other.line match {
    case Some(_) => 
      CoffeeFile( this.lines ::: List( other ))  
    case _  =>
      val nr = lines.lastOption.map(  _.line getOrElse 1 ) getOrElse 1
      CoffeeFile( this.lines ::: List( CoffeeLine( Some(nr), other.content) ))
  }
  
  def ++( s: String ): CoffeeFile =
    ++( CoffeeLine( None, s))
    
  def ++( l: List[(Int, String)] ): CoffeeFile =
    ++( CoffeeFile( l.map( x => CoffeeLine( Some(x._1), x._2))))
    
  def +:( s: String ): CoffeeFile =
    lines.headOption match {
    case Some( h ) =>
      CoffeeFile( h.map( s + _) :: lines.tail)
    case _ =>
      CoffeeFile( CoffeeLine( Some(1), s) :: Nil)
  }
  
  def :+( s: String ): CoffeeFile =
    lines.lastOption match {
    case Some( l ) =>
      CoffeeFile( lines.take(lines.size-1) ::: List(l.map( _ + s )) )
    case _ =>
      CoffeeFile( CoffeeLine( Some(1), s) :: Nil)
  }
  
  def lineOf( n: Int) = 
    lines.drop(n-1).head.line.get

  def lineMapping = 
    lines.zipWithIndex.map{
      case (cf, index) => 
        (index, cf.line getOrElse index)
    }.toMap
    
  override def toString() = {
      lines.map( _.toString ).mkString("\n")
    }
  def printOut() {
    content.split( "\n" ).zipWithIndex.foreach( x => println( (x._2+1) + " | " + x._1 ) )
  }
}

object CoffeeFile{
  
  def fromString( content: String, withLineNumbers: Boolean = true) =
    if( withLineNumbers)
      CoffeeFile( content.split("\n").toList.zipWithIndex.map( v => CoffeeLine( Some(v._2+1), v._1)))
    else
      CoffeeFile( content.split("\n").toList.map( c => CoffeeLine( None, c)))  
      
  def fromString( content: String, line: Int) =
    CoffeeFile( content.split("\n").toList.map( c => CoffeeLine( Some(line), c)))   
}