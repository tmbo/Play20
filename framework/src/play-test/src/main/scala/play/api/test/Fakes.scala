package play.api.test;

case class FakeHeaders(data: Map[String, Seq[String]] = Map.empty) extends play.api.mvc.Headers {
  lazy val keys = data.keySet
  def getAll(key: String) = data.get(key).getOrElse(Seq.empty)
} 

case class FakeCookies(cookies: Seq[play.api.mvc.Cookie] = Seq.empty) extends play.api.mvc.Cookies {
  def get(name: String) = cookies.find(_.name == name)
}

case class FakeRequest[A](method: String, uri: String, headers: FakeHeaders, cookies: FakeCookies, body: A) extends play.api.mvc.Request[A] {
  
  lazy val path = uri.split('?').take(1).mkString
  lazy val queryString = play.core.parsers.UrlFormEncodedParser.parse(rawQueryString)
  
  def withHeaders(newHeaders: (String,String)*): FakeRequest[A] = {
    copy(headers = FakeHeaders(
      headers.data ++ newHeaders.groupBy(_._1).mapValues(_.map(_._2))
    ))
  }

}

object FakeRequest {
  
  def apply(): FakeRequest[play.api.mvc.AnyContent] = {
    FakeRequest("GET", "/", FakeHeaders(), FakeCookies(), play.api.mvc.AnyContentAsEmpty)
  }
  
  def apply(method: String, path: String): FakeRequest[play.api.mvc.AnyContent] = {
    FakeRequest(method, path, FakeHeaders(), FakeCookies(), play.api.mvc.AnyContentAsEmpty)
  }
  
}

case class FakeApplication(
  override val path: java.io.File = new java.io.File("."), 
  override val classloader: ClassLoader = classOf[FakeApplication].getClassLoader
) extends play.api.Application(path, classloader, None, play.api.Mode.Test) {
  
  private val addPlugins = scala.collection.mutable.ArrayBuffer.empty[String]
  private val removePlugins = scala.collection.mutable.ArrayBuffer.empty[String]
  
  def addPlugin(className: String) = {
    addPlugins += className
    this
  }
  
  def removePlugin(className: String) = {
    removePlugins += className
    this
  }
  
  override lazy val pluginClasses = {
    addPlugins ++ super.pluginClasses.diff(removePlugins)
  } 
  
}