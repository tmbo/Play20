package play.core

import java.net.{ URLEncoder, URLDecoder }
import scala.annotation._

import scala.collection.JavaConverters._

@implicitNotFound("No queryString binder found for type ${A}. Try to implement an implicit QueryStringBindable for this type.")
trait QueryStringBindable[A] {
  def bind(key: String, params: Map[String, Seq[String]]): Option[Either[String, A]]
  def unbind(key: String, value: A): String
  def javascriptUnbind: String = """function(k,v) {return k+'='+v}"""
}

@implicitNotFound("No URL path binder found for type ${A}. Try to implement an implicit PathBindable for this type.")
trait PathBindable[A] {
  def bind(key: String, value: String): Either[String, A]
  def unbind(key: String, value: A): String
  def javascriptUnbind: String = """function(k,v) {return v}"""
}

@implicitNotFound("No JavaScript litteral binder found for type ${A}. Try to implement an implicit JavascriptLitteral for this type.")
trait JavascriptLitteral[A] {
  def to(value: A): String
}

object JavascriptLitteral {

  implicit def litteralString = new JavascriptLitteral[String] {
    def to(value: String) = "\"" + value + "\""
  }

  implicit def litteralInt = new JavascriptLitteral[Int] {
    def to(value: Int) = value.toString
  }

  implicit def litteralInteger = new JavascriptLitteral[java.lang.Integer] {
    def to(value: java.lang.Integer) = value.toString
  }

  implicit def litteralLong = new JavascriptLitteral[Long] {
    def to(value: Long) = value.toString
  }

  implicit def litteralBoolean = new JavascriptLitteral[Boolean] {
    def to(value: Boolean) = value.toString
  }

  implicit def litteralOption[T](implicit jsl: JavascriptLitteral[T]) = new JavascriptLitteral[Option[T]] {
    def to(value: Option[T]) = value.map(jsl.to(_)).getOrElse("null")
  }

}

object QueryStringBindable {

  implicit def bindableString = new QueryStringBindable[String] {
    def bind(key: String, params: Map[String, Seq[String]]) = params.get(key).flatMap(_.headOption).map(v => Right(URLDecoder.decode(v, "utf-8")))
    def unbind(key: String, value: String) = key + "=" + (URLEncoder.encode(value, "utf-8"))
  }

  implicit def bindableInt = new QueryStringBindable[Int] {
    def bind(key: String, params: Map[String, Seq[String]]) = params.get(key).flatMap(_.headOption).map { i =>
      try {
        Right(java.lang.Integer.parseInt(i))
      } catch {
        case e: NumberFormatException => Left("Cannot parse parameter " + key + " as Int: " + e.getMessage)
      }
    }
    def unbind(key: String, value: Int) = key + "=" + value.toString
  }

  implicit def bindableLong = new QueryStringBindable[Long] {
    def bind(key: String, params: Map[String, Seq[String]]) = params.get(key).flatMap(_.headOption).map { i =>
      try {
        Right(java.lang.Long.parseLong(i))
      } catch {
        case e: NumberFormatException => Left("Cannot parse parameter " + key + " as Long: " + e.getMessage)
      }
    }
    def unbind(key: String, value: Long) = key + "=" + value.toString
  }

  implicit def bindableInteger = new QueryStringBindable[java.lang.Integer] {
    def bind(key: String, params: Map[String, Seq[String]]) = params.get(key).flatMap(_.headOption).map { i =>
      try {
        Right(java.lang.Integer.parseInt(i))
      } catch {
        case e: NumberFormatException => Left("Cannot parse parameter " + key + " as Integer: " + e.getMessage)
      }
    }
    def unbind(key: String, value: java.lang.Integer) = key + "=" + value.toString
  }

  implicit def bindableBoolean = new QueryStringBindable[Boolean] {
    def bind(key: String, params: Map[String, Seq[String]]) = params.get(key).flatMap(_.headOption).map { i =>
      try {
        java.lang.Integer.parseInt(i) match {
          case 0 => Right(false)
          case 1 => Right(true)
          case _ => Left("Cannot parse parameter " + key + " as Boolean: should be 0 or 1")
        }
      } catch {
        case e: NumberFormatException => Left("Cannot parse parameter " + key + " as Boolean: should be 0 or 1")
      }
    }
    def unbind(key: String, value: Boolean) = key + "=" + (if (value) "1" else "0")
  }

  implicit def bindableOption[T: QueryStringBindable] = new QueryStringBindable[Option[T]] {
    def bind(key: String, params: Map[String, Seq[String]]) = {
      Some(
        implicitly[QueryStringBindable[T]].bind(key, params)
          .map(_.right.map(Some(_)))
          .getOrElse(Right(None)))
    }
    def unbind(key: String, value: Option[T]) = value.map(implicitly[QueryStringBindable[T]].unbind(key, _)).getOrElse("")
  }
  
  implicit def bindableJavaOption[T: QueryStringBindable] = new QueryStringBindable[play.libs.F.Option[T]] {
    def bind(key: String, params: Map[String, Seq[String]]) = {
      Some(
        implicitly[QueryStringBindable[T]].bind(key, params)
          .map(_.right.map(play.libs.F.Option.Some(_)))
          .getOrElse(Right(play.libs.F.Option.None.asInstanceOf[play.libs.F.Option[T]])))
    }
    def unbind(key: String, value: play.libs.F.Option[T]) = {
      if(value.isDefined) {
        implicitly[QueryStringBindable[T]].unbind(key, value.get)
      } else {
        ""
      }
    }
  }
  
  implicit def javaQueryStringBindable[T <: play.mvc.QueryStringBindable[T]](implicit m: Manifest[T]) = new QueryStringBindable[T] {
    def bind(key: String, params: Map[String, Seq[String]]) = {
      try {
        val o = m.erasure.newInstance.asInstanceOf[T].bind(key, params.mapValues(_.toArray).asJava)
        if(o.isDefined) {
          Some(Right(o.get))
        } else {
          None
        }
      } catch {
        case e => Some(Left(e.getMessage))
      }
    }
    def unbind(key: String, value: T) = {
      value.unbind(key)
    }
  }

}

object PathBindable {

  implicit def bindableString = new PathBindable[String] {
    def bind(key: String, value: String) = Right(URLDecoder.decode(value, "utf-8"))
    def unbind(key: String, value: String) = value
  }

  implicit def bindableInt = new PathBindable[Int] {
    def bind(key: String, value: String) = {
      try {
        Right(java.lang.Integer.parseInt(URLDecoder.decode(value, "utf-8")))
      } catch {
        case e: NumberFormatException => Left("Cannot parse parameter " + key + " as Int: " + e.getMessage)
      }
    }
    def unbind(key: String, value: Int) = value.toString
  }

  implicit def bindableLong = new PathBindable[Long] {
    def bind(key: String, value: String) = {
      try {
        Right(java.lang.Long.parseLong(URLDecoder.decode(value, "utf-8")))
      } catch {
        case e: NumberFormatException => Left("Cannot parse parameter " + key + " as Long: " + e.getMessage)
      }
    }
    def unbind(key: String, value: Long) = value.toString
  }

  implicit def bindableInteger = new PathBindable[java.lang.Integer] {
    def bind(key: String, value: String) = {
      try {
        Right(java.lang.Integer.parseInt(URLDecoder.decode(value, "utf-8")))
      } catch {
        case e: NumberFormatException => Left("Cannot parse parameter " + key + " as Integer: " + e.getMessage)
      }
    }
    def unbind(key: String, value: java.lang.Integer) = value.toString
  }

  implicit def bindableBoolean = new PathBindable[Boolean] {
    def bind(key: String, value: String) = {
      try {
        java.lang.Integer.parseInt(URLDecoder.decode(value, "utf-8")) match {
          case 0 => Right(false)
          case 1 => Right(true)
          case _ => Left("Cannot parse parameter " + key + " as Boolean: should be 0 or 1")
        }
      } catch {
        case e: NumberFormatException => Left("Cannot parse parameter " + key + " as Boolean: should be 0 or 1")
      }
    }
    def unbind(key: String, value: Boolean) = key + "=" + (if (value) "1" else "0")
  }

  implicit def bindableOption[T: PathBindable] = new PathBindable[Option[T]] {
    def bind(key: String, value: String) = {
      implicitly[PathBindable[T]].bind(key, value).right.map(Some(_))
    }
    def unbind(key: String, value: Option[T]) = value.map(v => implicitly[PathBindable[T]].unbind(key, v)).getOrElse("")
  }
  
  implicit def javaPathBindable[T <: play.mvc.PathBindable[T]](implicit m: Manifest[T]) = new PathBindable[T] {
    def bind(key: String, value: String) = {
      try {
        Right(m.erasure.newInstance.asInstanceOf[T].bind(key, value))
      } catch {
        case e => Left(e.getMessage)
      }
    }
    def unbind(key: String, value: T) = {
      value.unbind(key)
    }
  }

}
