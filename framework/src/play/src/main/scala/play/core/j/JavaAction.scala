package play.core.j

import play.api.mvc._
import play.mvc.{ Action => JAction, Result => JResult }
import play.mvc.Http.{ Context => JContext, Request => JRequest, RequestBody => JBody, Cookies => JCookies, Cookie => JCookie }

/*
 * An action that's handling Java requests
 */
trait JavaAction extends Action[play.mvc.Http.RequestBody] with JavaHelpers {

  def parser = {
    Seq(method.getAnnotation(classOf[play.mvc.BodyParser.Of]), controller.getAnnotation(classOf[play.mvc.BodyParser.Of]))
      .filterNot(_ == null)
      .headOption.map { bodyParserOf =>
        bodyParserOf.value.newInstance.parser(bodyParserOf.maxLength)
      }.getOrElse(JavaParsers.anyContent(java.lang.Integer.MAX_VALUE))
  }

  def invocation: JResult
  def controller: Class[_]
  def method: java.lang.reflect.Method

  def apply(req: Request[play.mvc.Http.RequestBody]): Result = {

    val javaContext = createJavaContext(req)

    val rootAction = new JAction[Any] {

      def call(ctx: JContext): JResult = {
        try {
          JContext.current.set(ctx)
          invocation
        } finally {
          JContext.current.remove()
        }
      }
    }

    // Wrap into user defined Global action
    val baseAction = play.api.Play.maybeApplication.map { app =>
      app.global match {
        case global: JavaGlobalSettingsAdapter => {
          val action = global.underlying.onRequest(javaContext.request, method)
          action.delegate = rootAction
          action
        }
        case _ => rootAction
      }
    }.getOrElse(rootAction)

    val actionMixins = {
      (method.getDeclaredAnnotations ++ controller.getDeclaredAnnotations).collect {
        case a: play.mvc.With => a -> a.value()
        case a if a.annotationType.isAnnotationPresent(classOf[play.mvc.With]) => a -> a.annotationType.getAnnotation(classOf[play.mvc.With]).value()
      }.reverse
    }

    val finalAction = actionMixins.foldLeft[JAction[_ <: Any]](baseAction) {
      case (delegate, (annotation, actionClass)) => {
        val action = actionClass.newInstance()
        action.configuration = annotation
        action.delegate = delegate
        action
      }
    }

    createResult(javaContext, finalAction.call(javaContext))
  }

}
