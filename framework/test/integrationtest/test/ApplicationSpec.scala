package test

import play.api.test._
import play.api.test.Helpers._

import org.specs2.mutable._

object ApplicationSpec extends Specification {

  "an Application" should {
  
    "execute index" in {
      
      running(FakeApplication()) {
        
        val action = controllers.Application.index()
        val result = action(FakeRequest())
        
        status(result) must equalTo(OK)
        contentType(result) must equalTo(Some("text/html"))
        charset(result) must equalTo(Some("utf-8"))
        contentAsString(result) must contain("Hello world")
      }
    }
  
    "execute index again" in {
      
      running(FakeApplication()) {
        val action = controllers.Application.index()
        val result = action(FakeRequest())
        
        status(result) must equalTo(OK)
        contentType(result) must equalTo(Some("text/html"))
        charset(result) must equalTo(Some("utf-8"))
        contentAsString(result) must contain("Hello world")
      }
    }
    
    "execute json" in {
    running(FakeApplication()) {
      val Some(result) = routeAndCall(FakeRequest(GET, "/json"))
      status(result) must equalTo(OK)
      contentType(result) must equalTo(Some("application/json"))
      contentAsString(result) must contain("{\"id\":1.0,\"name\":\"Sadek\",\"favThings\":[\"tea\"]}")

      }
    }
   
  }
   
}
