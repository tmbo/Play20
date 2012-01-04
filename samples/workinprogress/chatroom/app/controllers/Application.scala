package controllers

import play.api._
import play.api.mvc._
import play.api.libs._

import play.api.libs.iteratee._
import play.api.libs.concurrent._
import play.api.libs.akka._

import actors._
import actors.ChatRoomActor._

object Application extends Controller {
  
  def index = Action {
    Ok(views.html.index()) 
  }
  
  // -- Comet chat room
  
  def chatRoom = Action {
    Ok(views.html.room())
  }
  
  def stream = Action {
    AsyncResult {
      (ChatRoomActor.ref ? Join()).mapTo[Enumerator[String]].asPromise.map { chunks =>
        Ok.stream(chunks &> Comet( callback = "parent.message"))
      }
    }
  }
  
  def say(message: String) = Action {
    ChatRoomActor.ref ! Message(message)
    Ok("Said " + message)
  }

}
