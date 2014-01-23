package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.libs.ws._
import play.api.libs.concurrent.Execution.Implicits.defaultContext
import java.util.concurrent.atomic._
import scala.concurrent._
import play.api.libs.concurrent.Akka
import akka.actor.{Actor, Props}
import play.api.Play.current
import akka.pattern.ask
import akka.actor._
import akka.util._
import scala.concurrent.duration._
import akka.util.Timeout
import akka.actor.ActorLogging

//import  Contexts.customContext

object Contexts {
  implicit val customContext: scala.concurrent.ExecutionContext = Akka.system.dispatchers.lookup("custom-context")
}

class Sleeper extends Actor with ActorLogging {
  def receive = {
    case (in, d: FiniteDuration) => {
      val snd = sender
      val slf = self
      log.debug("Received " + in.toString + " by " + slf.toString)
      context.system.scheduler.scheduleOnce(d, self, "end")
      context.become({
        case "end" => {
          log.debug("Received end by " + slf.toString)
          context.unbecome()
          snd ! in
          self ! PoisonPill
        }
      }, discardOld = false)
    }
  }
}


object Application extends Controller {

  def index = Action.async {
    val begin = System.currentTimeMillis()

    val futuresSeq = for {
      i <- 0 to 1000
    } yield {
      WS.url("http://localhost:9001/sleep").get()
    }
    val seqFutures = Future sequence futuresSeq

    for {
      futures <- seqFutures
    } yield {
      val futuresResponse = futures map {
        _.body
      } mkString (",")
      val response = futuresResponse + "\n" +
        "Get response in " + ((System.currentTimeMillis() - begin) / 1000).toString + " sec\n"
      Results.Ok(response)
    }
  }

  val counter = new AtomicInteger(0)

  def sleep = Action.async {
    val c = counter.incrementAndGet()
    println("sleep")
    val begin = System.currentTimeMillis()
    for {
      sleep <- {
        // sleep in default context
        // play.api.libs.concurrent.Promise.timeout("Sleep "+c.toString, 30.second)
        // import Contexts.customContext
        val sleeper = Akka.system.actorOf(Props[Sleeper], name = "sleeper" + c.toString)
        (sleeper ?("Sleep " + c.toString, 5 seconds))(30 seconds).mapTo[String]
      }
    } yield {
      Results.Ok(sleep + ": " + ((System.currentTimeMillis() - begin) / 1000).toString + " sec\n")
    }

  }

}
