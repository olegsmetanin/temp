/**
 * Copyright (C) 2014 Oleg Smetanin
 */
package sample.remote.pingpong


import com.googlecode.concurrentlinkedhashmap.ConcurrentLinkedHashMap
import scala.concurrent.duration._
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.Props
import akka.actor.Props
import java.util.concurrent.atomic.{AtomicLong}
import akka.actor.PoisonPill


object PingPongParam {
  var concurrency = 100
  var warmupRequest = 1000
  var maxRequest = 10000
  var pongHostname = "localhost"
}

object PingPong {

  def main(args: Array[String]): Unit = {

    args.toList match {
      case "ping" :: host :: conc :: warmup :: max :: xs => {
        PingPongParam.pongHostname = host
        PingPongParam.concurrency = conc.toInt
        PingPongParam.warmupRequest = warmup.toInt
        PingPongParam.maxRequest = max.toInt
        startPingSystem()
      }
      case "pong" :: xs => {
        startPongSystem()
      }
      case _ => println("nonvalid arg")
    }
  }

  def startPingSystem(): Unit = {

    val system = ActorSystem("PingSystem", ConfigFactory.load("ping"))
    val pingActor = system.actorOf(Props(classOf[PingActor]), "pingActor")

    println("Started PingSystem")

    import system.dispatcher
    system.scheduler.scheduleOnce(1.second) {
      pingActor ! WarmUp
    }

  }


  def startPongSystem(): Unit = {

    val system = ActorSystem("PongSystem", ConfigFactory.load("pong"))
    val pongActor = system.actorOf(Props(classOf[PongActor]), "pongActor")

    println("Started PongSystem")
    pongActor ! StartUp
  }

}

case class Ping(id: Long, tick: Long)

case class Pong(id: Long, tick: Long)

case class Save()

case class WarmUp()

case class PingThemAll()

case class StartUp()


class PingActor extends Actor {

  val pingCounter = new AtomicLong(0)
  val pongCounter = new AtomicLong(0)
  val pongActor = context.actorSelection("akka.tcp://PongSystem@" + PingPongParam.pongHostname + ":2553/user/pongActor")

  private val store = new ConcurrentLinkedHashMap.Builder[Long, (Long, Long)]
    .initialCapacity(1000000)
    .maximumWeightedCapacity(2000000)
    .build()

  var startTime = System.nanoTime()

  var isOver = false

  def receive = {

    case WarmUp => {
      println("WarmUp")
      for (i <- 0 to PingPongParam.concurrency) {
        pongActor ! Ping(pingCounter.incrementAndGet(), System.nanoTime())
      }
    }
    case Pong(id: Long, tick: Long) => {
      val c = pongCounter.incrementAndGet()
      if (!isOver) {
        if (c > PingPongParam.warmupRequest - PingPongParam.concurrency) {
          isOver = true

          import scala.concurrent.ExecutionContext.Implicits.global
          context.system.scheduler.scheduleOnce(5.second) {
            context.become(record)
            self ! PingThemAll
          }
        } else {
          pongActor ! Ping(pingCounter.incrementAndGet(), System.nanoTime())
        }
      }
    }
  }

  def record: Receive = {

    case PingThemAll => {
      println("PingThemAll")
      pingCounter.set(0)
      pongCounter.set(0)
      isOver = false

      for (i <- 0 to PingPongParam.concurrency) {
        pongActor ! Ping(pingCounter.incrementAndGet(), System.nanoTime())
      }
    }

    case Pong(id: Long, tick: Long) => {
      store.put(id, (tick, System.nanoTime()))
      val c = pongCounter.incrementAndGet()
      if (!isOver) {
        if (c > PingPongParam.maxRequest - PingPongParam.concurrency) {
          isOver = true
          import scala.concurrent.ExecutionContext.Implicits.global
          context.system.scheduler.scheduleOnce(5.second) {
            self ! Save
          }
        } else {
          pongActor ! Ping(pingCounter.incrementAndGet(), System.nanoTime())
        }
      }
    }

    case Save => {
      import collection.JavaConversions._
      val pw = new java.io.PrintWriter("stat.txt")
      store.descendingMap() foreach {
        case (k, v) => pw.println(k.toString + "," + ((v._2 - v._1) / 1000000).toString + "," + v._1.toString + "," + v._2.toString)
      }
      pw.close()
      println("Saved")

      import scala.concurrent.ExecutionContext.Implicits.global
      context.system.scheduler.scheduleOnce(10.second) {
        context.system.shutdown
      }
    }

  }

}


class PongActor extends Actor {

  def receive = {
    case StartUp => println("StartUp " + self.toString)
    case Ping(id: Long, tick: Long) =>
      sender ! Pong(id, tick)
  }

}
