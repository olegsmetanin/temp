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
      println("Start warming")
      pingActor ! PingThemAll
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

case class PingThemAll()

case class StartUp()


class PingActor extends Actor {

  val pingCounter = new AtomicLong(PingPongParam.maxRequest)
  val pongActor = context.actorSelection("akka.tcp://PongSystem@" + PingPongParam.pongHostname + ":2553/user/pongActor")

  private val store = new ConcurrentLinkedHashMap.Builder[Long, (Long, Long)]
    .initialCapacity(1000000)
    .maximumWeightedCapacity(2000000)
    .build()

  sealed trait State
  case class Warming() extends State
  case class Recording() extends State
  case class Waiting() extends State

  var state:State = Warming()
  var recordTime = System.nanoTime()

  def receive = {

    case PingThemAll => {
      for (i <- 0 to PingPongParam.concurrency) {
        pongActor ! Ping(pingCounter.incrementAndGet(), System.nanoTime())
      }
    }

    case Pong(id: Long, tick: Long) => {
      (id, state) match {
        case (id, Recording()) if id <= PingPongParam.maxRequest => {
          store.put(id, (tick, System.nanoTime()))
          pongActor ! Ping(pingCounter.incrementAndGet(), System.nanoTime())
        }
        case (id, Recording()) if id > PingPongParam.maxRequest => {
          state = Waiting()
          // Wait 10 sec after recording and exit
          import scala.concurrent.ExecutionContext.Implicits.global
          context.system.scheduler.scheduleOnce(10.second) {
            println("Start saving")
            pingCounter.set(0)
            self ! Save
          }
        }
        case (id, Warming()) if (id <= PingPongParam.maxRequest + PingPongParam.warmupRequest) => {
          pongActor ! Ping(pingCounter.incrementAndGet(), System.nanoTime())
        }
        case (id, Warming()) if (id > PingPongParam.maxRequest + PingPongParam.warmupRequest) => {
          state = Waiting()
          // Wait 10 sec after warmup and start pinging again
          import scala.concurrent.ExecutionContext.Implicits.global
          context.system.scheduler.scheduleOnce(10.second) {
            println("Start recording")
            state = Recording()
            pingCounter.set(0)
            recordTime = System.nanoTime()
            self ! PingThemAll
          }
        }
        case _ =>
      }
    }

    case Save => {
      import collection.JavaConversions._
      val pw = new java.io.PrintWriter("stat/stat"+PingPongParam.concurrency.toString+".csv")
      pw.println("id,time,latency,concurrency")
      store.ascendingMap() foreach {
        case (k, v) => pw.println(
          k.toString + "," +
          ((v._2-recordTime).toFloat / 1000000000).toString + ","+
          ((v._2 - v._1).toFloat / 1000000).toString+","+
          PingPongParam.concurrency.toString
        )
      }
      pw.close()
      println("Saved")
      context.system.shutdown
    }

  }

}


class PongActor extends Actor {

  def receive = {
    case StartUp => println("Started " + self.toString)
    case Ping(id: Long, tick: Long) =>
      sender ! Pong(id, tick)
  }

}
