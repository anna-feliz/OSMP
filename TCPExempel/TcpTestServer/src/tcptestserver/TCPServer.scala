package tcptestserver
import akka.io.{ IO, Tcp }
import akka.actor.{ Actor, ActorRef, Props }
import akka.util.Timeout
import akka.pattern.ask
import akka.util.ByteString
import scala.concurrent.Await
import java.net.InetSocketAddress
import scala.concurrent.duration._

sealed trait MyMsg
case class Print(msg: String) extends MyMsg
case class Send(msg: String) extends MyMsg
case object ConnClosed extends MyMsg
case object ShutDown extends MyMsg
case object Ok extends MyMsg
class TCPServer extends Actor {
  import context.system
  import Tcp._
  val manager = IO(Tcp)
  var Connections: Set[ActorRef] = Set()
  var SocketActor: Option[ActorRef] = None
  override def preStart() = {
    manager ! Bind(self, new InetSocketAddress("localhost", 1337))
  }
  def receive = {
    case ShutDown =>
      Connections foreach (c => c ! Close)
      implicit val timeout = Timeout(10.seconds)
      println("Unbinding...")
      val future = ask(SocketActor.get, Unbind)
      Await.result(future, timeout.duration)
      println("Unbound socket")
      sender ! Ok
      context stop self
    case Print(msg) =>
      println("Connection sent \"%s\"".format(msg))
      Connections foreach (c => if (c != sender){ c ! Send("Print " + msg) })
    case b @ Bound(inetAddr) => {
      println("\nBound to " + inetAddr)
      SocketActor = Some(sender)
    }
    case CommandFailed(_: Bind) => {
      println("Failed to bind!")
      context stop self
    }

    case CommandFailed(_: Write) => {
      println("Failed to write")
    }

    case ConnClosed => {
      Connections -= sender
      context stop sender
    }

    case c @ Connected(remote, local) =>
      val actor = context.actorOf(Props[TestConn])
      Connections += actor
      sender ! Register(actor)
    case _ =>
      println("Unhandled Message!")
  }
}