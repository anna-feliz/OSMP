package tcptestserver
import akka.io.{ IO, Tcp }
import akka.actor.{ Actor, ActorRef }
import akka.util.ByteString

class TestConn extends Actor {
  import Tcp._
  var conn = self
  def receive = {
    case Received(data) => {
      conn = sender
      println("Received data")
      context.parent ! Print(data.decodeString(java.nio.charset.Charset.defaultCharset().name()))
    }
    case Send(msg) => {
      val send = ByteString(msg)
      conn ! Write(send)
    }
    case CommandFailed(_: Received) => {
      println("Failed to receive data!")
    }
    case _: ConnectionClosed => {
      context.parent ! ConnClosed
    }
  }
}