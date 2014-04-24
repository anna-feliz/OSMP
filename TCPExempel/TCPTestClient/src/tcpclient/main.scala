package tcpclient
import akka.actor.{ Actor, ActorRef, ActorSystem, Props, Terminated }
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Await
import java.util.concurrent.TimeUnit
object Main {
  val system = ActorSystem("TCPTestClient")
  var Running = true
  object Watcher {
    def props(toWatch: ActorRef): Props = { Props(new Watcher(toWatch)) }
  }
  class Watcher(toWatch: ActorRef) extends Actor {
    override def preStart() = { context.watch(toWatch) }
    def receive() = {
      case Terminated(actor) =>
        Running = false
    }
  }
  def main(args: Array[String]) {
    var name = Console.readLine("Enter your name: ")
    println("Starting client actor...")
    val client = system.actorOf(TCPClient.props(name))
    var input = ""
    val watcher = system.actorOf(Watcher.props(client))
    while (Running) {
      input = Console.readLine("Enter \"Quit\" to exit > ")
      if (Running) {
        if (input.toLowerCase() == "quit") {
          client ! ShutDown
        } else {
          client ! Print(input)
        }
      }
    }
    system.shutdown()
  }
}