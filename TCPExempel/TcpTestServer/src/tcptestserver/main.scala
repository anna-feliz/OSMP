package tcptestserver
import akka.actor.{ Actor, ActorRef, ActorSystem, Props, Terminated }
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.concurrent.Await
object main {
  val system = ActorSystem("TCPTestServer")
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
    println("Starting server actor...")
    val server = system.actorOf(Props[TCPServer])
    var input = ""
    val watcher = system.actorOf(Watcher.props(server))
    while (Running) {
      input = Console.readLine("Enter \"Quit\" to exit > ")
      if (Running) {
        if (input.toLowerCase() == "quit") {
          server ! ShutDown
        }
      }
    }
    system.shutdown()
  }
}
