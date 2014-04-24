package lostvaults
import akka.actor.{ Actor, ActorRef }
import scala.collection.mutable.HashMap

sealed trait PMapMsg
case class PMapAddPlayer(name: String, ref: ActorRef) extends PMapMsg
case class PMapRemovePlayer(name: String) extends PMapMsg
case class PMapGetPlayer(name: String) extends PMapMsg
case class PMapGetPlayerResponse(player: Option[ActorRef]) extends PMapMsg
case class PMapIsOnline(name: String) extends PMapMsg
case class PMapIsOnlineResponse(online: Boolean) extends PMapMsg
case object PMapSuccess extends PMapMsg
case object PMapFailure extends PMapMsg

object PlayerMap extends Actor {
  var PMap: HashMap[String, ActorRef] = HashMap()

  def receive() = {
    case PMapAddPlayer(name: String, ref: ActorRef) => {
      val exist = PMap.find((A: Tuple2[String, ActorRef]) => A._1 == name)
      if (exist.isEmpty) {
        PMap += Tuple2[String, ActorRef](name, ref)
        sender ! PMapSuccess
      } else {
        sender ! PMapFailure
      }
    }
    case PMapRemovePlayer(name: String) => {
      val exist = PMap.find((A: Tuple2[String, ActorRef]) => A._1 == name)
      if (exist.isEmpty) {
        sender ! PMapFailure
      } else {
        PMap -= name
        sender ! PMapSuccess
      }
    }
    case PMapGetPlayer(name: String) => {
      val exist = PMap.find((A: Tuple2[String, ActorRef]) => A._1 == name)
      if (exist.isEmpty)
        sender ! PMapGetPlayerResponse(None)
      else
        sender ! PMapGetPlayerResponse(Some((exist.get)._2))
    }
    case PMapIsOnline(name: String) => {
      val exist = PMap.find((A: Tuple2[String, ActorRef]) => A._1 == name)
      sender ! PMapIsOnlineResponse(!exist.isEmpty)
    }
  }
}