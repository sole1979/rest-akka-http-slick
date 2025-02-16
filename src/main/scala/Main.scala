
import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.stream.ActorMaterializer
import spray.json._


trait ProductJsonProtocol extends DefaultJsonProtocol {
  implicit val productFormat /*: RootJsonFormat[Product]*/ = jsonFormat6(Product)
  implicit val userAccountRegisterFormat = jsonFormat3(UserAccountRegister)
  implicit val userAccountLoginFormat = jsonFormat2(UserAccountLogin)
  implicit val answerFormat = jsonFormat1(Answer)
  implicit val userAccountLoginAnswer = jsonFormat2(UserAccountLoginAnswer)
}

object Main extends App with Routes with ProductJsonProtocol with SprayJsonSupport {
  implicit val system = ActorSystem("ProductShop")
  //system.eventStream.setLogLevel(Logging.WarningLevel)
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  val serviceActor = system.actorOf(Props[ServiceActor], "serviceActor")

  Http().bindAndHandle(routes, "localhost", 8080)

}
