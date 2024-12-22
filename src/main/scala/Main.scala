
import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport

import akka.stream.ActorMaterializer

import spray.json._


trait ProductJsonProtocol extends DefaultJsonProtocol {
  implicit val productFormat = jsonFormat6(Product)
}

object Main extends App with Routes with ProductJsonProtocol with SprayJsonSupport {
  implicit val system = ActorSystem("ProductShop")
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  val productServiceActor = system.actorOf(Props[ProductServiceActor], "productServiceActor")

  Http().bindAndHandle(routes, "localhost", 8080)

}
