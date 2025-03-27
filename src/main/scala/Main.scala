
import akka.actor.{Actor, ActorLogging, ActorSystem, Props}
import akka.event.Logging
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.stream.ActorMaterializer
import spray.json._

import java.util.UUID
import java.time.LocalDateTime
import java.time.format.DateTimeFormatter



trait ProductJsonProtocol extends DefaultJsonProtocol {

  implicit object UUIDFormat extends JsonFormat[UUID] {
    def write(uuid: UUID): JsValue = JsString(uuid.toString)
    def read(json: JsValue): UUID = json match {
      case JsString(str) => UUID.fromString(str)
      case _ => throw new DeserializationException("Expected UUID as JsString")
    }
  }

  implicit object LocalDateTimeFormat extends JsonFormat[LocalDateTime] {
    private val formatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME
    def write(dt: LocalDateTime): JsValue = JsString(dt.format(formatter))
    def read(json: JsValue): LocalDateTime = json match {
      case JsString(str) => LocalDateTime.parse(str, formatter)
      case _ => throw new DeserializationException("Expected LocalDateTime as JsString")
    }
  }

  implicit val productFormat /*: RootJsonFormat[Product]*/ = jsonFormat6(Product)
  implicit val userAccountRegisterFormat = jsonFormat3(UserAccountRegister)
  implicit val userAccountLoginFormat = jsonFormat2(UserAccountLogin)
  implicit val answerFormat = jsonFormat1(Answer)
  implicit val userAccountLoginAnswerFormat = jsonFormat2(UserAccountLoginAnswer)
  implicit val favoriteFormat = jsonFormat2(Favorite)
  implicit val favoriteRequestFormat = jsonFormat1(FavoriteRequest)
  implicit val orderFormat = jsonFormat7(Order)
  implicit val publicUserFormat = jsonFormat4(PublicUser)
  implicit val orderItemRequestFormat = jsonFormat3(OrderItemRequest)
  implicit val orderRequestFormat = jsonFormat6(OrderRequest)


  override implicit def optionFormat[T: JsonFormat]: JsonFormat[Option[T]] = new JsonFormat[Option[T]] {
    def write(option: Option[T]): JsValue = option match {
      case Some(value) => value.toJson
      case None => JsNull
    }
    def read(json: JsValue): Option[T] = json match {
      case JsNull => None
      case otherValue => Some(otherValue.convertTo[T])
    }
  }
}

object Main extends App with Routes with ProductJsonProtocol with SprayJsonSupport {
  implicit val system = ActorSystem("ProductShop")
  //system.eventStream.setLogLevel(Logging.WarningLevel)
  implicit val materializer = ActorMaterializer()
  import system.dispatcher

  val serviceActor = system.actorOf(Props[ServiceActor], "serviceActor")

  Http().bindAndHandle(routes, "localhost", 8080)

}
