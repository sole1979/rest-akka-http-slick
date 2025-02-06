import Main.serviceActor
import ServiceActor.{GetProduct, GetProductsByCategory}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.Directives.{Segment, complete, get, pathEndOrSingleSlash, pathPrefix}
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt
import scala.concurrent.ExecutionContext.Implicits.global
import spray.json._
import akka.http.scaladsl.server.Route
import akka.http.scaladsl.model.{ContentTypes, HttpEntity, HttpResponse, StatusCode, StatusCodes}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._

object ValidationMiddleware {
  def isNameValid(name: String): Boolean = {
    val trimmedName = name.trim
    val nameRegex = "^[a-zA-Zа-яА-Я]+([ '-][a-zA-Zа-яА-Я]+)*$"
    trimmedName.length >= 3 && trimmedName.length <= 50 && trimmedName.matches(nameRegex)
  }
  def isEmailValid(email: String): Boolean = {
    email.contains("@") && !email.contains(" ") && email.nonEmpty
  }
  def isPasswordValid(password: String): Boolean = {
    val invalidChars = Set(' ', '?', '=', '&', '<', '>', '|', '\'', '\\', '"', ';', '{', '}', '^', '~')
    !password.exists(invalidChars.contains) &&
    //!password.contains(" ") &&
    (password.length >= 8) && (password.length <= 64)
  }
}

trait Routes extends ProductJsonProtocol with SprayJsonSupport{
  import ServiceActor._
  //import JsonFormats._
  //-----complete(StatusCodee.Unauthorized)
  implicit val timeout: Timeout = Timeout(5 seconds)
  val routes = {
    post{          //// CHANGE TWO POST!!!!!!!!!
      pathPrefix("login") {
        pathEndOrSingleSlash {
          entity(as[UserAccountLogin]) { loginData =>
            if (loginData.email.trim.isEmpty || loginData.password.trim.isEmpty)
              complete(StatusCodes.BadRequest -> "Email and password must not be empty".toJson)
            else if (!ValidationMiddleware.isEmailValid(loginData.email))
              complete(StatusCodes.BadRequest -> "Invalid email format".toJson)
            else {
              val authResult: Future[Either[String, UserAccountLoginAnswer]] =
                (serviceActor ? AuthenticateUser(loginData.email, loginData.password))
                .mapTo[Either[String, UserAccountLoginAnswer]]
              complete(authResult.map {
                case Right(userLoginAnswer) => StatusCodes.OK -> userLoginAnswer.toJson
                case Left(error) => StatusCodes.Unauthorized -> error.toJson
              })
            }
          }
        }
      }
    } ~
    post{                /// CHANGE TWO POST!!!!!!!!!!!
      pathPrefix("register") {
        pathEndOrSingleSlash {
          entity(as[UserAccountRegister]) { userRequest =>
            if (!ValidationMiddleware.isNameValid(userRequest.name))
              complete(StatusCodes.BadRequest -> "Invalid name format".toJson)
            else if (!ValidationMiddleware.isEmailValid(userRequest.email))
              complete(StatusCodes.BadRequest -> "Invalid email format".toJson)
            else if (!ValidationMiddleware.isPasswordValid(userRequest.password))
              complete(StatusCodes.BadRequest -> "Invalid password format".toJson)
            else {
              val registerResult =
                (serviceActor ? CreateUserAccount(userRequest.name, userRequest.email, userRequest.password))
                .mapTo[Either[String, UserAccountLoginAnswer]]
              complete(registerResult.map{
                case Right(userLoginAnswer) => StatusCodes.OK -> userLoginAnswer.toJson
                case Left(error) => StatusCodes.Unauthorized -> error.toJson
              })
            }
          }
        }
      }
    } ~
    get {
      pathPrefix("categories") {
        pathEndOrSingleSlash {
          val productList = (serviceActor ? GetAllCategories).mapTo[Either[String, Seq[String]]]
          complete(productList.map{
            case Right(products) => StatusCodes.OK -> products.toJson
            case Left(error) => StatusCodes.InternalServerError -> error.toJson
          })
        }
      } ~
      pathPrefix(Segment / Segment) { (category, sku) =>         // Get Product by sku
        pathEndOrSingleSlash {
          val productOptionFuture: Future[Either[String, Option[Product]]]  =
            (serviceActor ? GetProduct(sku)).mapTo[Either[String, Option[Product]]]
          complete(productOptionFuture.map {
            case Right(Some(product)) => StatusCodes.OK -> product.toJson
            case Right(None) => StatusCodes.NotFound -> "Product not found".toJson
            case Left(error) => StatusCodes.InternalServerError -> error.toJson
            })
        }
      } ~
      pathPrefix(Segment) { category => //| parameter('nickname)) { nickname =>
        pathEndOrSingleSlash {
          val productList = (serviceActor ? GetProductsByCategory(category)).mapTo[Either[String, Seq[Product]]]
          complete(productList.map{
            case Right(products) => StatusCodes.OK -> products.toJson
            case Left(error) => StatusCodes.InternalServerError -> error.toJson
          })
        }
      } ~
      pathEndOrSingleSlash {      // get All Product
        val productList = (serviceActor ? GetAllProducts).mapTo[Either[String, Seq[Product]]]
        complete(productList.map{
          case Right(products) => StatusCodes.OK -> products.toJson
          case Left(error) => StatusCodes.InternalServerError -> error.toJson
        })
      }
    }
  }
}

