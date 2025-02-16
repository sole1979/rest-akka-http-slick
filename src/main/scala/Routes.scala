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
//import akka.http.scaladsl.model.headers.{HttpCookie, RawHeader, `Set-Cookie`}
import akka.http.scaladsl.model.headers._
//import akka.http.scaladsl.model.headers.SameSite
//import akka.http.scaladsl.model.headers.HttpCookie.SameSite

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
              val authResult: Future[Either[String, TokensAnswer]] =
                (serviceActor ? AuthenticateUser(loginData.email, loginData.password))
                  .mapTo[Either[String, TokensAnswer]]
                  .recover { case _ => Left("Internal server error")}
              onSuccess(authResult) {
                case Right(tokensAnswer) =>
                  val refreshCookie = HttpCookie(
                    name = "refreshToken",
                    value = tokensAnswer.refreshToken,
                    //httpOnly = false, //true,
                    secure = true,
                    maxAge = Some (30 * 24 * 60 * 60)  // 30 day
                  )
                  val cookieHeader = RawHeader(
                    "Set-Cookie",
                    s"refreshToken=${refreshCookie.value}; Secure; Max-Age=${refreshCookie.maxAge.get}; SameSite=Lax; HttpOnly=true"
                  )
                  respondWithHeader(cookieHeader) {
                  //respondWithHeaders(`Set-Cookie` (refreshCookie)) {
                    complete(StatusCodes.OK -> UserAccountLoginAnswer(tokensAnswer.username, tokensAnswer.accessToken).toJson)
                  }
                case Left(error) => complete(StatusCodes.Unauthorized -> error.toJson)
              }
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
                  .mapTo[Either[String, TokensAnswer]]
                  .recover { case _ => Left("Internal server error")}
              onSuccess(registerResult) {
                case Right(tokensAnswer) =>
                  val refreshCookie = HttpCookie(
                    name = "refreshToken",
                    value = tokensAnswer.refreshToken,
                   // httpOnly = true,
                    secure = true,
                    maxAge = Some (30 * 24 * 60 * 60)  // 30 day
                  )
                  val cookieHeader = RawHeader(
                    "Set-Cookie",
                    s"refreshToken=${refreshCookie.value}; Secure; Max-Age=${refreshCookie.maxAge.get}; SameSite=Lax; HttpOnly=true"
                  )
                  respondWithHeader(cookieHeader) {
                  //respondWithHeaders(`Set-Cookie` (refreshCookie)) {
                    complete(StatusCodes.OK -> UserAccountLoginAnswer(tokensAnswer.username, tokensAnswer.accessToken).toJson)
                  }
                case Left(error) => complete(StatusCodes.Unauthorized -> error.toJson)
              }
            }
          }
        }
      }
    } ~
    post {
      pathPrefix("logout") {
        pathEndOrSingleSlash {
          respondWithHeaders(`Set-Cookie` (HttpCookie("refreshToken", value = "", maxAge = Some(0)))) {
            complete("Logged out")
          }
        }
      }
    } ~
    post {
      pathPrefix("refresh") {
        pathEndOrSingleSlash {
          optionalCookie("refreshToken") {
            case Some(cookie) =>
              val refreshToken = cookie.value
              val refreshResult: Future[Either[String, TokensAnswer]] =
                (serviceActor ? RefreshTokens(refreshToken))
                  .mapTo[Either[String, TokensAnswer]]
                  .recover { case _ => Left("Internal server error")}
              onSuccess(refreshResult) {
                case Right(tokensAnswer) =>
                  val refreshCookie = HttpCookie(
                    name = "refreshToken",
                    value = tokensAnswer.refreshToken,
                    //httpOnly = false, //true,
                    secure = true,
                    maxAge = Some(30 * 24 * 60 * 60)
                  )
                  val cookieHeader = RawHeader(
                    "Set-Cookie",
                    s"refreshToken=${refreshCookie.value}; Secure; Max-Age=${refreshCookie.maxAge.get}; SameSite=Lax; HttpOnly=true"
                  )
                  respondWithHeader(cookieHeader) {
                  //respondWithHeader(RawHeader("Set-Cookie", refreshCookie.toString)) {
                    complete(StatusCodes.OK -> UserAccountLoginAnswer(tokensAnswer.username, tokensAnswer.accessToken).toJson)
                  }
                case Left(error) => complete(StatusCodes.Unauthorized -> error)
              }
            case None => complete(StatusCodes.Unauthorized -> "Missing refresh token")
          }
        }
      }
    } ~
    get {
      pathPrefix("categories") {
        pathEndOrSingleSlash {
          val productList = (serviceActor ? GetAllCategories)
            .mapTo[Either[String, Seq[String]]]
            .recover { case _ => Left("Internal server error")}
          complete(productList.map{
            case Right(products) => StatusCodes.OK -> products.toJson
            case Left(error) => StatusCodes.InternalServerError -> error.toJson
          })
        }
      } ~
      pathPrefix(Segment / Segment) { (category, sku) =>         // Get Product by sku
        pathEndOrSingleSlash {
          val productOptionFuture: Future[Either[String, Option[Product]]]  =
            (serviceActor ? GetProduct(sku))
              .mapTo[Either[String, Option[Product]]]
              .recover { case _ => Left("Internal server error")}
          complete(productOptionFuture.map {
            case Right(Some(product)) => StatusCodes.OK -> product.toJson
            case Right(None) => StatusCodes.NotFound -> "Product not found".toJson
            case Left(error) => StatusCodes.InternalServerError -> error.toJson
            })
        }
      } ~
      pathPrefix(Segment) { category => //| parameter('nickname)) { nickname =>
        pathEndOrSingleSlash {
          val productList = (serviceActor ? GetProductsByCategory(category))
            .mapTo[Either[String, Seq[Product]]]
            .recover { case _ => Left("Internal server error")}
          complete(productList.map{
            case Right(products) => StatusCodes.OK -> products.toJson
            case Left(error) => StatusCodes.InternalServerError -> error.toJson
          })
        }
      } ~
      pathEndOrSingleSlash {      // get All Product
        val productList = (serviceActor ? GetAllProducts)
          .mapTo[Either[String, Seq[Product]]]
          .recover { case _ => Left("Internal server error")}
        complete(productList.map{
          case Right(products) => StatusCodes.OK -> products.toJson
          case Left(error) => StatusCodes.InternalServerError -> error.toJson
        })
      }
    }
  }
}

