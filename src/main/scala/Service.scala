import akka.actor.{Actor, ActorLogging}
import akka.pattern.pipe
import com.typesafe.config.ConfigFactory

import java.security.SecureRandom
import java.util.Base64
import de.mkammerer.argon2.Argon2Factory

import java.time.LocalDateTime
//import pdi.jwt._
//import pdi.jwt.spray.JwtSprayJson
import pdi.jwt.{JwtAlgorithm, JwtClaim, JwtSprayJson}
import spray.json._
import DefaultJsonProtocol._
import pdi.jwt._
import scala.concurrent.duration._

import java.util.concurrent.TimeUnit
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import java.util.UUID

object PasswordUtils {
  private val argon2 = Argon2Factory.create()
  // generate salt
  def generateSalt(length: Int = 16): String = {
    //Random.alphanumeric.take(length).mkString
    val random = new SecureRandom()
    val salt = new Array[Byte](length)
    random.nextBytes(salt)
    Base64.getEncoder.encodeToString(salt)
  }
  // hashing password
  def hashPassword(password: String, salt: String): String ={
    val combined = (salt + ":" + password).toCharArray
    try argon2.hash(10, 65536, 1, combined)
    finally java.util.Arrays.fill(combined, '\u0000')
  }
  // verify password
  def verifyPassword(hash: String, passwordVerify: String, salt: String): Boolean = {
    val combined = (salt + ":" + passwordVerify).toCharArray
    try argon2.verify(hash, combined)
    finally java.util.Arrays.fill(combined, '\u0000')
  }

  // Tokens part
  val algorithm = JwtAlgorithm.HS256
  val config = ConfigFactory.load()
  val secretKey = "okssishop" //config.getString("jwt.secret")

  def createToken(userId: Int, username: String, expirationPeriodInHours: Int): String = {
    val claims = JwtClaim(
      expiration = Some(System.currentTimeMillis() / 1000 + TimeUnit.HOURS.toSeconds(expirationPeriodInHours)),
      issuedAt = Some(System.currentTimeMillis() / 1000),
      content = s"""{"id":$userId,"name":"$username"}"""
    )
    JwtSprayJson.encode(claims, secretKey, algorithm)
  }

  def generateTokens(userId: Int, username: String): TokensAnswer =
    TokensAnswer(username, createToken(userId, username, 1), createToken(userId, username, 720))

  def extractUsername(token: String): Option[String] =
    JwtSprayJson.decode(token, secretKey, Seq(algorithm)).toOption.flatMap { claim =>
      claim.content.parseJson.asJsObject.fields.get("name").map(_.convertTo[String])
    }

  def validateToken(token: String): Option[JwtClaim] = {
    val decoded = JwtSprayJson.decode(token, secretKey, Seq(algorithm))//.toOption
      //println(s"+++Decoded result: $decoded")
    val maybeClaim = decoded.toOption
     // println(s"++++maybe claim: $maybeClaim")
       maybeClaim.filter(_.expiration.exists(_ > System.currentTimeMillis() / 1000))
  } // check work period

  def validateAndExtractUserInfo(token: String): Option[(Int, String)] = {
    //println(s"-----INPUT token: $token")
    validateToken(token).flatMap { claim =>
      val fields = claim.content.parseJson.asJsObject.fields
      //println(s"------fields: $fields")
      for {
        id <- fields.get("id").map(_.convertTo[Int])
        name <- fields.get("name").map(_.convertTo[String])
      } yield {/*println(s"---elements From Token: ${id}, ${name}");*/(id, name)}
    }
  }
}

object ServiceActor {
  case object GetAllProducts
  case object GetAllCategories
  case class GetProduct(sku: String)
  case class GetProductsByCategory(category: String)
  case class GetProductsBySku(skus: List[String])
  case class GetUserById(id: Int)
  case class AuthenticateUser(email: String, passVerify: String)
  case class CreateUserAccount(name: String, email: String, password: String)
  case class RefreshTokens(refreshToken: String)
  case class GetFavoritesByUserId(userId: Int)
  case class AddFavorite(userId: Int, productSku: String)
  case class DeleteFavorite(userId: Int, productSku: String)
  case class AddOrderWithItems(request: OrderRequest)

  //case object OperationSuccess
}

class ServiceActor extends Actor with ActorLogging with RepositorySlickImpl {
  import ServiceActor._
  import PasswordUtils._

  override def receive: Receive = {
    case GetAllProducts =>
      log.info("Getting all products")
      val replyTo = sender()
      findAllProducts().pipeTo(replyTo)

    case GetProduct(sku) =>
      log.info(s"Getting product with SKU $sku")
      val replyTo = sender()
      findProduct(sku).pipeTo(replyTo)

    case GetAllCategories =>
      log.info("Getting all Categories")
      val replyTo = sender()
      findUniqueCategories().pipeTo(replyTo)

    case GetProductsByCategory(category) =>
      log.info(s"Getting products from category $category")
      val replyTo = sender()
      findAllByCategory(category).pipeTo(replyTo)

    case GetProductsBySku(skus) =>
      log.info(s"Getting products from Sku's ${skus.mkString(";")}")
      val replyTo = sender()
      findProductsBySku(skus).pipeTo(replyTo)  

    //-----SEQURITY-------
    case GetUserById(id) =>
      log.info(s"Find User by Id: $id")
      val replyTo = sender()
      val publicUserRespond: Future[Either[String, PublicUser]]= findUserAccountById(id).map {
        case Right(Some(user)) =>
          val publicUser = PublicUser(
            id = user.id.get,
            name = user.name,
            email = user.email,
            phone = user.phone
          )
          Right(publicUser)
        case Right(None) => Left(s"User id=$id not found")
        case Left(error) => Left(s"DB error $error")
      }
      publicUserRespond.pipeTo(replyTo)

    case CreateUserAccount(name, email, password) =>
      log.info(s"Creating User $name, $email, $password ")
      val replyTo = sender()
      val registerUser:  Future[Either[String, TokensAnswer]] =
        isEmailExist(email).flatMap {
          case Left(error) => Future.successful(Left(s"DB error: $error"))
          case Right(true) => Future.successful(Left("Email already exists"))
          case Right(false) =>
            val salt = PasswordUtils.generateSalt()
            val hashedPassword =  PasswordUtils.hashPassword(password, salt)
            val userAccount = UserAccount(
              id = None,
              name = name,
              email = email,
              phone = None,
              password = hashedPassword,
              salt = salt,
              created_at = java.time.LocalDateTime.now
            )
            insertUserAccount(userAccount).map {
              case Left(error) => Left(s"DB error: $error")
              case Right(userAccount) =>
                //Right(UserAccountLoginAnswer(userAccount.name, PasswordUtils.createToken(userAccount.name, 1)))
                Right(generateTokens(userAccount.id.get, userAccount.name))
            }
        }
      registerUser.pipeTo(replyTo)

    case AuthenticateUser(email, passVerify) =>
      log.info(s"Get User with email $email")
      val replyTo = sender()
      val userAccount: Future[Either[String, TokensAnswer]] = findUserAccount(email).map {
        case Right(None) => Left("User doesn't exist")
        case Right(Some(user)) =>
          if (PasswordUtils.verifyPassword(user.password, passVerify, user.salt)) {
            //Right(UserAccountLoginAnswer(user.name, PasswordUtils.createToken(user.name, 1)))
            Right(generateTokens(user.id.get, user.name))
          } else Left("Password Incorrect")
        case Left(error) => Left(s"DB error: $error")
      }
      userAccount.pipeTo(replyTo)

    case RefreshTokens(currentRefreshToken) =>
      log.info(s"Refreshing Tokens")
      val replyTo = sender()
      Future {
        validateAndExtractUserInfo(currentRefreshToken)
          .fold[Either[String, TokensAnswer]](Left("Token overdue")) {
            case (userId, username) => Right(generateTokens(userId, username))
          }
      }.pipeTo(replyTo)

    //------FAVORITE--------
    case AddFavorite(userId, productSku) =>
      log.info(s"Creating Favorite $userId, $productSku ")
      val replyTo = sender()
      val storeFavorite:  Future[Either[String, List[Product]]] =
        isFavoriteExist(userId, productSku).flatMap {
          case Left(error) => Future.successful(Left(s"DB error: $error"))
          case Right(true) => getFavorites(userId)
          case Right(false) => insertFavorite(userId, productSku).flatMap {
            case Left(err) => Future.successful(Left(s"DB error: $err"))
            case Right(_) => getFavorites(userId)
          }
        }
      storeFavorite.pipeTo(replyTo)

    case DeleteFavorite(userId, productSku) =>
      log.info(s"Deleting Favorite $userId, $productSku ")
      val replyTo = sender()
      val delFavorite:  Future[Either[String, List[Product]]] =
        isFavoriteExist(userId, productSku).flatMap {
          case Left(error) => Future.successful(Left(s"DB error: $error"))
          case Right(false) => getFavorites(userId)
          case Right(true) => removeFavorite(userId, productSku).flatMap {
            case Left(err) => Future.successful(Left(s"DB error: $err"))
            case Right(_) => getFavorites(userId)
          }
        }
      delFavorite.pipeTo(replyTo)

    case GetFavoritesByUserId(userId) =>
      log.info(s"Get Favorites By UserId: $userId")
      val replyTo = sender()
      getFavorites(userId).map {
          case Right(products) => Right(products)
          case Left(error) => Left(s"DB error: $error")
      }.pipeTo(replyTo)
//------- ORDER------
    case AddOrderWithItems(request) =>
      log.info(s"Add Order With Items")
      val replyTo = sender()
      def createOrder(request: OrderRequest): Future[Either[String, UUID]] = {
        val orderId = UUID.randomUUID()
        val order = Order(
          id = orderId,
          createdAt = LocalDateTime.now(),
          userId = request.userId,
          name = request.name,
          phone = request.phone,
          email = request.email,
          status = request.status
        )
        val items = request.items.map { item =>
          OrderItem(
            orderItemId = UUID.randomUUID(),
            orderId = orderId,
            productSku = item.productSku,
            quantity = item.quantity,
            priceProduct = item.price
          )
        }
        insertOrderWithItems(order, items)
      }
      createOrder(request).map {
        case Right(uuid) => Right(uuid)
        case Left(error) => Left(s"DB error: $error")
      }.pipeTo(replyTo)

  }
}