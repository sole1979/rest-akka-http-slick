import akka.actor.{Actor, ActorLogging}
import akka.pattern.pipe
import com.typesafe.config.ConfigFactory

import java.security.SecureRandom
import java.util.Base64
import de.mkammerer.argon2.Argon2Factory
//import pdi.jwt._
//import pdi.jwt.spray.JwtSprayJson
import pdi.jwt.{JwtAlgorithm, JwtClaim, JwtSprayJson}
import spray.json._
import pdi.jwt._
import scala.concurrent.duration._

import java.util.concurrent.TimeUnit
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

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

  val algorithm = JwtAlgorithm.HS256
  val config = ConfigFactory.load()
  val secretKey = "okssishop" //config.getString("jwt.secret")
  def createToken(username: String, expirationPeriodInHours: Int): String = {
    val claims = JwtClaim(
      expiration = Some(System.currentTimeMillis() / 1000 + TimeUnit.HOURS.toSeconds(expirationPeriodInHours)),
      issuedAt = Some(System.currentTimeMillis() / 1000),
      content = s"""{"name":"$username"}"""
    )//.withClaim("name", username)

    JwtSprayJson.encode(claims, secretKey, algorithm)
  }
}

object ServiceActor {
  case object GetAllProducts
  case object GetAllCategories
  case class GetProduct(sku: String)
  case class GetProductsByCategory(category: String)
  //case class AddProduct(product: Product)
  case class AuthenticateUser(email: String, passVerify: String)
  case class CreateUserAccount(name: String, email: String, password: String)
  case object OperationSuccess
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

    case CreateUserAccount(name, email, password) =>
      log.info(s"Creating User $name, $email, $password ")
      val replyTo = sender()
      val registerUser:  Future[Either[String, UserAccountLoginAnswer]] =
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
                Right(UserAccountLoginAnswer(userAccount.name, PasswordUtils.createToken(userAccount.name, 1)))
            }
        }
      registerUser.pipeTo(replyTo)

    case AuthenticateUser(email, passVerify) =>
      log.info(s"Get User with email $email")
      val replyTo = sender()
      val userAccount: Future[Either[String, UserAccountLoginAnswer]] = findUserAccount(email).map {
        case Right(None) => Left("User doesn't exist")
        case Right(Some(user)) =>
          if (PasswordUtils.verifyPassword(user.password, passVerify, user.salt)) {
            Right(UserAccountLoginAnswer(user.name, PasswordUtils.createToken(user.name, 1)))
          } else Left("Password Incorrect")
        case Left(error) => Left(s"DB error: $error")
      }
      userAccount.pipeTo(replyTo)

  }
}