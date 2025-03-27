import SlickTables.OrderTable

import java.time.LocalDateTime
import slick.jdbc.PostgresProfile.api._
//import slick.lifted.MappedToBase.mappedToIsomorphism

import java.util.UUID
import spray.json._
import slick.jdbc.JdbcType
import slick.ast.BaseTypedType


case class Product(sku: String, category: String, name: String, price: BigDecimal, descr: String, srcImg: String)
case class UserAccount(id: Option[Int], name: String, email: String, phone: Option[String], password: String, salt: String, created_at: LocalDateTime)
case class UserAccountRegister(name: String, email: String, password: String)
case class UserAccountLogin(email: String, password: String)
case class UserAccountLoginAnswer(name: String, token: String)
case class PublicUser(id: Int, name: String, email: String, phone: Option[String])
case class TokensAnswer(username: String, accessToken: String, refreshToken: String)
case class Answer(message: String)
case class Favorite(userId: Int, productSku: String)
case class FavoriteRequest(productSku: String)
case class Order(id: UUID, createdAt: LocalDateTime, userId: Option[Int], name: String, phone: String, email: String, status: String)
case class OrderItem(orderItemId: UUID, orderId: UUID, productSku: String, quantity: Int, priceProduct: BigDecimal)
case class OrderRequest(userId: Option[Int], name: String, phone: String, email: String, status: String, items: List[OrderItemRequest])
case class OrderItemRequest(productSku: String, quantity: Int, price: BigDecimal)

//import CustomJsonFormats._
//object PublicUser extends DefaultJsonProtocol {
 // implicit val publicUserFormat: RootJsonFormat[PublicUser] = jsonFormat4(PublicUser.apply)
//}


object SlickTables {
  import slick.jdbc.PostgresProfile.api._

  class ProductTable(tag: Tag) extends Table[Product](tag,  Some("shop"), "Products") {
    def sku = column[String]("sku", O.PrimaryKey)
    def category = column[String]("category")
    def name = column[String]("name")
    def price = column[BigDecimal]("price")
    def descr = column[String]("description")
    def srcImg = column[String]("urlimg")
    override def * = (sku, category, name, price, descr, srcImg) <> (Product.tupled, Product.unapply)
  }
  lazy val productTable = TableQuery[ProductTable]

  class UserAccountTable(tag: Tag) extends Table[UserAccount](tag,  Some("shop"), "UserAccounts") {
    def id = column[Int]("id", O.PrimaryKey, O.AutoInc)
    def name = column[String]("name")
    def email = column[String]("email")
    def phone = column[Option[String]]("phone")
    def password = column[String]("password")
    def salt = column[String]("salt")
    def created_at = column[LocalDateTime]("created_at")
    override def * = (id.?, name, email, phone, password, salt, created_at) <> (UserAccount.tupled, UserAccount.unapply)
  }
  lazy val userAccountTable = TableQuery[UserAccountTable]

  class FavoriteTable(tag: Tag) extends Table[Favorite](tag, Some("shop"), "Favorites") {
    def userId = column[Int]("user_id")
    def productSku = column[String]("product_sku")

    def pk = primaryKey("pk_favorites", (userId, productSku))

    override def * = (userId, productSku) <> (Favorite.tupled, Favorite.unapply)

    def user = foreignKey("fk_user", userId, TableQuery[UserAccountTable])(_.id, onDelete = ForeignKeyAction.Cascade)
    def product = foreignKey("fk_product", productSku, TableQuery[ProductTable])(_.sku, onDelete = ForeignKeyAction.Cascade)
  }
  lazy val favoriteTable = TableQuery[FavoriteTable]

  class OrderTable(tag: Tag) extends Table[Order](tag,  Some("shop"), "Orders") {
    import OrderTable._
    def id = column[UUID]("id", O.PrimaryKey)
    def createdAt = column[LocalDateTime]("created_at")
    def userId = column[Option[Int]]("user_id")
    def name = column[String]("name")
    def phone = column[String]("phone")
    def email = column[String]("email")
    //def products = column[JsValue]("products", O.SqlType("jsonb"))
    def status = column[String]("status")

    def userFk = foreignKey("fk_user", userId, TableQuery[UserAccountTable])(_.id.?, onDelete = ForeignKeyAction.SetNull)

    override def * = (id, createdAt, userId, name, phone, email,/* products,*/ status) <> (Order.tupled, Order.unapply)
    }
  lazy val orderTable = TableQuery[OrderTable]

  class OrderItemTable(tag: Tag) extends Table[OrderItem](tag, Some("shop"), "OrderItems") {
    def orderItemId = column[UUID]("order_item_id", O.PrimaryKey)
    def orderId = column[UUID]("order_id")
    def productSku = column[String]("product_sku")
    def quantity = column[Int]("quantity")
    def priceProduct = column[BigDecimal]("price")

    def orderFk = foreignKey("order_fk", orderId, TableQuery[OrderTable])(_.id, onDelete = ForeignKeyAction.Cascade)
    def productFk = foreignKey("product_fk", productSku, TableQuery[ProductTable])(_.sku, onDelete = ForeignKeyAction.Cascade)

    override def * = (orderItemId, orderId, productSku, quantity, priceProduct) <> (OrderItem.tupled, OrderItem.unapply)
  }
  lazy val orderItemTable = TableQuery[OrderItemTable]
}

object OrderTable {
  implicit val uuidMapper = MappedColumnType.base[UUID, String](_.toString, UUID.fromString)
}
 //import spray.json._

//implicit val uuidMapper: JdbcType[UUID] with BaseTypedType[UUID] = MappedColumnType.base[UUID, String](_.toString, UUID.fromString)
//implicit val uuidMapper = MappedColumnType.base[UUID, String](_.toString, UUID.fromString)
//implicit val localDateTimeMapper = MappedColumnType.base[LocalDateTime, java.sql.Timestamp](java.sql.Timestamp.valueOf, _.toLocalDateTime)

