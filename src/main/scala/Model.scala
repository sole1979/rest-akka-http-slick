import java.time.LocalDateTime
import slick.jdbc.PostgresProfile.api._

case class Product(sku: String, category: String, name: String, price: BigDecimal, descr: String, srcImg: String)
case class UserAccount(id: Option[Int], name: String, email: String, phone: Option[String], password: String, salt: String, created_at: LocalDateTime)
case class UserAccountRegister(name: String, email: String, password: String)
case class UserAccountLogin(email: String, password: String)
case class UserAccountLoginAnswer(name: String, token: String)
case class TokensAnswer(username: String, accessToken: String, refreshToken: String)
case class Answer(message: String)
case class Favorite(userId: Int, productSku: String)
case class FavoriteRequest(productSku: String)


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

  class FavoritesTable(tag: Tag) extends Table[Favorite](tag, Some("shop"), "Favorites") {
    def userId = column[Int]("user_id")
    def productSku = column[String]("product_sku")

    def pk = primaryKey("pk_favorites", (userId, productSku))

    override def * = (userId, productSku) <> (Favorite.tupled, Favorite.unapply)

    def user = foreignKey("fk_user", userId, TableQuery[UserAccountTable])(_.id, onDelete = ForeignKeyAction.Cascade)
    def product = foreignKey("fk_product", productSku, TableQuery[ProductTable])(_.sku, onDelete = ForeignKeyAction.Cascade)
  }
  lazy val favoritesTable = TableQuery[FavoritesTable]

}

//var products = Map(
//  "one" -> Product("one", "outewear", "Coat", 100, "Goood Coat", "/img/kurtka1.jpg"),
//  "two" -> Product("two", "outewear", "Jacket", 200, "Amazing Jacket", "/img/kurtka2.jpg"),
//  "three" -> Product("three", "outewear", "Bikini", 300, "Crazy Bikini", "/img/kurtka3.jpg"),
//  "for" -> Product("for", "outewear", "Stocking1", 350, "Amazing super sexy stocking", "/img/kurtka4.jpg"),
//  "five" -> Product("five", "outewear", "Stocking2", 450, "Best stocking", "/img/kurtka5.jpg"),
//  "six" -> Product("six", "outewear", "Stocking3", 550, "Crazy stocking", "/img/kurtka6.jpg")
//)
