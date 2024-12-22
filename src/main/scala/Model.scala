
case class Product(sku: String, category: String, name: String, price: BigDecimal, descr: String, srcImg: String)


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

}

//var products = Map(
//  "one" -> Product("one", "outewear", "Coat", 100, "Goood Coat", "/img/kurtka1.jpg"),
//  "two" -> Product("two", "outewear", "Jacket", 200, "Amazing Jacket", "/img/kurtka2.jpg"),
//  "three" -> Product("three", "outewear", "Bikini", 300, "Crazy Bikini", "/img/kurtka3.jpg"),
//  "for" -> Product("for", "outewear", "Stocking1", 350, "Amazing super sexy stocking", "/img/kurtka4.jpg"),
//  "five" -> Product("five", "outewear", "Stocking2", 450, "Best stocking", "/img/kurtka5.jpg"),
//  "six" -> Product("six", "outewear", "Stocking3", 550, "Crazy stocking", "/img/kurtka6.jpg")
//)
