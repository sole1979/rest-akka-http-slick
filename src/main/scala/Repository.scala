import scala.concurrent.Future
//import MyExecContext._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import slick.jdbc.PostgresProfile.api._

//trait Repository {
 // def findAll(): Future[Either[String, Seq[Product]]]
 // def findProduct(sku: String): Future[Either[String, Option[Product]]]
  // def save(entity)
  // def delete(entity)
//}

trait RepositorySlickImpl { //extends Repository {
  import slick.jdbc.PostgresProfile.api._
//--------PRODUCT--------
  def findAllProducts(): Future[Either[String, Seq[Product]]] =
    Connection.db.run(SlickTables.productTable.result)
      .map(Right(_))
      .recover {case ex => Left(ex.getMessage)}

  def findProduct(sku: String): Future[Either[String, Option[Product]]]/*: Future[Option[Product]]*/ =
    Connection.db.run(SlickTables.productTable.filter(_.sku === sku).result.headOption)
      .map  (Right(_)) //// case Some(row) => Right(Some(row: Product))
      .recover {case ex => Left(ex.getMessage)}  // error db

  def findProductsBySku(skus: List[String]): Future[Either[String, Seq[Product]]] =
    Connection.db.run(SlickTables.productTable.filter(_.sku inSet skus).result)
      .map(Right(_))
      .recover {case ex => Left(ex.getMessage)}

  def findAllByCategory(category: String): Future[Either[String, Seq[Product]]] =
    Connection.db.run(SlickTables.productTable.filter(_.category === category).result)  //.map(_.toList)
      .map(Right(_))
      .recover {case ex => Left(ex.getMessage)}

  def findUniqueCategories(): Future[Either[String, Seq[String]]] =
    Connection.db.run(SlickTables.productTable.map(_.category).distinct.result)  //.map(_.toSet)
      .map(Right(_))
      .recover {case ex => Left(ex.getMessage)}
  //--------USER------------------------------------
  def findUserAccount(email: String): Future[Either[String, Option[UserAccount]]] =
    Connection.db.run(SlickTables.userAccountTable.filter(_.email === email).result.headOption)
      .map(Right(_))
      .recover {case ex => Left(ex.getMessage)}  // error db


  def isEmailExist(email: String): Future[Either[String, Boolean]] =
    Connection.db.run(SlickTables.userAccountTable.filter(_.email === email).exists.result)
      .map(Right(_))
      .recover {case ex => Left(ex.getMessage)}  // error db

  def insertUserAccount(userAccount: UserAccount): Future[Either[String, UserAccount]] = {
    Connection.db.run(SlickTables.userAccountTable.returning(SlickTables.userAccountTable) += userAccount)
      .map(Right(_))
      .recover {case ex => Left(ex.getMessage)}  // error db
    // Connection.db.run(SlickTables.userAccountTable returning SlickTables.userAccountTable.map(_.id) into ((userAccount, id) => userAccount.copy(id = id) += userAccount)
  }

  //--------FAVORITES-------
  def isFavoriteExist(userId: Int, productSku: String): Future[Either[String, Boolean]] = {
    Connection.db.run(SlickTables.favoritesTable.filter(f => f.userId === userId && f.productSku === productSku).exists.result)
      .map(Right(_))
      .recover { case ex => Left(ex.getMessage)}
  }

  def insertFavorite(userId: Int, productSku: String): Future[Either[String, Boolean]] = {
    Connection.db.run(SlickTables.favoritesTable += Favorite(userId, productSku))
      .map(_ => Right(true))
      .recover {case ex => Left(ex.getMessage)}
  }

  def removeFavorite(userId: Int, productSku: String): Future[Either[String, Boolean]] = {
    Connection.db.run(SlickTables.favoritesTable.filter(f => f.userId === userId && f.productSku === productSku).delete)
      .map(_ => Right(true))
      .recover { case ex => Left(ex.getMessage)}
  }

  def getFavorites(userId: Int): Future[Either[String, List[Product]]] = {
    Connection.db.run(
      SlickTables.favoritesTable
        .filter(_.userId === userId)
        .join(SlickTables.productTable)
        .on(_.productSku === _.sku)
        .map(_._2)
        .result
    ).map { products =>
        //println(s"======PRODUCTS: $products")
        Right(products.toList)}
      .recover { case ex => Left(ex.getMessage)}
    /*val query = for {
      (fav, prod) <- SlickTables.favoritesTable join SlickTables.productTable on (_.productSku === _.sku)
      if fav.userId === userId
    } yield prod
    Connection.db.run(query.result)
      .map(products => Right(products.toList))
      .recover { case ex => Left(ex.getMessage)}*/
  }
}
