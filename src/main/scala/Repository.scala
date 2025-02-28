import scala.concurrent.Future
//import MyExecContext._
import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global

//trait Repository {
 // def findAll(): Future[Either[String, Seq[Product]]]
 // def findProduct(sku: String): Future[Either[String, Option[Product]]]
  // def save(entity)
  // def delete(entity)
//}

trait RepositorySlickImpl { //extends Repository {
  import slick.jdbc.PostgresProfile.api._

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

}
