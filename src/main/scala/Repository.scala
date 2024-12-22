import scala.concurrent.Future
//import MyExecContext._

trait Repository {
  def findAll(): Future[Seq[Product]]
  def findOne(sku: String): Future[Option[Product]]
  // def save(entity)
  // def delete(entity)
}

trait RepositorySlickImpl extends Repository {
  import slick.jdbc.PostgresProfile.api._

  def findAll(): Future[Seq[Product]] =
    Connection.db.run(SlickTables.productTable.result)

  def findOne(sku: String): Future[Option[Product]] =
    Connection.db.run(SlickTables.productTable.filter(_.sku === sku).result.headOption)

  def findAllByCategory(category: String): Future[Seq[Product]] =
    Connection.db.run(SlickTables.productTable.filter(_.category === category).result)  //.map(_.toList)
}
