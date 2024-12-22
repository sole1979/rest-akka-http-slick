import akka.actor.{Actor, ActorLogging}

object ProductServiceActor {
  case object GetAllProducts
  case class GetProduct(sku: String)
  case class GetProductsByCategory(category: String)
  //case class AddProduct(product: Product)
  case object OperationSuccess
}

class ProductServiceActor extends Actor with ActorLogging with RepositorySlickImpl {
  import ProductServiceActor._

  override def receive: Receive = {
    case GetAllProducts =>
      log.info("Getting all products")
      sender() ! findAll()

    case GetProduct(sku) =>
      log.info(s"Getting product with SKU $sku")
      sender() ! findOne(sku)

    case GetProductsByCategory(category) =>
      log.info(s"Getting products from category $category")
      sender() ! findAllByCategory(category)

    // case AddItem(item) =>
    //   log.info(s"Trying to add item $item")
    //   items = items + (item.sku -> item)
    //   sender() ! OperationSuccess
  }
}