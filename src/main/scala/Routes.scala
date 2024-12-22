import Main.productServiceActor
import ProductServiceActor.{GetProduct, GetProductsByCategory}
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.server.Directives.{Segment, complete, get, pathEndOrSingleSlash, pathPrefix}
import akka.http.scaladsl.server.Directives._
import akka.pattern.ask
import akka.util.Timeout

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

trait Routes extends ProductJsonProtocol with SprayJsonSupport{
  import ProductServiceActor._
  implicit val timeout = Timeout(5 seconds)
  val routes =
    get {
      pathPrefix(Segment / Segment) { (category, sku) =>
        pathEndOrSingleSlash {
          val productOptionFuture = (productServiceActor ? GetProduct(sku)).mapTo[Future[Option[Product]]].flatten
          complete(productOptionFuture)
        }
      } ~
        pathPrefix(Segment) { category => //| parameter('nickname)) { nickname =>
          pathEndOrSingleSlash {
            // import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
            val productsFuture = (productServiceActor ? GetProductsByCategory(category)).mapTo[Future[Seq[Product]]].flatten
            complete(productsFuture)
          }
        }  ~
        pathEndOrSingleSlash {
          val productList = (productServiceActor ? GetAllProducts).mapTo[Future[Seq[Product]]].flatten
          complete(productList)
        }
    }
}

