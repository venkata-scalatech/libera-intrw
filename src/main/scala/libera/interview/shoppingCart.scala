package lebara.interview

  * Scala test v1.
  *
  * We have the following simplistic data model representing an e-commerce business:
  *
  * A user has a name and a list of orders. All attributes are required.
  *
  * An order is comprised of a list of products along with associated quantities.
  *
  * A product has a name, price and category. All attributes are required, except for category.
  *
  * Develop a representation of this data model and implement the following methods in Scala.
  * Include any code you developed in support of your solution.
  */

trait ScalaInterview {

  // 1 - Given an order, produce a list of product names.
  def productNamesFromOrder(order: Order): List[String]

  // 2 - Given a user, produce a list of products from all of their orders.
  def productsFromUser(user: User): List[Product]

  // 3 - Given an order, calculate the total price.
  def orderTotal(order: Order): Double

  // 4 - Given a list of products and a category name, produce a list of products in that category.
  // Make sure the solution handles a possibly missing category on a product.
  def productsInCategory(products: List[Product], category: String): List[Product]

  // 5 - Given list of products, produce a map of category to a list of products in that category.
  // Make sure the solution handles a possibly missing category on a product.
  def productsByCategory(products: List[Product]): Map[String, List[Product]]
}

case class User(uname: String, orders: Seq[Order])
case class Product(pname: String, price: Double, category: Option[String])
case class Order(items: Seq[(Product, Int)])

object shoppingCart extends ScalaInterview {
  override def productNamesFromOrder(order: Order): List[String] = {
    for {
      itemCount <- order.items
      item = itemCount._1
      quantity = itemCount._2
    } yield {
      item.pname
    }
  }.toList

  
  override def productsFromUser(user: User): List[Product] = {
    for {
      order <- user.orders
       itemCount <- order.items
      product =  itemCount._1
    } yield product
  }.toList

   override def orderTotal(order: Order): Double = {
    order.items.map(x => x._1.price * x._2).reduceleft(_ + _)
  }

    override def productsInCategory(products: List[Product], category: String): List[Product] = {
    for {
      product <- products
      productCategory <- product.category if productCategory.equals(category)
    } yield product
  }

    override def productsByCategory(products: List[Product]): Map[String, List[Product]] = {
    products.groupBy(_.category.getOrElse("None"))
  }
}





//Implementation

class shoppingCartTest extends App {

 val products = Seq[(Product("Learning Python", 120.0, Some("Stationary")),3) :: (Product("Sketch Pen", 120.0, Some("Stationary")),4) :: (Product("Remote Control", 120.0, Some("Electronics")),2) :: Product("Coke", 120.0, Some("Foods&Bevarages")),5) :: Nil]

val order1 = Order(products)
val user1 = User("testUser1",order1::Nil)

val productnames-Order = shoppingCart.productNamesFromOrder(order1)
val productnames-Usr = shoppingCart.productsFromUser(user1)
val productnames-Categry = shoppingCart.productsInCategory(products,"Stationary")

}





}