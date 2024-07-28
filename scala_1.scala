object InventorySystem {
  type Product = (String, Int, Double) // (name, quantity, price)
  type Inventory = Map[Int, Product]   // productId -> Product

  // I. Retrieve all product names from inventory1.
  def retrieveProductNames(inventory: Inventory): Seq[String] = {
    inventory.values.map(_._1).toSeq
  }

  // II. Calculate the total value of all products in inventory1.
  def calculateTotalValue(inventory: Inventory): Double = {
    inventory.values.map { case (_, quantity, price) => quantity * price }.sum
  }

  // III. Check if inventory1 is empty.
  def isEmpty(inventory: Inventory): Boolean = {
    inventory.isEmpty
  }

  // IV. Merge inventory1 and inventory2, updating quantities and retaining the highest price.
  def mergeInventories(inventory1: Inventory, inventory2: Inventory): Inventory = {
    inventory2.foldLeft(inventory1) { case (acc, (id, (name, quantity, price))) =>
      acc.get(id) match {
        case Some((_, existingQuantity, existingPrice)) =>
          acc.updated(id, (name, existingQuantity + quantity, existingPrice.max(price)))
        case None =>
          acc.updated(id, (name, quantity, price))
      }
    }
  }

  // V. Check if a product with a specific ID (e.g., 102) exists and print its details.
  def checkProductExists(inventory: Inventory, productId: Int): Unit = {
    inventory.get(productId) match {
      case Some((name, quantity, price)) =>
        println(s"Product ID: $productId, Name: $name, Quantity: $quantity, Price: $price")
      case None =>
        println(s"Product ID: $productId does not exist in the inventory.")
    }
  }

  def main(args: Array[String]): Unit = {
    val inventory1: Inventory = Map(
      101 -> ("Widget", 10, 25.50),
      102 -> ("Gadget", 5, 99.99),
      103 -> ("Thingamajig", 20, 15.75)
    )

    val inventory2: Inventory = Map(
      102 -> ("Gadget", 3, 110.00),
      104 -> ("Doohickey", 7, 45.00)
    )

    // I. Retrieve all product names from inventory1.
    val productNames = retrieveProductNames(inventory1)
    println(s"Product Names: ${productNames.mkString(", ")}")

    // II. Calculate the total value of all products in inventory1.
    val totalValue = calculateTotalValue(inventory1)
    println(s"Total Value: $$${totalValue}")

    // III. Check if inventory1 is empty.
    val isEmptyInventory = isEmpty(inventory1)
    println(s"Is inventory1 empty? $isEmptyInventory")

    // IV. Merge inventory1 and inventory2.
    val mergedInventory = mergeInventories(inventory1, inventory2)
    println(s"Merged Inventory: $mergedInventory")

    // V. Check if product ID 102 exists in inventory1 and print its details.
    checkProductExists(inventory1, 102)
  }
}
