object ElectricityBill extends App {
  case class Electricty(accountNumber: Int, accountUserName: String, address: String, previousUnit: Int, currentUnit: Int, result: Int)
  //calculateCharges method is used to calculate the charges according to unit slab

  def calculateCharges(consumedUnit: Int): Double = {
    val (less, medium, high) = if (consumedUnit <= 250) {
      (consumedUnit * 5.25, 0.0, 0.0)
    }
    else if (consumedUnit <= 450) {
      (250 * 5.25, (consumedUnit - 250) * 6.75, 0.0)
    }
    else {
      (250 * 5.25, 200 * 6.75, (consumedUnit - 450) * 8.50)
    }
    val total = less + medium + high
    total
  }
  //Checking for Execption like NumberFormatException
  try {
    println("Account Number: ")
    val accountNumber = scala.io.StdIn.readInt()
    val accountUserName = scala.io.StdIn.readLine("What's your name? ")
    val address = scala.io.StdIn.readLine("What's your Address? ")
    println("Previous Unit : ")
    val previousUnit = scala.io.StdIn.readInt()
    println("Current Unit : ")
    val currentUnit = scala.io.StdIn.readInt()
    val actualUnit = (currentUnit - previousUnit)
    val withOutGst = (calculateCharges((actualUnit)))
    println(s"Amount Without GST : " + withOutGst)
    val afterGst = (withOutGst + (withOutGst * 0.18))
    println(s"Amount After GST : " + afterGst)
  }
  catch {
    case e: NumberFormatException =>
      println("Exception Caught : Invalid input...")
  }
}
