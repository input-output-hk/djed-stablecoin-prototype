package stablecoin

import org.scalatest.FunSuite
import stablecoin.Currency.{BaseCoin, PegCurrency}
import stablecoin.MinimalDjedStablecoinTest.{createStablecoinContract}

class MinimalDjedStablecoinTest extends FunSuite {

  test("create Minimal Djed contract") {
    val contract = createStablecoinContract(5.0, 10.0, 3.0)

    assert(contract.getReservesAmount == 5.0)
    assert(contract.getStablecoinsAmount == 10.0)
    assert(contract.getReservecoinsAmount == 3.0)
  }

  test("buy stablecoins") {
    val contract = createStablecoinContract(5.0, 10.0, 1.0)
    val amountSC = 5
    val amountBaseToPay = amountSC * contract.oracle.conversionRate(PegCurrency, BaseCoin)
    val feeToPay = amountBaseToPay * contract.fee
    val totalToPay = amountBaseToPay + feeToPay

    assert(contract.buyStablecoins(amountSC).get == totalToPay)
    assert(contract.getStablecoinsAmount == contract.initStablecoins + amountSC)
    assert(contract.getReservesAmount == contract.initReserves + totalToPay)
    assert(contract.getReservecoinsAmount == contract.initReservecoins)

    // test buying when the reserve below the minimal reserve ratio
    val contract2 = createStablecoinContract(1.0, 4.0, 1.0)
    assert(contract2.buyStablecoins(1.0).isFailure)

    // test buying when the reserve below the liabilities
    val contract3 = createStablecoinContract(1.0, 6.0, 1.0)
    assert(contract3.buyStablecoins(1.0).isFailure)
  }

  test("sell stablecoins") {
    val contract = createStablecoinContract(5.0, 10.0, 1.0)
    val amountSC = 3
    val expectedAmountBase = amountSC * (1 - contract.fee) * contract.oracle.conversionRate(PegCurrency, BaseCoin)

    assert(contract.sellStablecoins(amountSC).get == expectedAmountBase)
    assert(contract.getStablecoinsAmount == contract.initStablecoins - amountSC)
    assert(contract.getReservesAmount == contract.initReserves - expectedAmountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins)
  }

  test("sell stablecoins when reserve below liabilities") {
    val contract = createStablecoinContract(5.0, 100.0, 1.0)
    val amountSC = 10
    val nominalPrice = contract.getReservesAmount / contract.getStablecoinsAmount
    val expectedAmountBase = amountSC * (1 - contract.fee) * nominalPrice

    assert(contract.sellStablecoins(amountSC).get == expectedAmountBase)
    assert(contract.getStablecoinsAmount == contract.initStablecoins - amountSC)
    assert(contract.getReservesAmount == contract.initReserves - expectedAmountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins)

    val nominalPrice2 = contract.getReservesAmount / contract.getStablecoinsAmount
    val expectedAmountBase2 = amountSC * (1 - contract.fee) * nominalPrice2
    assert(contract.sellStablecoins(amountSC).get == expectedAmountBase2)
    // Note that the nominalPrice2 will be higher than the nominalPrice so the second player
    // will receive slightly more base coins for the same amount of stablecoins. This happens due to collected
    // fees from the first player
    assert(nominalPrice2 > nominalPrice)
    assert(expectedAmountBase2 > expectedAmountBase)
  }

  test("buy reservecoins") {
    // test initial buying of reservecoins
    val contract = createStablecoinContract(2.0, 10.0, 1.0)
    val amountRC = 6
    val amountBaseToPay = contract.calculateBasecoinsForMintedReservecoins(amountRC)

    assert(contract.buyReservecoins(amountRC).get == amountBaseToPay)
    assert(contract.getStablecoinsAmount == contract.initStablecoins)
    assert(contract.getReservesAmount == contract.initReserves + amountBaseToPay)
    assert(contract.getReservecoinsAmount == amountRC + 1)

    // test buying when there are already some reserve coins in the bank
    val amountRC2 = 4
    val amountBaseToPay2 = contract.calculateBasecoinsForMintedReservecoins(amountRC2)

    assert(contract.buyReservecoins(amountRC2).get == amountBaseToPay2)
    assert(contract.getStablecoinsAmount == contract.initStablecoins)
    assert(contract.getReservesAmount == contract.initReserves + amountBaseToPay + amountBaseToPay2)
    assert(contract.getReservecoinsAmount == amountRC + amountRC2 + 1)

    // test buying when reserves surpass max limit
    assert(contract.buyReservecoins(10).isFailure)

    // test buying when reserves below the min limit
    val contract2 = createStablecoinContract(1.0, 4.0, 1.0)
    assert(contract2.buyReservecoins(1.0).isSuccess)

    // test buying when the reserve below the liabilities
    val contract3 = createStablecoinContract(1.0, 6.0, 1.0)
    assert(contract3.buyReservecoins(1.0).isSuccess)
  }

  test("buy reservecoins nominal formula") {
    val contract = createStablecoinContract(11, 10, 10, 0.1, 0.01)
    contract.oracle.updateConversionRate(PegCurrency, BaseCoin, 1.0)

    val amount1 = contract.calculateBasecoinsForMintedReservecoinsIter(10, 100000)
    val amount2 = contract.calculateBasecoinsForMintedReservecoins(10.0)

    /* Accuracy of the iterative price calculation should be enough to match 5 digits after decimal point */
    assert(roundAt(amount1, 5) == roundAt(amount2, 5))
  }

  test("buy reservecoins with splitting") {
    def createContract(reserves: N) = {
      val contract = createStablecoinContract(reserves, 10, 10, 0.1, 0.5)
      contract.oracle.updateConversionRate(PegCurrency, BaseCoin, 1.0)
      contract
    }

    // Buy reservecoins with splitting when price below minimal
    val contract = createContract(11.0)
    val contract2 = createContract(11.0)

    var paid = BigDecimal(0)
    for (i <- 0 until 10)
      paid += contract.buyReservecoins(1.0).get

    val paid2 = contract2.buyReservecoins(10.0).get

    assert(contract.getReservesAmount == contract2.getReservesAmount)
    assert(contract.getReservecoinsAmount == contract2.getReservecoinsAmount)
    assert(paid == paid2)

    // Buy reservecoins with splitting when price is above minimal (i.e., calculated by formula)
    val contract3 = createContract(20.0)
    val contract4 = createContract(20.0)

    var paid3 = BigDecimal(0)
    for (i <- 0 until 100)
      paid3 += contract3.buyReservecoins(0.1).get

    val paid4 = contract4.buyReservecoins(10.0).get

    println("Reserves contract 3: " + contract3.getReservesAmount)
    println("Reserves contract 4: " + contract4.getReservesAmount)

    // We do rounding because amounts may be slightly different due to implementation specifics (specifically due to
    // BigDecimal->Double type casting, see more comments in 'buyReservecoin()' implementation)
    assert(roundAt(contract3.getReservesAmount, 10) == roundAt(contract4.getReservesAmount, 10))
    assert(contract3.getReservecoinsAmount == contract4.getReservecoinsAmount)
    assert(roundAt(paid3,10) == roundAt(paid4,10))
  }

  test("buy reservecoins when reserves below liablities (equity = 0)") {
    val bankFee = BigDecimal(0.1)
    val reservecoinDefaultPrice = BigDecimal(0.5)

    val contract = createStablecoinContract(10, 11, 10, bankFee, reservecoinDefaultPrice)
    contract.oracle.updateConversionRate(PegCurrency, BaseCoin, 1.0)

    val amountRC = 20

    println("Reserves before: " + contract.getReservesAmount)
    println("Reservecoin price before: " + contract.reservecoinNominalPrice())
    val paidAmount = contract.buyReservecoins(amountRC).get
    println("Reserves after: " + contract.getReservesAmount)
    println("Reservecoin price after: " + contract.reservecoinNominalPrice())
    println("Paid: " + paidAmount)

    // Since nominal price of reservecoins is zero (because equity is zero), the coins should be sell with default price
    val expectedAmount = amountRC * reservecoinDefaultPrice * (1+bankFee)
    assert(paidAmount == expectedAmount)
  }

  test("buy reservecoins when nominal price < default price") {
    val bankFee = BigDecimal(0.1)
    val reservecoinDefaultPrice = BigDecimal(0.2)

    val contract = createStablecoinContract(11, 10, 10, bankFee, reservecoinDefaultPrice)
    contract.oracle.updateConversionRate(PegCurrency, BaseCoin, 1.0)

    /////////////////////////////////////////////////////////
    // 1 Test when whole amount is purchased with default price
    /////////////////////////////////////////////////////////
    val amountRC = 5

    println("Reserves before: " + contract.getReservesAmount)
    println("Reservecoin price before: " + contract.reservecoinNominalPrice())
    val paidAmount = contract.buyReservecoins(amountRC).get
    println("Reserves after: " + contract.getReservesAmount)
    println("Reservecoin price after: " + contract.reservecoinNominalPrice())
    println("Paid: " + paidAmount)

    // Since nominal price of reservecoins is zero (because equity is zero), the coins should be sell with default price
    val expectedAmount = amountRC * reservecoinDefaultPrice * (1+bankFee)
    assert(paidAmount == expectedAmount)

    /////////////////////////////////////////////////////////
    // 2 Test when only part of the amount is purchased with default price (until the price naturally increased to the min level) and
    // the rest is purchased with nominal price (using regular formula for differential price calculation)
    /////////////////////////////////////////////////////////
    val amountRC2 = 50 // 45 will be bought with default price and the rest will be priced according to the formula

    val expectedAmountToPay = {
      val liabilities = contract.getStablecoinsAmount * contract.oracle.conversionRate(PegCurrency, BaseCoin)
      val maxToBuyWithMinPrice = (liabilities - contract.getReservesAmount + contract.getReservecoinsAmount * reservecoinDefaultPrice) / (bankFee * reservecoinDefaultPrice)
      val toPayWithMinPrice = maxToBuyWithMinPrice * reservecoinDefaultPrice * (1 + bankFee)

      val updatedReservecoins = contract.getReservecoinsAmount + maxToBuyWithMinPrice
      val updatedReserves = contract.getReservesAmount + toPayWithMinPrice
      val newReservecoins = updatedReservecoins + (amountRC2 - maxToBuyWithMinPrice)
      val tmp1 = math.pow((newReservecoins / updatedReservecoins).doubleValue, (1 + bankFee).doubleValue)
      val tmp2 = updatedReserves - liabilities
      val newReserves = tmp1 * tmp2 + liabilities
      val toPayWithNominalPrice = newReserves - updatedReserves
      toPayWithMinPrice + toPayWithNominalPrice
    }


    val paidAmount2 = contract.buyReservecoins(amountRC2).get
    println("Reserves after 2: " + contract.getReservesAmount)
    println("Reservecoin price after 2: " + contract.reservecoinNominalPrice())
    println("Paid 2: " + paidAmount2)
    println("Expected amount to pay: " + expectedAmountToPay)

    assert(expectedAmountToPay == paidAmount2)
  }

  test("sell reservecoins") {
    val contract = createStablecoinContract(5.0, 10.0, 3.0)
    val amountRC = 2
    val expectedAmountBase = contract.calculateBasecoinsForBurnedReservecoins(amountRC)

    /* Accuracy of the iterative price calculation should be enough to match 5 digits after decimal point */
    val expectedAmountBaseIter = contract.calculateBasecoinsForBurnedReservecoinsIter(amountRC, 100000)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))

    assert(contract.sellReservecoins(amountRC).get == expectedAmountBase)
    assert(contract.getStablecoinsAmount == contract.initStablecoins)
    assert(contract.getReservesAmount == contract.initReserves - expectedAmountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins - amountRC)

    // test selling when reserves below the min limit
    val contract2 = createStablecoinContract(2.0, 11.0, 3.0)
    assert(contract.sellReservecoins(amountRC).isFailure)
  }

  test("sell reservecoins when nominal price < default price") {
    val bankFee = BigDecimal(0.1)
    val reservecoinDefaultPrice = BigDecimal(0.2)

    // 1 Test when default price > nominal price > 0
    val contract = createStablecoinContract(11, 10, 10, bankFee, reservecoinDefaultPrice)
    contract.oracle.updateConversionRate(PegCurrency, BaseCoin, 1.0)

    val amountRC = 2

    println("Reservecoin price before: " + contract.reservecoinNominalPrice())
    println("Expected basecoins to return: " + contract.calculateBasecoinsForBurnedReservecoins(amountRC))
    assert(contract.sellReservecoins(amountRC).isFailure)

    // 2 Test when nominal price = 0
    val contract2 = createStablecoinContract(10, 11, 10, bankFee, reservecoinDefaultPrice)
    contract2.oracle.updateConversionRate(PegCurrency, BaseCoin, 1.0)

    println("Reservecoin price before 2: " + contract2.reservecoinNominalPrice())
    println("Expected basecoins to return 2: " + contract2.calculateBasecoinsForBurnedReservecoins(amountRC))
    assert(contract2.sellReservecoins(amountRC).isFailure)
  }

  test("sell reservecoins with splitting") {
    def createContract(reserves: N) = {
      val contract = createStablecoinContract(reserves, 10, 10, 0.1, 0.5)
      contract.oracle.updateConversionRate(PegCurrency, BaseCoin, 1.0)
      contract
    }

    val contract3 = createContract(25.0)
    val contract4 = createContract(25.0)

    var paid3 = BigDecimal(0)
    for (i <- 0 until 50)
      paid3 += contract3.sellReservecoins(0.1).get

    val paid4 = contract4.sellReservecoins(5.0).get

    println("Reserves contract 3: " + contract3.getReservesAmount)
    println("Reserves contract 4: " + contract4.getReservesAmount)

    // We do rounding because amounts may be slightly different due to implementation specifics (specifically due to
    // BigDecimal->Double type casting, see more comments in 'buyReservecoin()' implementation)
    assert(roundAt(contract3.getReservesAmount, 10) == roundAt(contract4.getReservesAmount, 10))
    assert(contract3.getReservecoinsAmount == contract4.getReservecoinsAmount)
    assert(roundAt(paid3,10) == roundAt(paid4,10))
  }

  test("sell/buy reservecoins difference") {
    val contract = createStablecoinContract(5.0, 10.0, 3.0)
    val amountRC = 2

    val nominalPrice1 = contract.reservecoinNominalPrice()
    val amountPaid = contract.buyReservecoins(amountRC).get
    val nominalPrice2 = contract.reservecoinNominalPrice()
    val amountReturned = contract.sellReservecoins(amountRC).get
    val nominalPrice3 = contract.reservecoinNominalPrice()

    require(amountPaid > amountReturned)
    require(nominalPrice1 < nominalPrice2)
    require(nominalPrice2 < nominalPrice3)
  }
}

object MinimalDjedStablecoinTest {
  val bankFee = 0.01
  val minReserveRatio = 1.5
  val maxReserveRatio = 4
  val reservecoinDefaultPrice = 0.5

  def createStablecoinContract(initReserves: N,
                               initStablecoins: N,
                               initReservecoins: N,
                               fee: N = bankFee,
                               defaultPrice: N = reservecoinDefaultPrice) = {
    val oracle = new Oracle
    oracle.updateConversionRate(PegCurrency, BaseCoin, 0.2) // 1 BaseCoin = 5 USD (PegCurrency)

    new MinimalDjedStablecoin(
      oracle,
      fee,
      minReserveRatio,
      maxReserveRatio,
      defaultPrice,
      initReserves,
      initStablecoins,
      initReservecoins)
  }
}