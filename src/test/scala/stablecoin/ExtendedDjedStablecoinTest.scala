package stablecoin

import org.scalatest.FunSuite
import stablecoin.Currency.{BaseCoin, PegCurrency}
import stablecoin.ExtendedDjedStablecoinTest.createStablecoin

import scala.util.Try

class ExtendedDjedStablecoinTest extends FunSuite {

  test("buy stablecoins when init/final reserve ratio above optimum") {
    val contract = createStablecoin(60000.0, 20000.0, 5000.0, 1.0, optReservesRatio = 2)
    assert(contract.reservesRatio() > contract.optimalReservesRatio)
    val amountSC = 100

    val expectedAmountBaseIter = contract.calculateBasecoinsForMintedStablecoinsIter(amountSC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForMintedStablecoins(amountSC)
    val referenceAmountBase = amountSC * contract.oracle.conversionRate(PegCurrency, BaseCoin) * (1 + contract.baseFee)

    assert(expectedAmountBaseIter == referenceAmountBase)
    assert(expectedAmountBase == referenceAmountBase)

    val amountBase = contract.buyStablecoins(amountSC).get
    assert(amountBase == referenceAmountBase)
    assert(contract.getStablecoinsAmount == contract.initStablecoins + amountSC)
    assert(contract.getReservesAmount == contract.initReserves + amountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins)
  }

  test("buy stablecoins when init/final reserve ratio below optimum") {
    val contract = createStablecoin(60000.0, 20000.0, 5000.0, 1.0, optReservesRatio = 4)
    assert(contract.reservesRatio() < contract.optimalReservesRatio)
    assert(contract.reservesRatio() > contract.pegReservesRatio)
    val amountSC = 100

    val expectedAmountBaseIter = contract.calculateBasecoinsForMintedStablecoinsIter(amountSC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForMintedStablecoins(amountSC)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))

    val amountBase = contract.buyStablecoins(amountSC).get
    assert(amountBase == expectedAmountBase)
    assert(contract.getStablecoinsAmount == contract.initStablecoins + amountSC)
    assert(contract.getReservesAmount == contract.initReserves + amountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins)
  }

  test("buy stablecoins when init reserve ratio above optimum but final ratio is below optimum") {
    val contract = createStablecoin(80200.0, 20000.0, 5000.0, 1.0, optReservesRatio = 4)
    assert(contract.reservesRatio() > contract.optimalReservesRatio)
    val amountSC = 300

    val expectedAmountBaseIter = contract.calculateBasecoinsForMintedStablecoinsIter(amountSC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForMintedStablecoins(amountSC)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))

    val amountBase = contract.buyStablecoins(amountSC).get
    assert(amountBase == expectedAmountBase)
    assert(contract.reservesRatio() < contract.optimalReservesRatio)
    assert(contract.getStablecoinsAmount == contract.initStablecoins + amountSC)
    assert(contract.getReservesAmount == contract.initReserves + amountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins)
  }

  test("buy stablecoins when reserve ratio below optimum minimum") {
    val contract = createStablecoin(29000.0, 20000.0, 5000.0, 1.0, optReservesRatio = 4)
    assert(contract.reservesRatio() < contract.pegReservesRatio)
    assert(Try(contract.calculateBasecoinsForMintedStablecoins(1)).isFailure)
    assert(contract.buyStablecoins(1).isFailure)

    val contract2 = createStablecoin(30100.0, 20000.0, 5000.0, 1.0, optReservesRatio = 2)
    assert(contract2.reservesRatio() > contract2.pegReservesRatio)
    assert(Try(contract2.calculateBasecoinsForMintedStablecoins(2000)).isFailure)
    assert(contract2.buyStablecoins(2000).isFailure)
  }

  test("sell stablecoins when init/final reserve ratio above optimum") {
    val contract = createStablecoin(41000.0, 20000.0, 5000.0, 1.0, optReservesRatio = 2)
    assert(contract.reservesRatio() > contract.optimalReservesRatio)
    val amountSC = 90

    val expectedAmountBaseIter = contract.calculateBasecoinsForRedeemedStablecoinsIter(amountSC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForRedeemedStablecoins(amountSC)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))

    val (amountBase, amountRC) = contract.sellStablecoinsWithSwap(amountSC).get
    assert(amountBase == expectedAmountBase)
    assert(contract.getStablecoinsAmount == contract.initStablecoins - amountSC)
    assert(contract.getReservesAmount == contract.initReserves - amountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins + amountRC)
    assert(contract.reservesRatio() > contract.optimalReservesRatio)
  }

  test("sell stablecoins when init/final reserve ratio below optimum and above minimum") {
    val contract = createStablecoin(35000.0, 20000.0, 5000.0, 1.1, optReservesRatio = 2)
    assert(contract.reservesRatio() < contract.optimalReservesRatio)
    assert(contract.reservesRatio() > contract.pegReservesRatio)
    val amountSC = 100

    val expectedAmountBaseIter = contract.calculateBasecoinsForRedeemedStablecoinsIter(amountSC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForRedeemedStablecoins(amountSC)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))

    val (amountBase, amountRC) = contract.sellStablecoinsWithSwap(amountSC).get
    assert(amountBase == expectedAmountBase)
    assert(contract.getStablecoinsAmount == contract.initStablecoins - amountSC)
    assert(contract.getReservesAmount == contract.initReserves - amountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins + amountRC)
    assert(contract.reservesRatio() < contract.optimalReservesRatio)
    assert(contract.reservesRatio() > contract.pegReservesRatio)
  }

  test("sell stablecoins when init/final reserve ratio below minimum") {
    val contract = createStablecoin(29000.0, 20000.0, 5000.0, 1.1, optReservesRatio = 2)
    assert(contract.reservesRatio() < contract.pegReservesRatio)
    val amountSC = 100

    val expectedAmountBaseIter = contract.calculateBasecoinsForRedeemedStablecoinsIter(amountSC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForRedeemedStablecoins(amountSC)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))

    val (amountBase, amountRC) = contract.sellStablecoinsWithSwap(amountSC).get
    assert(amountBase == expectedAmountBase)
    assert(contract.getStablecoinsAmount == contract.initStablecoins - amountSC)
    assert(contract.getReservesAmount == contract.initReserves - amountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins + amountRC)
    assert(contract.reservesRatio() < contract.pegReservesRatio)
  }

  test("sell stablecoins when init reserve ratio below optimum and above minimum, final ratio above optimum") {
    val contract = createStablecoin(39900.0, 20000.0, 5000.0, 1.0, optReservesRatio = 2)
    assert(contract.reservesRatio() < contract.optimalReservesRatio)
    assert(contract.reservesRatio() > contract.pegReservesRatio)
    val amountSC = 500

    val expectedAmountBaseIter = contract.calculateBasecoinsForRedeemedStablecoinsIter(amountSC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForRedeemedStablecoins(amountSC)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))

    val expectedAmountRcIter = contract.calculateReservecoinsForRedeemedStablecoinsIter(amountSC, 100)
    val expectedAmountRc = contract.calculateReservecoinsForRedeemedStablecoins(amountSC)
    assert(roundAt(expectedAmountRc, 5) == roundAt(expectedAmountRcIter, 5))

    val (amountBase, amountRC) = contract.sellStablecoinsWithSwap(amountSC).get
    assert(amountBase == expectedAmountBase)
    assert(amountRC == expectedAmountRc)
    assert(contract.getStablecoinsAmount == contract.initStablecoins - amountSC)
    assert(contract.getReservesAmount == contract.initReserves - amountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins + amountRC)
    assert(contract.reservesRatio() > contract.optimalReservesRatio)
  }

  test("sell stablecoins when init reserve ratio below minimum, final ratio above optimum") {
    val contract = createStablecoin(29500.0, 20000.0, 5000.0, 1.0, optReservesRatio = 1.6)
    assert(contract.reservesRatio() < contract.pegReservesRatio)
    val amountSC = 5000

    val expectedAmountBaseIter = contract.calculateBasecoinsForRedeemedStablecoinsIter(amountSC, 100)
    val expectedAmountBase = contract.calculateBasecoinsForRedeemedStablecoins(amountSC)
    assert(roundAt(expectedAmountBase, 3) == roundAt(expectedAmountBaseIter, 3))

    val expectedAmountRcIter = contract.calculateReservecoinsForRedeemedStablecoinsIter(amountSC, 100)
    val expectedAmountRc = contract.calculateReservecoinsForRedeemedStablecoins(amountSC)
    assert(roundAt(expectedAmountRc, 3) == roundAt(expectedAmountRcIter, 3))

    val (amountBase, amountRC) = contract.sellStablecoinsWithSwap(amountSC).get
    assert(amountBase == expectedAmountBase)
    assert(amountRC == expectedAmountRc)
    assert(contract.getStablecoinsAmount == contract.initStablecoins - amountSC)
    assert(contract.getReservesAmount == contract.initReserves - amountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins + amountRC)
    assert(contract.reservesRatio() > contract.optimalReservesRatio)
  }

  test("sell stablecoins when init reserve ratio below minimum, final ratio above minimum below optimum") {
    val contract = createStablecoin(29500.0, 20000.0, 5000.0, 1.0, optReservesRatio = 2)
    assert(contract.reservesRatio() < contract.pegReservesRatio)
    val amountSC = 5000

    val expectedAmountBaseIter = contract.calculateBasecoinsForRedeemedStablecoinsIter(amountSC, 100)
    val expectedAmountBase = contract.calculateBasecoinsForRedeemedStablecoins(amountSC)
    assert(roundAt(expectedAmountBase, 3) == roundAt(expectedAmountBaseIter, 3))

    val expectedAmountRcIter = contract.calculateReservecoinsForRedeemedStablecoinsIter(amountSC, 100)
    val expectedAmountRc = contract.calculateReservecoinsForRedeemedStablecoins(amountSC)
    assert(roundAt(expectedAmountRc, 3) == roundAt(expectedAmountRcIter, 3))

    val (amountBase, amountRC) = contract.sellStablecoinsWithSwap(amountSC).get
    assert(amountBase == expectedAmountBase)
    assert(amountRC == expectedAmountRc)
    assert(contract.getStablecoinsAmount == contract.initStablecoins - amountSC)
    assert(contract.getReservesAmount == contract.initReserves - amountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins + amountRC)
    assert(contract.reservesRatio() > contract.pegReservesRatio)
    assert(contract.reservesRatio() < contract.optimalReservesRatio)
  }

  test("sell stablecoins when init reserve ratio below 1") {
    val contract = createStablecoin(19500.0, 20000.0, 5000.0, 1.1, optReservesRatio = 2)
    assert(contract.reservesRatio() < 1)
    val amountSC = 1800

    val expectedAmountBaseIter = contract.calculateBasecoinsForRedeemedStablecoinsIter(amountSC, 100)
    val expectedAmountBase = contract.calculateBasecoinsForRedeemedStablecoins(amountSC)
    assert(roundAt(expectedAmountBase, 3) == roundAt(expectedAmountBaseIter, 3))

    val expectedAmountRcIter = contract.calculateReservecoinsForRedeemedStablecoinsIter(amountSC, 100)
    val expectedAmountRc = contract.calculateReservecoinsForRedeemedStablecoins(amountSC)
    assert(roundAt(expectedAmountRc, 3) == roundAt(expectedAmountRcIter, 3))

    val (amountBase, amountRC) = contract.sellStablecoinsWithSwap(amountSC).get
    assert(amountBase == expectedAmountBase)
    assert(amountRC == expectedAmountRc)
  }

  test("sell stablecoins with swap") {
    val contract = createStablecoin(19500.0, 20000.0, 5000.0, 1.1, optReservesRatio = 2)
    assert(contract.reservesRatio() < contract.pegReservesRatio)
    val amountSC = 1000

    val expectedAmountBaseIter = contract.calculateBasecoinsForRedeemedStablecoinsIter(amountSC, 100)
    val expectedAmountBase = contract.calculateBasecoinsForRedeemedStablecoins(amountSC)
    assert(roundAt(expectedAmountBase, 4) == roundAt(expectedAmountBaseIter, 4))

    val expectedAmountRcIter = contract.calculateReservecoinsForRedeemedStablecoinsIter(amountSC, 100)
    val expectedAmountRc = contract.calculateReservecoinsForRedeemedStablecoins(amountSC)
    assert(roundAt(expectedAmountRc, 3) == roundAt(expectedAmountRcIter, 3))

    val (amountBase, amountRC) = contract.sellStablecoinsWithSwap(amountSC).get
    assert(amountBase == expectedAmountBase)
    assert(amountRC == expectedAmountRc)
  }

  test("buy reservecoins") {
    val contract = createStablecoin(20000.0, 20000.0, 5000.0, 1.0)
    val amountRC = 100

    val expectedAmountBaseIter = contract.calculateBasecoinsForMintedReservecoinsIter(amountRC, 10000)
    val expectedAmountBase = contract.calculateBasecoinsForMintedReservecoins(amountRC)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))

    val amountBase = contract.buyReservecoins(amountRC).get
    assert(amountBase == expectedAmountBase)
    assert(contract.getStablecoinsAmount == contract.initStablecoins)
    assert(contract.getReservesAmount == contract.initReserves + amountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins + amountRC)
  }

  test("buy reservecoins (1-st interval):" +
    " initial and new reserve ratio are below minimum") {
    val contract = createStablecoin(20000.0, 20000.0, 5000.0, 1.0)
    println("Initial reserve ratio 1: " + contract.reservesRatio())
    assert(contract.reservesRatio() < contract.pegReservesRatio)
    val amountRC = 100
    val expectedAmountBaseIter = contract.calculateBasecoinsForMintedReservecoinsIter(amountRC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForMintedReservecoins(amountRC)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))
    val amountBase = contract.buyReservecoins(amountRC).get
    assert(amountBase == expectedAmountBase)
    assert(contract.reservesRatio() < contract.pegReservesRatio)
    println("New reserve ratio 1: " + contract.reservesRatio())
  }

  test("buy reservecoins (2-nd interval):" +
    "initial reserve ratio is below minimum, new ratio is above minimum but below optimum") {
    val contract2 = createStablecoin(29000.0, 20000.0, 5000.0, 1.0)
    println("Initial reserve ratio 2: " + contract2.reservesRatio())
    assert(contract2.reservesRatio() < contract2.pegReservesRatio)
    val amountRC2 = 2000
    val expectedAmountBaseIter2 = contract2.calculateBasecoinsForMintedReservecoinsIter(amountRC2, 1000)
    val expectedAmountBase2 = contract2.calculateBasecoinsForMintedReservecoins(amountRC2)
    assert(roundAt(expectedAmountBase2, 4) == roundAt(expectedAmountBaseIter2, 4))
    val amountBase2 = contract2.buyReservecoins(amountRC2).get
    assert(amountBase2 == expectedAmountBase2)
    assert(contract2.reservesRatio() > contract2.pegReservesRatio
      && contract2.reservesRatio() < contract2.optimalReservesRatio)
    println("New reserve ratio 2: " + contract2.reservesRatio())
  }

  test("buy reservecoins (3-rd interval):" +
    " initial reserve ratio is below minimum, new ratio is above optimum") {
    val optimalReserveRatio = 2
    val contract3 = createStablecoin(29000.0, 20000.0, 5000.0, 1.0, optReservesRatio = optimalReserveRatio)
    println("Initial reserve ratio 3: " + contract3.reservesRatio())
    assert(contract3.reservesRatio() < contract3.pegReservesRatio)
    val amountRC3 = 7000
    val expectedAmountBaseIter3 = contract3.calculateBasecoinsForMintedReservecoinsIter(amountRC3, 1000)
    val expectedAmountBase3 = contract3.calculateBasecoinsForMintedReservecoins(amountRC3)
    assert(roundAt(expectedAmountBase3, 3) == roundAt(expectedAmountBaseIter3, 3))
    val amountBase3 = contract3.buyReservecoins(amountRC3).get
    assert(amountBase3 == expectedAmountBase3)
    assert(contract3.reservesRatio() > optimalReserveRatio)
    println("New reserve ratio 3: " + contract3.reservesRatio())
  }

  test("buy reservecoins (4-th interval):" +
    " initial and new reserve ratio are above minimum but below optimum") {
    val contract = createStablecoin(35000.0, 20000.0, 5000.0, 1.0)
    println("Initial reserve ratio 4: " + contract.reservesRatio())
    assert(contract.reservesRatio() > contract.pegReservesRatio
      && contract.reservesRatio() < contract.optimalReservesRatio)
    val amountRC = 100
    val expectedAmountBaseIter = contract.calculateBasecoinsForMintedReservecoinsIter(amountRC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForMintedReservecoins(amountRC)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))
    val amountBase = contract.buyReservecoins(amountRC).get
    assert(amountBase == expectedAmountBase)
    println("New reserve ratio 4: " + contract.reservesRatio())
    assert(contract.reservesRatio() > contract.pegReservesRatio
      && contract.reservesRatio() < contract.optimalReservesRatio)
  }

  test("buy reservecoins (5-th interval):" +
    " initial reserve ratio is above minimum but below optimum, new ratio is above optimum") {
    val optimalReserveRatio = 2
    val contract2 = createStablecoin(37000.0, 20000.0, 5000.0, 1.0, optReservesRatio = optimalReserveRatio)
    println("Initial reserve ratio 5: " + contract2.reservesRatio())
    assert(contract2.reservesRatio() > contract2.pegReservesRatio
      && contract2.reservesRatio() < contract2.optimalReservesRatio)
    val amountRC2 = 1000
    val expectedAmountBaseIter2 = contract2.calculateBasecoinsForMintedReservecoinsIter(amountRC2, 1000)
    val expectedAmountBase2 = contract2.calculateBasecoinsForMintedReservecoins(amountRC2)
    assert(roundAt(expectedAmountBase2, 3) == roundAt(expectedAmountBaseIter2, 3))
    val amountBase2 = contract2.buyReservecoins(amountRC2).get
    assert(amountBase2 == expectedAmountBase2)
    assert(contract2.reservesRatio() > contract2.optimalReservesRatio)
    println("New reserve ratio 5: " + contract2.reservesRatio())
  }

  test("buy reservecoins (6-th interval): initial and new reserve ratio are above the optimum") {
    val optimalReserveRatio = 2
    val contract = createStablecoin(45000.0, 20000.0, 5000.0, 1.0, optReservesRatio = optimalReserveRatio)
    println("Initial reserve ratio 6: " + contract.reservesRatio())
    assert(contract.reservesRatio() > contract.optimalReservesRatio)
    val amountRC = 500
    val expectedAmountBaseIter = contract.calculateBasecoinsForMintedReservecoinsIter(amountRC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForMintedReservecoins(amountRC)
    assert(roundAt(expectedAmountBase, 3) == roundAt(expectedAmountBaseIter, 3))
    val amountBase = contract.buyReservecoins(amountRC).get
    assert(amountBase == expectedAmountBase)
    assert(contract.reservesRatio() > contract.optimalReservesRatio)
    println("New reserve ratio 5: " + contract.reservesRatio())
  }

  test("buy reservecoins when initial reserve ratio at minimum and optimum boundaries") {
    // Test when initial ratio at the minimum
    val contract = createStablecoin(30000.0, 20000.0, 5000.0, 1.0)
    assert(contract.reservesRatio() == contract.pegReservesRatio)
    val amountRC = 100
    val expectedAmountBaseIter = contract.calculateBasecoinsForMintedReservecoinsIter(amountRC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForMintedReservecoins(amountRC)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))
    val amountBase = contract.buyReservecoins(amountRC).get
    assert(amountBase == expectedAmountBase)

    // Test when initial ratio at the optimum
    val contract2 = createStablecoin(80000.0, 20000.0, 5000.0, 1.0)
    assert(contract2.optimalReservesRatio == contract2.reservesRatio())
    val amountRC2 = 100
    val expectedAmountBaseIter2 = contract2.calculateBasecoinsForMintedReservecoinsIter(amountRC2, 1000)
    val expectedAmountBase2 = contract2.calculateBasecoinsForMintedReservecoins(amountRC2)
    assert(roundAt(expectedAmountBase2, 3) == roundAt(expectedAmountBaseIter2, 3))
    assert(expectedAmountBase2 == contract2.buyReservecoins(amountRC2).get)
    assert(contract2.reservesRatio() > contract2.optimalReservesRatio)
  }

  test("buy reservecoins when base fee or k_rm equals zero") {
    // Test when base fee equals zero
    val contract = createStablecoin(29000.0, 20000.0, 5000.0, 1.0, fee = 0.0, optReservesRatio = 1.7)
    assert(contract.reservesRatio() < contract.pegReservesRatio)
    val amountRC = 3000
    val expectedAmountBaseIter = contract.calculateBasecoinsForMintedReservecoinsIter(amountRC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForMintedReservecoins(amountRC)
    assert(roundAt(expectedAmountBase, 3) == roundAt(expectedAmountBaseIter, 3))
    assert(expectedAmountBase == contract.buyReservecoins(amountRC).get)
    assert(contract.optimalReservesRatio < contract.reservesRatio())

    // Test when k_rm equals zero
    val contract2 = createStablecoin(29000.0, 20000.0, 5000.0, 1.0, k_rm = 0.0, optReservesRatio = 1.7)
    assert(contract2.reservesRatio() < contract2.pegReservesRatio)
    val amountRC2 = 3000
    val expectedAmountBaseIter2 = contract2.calculateBasecoinsForMintedReservecoinsIter(amountRC2, 1000)
    val expectedAmountBase2 = contract2.calculateBasecoinsForMintedReservecoins(amountRC2)
    assert(roundAt(expectedAmountBase2, 3) == roundAt(expectedAmountBaseIter2, 3))
    assert(expectedAmountBase2 == contract2.buyReservecoins(amountRC2).get)
    assert(contract2.reservesRatio() > contract2.optimalReservesRatio)
  }

  test("sell reservecoins") {
    val contract = createStablecoin(20000.0, 20000.0, 5000.0, 1.0)
    val amountRC = 100

    val expectedAmountBaseIter = contract.calculateBasecoinsForRedeemedReservecoinsIter(amountRC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForRedeemedReservecoins(amountRC)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))

    val amountBase = contract.sellReservecoins(amountRC).get
    assert(amountBase == expectedAmountBase)
    assert(contract.getStablecoinsAmount == contract.initStablecoins)
    assert(contract.getReservesAmount == contract.initReserves - amountBase)
    assert(contract.getReservecoinsAmount == contract.initReservecoins - amountRC)
  }

  test("sell reservecoins (1-st interval): initial and new reserve ratio are below minimum") {
    val contract = createStablecoin(20000.0, 20000.0, 5000.0, 1.0)
    println("Initial reserve ratio 1: " + contract.reservesRatio())
    assert(contract.reservesRatio() < contract.pegReservesRatio)
    val amountRC = 100
    val expectedAmountBaseIter = contract.calculateBasecoinsForRedeemedReservecoinsIter(amountRC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForRedeemedReservecoins(amountRC)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))
    val amountBase = contract.sellReservecoins(amountRC).get
    assert(amountBase == expectedAmountBase)
    println("New reserve ratio 1: " + contract.reservesRatio())
    assert(contract.reservesRatio() < contract.pegReservesRatio)
  }

  test("sell reservecoins (2-nd interval):" +
    "initial reserve ratio is above minimum but below optimum, new ratio is below optimum") {
    val contract2 = createStablecoin(30010.0, 20000.0, 5000.0, 1.0)
    println("Initial reserve ratio 2: " + contract2.reservesRatio())
    assert(contract2.reservesRatio() < contract2.optimalReservesRatio
      && contract2.reservesRatio() > contract2.pegReservesRatio)
    val amountRC2 = 200
    val expectedAmountBaseIter2 = contract2.calculateBasecoinsForRedeemedReservecoinsIter(amountRC2, 1000)
    val expectedAmountBase2 = contract2.calculateBasecoinsForRedeemedReservecoins(amountRC2)
    assert(roundAt(expectedAmountBase2, 4) == roundAt(expectedAmountBaseIter2, 4))
    val amountBase2 = contract2.sellReservecoins(amountRC2).get
    assert(amountBase2 == expectedAmountBase2)
    println("New reserve ratio 2: " + contract2.reservesRatio())
    assert(contract2.reservesRatio() < contract2.pegReservesRatio)
  }

  test("sell reservecoins (3-rd interval):" +
    " initial reserve ratio above the optimum, new ratio is below the minimum") {

    val optimalReserveRatio = 1.6
    val contract3 = createStablecoin(32500.0, 20000.0, 5000.0, 1.0, optReservesRatio = optimalReserveRatio)
    println("Initial reserve ratio 3: " + contract3.reservesRatio())
    assert(contract3.reservesRatio() > contract3.optimalReservesRatio)
    val amountRC3 = 2000
    val expectedAmountBaseIter3 = contract3.calculateBasecoinsForRedeemedReservecoinsIter(amountRC3, 1000)
    val expectedAmountBase3 = contract3.calculateBasecoinsForRedeemedReservecoins(amountRC3)
    assert(roundAt(expectedAmountBase3, 4) == roundAt(expectedAmountBaseIter3, 4))
    val amountBase3 = contract3.sellReservecoins(amountRC3).get
    assert(amountBase3 == expectedAmountBase3)
    assert(contract3.reservesRatio() < contract3.pegReservesRatio)
    println("New reserve ratio 3: " + contract3.reservesRatio())
  }

  test("sell reservecoins (4-th interval):" +
    "initial and new reserve ratio are above minimum but below optimum") {
    val contract4 = createStablecoin(40000.0, 20000.0, 5000.0, 1.0)
    println("Initial reserve ratio 4: " + contract4.reservesRatio())
    assert(contract4.reservesRatio() < contract4.optimalReservesRatio
      && contract4.reservesRatio() > contract4.pegReservesRatio)
    val amountRC4 = 200
    val expectedAmountBaseIter4 = contract4.calculateBasecoinsForRedeemedReservecoinsIter(amountRC4, 1000)
    val expectedAmountBase4 = contract4.calculateBasecoinsForRedeemedReservecoins(amountRC4)
    assert(roundAt(expectedAmountBase4, 4) == roundAt(expectedAmountBaseIter4, 4))
    val amountBase4 = contract4.sellReservecoins(amountRC4).get
    assert(amountBase4 == expectedAmountBase4)
    println("New reserve ratio 4: " + contract4.reservesRatio())
    assert(contract4.reservesRatio() < contract4.optimalReservesRatio
      && contract4.reservesRatio() > contract4.pegReservesRatio)
  }

  test("sell reservecoins (5-th interval): " +
    "initial reserve ratio is above the optimum" +
    "new ratio is above minimal but below optimal level") {

    val optimalReserveRatio = 2
    val contract5 = createStablecoin(41000.0, 20000.0, 5000.0, 1.0, optReservesRatio = optimalReserveRatio)
    println("Initial reserve ratio 5: " + contract5.reservesRatio())
    assert(contract5.reservesRatio() > contract5.optimalReservesRatio)
    val amountRC5 = 1000
    val expectedAmountBaseIter5 = contract5.calculateBasecoinsForRedeemedReservecoinsIter(amountRC5, 1000)
    val expectedAmountBase5 = contract5.calculateBasecoinsForRedeemedReservecoins(amountRC5)
    assert(roundAt(expectedAmountBase5, 3) == roundAt(expectedAmountBaseIter5, 3))
    val amountBase5 = contract5.sellReservecoins(amountRC5).get
    assert(amountBase5 == expectedAmountBase5)
    assert(contract5.reservesRatio() > contract5.pegReservesRatio
      && contract5.reservesRatio() < contract5.optimalReservesRatio)
    println("New reserve ratio 5: " + contract5.reservesRatio())
  }

  test("sell reservecoins (6-th interval): " +
    "initial and new reserve ratio are above the optimum") {

    val optimalReserveRatio = 2
    val contract6 = createStablecoin(50000.0, 20000.0, 5000.0, 1.0, optReservesRatio = optimalReserveRatio)
    println("Initial reserve ratio 6: " + contract6.reservesRatio())
    assert(contract6.reservesRatio() > contract6.optimalReservesRatio)
    val amountRC6 = 1000
    val expectedAmountBaseIter6 = contract6.calculateBasecoinsForRedeemedReservecoinsIter(amountRC6, 1000)
    val expectedAmountBase6 = contract6.calculateBasecoinsForRedeemedReservecoins(amountRC6)
    assert(roundAt(expectedAmountBase6, 4) == roundAt(expectedAmountBaseIter6, 4))
    val amountBase6 = contract6.sellReservecoins(amountRC6).get
    assert(amountBase6 == expectedAmountBase6)
    assert(contract6.reservesRatio() > contract6.optimalReservesRatio)
    println("New reserve ratio 6: " + contract6.reservesRatio())
  }

  test("sell reservecoins when initial ratio at minimum/optimum boundaries") {
    // Test when initial ratio at the minimum
    val contract = createStablecoin(30000.0, 20000.0, 5000.0, 1.0)
    assert(contract.reservesRatio() == contract.pegReservesRatio)
    val amountRC = 100
    val expectedAmountBaseIter = contract.calculateBasecoinsForRedeemedReservecoinsIter(amountRC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForRedeemedReservecoins(amountRC)
    assert(roundAt(expectedAmountBase, 5) == roundAt(expectedAmountBaseIter, 5))
    assert(expectedAmountBase == contract.sellReservecoins(amountRC).get)
    assert(contract.reservesRatio() < contract.optimalReservesRatio)

    // Test when initial ratio at the optimum
    val contract2 = createStablecoin(40000.0, 20000.0, 5000.0, 1.0, optReservesRatio = 2)
    assert(contract2.optimalReservesRatio == contract2.reservesRatio())
    val expectedAmountBaseIter2 = contract2.calculateBasecoinsForRedeemedReservecoinsIter(amountRC, 1000)
    val expectedAmountBase2 = contract2.calculateBasecoinsForRedeemedReservecoins(amountRC)
    assert(roundAt(expectedAmountBase2, 3) == roundAt(expectedAmountBaseIter2, 3))
    assert(expectedAmountBase2 == contract2.sellReservecoins(amountRC).get)
    assert(contract2.reservesRatio() < contract2.optimalReservesRatio)
  }

  test("sell reservecoins when base fee or k_rr equals zero") {
    // Test when base fee equals zero
    val contract = createStablecoin(34100.0, 20000.0, 5000.0, 1.0, fee = 0.0, optReservesRatio = 1.7)
    assert(contract.reservesRatio() > contract.optimalReservesRatio)
    val amountRC = 3000
    val expectedAmountBaseIter = contract.calculateBasecoinsForRedeemedReservecoinsIter(amountRC, 1000)
    val expectedAmountBase = contract.calculateBasecoinsForRedeemedReservecoins(amountRC)
    assert(roundAt(expectedAmountBase, 3) == roundAt(expectedAmountBaseIter, 3))
    assert(expectedAmountBase == contract.sellReservecoins(amountRC).get)
    assert(contract.pegReservesRatio > contract.reservesRatio())

    // Test when k_rm equals zero
    val contract2 = createStablecoin(34100.0, 20000.0, 5000.0, 1.0, k_rr = 0.0, optReservesRatio = 1.7)
    assert(contract2.reservesRatio() > contract2.optimalReservesRatio)
    val expectedAmountBaseIter2 = contract2.calculateBasecoinsForRedeemedReservecoinsIter(amountRC, 1000)
    val expectedAmountBase2 = contract2.calculateBasecoinsForRedeemedReservecoins(amountRC)
    assert(roundAt(expectedAmountBase2, 3) == roundAt(expectedAmountBaseIter2, 3))
    assert(expectedAmountBase2 == contract2.sellReservecoins(amountRC).get)
    assert(contract2.reservesRatio() < contract2.pegReservesRatio)
  }
}

object ExtendedDjedStablecoinTest {
  val baseFee = 0.03
  val pegReservesRatio = 1.5
  val optimalReserveRatio = 4
  val reservecoinDefaultPrice = 0.5
  val k_rm_def = 1.1    // fee deviation coeff for RC minting
  val k_rr_def = 1.1    // fee deviation coeff for RC redeeming
  val k_sm_def = 1.1    // fee deviation coeff for SC minting
  val k_sr_def = 1.1    // fee deviation coeff for SC redeeming

  def createStablecoin(initReserves: N,
                       initStablecoins: N,
                       initReservecoins: N,
                       exchangeRate: N,
                       fee: N = baseFee,
                       k_rm: N = k_rm_def,
                       k_rr: N = k_rr_def,
                       k_sm: N = k_sm_def,
                       k_sr: N = k_sr_def,
                       defaultPrice: N = reservecoinDefaultPrice,
                       optReservesRatio: N = optimalReserveRatio) = {
    val oracle = new Oracle
    oracle.updateConversionRate(PegCurrency, BaseCoin, exchangeRate)

    new ExtendedDjedStablecoin(
      oracle,
      fee,
      pegReservesRatio,
      optReservesRatio,
      defaultPrice,
      k_rm,
      k_rr,
      k_sm,
      k_sr,
      initReserves,
      initStablecoins,
      initReservecoins)
  }
}