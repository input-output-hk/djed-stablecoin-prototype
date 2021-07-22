package stablecoin

import org.scalatest.FunSuite
import stablecoin.Ledger.CoinType.{BaseCoin, ReserveCoin, StableCoin}

class LedgerTest extends FunSuite {

  val initBaseAccounts = Map[Address,N]((0x1->5.0), (0x2->10.0), (0x3->3.0))
  val initStablecoinAccounts = Map[Address,N]((0x1->50), (0x2->30), (0x3->7))
  val initReservecoinAccounts = Map[Address,N]((0x1->1), (0x2->2), (0x3->3))

  def createDefaultLedger() = {
    val stablecoinContract = MinimalDjedStablecoinTest.createStablecoinContract(30, 87, 6)
    new Ledger(initBaseAccounts, initStablecoinAccounts, initReservecoinAccounts, stablecoinContract)
  }

  test("transfer transaction") {
    val ledger = createDefaultLedger()

    val tx1 = TransferTransaction(0x1, 0x2, 1.3, BaseCoin)
    assert(ledger.addTransaction(tx1).isSuccess)
    assert(ledger.getBasecoinAccounts(0x1) == initBaseAccounts(0x1) - 1.3)
    assert(ledger.getBasecoinAccounts(0x2) == initBaseAccounts(0x2) + 1.3)
    assert(ledger.getStablecoinAccounts == initStablecoinAccounts)
    assert(ledger.getReservecoinAccounts == initReservecoinAccounts)

    val tx2 = TransferTransaction(0x2, 0x5, 10, StableCoin)
    assert(ledger.addTransaction(tx2).isSuccess)
    assert(ledger.getStablecoinAccounts(0x2) == initStablecoinAccounts(0x2) - 10)
    assert(ledger.getStablecoinAccounts(0x5) == 10)
    assert(ledger.getReservecoinAccounts == initReservecoinAccounts)

    val tx3 = TransferTransaction(0x3, 0x1, 3, ReserveCoin)
    assert(ledger.addTransaction(tx3).isSuccess)
    assert(ledger.getReservecoinAccounts(0x3) == initReservecoinAccounts(0x3) - 3)
    assert(ledger.getReservecoinAccounts(0x1) == initReservecoinAccounts(0x1) + 3)

    val badTxs = List(
      TransferTransaction(0x1, 0x2, -1, BaseCoin),
      TransferTransaction(0x1, 0x2, 0, StableCoin),
      TransferTransaction(0x0, 0x2, 1, BaseCoin),
      TransferTransaction(0x1, 0x2, 5, ReserveCoin)
    )
    badTxs.foreach { tx =>
      require(ledger.addTransaction(tx).isFailure)
    }

    // check final ledger state
    assert(ledger.getBasecoinAccounts.size == 3)
    assert(ledger.getBasecoinAccounts(0x1) == initBaseAccounts(0x1) - 1.3)
    assert(ledger.getBasecoinAccounts(0x2) == initBaseAccounts(0x2) + 1.3)
    assert(ledger.getBasecoinAccounts(0x3) == initBaseAccounts(0x3))
    assert(ledger.getStablecoinAccounts.size == 4)
    assert(ledger.getStablecoinAccounts(0x1) == initStablecoinAccounts(0x1))
    assert(ledger.getStablecoinAccounts(0x2) == initStablecoinAccounts(0x2) - 10)
    assert(ledger.getStablecoinAccounts(0x3) == initStablecoinAccounts(0x3))
    assert(ledger.getStablecoinAccounts(0x5) == 10)
    assert(ledger.getReservecoinAccounts.size == 3)
    assert(ledger.getReservecoinAccounts(0x1) == initReservecoinAccounts(0x1) + 3)
    assert(ledger.getReservecoinAccounts(0x2) == initReservecoinAccounts(0x2))
    assert(ledger.getReservecoinAccounts(0x3) == initReservecoinAccounts(0x3) - 3)

  }

  test("buy stablecoins transaction") {
    val ledger = createDefaultLedger()

    val tx1 = BuyStablecoinTransaction(0x1, 5)
    val amountBaseToPay = ledger.stablecoinContract.calculateAmountBaseToPayForStablecoins(5)
    assert(ledger.addTransaction(tx1).isSuccess)
    assert(ledger.getBasecoinAccounts(0x1) == initBaseAccounts(0x1) - amountBaseToPay)
    assert(ledger.getStablecoinAccounts(0x1) == initStablecoinAccounts(0x1) + 5)
    assert(ledger.stablecoinContract.getReservesAmount == 30 + amountBaseToPay)
    assert(ledger.stablecoinContract.getStablecoinsAmount == 87 + 5)

    val badTxs = List(
      BuyStablecoinTransaction(0x1, 100),
      BuyStablecoinTransaction(0x1, 0),
      BuyStablecoinTransaction(0x0, 5))
    badTxs.foreach(ledger.addTransaction(_).isFailure)
    assert(ledger.stablecoinContract.getReservesAmount == 30 + amountBaseToPay)
    assert(ledger.stablecoinContract.getStablecoinsAmount == 87 + 5)
  }

  test("sell stablecoins transaction") {
    val ledger = createDefaultLedger()
    val contract = ledger.stablecoinContract

    val tx1 = SellStablecoinTransaction(0x1, 10)
    val price = contract.stablecoinNominalPrice(contract.getReservesAmount, contract.getStablecoinsAmount)
    val expectedBaseAmountToReturn = 10 * (1 - contract.fee) * price

    assert(ledger.addTransaction(tx1).isSuccess)
    assert(ledger.getBasecoinAccounts(0x1) == initBaseAccounts(0x1) + expectedBaseAmountToReturn)
    assert(ledger.getStablecoinAccounts(0x1) == initStablecoinAccounts(0x1) - 10)
    assert(ledger.stablecoinContract.getReservesAmount == 30 - expectedBaseAmountToReturn)
    assert(ledger.stablecoinContract.getStablecoinsAmount == 87 - 10)

    val badTxs = List(
      SellStablecoinTransaction(0x1, 100),
      SellStablecoinTransaction(0x1, 0),
      SellStablecoinTransaction(0x0, 5))
    badTxs.foreach(ledger.addTransaction(_).isFailure)
    assert(ledger.stablecoinContract.getStablecoinsAmount == 87 - 10)
  }

  test("buy reservecoins transaction") {
    val ledger = createDefaultLedger()
    val contract = ledger.stablecoinContract

    val tx1 = BuyReservecoinTransaction(0x1, 2)
    val amountBaseToPay = contract.calculateAmountBaseToPayForReservecoins(2)
    assert(ledger.addTransaction(tx1).isSuccess)
    assert(ledger.getBasecoinAccounts(0x1) == initBaseAccounts(0x1) - amountBaseToPay)
    assert(ledger.getReservecoinAccounts(0x1) == initReservecoinAccounts(0x1) + 2)
    assert(ledger.getStablecoinAccounts == initStablecoinAccounts)
    assert(contract.getReservesAmount == 30 + amountBaseToPay)
    assert(contract.getReservecoinsAmount == 6 + 2)

    val badTxs = List(
      BuyReservecoinTransaction(0x1, 10),
      BuyReservecoinTransaction(0x1, 0),
      BuyReservecoinTransaction(0x0, 1))
    badTxs.foreach(ledger.addTransaction(_).isFailure)
    assert(contract.getReservecoinsAmount == 6 + 2)
  }

  test("sell reservecoins transaction") {
    val ledger = createDefaultLedger()
    val contract = ledger.stablecoinContract

    val amountRC = 1.5
    val tx1 = SellReservecoinTransaction(0x3, amountRC)
    val price = contract.reservecoinNominalPrice(contract.getReservesAmount,
      contract.getStablecoinsAmount, contract.getReservecoinsAmount)
    val expectedBaseAmountToReturn = amountRC * (1 - contract.fee) * price

    assert(ledger.addTransaction(tx1).isSuccess)
    assert(ledger.getBasecoinAccounts(0x3) == initBaseAccounts(0x3) + expectedBaseAmountToReturn)
    assert(ledger.getReservecoinAccounts(0x3) == initReservecoinAccounts(0x3) - amountRC)
    assert(contract.getReservesAmount == 30 - expectedBaseAmountToReturn)
    assert(contract.getReservecoinsAmount == 6 - amountRC)

    val badTxs = List(
      SellStablecoinTransaction(0x1, 2),
      SellStablecoinTransaction(0x1, 0),
      SellStablecoinTransaction(0x0, 5))
    badTxs.foreach(ledger.addTransaction(_).isFailure)
    assert(contract.getReservecoinsAmount == 6 - amountRC)
  }

  test("transactions history") {
    val ledger  = createDefaultLedger()

    val txs = List(
      TransferTransaction(0x1, 0x2, 1, BaseCoin),
      BuyStablecoinTransaction(0x3, 1),
      BuyStablecoinTransaction(0x1, 1000), // invalid tx
      SellReservecoinTransaction(0x2, 1)
    )
    txs.foreach(ledger.addTransaction(_))

    assert(ledger.getTransactionsHistory.size == 3)
    assert(ledger.getTransactionsHistory(0) == txs(0))
    assert(ledger.getTransactionsHistory(1) == txs(1))
    assert(ledger.getTransactionsHistory(2) == txs(3))
  }
}
