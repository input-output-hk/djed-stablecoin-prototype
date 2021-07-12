package stablecoin

import stablecoin.Ledger.CoinType.CoinType

import scala.util.Random

trait Transaction {
  val id: String = System.currentTimeMillis() + ":" + Random.nextBytes(5)
}
case class TransferTransaction(from: Address, to: Address, amount: N, currency: CoinType) extends Transaction {
  override def toString: String = {
    "TransferTransaction " + "(" + currency + "): " + id + ", from: " + from + ", to: " + to + ", amount: " + amount
  }
}

abstract class ContractCallTransaction(from: Address) extends Transaction

case class BuyStablecoinTransaction(from: Address, amountSC: N) extends ContractCallTransaction(from) {
  override def toString: String = "BuyStablecoinTransaction: " + id + ", from: " + from + ", amountSC: " + amountSC
}

case class SellStablecoinTransaction(from: Address, amountSC: N) extends ContractCallTransaction(from) {
  override def toString: String = "SellStablecoinTransaction: " + id + ", from: " + from + ", amountSC: " + amountSC
}

case class BuyReservecoinTransaction(from: Address, amountRC: N) extends ContractCallTransaction(from) {
  override def toString: String = "BuyReservecoinTransaction: " + id + ", from: " + from + ", amountRC: " + amountRC
}

case class SellReservecoinTransaction(from: Address, amountRC: N) extends ContractCallTransaction(from) {
  override def toString: String = "SellReservecoinTransaction: " + id + ", from: " + from + ", amountRC: " + amountRC
}
