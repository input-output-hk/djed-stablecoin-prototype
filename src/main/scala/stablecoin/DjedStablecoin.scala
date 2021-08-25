package stablecoin

import scala.util.Try

trait DjedStablecoin {

  def getReservesAmount: N
  def getStablecoinsAmount: N
  def getReservecoinsAmount: N

  def targetPrice: N
  def targetLiabilities(Nsc: N = getStablecoinsAmount): N = Nsc * targetPrice       // full amount of liabilities to SC holders
  def normLiabilities(R: N = getReservesAmount, Nsc: N = getStablecoinsAmount): N   // normalized liabilities - the amount of liabilities that currently can be paid off (lower or equal to targetLiabilities)

  def reservesRatio(R: N = getReservesAmount, Nsc: N = getStablecoinsAmount): N = {
    require(targetLiabilities(Nsc) > 0)
    R / targetLiabilities(Nsc)
  } ensuring(_ >= 0)

  def equity(R: N = getReservesAmount, Nsc: N = getStablecoinsAmount): N = {
    R - normLiabilities(R, Nsc)
  } ensuring(_ >= 0)

  def buyStablecoins(amountSC: N): Try[N]
  def sellStablecoins(amountSC: N): Try[N]
  def buyReservecoins(amountRC: N): Try[N]
  def sellReservecoins(amountRC: N): Try[N]
}
