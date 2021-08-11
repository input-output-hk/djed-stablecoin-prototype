package stablecoin

import stablecoin.Currency.{BaseCoin, PegCurrency}

import scala.math.BigDecimal.RoundingMode
import scala.util.Try

// The bank keeps a reserve of an underlying base cryptocurrency
// The bank issues and redeems stablecoins and reservecoins
//
// The stablecoin is pegged to a peg currency
// The exchange rate between the peg currency and the base cryptocurrency is provided by an oracle
//
// The holder of a stablecoin has a claim to a variable portion of the reserves according to the exchange rate
//
// The bank's equity is the bank's reserves minus the bank's liabilities to the stablecoin holders
// The bank's equity is owned by the reservecoin holders
//
// The bank profits (and its reserve grows) by minting and redeeming the stablecoins and reservecoins for a fee
//
// The bank's reserve ratio is the bank's reserves divided by the amount of stablecoins in circulation
// The bank is only allowed to issue and sell stablecoins and reservecoins if the reserve ratio
// remains above a minimum threshold and below a maximum threshold.
// The maximum threshold prevents dilution (https://en.wikipedia.org/wiki/Stock_dilution) for reservecoin holders.
// The minimum threshold aims to ensure that stablecoins remain fully backed by reserves
// even if the price of the base currency falls.

class MinimalDjedStablecoin(val oracle: Oracle, // Oracle used by the bank
                            val fee: N, // the fee charged by the bank when buying and selling stablecoins and reservecoins
                            val minReserveRatio: N, // the bank's minimum reserve ratio
                            val maxReserveRatio: N, // the bank's maximum reserve ratio
                            val reservecoinDefaultPrice: N, // default price of reservecoins, used when there are 0 reservecoins, 1 RC = 1 baseCoin * defaultPrice
                            val initReserves: N = 0.0, // initial amount of basecoin reserves
                            val initStablecoins: N = 0.0, // initial amount of stablecoins
                            val initReservecoins: N = 0.0) // initial amount of reservecoins
{
  // ## Bank's State
  protected var reserves: N = initReserves          // The bank's reserves in the base cryptocurrency
  protected var stablecoins: N = initStablecoins    // The amount of stablecoins currently in circulation
  protected var reservecoins: N = initReservecoins  // The amount of reservecoins currently in circulation

  require(initReservecoins > 0) // we should have some RC at the beginning to be able to calculate it's price, otherwise we will have exceptions

  def getReservesAmount = reserves
  def getStablecoinsAmount = stablecoins
  def getReservecoinsAmount = reservecoins

  // ## Auxiliary Functions
  // All functions here are total, side-effect-free and referentially transparent

  protected def liabilities(r: N, sc: N): N = {
    min(r, sc * oracle.conversionRate(PegCurrency, BaseCoin))
  } ensuring { _ <= r}

  protected def equity(r: N, sc: N): N = {
    r - liabilities(r, sc)
  } ensuring { _ >= 0 }

  protected def maxReserve(sc: N): N = maxReserveRatio * sc * oracle.conversionRate(PegCurrency, BaseCoin)

  protected def minReserve(sc: N): N = minReserveRatio * sc * oracle.conversionRate(PegCurrency, BaseCoin)

  def reservesRatio(r: N, sc: N): BigDecimal = {
    r / (sc * oracle.conversionRate(PegCurrency, BaseCoin))
  }

  def reservesRatio(): BigDecimal = reservesRatio(reserves, stablecoins)

  // The reservecoin's nominal price is its book value
  // (i.e. the equity divided by the number of reservecoins in circulation).
  // Alternatively, we could have taken into account the present value of expected future fee revenues
  // or the market price of reservecoins provided by the oracle.
  def reservecoinNominalPrice(r: N, sc: N, rc: N): N = {
    if (rc != 0) equity(r, sc)/rc else reservecoinDefaultPrice   // FIXME: if reserves = liabilities then equity = 0 and price will be zero
  }

  def reservecoinNominalPrice(): N = reservecoinNominalPrice(reserves, stablecoins, reservecoins)

  def stablecoinNominalPrice(r: N, sc: N): N = {
    val p = oracle.conversionRate(PegCurrency, BaseCoin)
    if (sc == 0) p else min(p, liabilities(r, sc)/sc)         // NOTE: if reserves are below liabilities the nominal price will increase for the stablecoins redeemed later due to collected fees. Is it Ok?
  }

  def stablecoinNominalPrice(): N = stablecoinNominalPrice(reserves, stablecoins)

  // ## General Functions
  // All functions here are total and side-effect free,
  // but they are not referentially transparent, because they depend on the bank's mutable state

  // There are two conditions for the acceptability of a reserve change:
  //  * If we are minting stablecoins or redeeming reservecoins, the new reserves shouldn't drop below the minimum.
  //  * If we are minting reservecoins, the new reserves shouldn't rise above the maximum.
  // Note that the new reserves can go above the maximum when stablecoins are being redeemed.
  // This ensures that stablecoin holders can always redeem their stablecoins. The only effect on
  // reservecoin holders when the reserves rise above the maximum is a reduction of the leverage of
  // the reservecoins in relation to the base currency.
  def acceptableReserveChange(mintsSC: Boolean,
                              mintsRC: Boolean,
                              redeemsRC: Boolean,
                              r: N, sc: N): Boolean = {
    def implies(a: Boolean, b: Boolean) = !a || b
    implies((mintsSC || redeemsRC), (r >= minReserve(sc))) && implies(mintsRC, (r <= maxReserve(sc)))
  }


  // A transaction that mints/redeems `amountSC` and `amountRC`
  // and withdraws/deposits `amountBase` in/from the bank
  // is valid if and only if all of the following hold:
  //    * the change in the reserves is acceptable
  //    * the price paid per reservecoin is the current nominal price
  //    * the price paid per stablecoin is the current nominal price
  //    * the fee paid is correct
  //
  // Note that a positive value means minting (in the case of `amountSC` and `amountRC`)
  // or withdrawing (in the case of `amountBase`) and a negative value means redeeming or depositing (respectively)
  def isValidTransaction(amountBase: N, amountSC: N, amountRC: N, feee: N): Boolean = {
    val scValueInBase = amountSC * stablecoinNominalPrice(reserves, stablecoins)
    val rcValueInBase = amountRC * reservecoinNominalPrice(reserves, stablecoins, reservecoins)

    val newReserves = reserves - amountBase + feee
    val newStablecoins = stablecoins + amountSC

    val correctPrices = { scValueInBase + rcValueInBase + amountBase == 0 }
    val correctFee = { feee == (abs(scValueInBase) + abs(rcValueInBase)) * fee }

    acceptableReserveChange(amountSC > 0, amountRC > 0, amountRC < 0, newReserves, newStablecoins) && correctPrices && correctFee
  }

  // Given amounts of stablecoins and reservecoins that one wants to mint (if positive) or redeem (if negative),
  // this function calculates how much one should withdraw (of positive) or deposit in the base currency and the fee
  // that must be paid in base currency.
  // Dmytro:
  // TODO: I found this function confusing. For instance:
  //    - in case of minting new SC/RC it returns negative amountBase and positive fee.
  //      Eventually, a user to calculate amount that should be paid will need to do 'abs(amountBase) + fee'
  //    - in case of redeeming SC/RC it returns positive amountBase, which includes also paid fees, so a user to calculate
  //      the returned amount should do this: 'amountBase - fee'
  // I found all this quite confusing. So, even though the function seems to work properly I decided not to use it to
  // implement 4 basic operations and implement each operation separately for better understandability
  def mintOrRedeem(amountSC: N, amountRC: N): Option[(N,N)] = {
    val scValueInBase = amountSC * stablecoinNominalPrice(reserves, stablecoins)
    val rcValueInBase = amountRC * reservecoinNominalPrice(reserves, stablecoins, reservecoins)

    val amountBase = - (scValueInBase + rcValueInBase)
    val feee = (abs(scValueInBase) + abs(rcValueInBase)) * fee

    val newReserves = reserves - amountBase + feee
    val newStablecoins = stablecoins + amountSC

    if (acceptableReserveChange(amountSC > 0, amountRC > 0, amountRC < 0, newReserves, newStablecoins)) {
      Some((amountBase, feee))
    }
    else None
  } ensuring { _ match {
    case Some((amountBase, feeInBase)) => isValidTransaction(amountBase, amountSC, amountRC, feeInBase)
    case None => true
  }}

  /**
   * Calculates how much basecoins a user should pay (including fee) to receive amountSC stablecoins
   */
  def calculateAmountBaseToPayForStablecoins(amountSC: N) = {
    val amountBaseToPay = amountSC * stablecoinNominalPrice(reserves, stablecoins)
    val feeToPay = amountBaseToPay * fee
    amountBaseToPay + feeToPay
  }

  /**
   * @param amountSC amount of stablecoins that will be issued
   * @return amount of basecoins that is put to the reserve
   */
  def buyStablecoin(amountSC: N): Try[N] = Try {
    require(amountSC > 0)

    val amountBase = calculateAmountBaseToPayForStablecoins(amountSC)

    val newReserves = reserves + amountBase
    val newStablecoins = stablecoins + amountSC
    require(acceptableReserveChange(true, false, false, newReserves, newStablecoins))

    reserves += amountBase
    stablecoins += amountSC
    amountBase
  }

  /**
   * @param amountSC amount of stablecoins that should be exchanged for base coins
   * @return amount of base coins withdrawn from the bank
   */
  def sellStablecoin(amountSC: N): Try[N] = Try {
    require(amountSC > 0)

    val scValueInBase = amountSC * stablecoinNominalPrice(reserves, stablecoins)
    val collectedFee = scValueInBase * fee
    val amountBaseToReturn = scValueInBase - collectedFee

    val newReserves = reserves - amountBaseToReturn
    val newStablecoins = stablecoins - amountSC
    require(acceptableReserveChange(false, false, false, newReserves, newStablecoins))

    reserves = newReserves
    stablecoins = newStablecoins
    amountBaseToReturn
  }

  /* Iterative price calculation that recalculate the price after buying each coin. Accuracy parameter defines
  *  how often price is recalculated. I.e. after each coin or after 0.1 (accuracy=10) part of a coin etc.
  *  For testing puproses. */
  def calculateAmountBaseToPayForReservecoinsIter(amountRC: Int, accuracy: Int = 1) = {
    var newReserves = reserves
    var newReservecoins = reservecoins
    var totalAmountBaseToPay: N = 0.0

    for(i <- 0 until amountRC * accuracy) {
      val price = reservecoinNominalPrice(newReserves, stablecoins, newReservecoins)
      val amountBaseToPay = price * (1 + fee) / accuracy
      newReserves += amountBaseToPay
      newReservecoins += (1.0 / accuracy)
      totalAmountBaseToPay += amountBaseToPay
    }

    totalAmountBaseToPay
  }

  /**
   * Calculates how many base coins (including fees) should be paid to purchase amountRC reservecoins. If current reservecoins
   * price is above minimal price, we use a special formula to calculate the price of coins, otherwise we sell with minimal price.
   * Note that there is a limit to how many coins can be bought with minimal price. Since the price increases with each buying
   * we also have a special formula that calculates how many reservecoins can be bought with minimal price until their nominal
   * price reaches minimal price. After that the nominal price is used.
   */
  def calculateAmountBaseToPayForReservecoins(amountRC: N) = {
    /* Differential price calculation. The aim is to recalculate the price after buying each smallest portion of the coin (like
    * in previous version for iterative price calculation). But here we assume that the coin is divided infinitely. Thus we
    * used mathematical analysis to obtain the precise formula.
    */
    def calculateAmountToPay(amount: N, inputReserves: N, inputReservecoins: N) = {
      require(reservecoinNominalPrice(inputReserves, stablecoins, inputReservecoins) >= reservecoinDefaultPrice)

      val newReservecoins = inputReservecoins + amount
      val l = liabilities(inputReserves, stablecoins)

      // BigDecimal doesn't allow fractional pow, so we use math.pow(Double,Double) as a workaround.
      // It shouldn't be a problem since we don't expect big numbers here. But it has a side effect of rounding decimal part of BigDecimals,
      // which may result in tiny inconsistencies of paid amounts if we split buy operation into several transactions compared to
      // the wholesale buy. This should be handled properly in the production implementation.
      val tmp1 = math.pow((newReservecoins / inputReservecoins).doubleValue, (1 + fee).doubleValue)
      val tmp2 = inputReserves - l
      val newReserves = tmp1 * tmp2 + l
      newReserves - inputReserves
    }

    if (reservecoinNominalPrice() < reservecoinDefaultPrice) {
      val l = liabilities(reserves, stablecoins)
      val maxToBuyForMinPrice = (l - reserves + reservecoins*reservecoinDefaultPrice) / (fee*reservecoinDefaultPrice)

      if (maxToBuyForMinPrice >= amountRC)
        amountRC*reservecoinDefaultPrice*(1+fee)
      else {
        val toPayWithMinPrice = maxToBuyForMinPrice * reservecoinDefaultPrice * (1+fee)
        val toPayWithNominalPrice = calculateAmountToPay(amountRC-maxToBuyForMinPrice,
                                                         reserves+toPayWithMinPrice,
                                                         reservecoins+maxToBuyForMinPrice)
        toPayWithMinPrice + toPayWithNominalPrice
      }
    }
    else calculateAmountToPay(amountRC, reserves, reservecoins)
  }

  /**
   * @param amountRC amount of reservecoins that will be issued
   * @return amount of basecoins that is put to the reserve
   */
  def buyReservecoin(amountRC: N): Try[N] = Try {
    require(amountRC > 0)

    val amountBase = calculateAmountBaseToPayForReservecoins(amountRC)
    val newReserves = reserves + amountBase
    val newReservecoins = reservecoins + amountRC
    require(acceptableReserveChange(false, true, false, newReserves, stablecoins))

    reserves = newReserves
    reservecoins = newReservecoins
    amountBase
  }

  /* Iterative price calculation that recalculate the price after selling each coin. Accuracy parameter defines
  *  how often price is recalculated. I.e. after each coin or after 0.1 (accuracy=10) part of a coin etc.
  *  For testing puproses. */
  def calculateAmountBaseToRedeemReservecoinsIter(amountRC: Int, accuracy: Int = 1) = {
    var newReserves = reserves
    var newReservecoins = reservecoins
    var totalAmountBaseToReturn: N = 0.0

    for(i <- 0 until amountRC * accuracy) {
      val price = reservecoinNominalPrice(newReserves, stablecoins, newReservecoins)
      val amountBase = price / accuracy
      val amountBaseToReturn = amountBase * (1 - fee)
      newReserves -= amountBaseToReturn
      newReservecoins -= (1.0 / accuracy)
      totalAmountBaseToReturn += amountBaseToReturn
    }

    totalAmountBaseToReturn
  }

  def calculateAmountBaseToRedeemReservecoins(amountRC: N) = {
    val l = liabilities(reserves, stablecoins)
    val newReservecoins = reservecoins - amountRC
    require(newReservecoins > 0)

    // BigDecimal doesn't allow fractional pow, so we use math.pow(Double,Double) as a workaround.
    // It shouldn't be a problem since we don't expect big numbers here. But it has a side effect of rounding decimal part of BigDecimals,
    // which may result in tiny inconsistencies of paid amounts if we split buy operation into several transactions compared to
    // the wholesale buy. This should be handled properly in the production implementation.
    val tmp1 = math.pow((newReservecoins / reservecoins).doubleValue, (1 - fee).doubleValue)
    val tmp2 = reserves - l
    val newReserves = tmp1 * tmp2 + l
    reserves - newReserves
  }

  def sellReservecoin(amountRC: N): Try[N] = Try {
    require(amountRC > 0)

    val amountBaseToReturn = calculateAmountBaseToRedeemReservecoins(amountRC)

    val newReserves = reserves - amountBaseToReturn
    val newReservecoins = reservecoins - amountRC
    require(acceptableReserveChange(false, false, true, newReserves, stablecoins))

    reserves = newReserves
    reservecoins = newReservecoins
    amountBaseToReturn
  }

  override def toString: String = {
    "Stablecoin state:\n" +
    "\tBasecoin amount: " + getReservesAmount + "\n" +
    "\tStablecoin amount: " + getStablecoinsAmount + "\n" +
    "\tReservecoin amount: " + getReservecoinsAmount + "\n" +
    "\tStablecoin nominal price: " + stablecoinNominalPrice(reserves, stablecoins) + "\n" +
    "\tReservecoin nominal price: " + reservecoinNominalPrice(reserves, stablecoins, reservecoins) + "\n" +
    "\tReserve ratio: " + reserves / (stablecoins * oracle.conversionRate(PegCurrency, BaseCoin)) + "\n" +
    "\tConversion rate(PegCurrency -> BaseCoin): " + oracle.conversionRate(PegCurrency, BaseCoin)
  }
}