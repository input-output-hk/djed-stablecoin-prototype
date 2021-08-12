package stablecoin

import stablecoin.Currency.{BaseCoin, PegCurrency}

import scala.util.Try

// The bank keeps a reserve of an underlying base cryptocurrency
// The bank issues and burns stablecoins and reservecoins
//
// The stablecoin is pegged to a target price which can be the price of some fiat currency or a basket of currencies or something else
// The exchange rate between the peg currency and the base cryptocurrency is provided by an oracle
//
// The holder of a stablecoin has a claim to a variable portion of the reserves according to the exchange rate
//
// The bank's equity is the bank's reserves minus the bank's liabilities to the stablecoin holders
// The bank's equity is owned by the reservecoin holders
//
// The bank profits (and its reserve grows) by minting and burning stablecoins and reservecoins for a fee
//
// The bank's reserve ratio is the bank's reserves divided by its liabilities
// The bank is only allowed to issue and sell stablecoins and reservecoins if the reserve ratio
// remains above a minimum threshold and below a maximum threshold.
// The maximum threshold prevents dilution (https://en.wikipedia.org/wiki/Stock_dilution) for reservecoin holders.
// The minimum threshold aims to ensure that stablecoins remain fully backed by reserves
// even if the price of the base currency falls.

class MinimalDjedStablecoin(val oracle: Oracle, // Oracle used by the bank
                            val fee: N, // the fee charged by the bank when buying and selling stablecoins and reservecoins
                            val minReservesRatio: N, // the bank's minimum reserve ratio
                            val maxReservesRatio: N, // the bank's maximum reserve ratio
                            val reservecoinDefaultPrice: N, // default price of RCs, used when there are 0 RCs
                            val initReserves: N = 0.0, // initial amount of basecoin reserves
                            val initStablecoins: N = 0.0, // initial amount of stablecoins
                            val initReservecoins: N = 0.0) // initial amount of reservecoins
extends DjedStablecoin {
  // Bank's State
  private var reserves: N = initReserves          // The bank's reserves in the base cryptocurrency
  private var stablecoins: N = initStablecoins    // The amount of stablecoins currently in circulation
  private var reservecoins: N = initReservecoins  // The amount of reservecoins currently in circulation

  override def getReservesAmount = reserves
  override def getStablecoinsAmount = stablecoins
  override def getReservecoinsAmount = reservecoins

  override def targetPrice = oracle.conversionRate(PegCurrency, BaseCoin)

  override def normLiabilities(R: N, Nsc: N) = {
    min(R, Nsc * targetPrice)
  } ensuring { _ <= R}

  def reservecoinNominalPrice(R: N = getReservesAmount, Nsc: N = getStablecoinsAmount, Nrc: N = getReservecoinsAmount): N = {
    if (Nrc != 0) equity(R, Nsc) / Nrc else reservecoinDefaultPrice
  }

  def stablecoinNominalPrice(R: N = getReservesAmount, Nsc: N = getStablecoinsAmount): N = {
    val p = targetPrice
    if (Nsc == 0) p else min(p, normLiabilities(R, Nsc) / Nsc)
  }

  // There are two conditions for the acceptability of a reserve change:
  //  * If we are minting stablecoins or burning reservecoins, the new reserves shouldn't drop below the minimum.
  //  * If we are minting reservecoins, the new reserves shouldn't rise above the maximum.
  // Note that the new reserves can go above the maximum when stablecoins are being sold.
  // This ensures that stablecoin holders can always sell their stablecoins. The only effect on
  // reservecoin holders when the reserves rise above the maximum is a reduction of the leverage of
  // the reservecoins in relation to the base currency.
  def acceptableReserveChange(mintsSC: Boolean,
                              mintsRC: Boolean,
                              burnsRC: Boolean,
                              R: N, Nsc: N): Boolean = {
    def maxReserve(Nsc: N): N = maxReservesRatio * Nsc * targetPrice
    def minReserve(Nsc: N): N = minReservesRatio * Nsc * targetPrice
    def implies(a: Boolean, b: Boolean) = !a || b
    implies((mintsSC || burnsRC), (R >= minReserve(Nsc))) && implies(mintsRC, (R <= maxReserve(Nsc)))
  }

  // Calculates how many basecoins a user should pay (including fee) to receive `amountSC` stablecoins
  def calculateBasecoinsForMintedStablecoins(amountSC: N) = {
    val amountBaseToPay = amountSC * stablecoinNominalPrice(reserves, stablecoins)
    val feeToPay = amountBaseToPay * fee
    amountBaseToPay + feeToPay
  }

  /**
   * @param amountSC amount of stablecoins that will be issued
   * @return amount of basecoins that is put to the reserve
   */
  def buyStablecoins(amountSC: N): Try[N] = Try {
    require(amountSC > 0)

    val amountBase = calculateBasecoinsForMintedStablecoins(amountSC)

    val newReserves = reserves + amountBase
    val newStablecoins = stablecoins + amountSC
    require(acceptableReserveChange(true, false, false, newReserves, newStablecoins))

    reserves += amountBase
    stablecoins += amountSC
    amountBase
  }

  /**
   * @param amountSC amount of stablecoins that should be exchanged for base coins
   * @return amount of base coins withdrawn from the reserve
   */
  def sellStablecoins(amountSC: N): Try[N] = Try {
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

  /**
   *  Calculates how many basecoins should be paid for `amountRC` minted reservecoins.
   *  Utilizes iterative price recalculation. Accuracy parameter defines how often the price is recalculated.
   *  E.g., if `accuracy=1` the price is recalculated after minting each single coin, if `accuracy=10` the price is
   *  recalculated after 0.1 coins.
   *  The function is used mostly for testing purposes to cross-check the price calculation in the continuous setting.
   */
  def calculateBasecoinsForMintedReservecoinsIter(amountRC: Int, accuracy: Int = 1) = {
    var newReserves = reserves
    var newReservecoins = reservecoins
    var totalAmountBaseToPay: N = 0.0

    for(i <- 0 until amountRC * accuracy) {
      val nomPrice = reservecoinNominalPrice(newReserves, stablecoins, newReservecoins)
      val price = nomPrice.max(reservecoinDefaultPrice)
      val amountBaseToPay = price * (1 + fee) / accuracy
      newReserves += amountBaseToPay
      newReservecoins += (1.0 / accuracy)
      totalAmountBaseToPay += amountBaseToPay
    }

    totalAmountBaseToPay
  }

  /**
   * Calculates how many basecoins (including fees) should be paid to mint `amountRC` reservecoins in the continuous setting.
   * The price changing continuously as new RCs are minted, a special formula is used to capture such continuous change.
   * If current nominal RC price is below the minimal price, the minimal price is used.
   * Note that there is a limit to how many coins can be bought with minimal price. Since the price increases with each buying
   * we also have a special formula that calculates how many reservecoins can be bought with minimal price until their nominal
   * price reaches minimal price. After that the nominal price is used.
   */
  def calculateBasecoinsForMintedReservecoins(amountRC: N) = {
    /* Continuous price calculation. The aim is to recalculate the price after buying each smallest portion of the coin (like
    * in previous version for iterative price calculation). But here we assume that the coin is divided infinitely. Thus we
    * used mathematical analysis to obtain the precise formula.
    */
    def calculateAmountToPay(amount: N, inputReserves: N, inputReservecoins: N) = {
      require(reservecoinNominalPrice(inputReserves, stablecoins, inputReservecoins) >= reservecoinDefaultPrice)

      val newReservecoins = inputReservecoins + amount
      val l = normLiabilities(inputReserves, stablecoins)

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
      val l = normLiabilities(reserves, stablecoins)
      val maxToBuyWithMinPrice = (l - reserves + reservecoins*reservecoinDefaultPrice) / (fee*reservecoinDefaultPrice)

      if (maxToBuyWithMinPrice >= amountRC)
        amountRC*reservecoinDefaultPrice*(1+fee)
      else {
        val toPayWithMinPrice = maxToBuyWithMinPrice * reservecoinDefaultPrice * (1+fee)
        val toPayWithNominalPrice = calculateAmountToPay(amountRC-maxToBuyWithMinPrice,
                                                         reserves+toPayWithMinPrice,
                                                         reservecoins+maxToBuyWithMinPrice)
        toPayWithMinPrice + toPayWithNominalPrice
      }
    }
    else calculateAmountToPay(amountRC, reserves, reservecoins)
  }

  /**
   * @param amountRC amount of reservecoins that will be minted
   * @return amount of basecoins that is put to the reserve
   */
  def buyReservecoins(amountRC: N): Try[N] = Try {
    require(amountRC > 0)

    val amountBase = calculateBasecoinsForMintedReservecoins(amountRC)
    val newReserves = reserves + amountBase
    val newReservecoins = reservecoins + amountRC
    require(acceptableReserveChange(false, true, false, newReserves, stablecoins))

    reserves = newReserves
    reservecoins = newReservecoins
    amountBase
  }

  /** Calculates how many basecoins should be returned for burning `amountRC` reservecoins.
   *  Utilizes iterative price recalculation. Used mostly for testing purposes to cross-check the price calculation
   *  in the continuous setting. */
  def calculateBasecoinsForBurnedReservecoinsIter(amountRC: Int, accuracy: Int = 1) = {
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

  /** Calculates how many basecoins should be returned for burning `amountRC` reservecoins in the continuous setting. */
  def calculateBasecoinsForBurnedReservecoins(amountRC: N) = {
    val l = normLiabilities(reserves, stablecoins)
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

  /**
   * @param amountRC amount of reservecions to burn
   * @return amount of basecoins withdrawn from the reserve
   */
  def sellReservecoins(amountRC: N): Try[N] = Try {
    require(amountRC > 0)

    val amountBaseToReturn = calculateBasecoinsForBurnedReservecoins(amountRC)

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