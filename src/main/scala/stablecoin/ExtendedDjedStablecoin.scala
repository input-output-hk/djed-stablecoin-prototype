package stablecoin

import stablecoin.Currency.{BaseCoin, PegCurrency}

import scala.util.Try

/**
 * Implements Extended Djed stablecoin with dynamic fees, debt-for-equity swaps and continuous price recalculation.
 * Note that this version does not constrain operations by min and max reserve ratio, except when buying SCs (in this case
 * it is enforced that the reserves ratio should be above the peg reserves ratio). It is assumed that operations would be
 * constrained by dynamically adjusted fees, e.g., if reserves ratio deviates from the optimal too much, the fees rise to
 * discourage operations that further worsen it (up to 100% which basically forbids the operation).
 */
class ExtendedDjedStablecoin(val oracle: Oracle, // Oracle used by the bank
                             val baseFee: N, // the base fee charged by the bank when buying and selling SCs and RCs
                             val pegReservesRatio: N, // the minimal reserves ratio for which the peg holds
                             val optimalReservesRatio: N, // the bank's optimal reserve ratio
                             val reservecoinDefaultPrice: N, // default price of RCs, used when there are 0 RCs
                             val k_rm: N, // linear correlation coefficient for fee calculation for buying RC
                             val k_rr: N, // linear correlation coefficient for fee calculation for selling RC
                             val k_sm: N, // linear correlation coefficient for fee calculation for buying SC
                             val k_sr: N, // linear correlation coefficient for fee calculation for selling SC
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

  override def normLiabilities(R: N, Nsc: N) = stablecoinNominalPrice(R, Nsc) * Nsc

  def stablecoinNominalPrice(R: N = getReservesAmount, Nsc: N = getStablecoinsAmount): N = {
    val p = targetPrice
    if (Nsc == 0) p
    else {
      val ratio = reservesRatio(R, Nsc)
      val k = BigDecimal(1.0).min(ratio / pegReservesRatio)
      k * p
    }
  }

  def reservecoinNominalPrice(R: N, Nsc: N, Nrc: N): N = {
    if (Nrc != 0)
      equity(R, Nsc) / Nrc
    else reservecoinDefaultPrice
  }

  /** Iterative price calculation for minting stablecoins.
   * Used for testing purposes to cross-check continuous price calculation. */
  def calculateBasecoinsForMintedStablecoinsIter(amountSC: Int, accuracy: Int = 1) = {
    require(reservesRatio() > pegReservesRatio)
    var newReserves = reserves
    var newStablecoins = stablecoins
    var totalAmountBaseToPay: N = 0.0

    def fee(R: N, Nsc: N): N = {
      val ratio = reservesRatio(R, Nsc)
      val additionalFee =
        if (ratio < optimalReservesRatio) {
          val optimalReserves = Nsc * targetPrice * optimalReservesRatio
          k_sm * (optimalReserves - R) / optimalReserves
        }
        else BigDecimal(0.0)
      baseFee + additionalFee
    }

    for(i <- 0 until amountSC * accuracy) {
      val price = stablecoinNominalPrice(newReserves, newStablecoins)
      val amountBase = price / accuracy
      val amountBaseToPay = amountBase * (1 + fee(newReserves, newStablecoins))
      newReserves += amountBaseToPay
      newStablecoins += (1.0 / accuracy)
      require(reservesRatio(newReserves, newStablecoins) >= pegReservesRatio)
      totalAmountBaseToPay += amountBaseToPay
    }

    totalAmountBaseToPay
  }

  def calculateBasecoinsForMintedStablecoins(amountSC: N) = {
    require(reservesRatio() > pegReservesRatio)
    val Pt_sc = targetPrice

    def calculateAmountWithDynamicFee(Nsc0: N, R0: N, t: N) = {
      val K = k_sm / optimalReservesRatio
      val one_plus_K = 1 + K
      val P = Pt_sc * (1 + baseFee + k_sm)
      val Nsc0_plus_t = Nsc0 + t
      val C = (R0 - P * (Nsc0 / one_plus_K)) * math.pow(Nsc0.toDouble, K.toDouble)
      val x1 = P * (Nsc0_plus_t) / one_plus_K
      val x2 = math.pow(Nsc0_plus_t.toDouble, K.toDouble)
      x1 + (C / x2) - R0
    }

    val amountBasecoinsToPay =
      if (reservesRatio() > optimalReservesRatio) {
        // calculating how much SCs need to be bought to decrease ratio to optimal level
        val amountToOptimum = (reserves - optimalReservesRatio * Pt_sc * stablecoins) / (Pt_sc*(optimalReservesRatio-1-baseFee))
        if (amountToOptimum >= amountSC) {
          Pt_sc * (1+baseFee) * amountSC
        } else {
          val amountWithBaseFee = Pt_sc * (1+baseFee) * amountToOptimum
          val Nsc0 = stablecoins + amountToOptimum
          val R0 = reserves + amountWithBaseFee
          val t = amountSC - amountToOptimum
          amountWithBaseFee + calculateAmountWithDynamicFee(Nsc0, R0, t)
        }
      } else {
        calculateAmountWithDynamicFee(stablecoins, reserves, amountSC)
      }

    // sanity check
    val newReserveRatio = reservesRatio(reserves + amountBasecoinsToPay, stablecoins + amountSC)
    require(newReserveRatio >= pegReservesRatio, "The formulas are not suitable if ratio falls below optimum.")

    amountBasecoinsToPay
  }

  /**
   * Uses continuous price recalculation to define the price of minting stablecoins
   *
   * @param amountSC amount of stablecoins that will be minted
   * @return amount of basecoins that is paid to the reserve
   */
  override def buyStablecoins(amountSC: N): Try[N] = Try {
    require(amountSC > 0)
    require(reservesRatio() > pegReservesRatio)

    val baseCoinsToPay = calculateBasecoinsForMintedStablecoins(amountSC)

    val newReserves = reserves + baseCoinsToPay
    val newStablecoins = stablecoins + amountSC

    require(reservesRatio(newReserves, newStablecoins) >= pegReservesRatio)

    reserves = newReserves
    stablecoins = newStablecoins

    baseCoinsToPay
  }

  /** Iterative price calculation for selling stablecoins.
   * Used for testing purposes to cross-check continuous price calculation. */
  def calculateBasecoinsForBurnedStablecoinsIter(amountSC: Int, accuracy: Int = 1) = {
    var newReserves = reserves
    var newStablecoins = stablecoins
    var totalAmountBaseToReturn: N = 0.0

    def fee(R: N, Nsc: N): N = {
      val ratio = reservesRatio(R, Nsc)
      val additionalFee =
        if (ratio > optimalReservesRatio) {
          val optimalReserves = Nsc * targetPrice * optimalReservesRatio
          k_sr * (R - optimalReserves) / optimalReserves
        }
        else BigDecimal(0.0)
      baseFee + additionalFee
    }

    for(i <- 0 until amountSC * accuracy) {
      val price = stablecoinNominalPrice(newReserves, newStablecoins)
      val amountBase = price / accuracy
      val amountBaseToReturn = amountBase * (1 - fee(newReserves, newStablecoins))
      newReserves -= amountBaseToReturn
      newStablecoins -= (1.0 / accuracy)
      totalAmountBaseToReturn += amountBaseToReturn
    }

    totalAmountBaseToReturn
  }

  def calculateBasecoinsForBurnedStablecoins(amountSC: N) = {
    val Pt_sc = targetPrice

    def calculateVariant1(Nsc0: N, R0: N, t: N) = {
      val K = k_sr / optimalReservesRatio
      val one_plus_K = 1 + K
      val P = -Pt_sc * (1 - baseFee + k_sr)
      val Nsc0_min_t = Nsc0 - t
      val C = (R0 + P * (Nsc0 / one_plus_K)) * math.pow(Nsc0.toDouble, K.toDouble)
      val x1 = -P * (Nsc0_min_t) / one_plus_K
      val x2 = math.pow(Nsc0_min_t.toDouble, K.toDouble)
      R0 - (x1 + (C / x2))
    }

    def calculateVariant2(t: N) = {
      t * Pt_sc * (1 - baseFee)
    }

    def calculateVariant3(Nsc0: N, R0: N, t: N) = {
      val x1 = (1 - baseFee) / pegReservesRatio
      val x2 = (Nsc0 - t) / Nsc0
      val x = R0 * math.pow(x2.toDouble, x1.toDouble)
      R0 - x
    }

    def initRatioAbovePeg(Nsc0: N, R0: N, t: N) = {
      val rounded_ratio = reservesRatio(R0, Nsc0).setScale(10, BigDecimal.RoundingMode.HALF_UP)
      // we do rounding to pass the check cause reservesRatio() might be slightly less due to rounding issues when we do BigDecimal->Double conversion in math.pow() on the previous step
      require(rounded_ratio >= pegReservesRatio)
      require(reservesRatio(R0, Nsc0) < optimalReservesRatio)
      // calculating how many SCs need to be sold to increase ratio to optimal level
      val amountToOptimum = (R0 - optimalReservesRatio * Pt_sc * Nsc0) / (Pt_sc*(1 - optimalReservesRatio-baseFee))
      if (amountToOptimum >= t) {
        calculateVariant2(t)
      } else {
        val amountWithBaseFee = calculateVariant2(amountToOptimum)
        val new_Nsc0 = Nsc0 - amountToOptimum
        val new_Nr0 = R0 - amountWithBaseFee
        val new_t = t - amountToOptimum
        amountWithBaseFee + calculateVariant1(new_Nsc0, new_Nr0, new_t)
      }
    }

    def initRatioBelowPeg(Nsc0: N, R0: N, t: N) = {
      require(reservesRatio(R0, Nsc0) < pegReservesRatio)
      // calculating how many SCs need to be sold to increase ratio to peg level
      val amountToPeg = {
        val d = (1 - baseFee) / pegReservesRatio
        val x1 = pegReservesRatio * Pt_sc * math.pow(Nsc0.toDouble, d.toDouble) / R0
        Nsc0 - math.pow(x1.toDouble, (1/(d-1)).toDouble)
      }
      if (amountToPeg > t)
        calculateVariant3(Nsc0, R0, t)
      else {
        val amountToReturn1 = calculateVariant3(Nsc0, R0, amountToPeg)
        val new_R0 = R0 - amountToReturn1
        val new_Nsc0 = Nsc0 - amountToPeg
        val new_t = t - amountToPeg
        amountToReturn1 + initRatioAbovePeg(new_Nsc0, new_R0, new_t)
      }
    }

    val amountBasecoinsToReturn =
      if (reservesRatio() >= optimalReservesRatio)
        calculateVariant1(stablecoins, reserves, amountSC)
      else {
        if (reservesRatio() < pegReservesRatio)
          initRatioBelowPeg(stablecoins, reserves, amountSC)
        else
          initRatioAbovePeg(stablecoins, reserves, amountSC)
      }

    amountBasecoinsToReturn
  }

  /** Iterative calculation of compensated reservecoins for selling stablecoins.
   * Used for testing purposes to cross-check continuous calculation. */
  def calculateReservecoinsForBurnedStablecoinsIter(amountSC: Int, accuracy: Int = 1) = {
    require(amountSC <= stablecoins)
    var newStablecoins = stablecoins
    var newReservecoins = reservecoins
    var newReserves = reserves
    var totalAmountReservecoinsToReturn: N = 0.0

    def reservecoinsSwapAmount(R: N, Nsc: N, Nrc: N) = {
      val k = min(1, reservesRatio(R, Nsc) / pegReservesRatio)
      ((1-k) * targetPrice) / reservecoinNominalPrice(R, Nsc, Nrc)
    }

    // we also need to track how many basecoins are returned with each peace of stablecoin, because it affects reserves
    def basecoinsAmount(R: N, Nsc: N) = {
      val price = stablecoinNominalPrice(R, Nsc)
      price / accuracy * (1 - baseFee)
    }

    for(i <- 0 until amountSC * accuracy) {
      val amountReservecoinsToReturn = (reservecoinsSwapAmount(newReserves, newStablecoins, newReservecoins) / accuracy) * (1 - baseFee)
      newReservecoins += amountReservecoinsToReturn
      newStablecoins -= (1.0 / accuracy)
      newReserves -= basecoinsAmount(newReserves, newStablecoins)
      totalAmountReservecoinsToReturn += amountReservecoinsToReturn
    }

    totalAmountReservecoinsToReturn
  }

  def calculateReservecoinsForBurnedStablecoins(amountSC: N): BigDecimal = {
    require(amountSC <= stablecoins)

    if (reservesRatio() < pegReservesRatio) {
      val Pt_sc = targetPrice
      val one_min_fee = 1 - baseFee
      val inv_rpeg = (1 / pegReservesRatio)
      val d = inv_rpeg * one_min_fee

      val amountToPeg = {
        val x1 = pegReservesRatio * Pt_sc * math.pow(stablecoins.toDouble, d.toDouble) / reserves
        stablecoins - math.pow(x1.toDouble, (1 / (d - 1)).toDouble)
      }
      val compensatedSC = amountToPeg.min(amountSC)

      val Nsc_min_N = stablecoins - compensatedSC
      val one_min_inv_rpeg = 1 - inv_rpeg
      val x1 = one_min_fee / one_min_inv_rpeg
      val x2 = inv_rpeg * math.log((Nsc_min_N / stablecoins).toDouble)
      val one_min_d = 1 - d
      val x3_1 = math.pow(Nsc_min_N.toDouble, one_min_d.toDouble)
      val x3_2 = math.pow(stablecoins.toDouble, one_min_d.toDouble)
      val x3_3 = math.pow(stablecoins.toDouble, d.toDouble)
      val x3 = Pt_sc * x3_3 * (x3_1 - x3_2) / (reserves * one_min_d)
      val A = x1 * (x2 - x3)

      val newReservecoins = math.exp(A.doubleValue) * reservecoins
      newReservecoins - reservecoins
    }
    else 0.0  // if (reservesRatio() >= pegReserveRatio) reservecoins are not paid
  }

  /**
   * Utilizes continous price recalculation
   *
   * @param amountSC amount of stablecoins to sell
   * @return amount of basecoins withdrawn from the reserve
   */
  override def sellStablecoins(amountSC: N): Try[N] = Try {
    require(amountSC > 0 && amountSC < stablecoins) // don't allow to sell all stablecoins to avoid division by zero when calculating reserves ratio

    val baseCoinsToReturn = calculateBasecoinsForBurnedStablecoins(amountSC)
    reserves -= baseCoinsToReturn
    stablecoins -= amountSC

    baseCoinsToReturn
  }

  /**
   * If the system is under-collateralized (i.e., reserves ratio < pegReservesRatio),
   * redemption is partially fulfilled in basecoins and the rest is returned in reservecoins.
   *
   * @param amountSC amount of stablecoins to sell
   * @return returned number of Basecoins and Reservecoins
   */
  def sellStablecoinsWithSwap(amountSC: N): Try[(N,N)] = Try {

    val reserveCoinsToReturn = calculateReservecoinsForBurnedStablecoins(amountSC)
    val newReservecoins = reservecoins + reserveCoinsToReturn

    val baseCoinsToReturn = sellStablecoins(amountSC).get

    reservecoins = newReservecoins

    (baseCoinsToReturn, reserveCoinsToReturn)
  }

  /** Iterative price calculation for minting reservecoins.
   * Used for testing purposes to cross-check continuous price calculation. */
  def calculateBasecoinsForMintedReservecoinsIter(amountRC: Int, accuracy: Int = 1) = {
    require(amountRC > 0)
    var newReservecoins = reservecoins
    var newReserves = reserves
    var totalAmountBasecoinsToPay: N = 0.0

    def fee(R: N, Nsc: N): N = {
      val ratio = reservesRatio(R, Nsc)
      val additionalFee =
        if (ratio >= optimalReservesRatio) {
          val optimalReserves = Nsc * targetPrice * optimalReservesRatio
          k_rm * (R - optimalReserves) / optimalReserves
        }
        else BigDecimal(0.0)
      baseFee + additionalFee
    }

    // we need to track how many basecoins to pay for each peace of reservecoin
    def basecoinsAmount(R: N, Nsc: N, Nrc: N) = {
      val price = reservecoinNominalPrice(R, Nsc, Nrc)
      price * (1 + fee(R,Nsc))
    }

    for(i <- 0 until amountRC * accuracy) {
      val amountBasecoinsToPay = basecoinsAmount(newReserves, stablecoins, newReservecoins) / accuracy
      newReservecoins += 1.0 / accuracy
      newReserves += amountBasecoinsToPay
      totalAmountBasecoinsToPay += amountBasecoinsToPay
    }

    totalAmountBasecoinsToPay
  }

  def calculateBasecoinsForMintedReservecoins(amountRC: N): N = {
    require(amountRC > 0)
    val R0 = reserves
    val Nrc0 = reservecoins
    val Nrc0_plus_N_div_Nrc0 = (Nrc0 + amountRC) / Nrc0
    val one_plus_fee0 = 1 + baseFee
    val Lt = targetLiabilities(stablecoins)
    val A = optimalReservesRatio / (optimalReservesRatio * (one_plus_fee0 - k_rm) + k_rm)

    def calculateVariant1() = {
      val X = (1 - 1/pegReservesRatio) * one_plus_fee0
      math.pow(Nrc0_plus_N_div_Nrc0.toDouble, X.toDouble) * R0
    }

    def calculateVariant2() = {
      val X = 1 / (1 - 1/pegReservesRatio)
      val rpeg_mul_Lt = pegReservesRatio * Lt
      val x1 = math.pow(Nrc0_plus_N_div_Nrc0.toDouble, one_plus_fee0.toDouble)
      val x2 = math.pow((rpeg_mul_Lt / R0).toDouble, (-X).toDouble)
      val x3 = rpeg_mul_Lt - Lt
      x1 * x2 * x3 + Lt
    }

    def calculateVariant3() = {
      val ropt_mul_Lt = optimalReservesRatio * Lt
      val Y = 1 / one_plus_fee0
      val X = Y / (1 - 1/pegReservesRatio)
      val R0_pow_X = math.pow(R0.toDouble, X.toDouble)
      val rpeg_min_one_pow_Y = math.pow((pegReservesRatio - 1).toDouble, Y.toDouble)
      val ropt_min_one_pow_Y = math.pow((optimalReservesRatio - 1).toDouble, Y.toDouble)
      val rpeg_mul_Lt_pow_X = math.pow((pegReservesRatio * Lt).toDouble, X.toDouble)
      val x1 = Nrc0_plus_N_div_Nrc0 * R0_pow_X * rpeg_min_one_pow_Y / (rpeg_mul_Lt_pow_X * ropt_min_one_pow_Y)
      val Z = math.pow(x1.toDouble, (1/A).toDouble)
      val V = one_plus_fee0 / (ropt_mul_Lt - Lt)
      val E = one_plus_fee0 - k_rm
      (Z*E + V*Lt) / (V - Z*k_rm/ropt_mul_Lt)
    }

    def calculateVariant4() = {
      val x1 = math.pow(Nrc0_plus_N_div_Nrc0.toDouble, one_plus_fee0.toDouble)
      val x2 = R0 - Lt
      x1 * x2 + Lt
    }

    def calculateVariant5() = {
      val ropt_mul_Lt = optimalReservesRatio * Lt
      val ropt_mul_Lt_min_Lt = ropt_mul_Lt - Lt
      val x1 = (R0-Lt) / ropt_mul_Lt_min_Lt
      val x2 = math.pow(x1.toDouble, (1 / one_plus_fee0).toDouble)
      val Z = math.pow((Nrc0_plus_N_div_Nrc0*x2).toDouble, (1/A).toDouble)
      val V = one_plus_fee0 / ropt_mul_Lt_min_Lt
      val E = one_plus_fee0 - k_rm
      (Z*E + V*Lt) / (V - Z*k_rm/ropt_mul_Lt)
    }

    def calculateVariant6() = {
      val ropt_mul_Lt = optimalReservesRatio * Lt
      val V = (one_plus_fee0 + (k_rm * (R0 - ropt_mul_Lt) / ropt_mul_Lt)) / (R0 - Lt)
      val Z = math.pow(Nrc0_plus_N_div_Nrc0.toDouble, (1/A).toDouble)
      val E = one_plus_fee0 - k_rm
      (Z*E + V*Lt) / (V - Z*k_rm/ropt_mul_Lt)
    }

    /*
      Depending on the initial and new reserve ratios we need to use different formulas.
      Initial ratio is known in advance, so we can eliminate using inappropriate formulas, but
      new ratio isn't known so we need to calculate all possible variants and then, by analyzing the results,
      pick only the correct one. See more details in paper.
     */
    val initReserveRatio = R0 / Lt
    val newReserves =
      if (initReserveRatio < pegReservesRatio) {
        val newReserves1 = calculateVariant1()
        val newReserveRatio1 = newReserves1 / Lt
        lazy val newReserves2 = calculateVariant2()
        lazy val newReserveRatio2 = newReserves2 / Lt
        lazy val newReserves3 = calculateVariant3()
        lazy val newReserveRatio3 = newReserves3 / Lt

        if (newReserveRatio1 < pegReservesRatio) {
          newReserves1
        }
        else if (pegReservesRatio <= newReserveRatio2 && newReserveRatio2 < optimalReservesRatio) {
          assert(pegReservesRatio <= newReserveRatio1)
          newReserves2
        }
        else {
          assert(optimalReservesRatio <= newReserveRatio2)
          assert(optimalReservesRatio <= newReserveRatio3)
          newReserves3
        }
      }
      else if (pegReservesRatio <= initReserveRatio && initReserveRatio < optimalReservesRatio) {
        val newReserves4 = calculateVariant4()
        val newReserveRatio4 = newReserves4 / Lt
        lazy val newReserves5 = calculateVariant5()
        lazy val newReserveRatio5 = newReserves5 / Lt

        if (newReserveRatio4 < optimalReservesRatio)
          newReserves4
        else {
          assert(optimalReservesRatio <= newReserveRatio4)
          assert(optimalReservesRatio <= newReserveRatio5)
          newReserves5
        }
      }
      else {
        assert(optimalReservesRatio <= initReserveRatio)
        calculateVariant6()
      }

    newReserves - reserves
  }

  override def buyReservecoins(amountRC: N): Try[N] = Try {
    require(amountRC > 0)

    val baseCoinsToPay = calculateBasecoinsForMintedReservecoins(amountRC)

    val newReserves = reserves + baseCoinsToPay
    val newReservecoins = reservecoins + amountRC

    reserves = newReserves
    reservecoins = newReservecoins

    baseCoinsToPay
  }

  /** Iterative price calculation for selling reservecoins.
   * Used for testing purposes to cross-check continuous price calculation. */
  def calculateBasecoinsForBurnedReservecoinsIter(amountRC: Int, accuracy: Int = 1) = {
    require(amountRC > 0)
    var newReservecoins = reservecoins
    var newReserves = reserves
    var totalAmountBasecoinsToReturn: N = 0.0

    def fee(R: N, Nsc: N): N = {
      val ratio = reservesRatio(R, Nsc)
      val additionalFee =
        if (ratio < optimalReservesRatio) {
          val optimalReserves = Nsc * targetPrice * optimalReservesRatio
          k_rr * (optimalReserves - R) / optimalReserves
        } else BigDecimal(0.0)
      baseFee + additionalFee
    }

    // we need to track how many basecoins to return for each peace of reservecoin
    def basecoinsAmount(R: N, Nsc: N, Nrc: N) = {
      val price = reservecoinNominalPrice(R, Nsc, Nrc)
      price * (1 - fee(R,Nsc))
    }

    for(i <- 0 until amountRC * accuracy) {
      val amountBasecoinsToReturn = basecoinsAmount(newReserves, stablecoins, newReservecoins) / accuracy
      newReservecoins -= 1.0 / accuracy
      newReserves -= amountBasecoinsToReturn
      totalAmountBasecoinsToReturn += amountBasecoinsToReturn
    }

    totalAmountBasecoinsToReturn
  }

  def calculateBasecoinsForBurnedReservecoins(amountRC: N): N = {
    require(amountRC > 0)
    val R0 = reserves
    val Nrc0 = reservecoins
    val Nrc0_min_N_div_Nrc0 = (Nrc0 - amountRC) / Nrc0
    val E = 1 - baseFee - k_rr
    val Lt = targetLiabilities(stablecoins)
    val krr_div_Lt_ropt = k_rr / (Lt * optimalReservesRatio)
    val C = optimalReservesRatio / (optimalReservesRatio * E + k_rr)

    def calculateVariant1() = {
      val V = (E + R0*krr_div_Lt_ropt) / R0
      val p = (pegReservesRatio - 1) * E / pegReservesRatio
      val Z = math.pow(Nrc0_min_N_div_Nrc0.toDouble, p.toDouble)
      (Z * E) / (V - Z * (krr_div_Lt_ropt))
    }

    def calculateVariant2() = {
      val rpeg_mul_Lt = pegReservesRatio * Lt
      val x1 = (E + pegReservesRatio*k_rr/optimalReservesRatio)
      val x2 = E + R0*krr_div_Lt_ropt
      val x3 = (R0-Lt)*x1 / ((rpeg_mul_Lt-Lt)*x2)
      val x4 = math.pow(x3.toDouble, C.toDouble) * Nrc0_min_N_div_Nrc0
      val p = (E - (E / pegReservesRatio))
      val Z = math.pow(x4.toDouble, p.toDouble)
      val V = x1 / rpeg_mul_Lt
      (Z * E) / (V - Z * (krr_div_Lt_ropt))
    }

    def calculateVariant3() = {
      val ropt_min_one = optimalReservesRatio - 1
      val one_min_fee0 = 1 - baseFee

      val x1_1 = (R0-Lt) / (ropt_min_one*Lt)
      val x1 = math.pow(x1_1.toDouble, (1/one_min_fee0).toDouble)
      val x2 = (optimalReservesRatio-1) / (pegReservesRatio-1)
      val x3_1 = E + pegReservesRatio * k_rr / optimalReservesRatio
      val x3 = x3_1 / one_min_fee0
      val x4 = math.pow((x2*x3).toDouble, C.toDouble)
      val x = Nrc0_min_N_div_Nrc0 * x1 * x4
      val p = (E - (E / pegReservesRatio))
      val Z = math.pow(x.toDouble, p.toDouble)
      val V = x3_1 / (pegReservesRatio*Lt)
      (Z * E) / (V - Z * (krr_div_Lt_ropt))
    }

    def calculateVariant4(): BigDecimal = {
      val V = (E + R0 * krr_div_Lt_ropt) / (R0 - Lt)
      val Z = math.pow(Nrc0_min_N_div_Nrc0.toDouble, (1/C).toDouble)
      (Z * E + V*Lt) / (V - Z * (krr_div_Lt_ropt))
    }

    def calculateVariant5(): BigDecimal = {
      val one_min_fee0 = 1 - baseFee
      val roptLt_min_Lt = optimalReservesRatio * Lt - Lt
      val x1_1 = (R0 - Lt) / roptLt_min_Lt
      val x1 = math.pow(x1_1.toDouble, (1/one_min_fee0).toDouble)
      val Z = math.pow((Nrc0_min_N_div_Nrc0 * x1).toDouble, (1/C).toDouble)
      val V = one_min_fee0 / roptLt_min_Lt
      (Z * E + V*Lt) / (V - Z * (krr_div_Lt_ropt))
    }

    def calculateVariant6(): BigDecimal = {
      val x1 = math.pow(Nrc0_min_N_div_Nrc0.toDouble, (1-baseFee).toDouble)
      val x2 = R0 - Lt
      x1 * x2 + Lt
    }

    /*
      Depending on the initial and new reserve ratios we need to use different formulas.
      Initial ratio is known in advance, so we can eliminate using inappropriate formulas, but
      new ratio isn't known so we need to calculate all possible variants and then, by analyzing the results,
      pick only the correct one. See more details in paper.
     */
    val initReserveRatio = R0 / Lt
    val newReserves =
      if (initReserveRatio < pegReservesRatio) {
        calculateVariant1()
      }
      else if (pegReservesRatio <= initReserveRatio && initReserveRatio < optimalReservesRatio) {
        val newReserves4 = calculateVariant4()
        val newReserveRatio4 = newReserves4 / Lt
        if (newReserveRatio4 >= pegReservesRatio)
          newReserves4
        else
          calculateVariant2()
      }
      else {
        val newReserves6 = calculateVariant6()
        val newReserveRatio6 = newReserves6 / Lt
        if (newReserveRatio6 >= optimalReservesRatio)
          newReserves6
        else {
          val newReserves5 = calculateVariant5()
          val newReserveRatio5 = newReserves5 / Lt
          if (newReserveRatio5 >= pegReservesRatio)
            newReserves5
          else
            calculateVariant3()
        }
      }

     reserves - newReserves
  }

  override def sellReservecoins(amountRC: N): Try[N] = Try {
    require(amountRC > 0)

    val baseCoinsToReturn = calculateBasecoinsForBurnedReservecoins(amountRC)

    val newReserves = reserves - baseCoinsToReturn
    val newReservecoins = reservecoins - amountRC

    require(newReservecoins > 0) // don't allow to sell all reservecoins to be able to calculate nominal RC price

    reserves = newReserves
    reservecoins = newReservecoins

    baseCoinsToReturn
  }
}
