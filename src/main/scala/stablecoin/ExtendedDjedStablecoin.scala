package stablecoin

import stablecoin.Currency.{BaseCoin, PegCurrency}

import scala.util.Try

class ExtendedDjedStablecoin(val oracle: Oracle, // Oracle used by the bank
                             val baseFee: N, // the base fee charged by the bank when buying and selling SCs and RCs
                             val pegReservesRatio: N, // the minimal reserves ratio for which the peg holds
                             val optimalReservesRatio: N, // the bank's optimal reserve ratio
                             val reservecoinDefaultPrice: N, // default price of RCs, used when there are 0 RCs
                             val k_rm: N, // linear correlation coefficient for fee calculation for RC minting
                             val k_rr: N, // linear correlation coefficient for fee calculation for RC redeeming
                             val k_sm: N, // linear correlation coefficient for fee calculation for SC minting
                             val k_sr: N, // linear correlation coefficient for fee calculation for SC redeeming
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

  def calculateBasecoinsForMintedStablecoinsIter(amountSC: Int, accuracy: Int = 1) = {
    require(reservesRatio() > pegReservesRatio)
    var newReserves = reserves
    var newStablecoins = stablecoins
    var totalAmountBaseToPay: N = 0.0

    def fee(r: N, sc: N): N = {
      val ratio = reservesRatio(r, sc)
      val additionalFee =
        if (ratio < optimalReservesRatio) {
          val optimalReserves = sc * oracle.conversionRate(PegCurrency, BaseCoin) * optimalReservesRatio
          k_sm * (optimalReserves - r) / optimalReserves
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
    val Pexch = oracle.conversionRate(PegCurrency, BaseCoin)

    def calculateAmountWithDynamicFee(sc0: N, r0: N, amount: N) = {
      val K = k_sm / optimalReservesRatio
      val one_plus_K = 1 + K
      val P = Pexch * (1 + baseFee + k_sm)
      val sc0_plus_N = sc0 + amount
      val C = (r0 - P * (sc0 / one_plus_K)) * math.pow(sc0.toDouble, K.toDouble)
      val x1 = P * (sc0_plus_N) / one_plus_K
      val x2 = math.pow(sc0_plus_N.toDouble, K.toDouble)
      x1 + (C / x2) - r0
    }

    val amountBasecoinsToPay =
      if (reservesRatio() > optimalReservesRatio) {
        // calculating how much SCs need to be bought to decrease ratio to optimal level
        val amountToOptimum = (reserves - optimalReservesRatio * Pexch * stablecoins) / (Pexch*(optimalReservesRatio-1-baseFee))
        if (amountToOptimum >= amountSC) {
          Pexch * (1+baseFee) * amountSC
        } else {
          val amountWithBaseFee = Pexch * (1+baseFee) * amountToOptimum
          val sc0 = stablecoins + amountToOptimum
          val r0 = reserves + amountWithBaseFee
          val sc = amountSC - amountToOptimum
          amountWithBaseFee + calculateAmountWithDynamicFee(sc0, r0, sc)
        }
      } else {
        calculateAmountWithDynamicFee(stablecoins, reserves, amountSC)
      }

    // sanity check
    val newReserveRatio = reservesRatio(reserves + amountBasecoinsToPay, stablecoins + amountSC)
    require(newReserveRatio >= pegReservesRatio, "The formulas are not suitable if ratio falls below optimum.")

    amountBasecoinsToPay
  }

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

  def calculateBasecoinsForRedeemedStablecoinsIter(amountSC: Int, accuracy: Int = 1) = {
    var newReserves = reserves
    var newStablecoins = stablecoins
    var totalAmountBaseToReturn: N = 0.0

    def fee(r: N, sc: N): N = {
      val ratio = reservesRatio(r, sc)
      val additionalFee =
        if (ratio > optimalReservesRatio) {
          val optimalReserves = sc * oracle.conversionRate(PegCurrency, BaseCoin) * optimalReservesRatio
          k_sr * (r - optimalReserves) / optimalReserves
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

  def calculateBasecoinsForRedeemedStablecoins(amountSC: N) = {
    val Pexch = oracle.conversionRate(PegCurrency, BaseCoin)

    def calculateRange1(sc0: N, r0: N, amount: N) = {
      val K = k_sr / optimalReservesRatio
      val one_plus_K = 1 + K
      val P = -Pexch * (1 - baseFee + k_sr)
      val sc0_min_N = sc0 - amount
      val C = (r0 + P * (sc0 / one_plus_K)) * math.pow(sc0.toDouble, K.toDouble)
      val x1 = -P * (sc0_min_N) / one_plus_K
      val x2 = math.pow(sc0_min_N.toDouble, K.toDouble)
      r0 - (x1 + (C / x2))
    }

    def calculateRange2(amount: N) = {
      amount * Pexch * (1 - baseFee)
    }

    def calculateRange3(sc0: N, r0: N, amount: N) = {
      val x1 = (1 - baseFee) / pegReservesRatio
      val x2 = (sc0 - amount) / sc0
      val x = r0 * math.pow(x2.toDouble, x1.toDouble)
      r0 - x
    }

    def calculateRange4(sc0: N, r0: N, amount: N) = {
      val rounded_ratio = reservesRatio(r0, sc0).setScale(10, BigDecimal.RoundingMode.HALF_UP)
      require(rounded_ratio >= pegReservesRatio)   // we do rounding to pass the check cause reservesRatio() might be slightly less due to rounding issues when we do BigDecimal->Double conversion in math.pow() on the previous step
      require(reservesRatio(r0, sc0) < optimalReservesRatio)
      // calculating how much SCs need to be redeemed to increase ratio to optimal level
      val amountToOptimum = (r0 - optimalReservesRatio * Pexch * sc0) / (Pexch*(1 - optimalReservesRatio-baseFee))
      if (amountToOptimum >= amount) {
        calculateRange2(amount)
      } else {
        val amountWithBaseFee = calculateRange2(amountToOptimum)
        val new_sc0 = sc0 - amountToOptimum
        val new_r0 = r0 - amountWithBaseFee
        val new_amount = amount - amountToOptimum
        amountWithBaseFee + calculateRange1(new_sc0, new_r0, new_amount)
      }
    }

    def calculateRange5(sc0: N, r0: N, amount: N) = {
      require(reservesRatio(r0, sc0) < pegReservesRatio)
      // calculating how much SCs need to be redeemed to increase ratio to minimum level
      val amountToMinimum = {
        val d = (1 - baseFee) / pegReservesRatio
        val x1 = pegReservesRatio * Pexch * math.pow(sc0.toDouble, d.toDouble) / r0
        sc0 - math.pow(x1.toDouble, (1/(d-1)).toDouble)
      }
      if (amountToMinimum > amount)
        calculateRange3(sc0, r0, amount)
      else {
        val amountToReturn1 = calculateRange3(sc0, r0, amountToMinimum)
        val new_r0 = r0 - amountToReturn1
        val new_sc0 = sc0 - amountToMinimum
        val new_amount = amount - amountToMinimum
        amountToReturn1 + calculateRange4(new_sc0, new_r0, new_amount)
      }
    }

    val amountBasecoinsToReturn =
      if (reservesRatio() >= optimalReservesRatio)
        calculateRange1(stablecoins, reserves, amountSC)
      else {
        if (reservesRatio() < pegReservesRatio)
          calculateRange5(stablecoins, reserves, amountSC)
        else
          calculateRange4(stablecoins, reserves, amountSC)
      }

    amountBasecoinsToReturn
  }

  def calculateReservecoinsForRedeemedStablecoinsIter(amountSC: Int, accuracy: Int = 1) = {
    require(amountSC <= stablecoins)
    var newStablecoins = stablecoins
    var newReservecoins = reservecoins
    var newReserves = reserves
    var totalAmountReservecoinsToReturn: N = 0.0

    def reservecoinsSwapAmount(r: N, sc: N, rc: N) = {
      val p = oracle.conversionRate(PegCurrency, BaseCoin)
      val k = min(1, reservesRatio(r, sc) / pegReservesRatio)
      ((1-k)*p) / reservecoinNominalPrice(r, sc, rc)
    }

    // we also need to track how many basecoins are returned with each peace of stablecoin, because it affects reserves
    def basecoinsAmount(r: N, sc: N) = {
      val price = stablecoinNominalPrice(r, sc)
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

  def calculateReservecoinsForRedeemedStablecoins(amountSC: N): BigDecimal = {
    require(amountSC <= stablecoins)

    if (reservesRatio() < pegReservesRatio) {
      val Pexch = oracle.conversionRate(PegCurrency, BaseCoin)
      val one_min_fee = 1 - baseFee
      val inv_rm = (1 / pegReservesRatio)
      val d = inv_rm * one_min_fee

      val amountToMinimum = {
        val x1 = pegReservesRatio * Pexch * math.pow(stablecoins.toDouble, d.toDouble) / reserves
        stablecoins - math.pow(x1.toDouble, (1 / (d - 1)).toDouble)
      }
      val compensatedSC = amountToMinimum.min(amountSC)

      val sc_min_n = stablecoins - compensatedSC
      val one_min_inv_rm = 1 - inv_rm
      val x1 = one_min_fee / one_min_inv_rm
      val x2 = inv_rm * math.log((sc_min_n / stablecoins).toDouble)
      val one_min_d = 1 - d
      val x3_1 = math.pow(sc_min_n.toDouble, one_min_d.toDouble)
      val x3_2 = math.pow(stablecoins.toDouble, one_min_d.toDouble)
      val x3_3 = math.pow(stablecoins.toDouble, d.toDouble)
      val x3 = Pexch * x3_3 * (x3_1 - x3_2) / (reserves * one_min_d)
      val A = x1 * (x2 - x3)

      val newReservecoins = math.exp(A.doubleValue) * reservecoins
      newReservecoins - reservecoins
    }
    else 0.0  // if (reservesRatio() >= minReserveRatio) reservecoins are not paid
  }

  // TODO: reconcile with swap
  override def sellStablecoins(amountSC: N): Try[N] = ???

  /**
   * New version of stablecoin redemption. If the system is under-collateralized (i.e., reserves ratio < minReserveRatio),
   * redemption is partially fulfilled in basecoins and the rest is returned in reservecoins
   * @param amountSC amount of stablecoins to redeem
   * @return returned number of Basecoins and Reservecoins
   */
  def sellStablecoinWithSwap(amountSC: N): Try[(N,N)] = Try {
    require(amountSC > 0)

    val baseCoinsToReturn = calculateBasecoinsForRedeemedStablecoins(amountSC)
    val reserveCoinsToReturn = calculateReservecoinsForRedeemedStablecoins(amountSC)

    val newReserves = reserves - baseCoinsToReturn
    val newReservecoins = reservecoins + reserveCoinsToReturn
    val newStablecoins = stablecoins - amountSC

    reserves = newReserves
    reservecoins = newReservecoins
    stablecoins = newStablecoins

    (baseCoinsToReturn, reserveCoinsToReturn)
  }

  def calculateBasecoinsForMintedReservecoinsIter(amountRC: Int, accuracy: Int = 1) = {
    require(amountRC > 0)
    var newReservecoins = reservecoins
    var newReserves = reserves
    var totalAmountBasecoinsToPay: N = 0.0

    def fee(r: N, sc: N): N = {
      val ratio = reservesRatio(r, sc)
      val additionalFee =
        if (ratio >= optimalReservesRatio) {
          val optimalReserves = sc * oracle.conversionRate(PegCurrency, BaseCoin) * optimalReservesRatio
          k_rm * (r - optimalReserves) / optimalReserves
        }
        else BigDecimal(0.0)
      baseFee + additionalFee
    }

    // we need to track how many basecoins to pay for each peace of reservecoin
    def basecoinsAmount(r: N, sc: N, rc: N) = {
      val price = reservecoinNominalPrice(r, sc, rc)
      price * (1 + fee(r,sc))
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
    val r0 = reserves
    val rc0 = reservecoins
    val rc0_plus_N_div_rc0 = (rc0 + amountRC) / rc0
    val one_plus_fee0 = 1 + baseFee
    val L = stablecoins * oracle.conversionRate(PegCurrency, BaseCoin)
    val A = optimalReservesRatio / (optimalReservesRatio * (one_plus_fee0 - k_rm) + k_rm)

    def calculateRange1() = {
      val X = (1 - 1/pegReservesRatio) * one_plus_fee0
      math.pow(rc0_plus_N_div_rc0.toDouble, X.toDouble) * r0
    }

    def calculateRange2() = {
      val X = 1 / (1 - 1/pegReservesRatio)
      val rmin_mul_L = pegReservesRatio * L
      val x1 = math.pow(rc0_plus_N_div_rc0.toDouble, one_plus_fee0.toDouble)
      val x2 = math.pow((rmin_mul_L / r0).toDouble, (-X).toDouble)
      val x3 = rmin_mul_L - L
      x1 * x2 * x3 + L
    }

    def calculateRange3() = {
      val ropt_mul_l = optimalReservesRatio * L
      val X = 1 / ((1-1/pegReservesRatio)*one_plus_fee0)
      val Y = 1 / one_plus_fee0
      val r0_pow_X = math.pow(r0.toDouble, X.toDouble)
      val rmin_min_one_pow_Y = math.pow((pegReservesRatio - 1).toDouble, Y.toDouble)
      val ropt_min_one_pow_Y = math.pow((optimalReservesRatio - 1).toDouble, Y.toDouble)
      val rmin_mul_L_pow_X = math.pow((pegReservesRatio * L).toDouble, X.toDouble)
      val x1 = rc0_plus_N_div_rc0 * r0_pow_X * rmin_min_one_pow_Y / (rmin_mul_L_pow_X * ropt_min_one_pow_Y)
      val Z = math.pow(x1.toDouble, (1/A).toDouble)
      val V = one_plus_fee0 / (ropt_mul_l - L)
      val E = one_plus_fee0 - k_rm

      val newReserves = (Z*E + V*L) / (V - Z*k_rm/ropt_mul_l)
      newReserves
    }

    def calculateRange4() = {
      val x1 = math.pow(rc0_plus_N_div_rc0.toDouble, one_plus_fee0.toDouble)
      val x2 = r0 - L
      x1 * x2 + L
    }

    def calculateRange5() = {
      val ropt_mul_l = optimalReservesRatio * L
      val ropt_mul_L_min_L = ropt_mul_l - L
      val x1 = (r0-L) / ropt_mul_L_min_L
      val x2 = math.pow(x1.toDouble, (1 / one_plus_fee0).toDouble)
      val Z = math.pow((rc0_plus_N_div_rc0*x2).toDouble, (1/A).toDouble)
      val V = one_plus_fee0 / ropt_mul_L_min_L
      val E = one_plus_fee0 - k_rm

      val newReserves = (Z*E + V*L) / (V - Z*k_rm/ropt_mul_l)
      newReserves
    }

    def calculateRange6() = {
      val ropt_mul_l = optimalReservesRatio * L
      val V = (one_plus_fee0 + (k_rm * (r0 - ropt_mul_l) / ropt_mul_l)) / (r0 - L)
      val Z = math.pow(rc0_plus_N_div_rc0.toDouble, (1/A).toDouble)
      val E = one_plus_fee0 - k_rm
      val newReserves = (Z*E + V*L) / (V - Z*k_rm/ropt_mul_l)
      newReserves
    }

    /*
      Depending on the initial and new reserve ratios we need to use different formulas.
      Initial ratio is known in advance, so we can eliminate using inappropriate formulas, but
      new ratio isn't known so we need to calculate all possible variants and then, by analyzing the results,
      pick only the correct one. See more details in paper.
     */
    val initReserveRatio = r0 / L
    val newReserves =
      if (initReserveRatio < pegReservesRatio) {
        val newReserves1 = calculateRange1()
        val newReserveRatio1 = newReserves1 / L
        lazy val newReserves2 = calculateRange2()
        lazy val newReserveRatio2 = newReserves2 / L
        lazy val newReserves3 = calculateRange3()
        lazy val newReserveRatio3 = newReserves3 / L

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
        val newReserves4 = calculateRange4()
        val newReserveRatio4 = newReserves4 / L
        lazy val newReserves5 = calculateRange5()
        lazy val newReserveRatio5 = newReserves5 / L

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
        calculateRange6()
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

  def calculateBasecoinsForRedeemedReservecoinsIter(amountRC: Int, accuracy: Int = 1) = {
    require(amountRC > 0)
    var newReservecoins = reservecoins
    var newReserves = reserves
    var totalAmountBasecoinsToReturn: N = 0.0

    def fee(r: N, sc: N): N = {
      val ratio = reservesRatio(r, sc)
      val additionalFee =
        if (ratio < optimalReservesRatio) {
          val optimalReserves = sc * oracle.conversionRate(PegCurrency, BaseCoin) * optimalReservesRatio
          k_rr * (optimalReserves - r) / optimalReserves
        } else BigDecimal(0.0)
      baseFee + additionalFee
    }

    // we need to track how many basecoins to return for each peace of reservecoin
    def basecoinsAmount(r: N, sc: N, rc: N) = {
      val price = reservecoinNominalPrice(r, sc, rc)
      price * (1 - fee(r,sc))
    }

    for(i <- 0 until amountRC * accuracy) {
      val amountBasecoinsToReturn = basecoinsAmount(newReserves, stablecoins, newReservecoins) / accuracy
      newReservecoins -= 1.0 / accuracy
      newReserves -= amountBasecoinsToReturn
      totalAmountBasecoinsToReturn += amountBasecoinsToReturn
    }

    totalAmountBasecoinsToReturn
  }

  def calculateBasecoinsForRedeemedReservecoins(amountRC: N): N = {
    require(amountRC > 0)
    val r0 = reserves
    val rc0 = reservecoins
    val rc0_min_N_div_rc0 = (rc0 - amountRC) / rc0
    val one_min_fee0_min_krr = 1 - baseFee - k_rr
    val L = stablecoins * oracle.conversionRate(PegCurrency, BaseCoin)
    val krr_div_L_ropt = k_rr/(L*optimalReservesRatio)
    val A1 = 1 / one_min_fee0_min_krr
    val A2 = optimalReservesRatio / (optimalReservesRatio*one_min_fee0_min_krr + k_rr)

    def calculateRange1() = {
      val V = (one_min_fee0_min_krr + r0*krr_div_L_ropt) / r0
      val p = (pegReservesRatio - 1) / (pegReservesRatio * A1)
      val Z = math.pow(rc0_min_N_div_rc0.toDouble, p.toDouble)
      (Z * one_min_fee0_min_krr) / (V - Z * (krr_div_L_ropt))
    }

    def calculateRange2() = {
      val rmin_mul_L = pegReservesRatio * L
      val x1 = (one_min_fee0_min_krr + pegReservesRatio*k_rr/optimalReservesRatio)
      val x2 = one_min_fee0_min_krr + r0*krr_div_L_ropt
      val x3 = (r0-L)*x1 / ((rmin_mul_L-L)*x2)
      val x4 = math.pow(x3.toDouble, A2.toDouble) * rc0_min_N_div_rc0
      val p = (1 - (1 / pegReservesRatio)) / A1
      val Z = math.pow(x4.toDouble, p.toDouble)
      val V = x1 / rmin_mul_L
      (Z * one_min_fee0_min_krr) / (V - Z * (krr_div_L_ropt))
    }

    def calculateRange3() = {
      val ropt_min_one = optimalReservesRatio - 1
      val one_min_fee0 = 1 - baseFee

      val x1_1 = (r0-L) / (ropt_min_one*L)
      val x1 = math.pow(x1_1.toDouble, (1/one_min_fee0).toDouble)
      val x2 = (optimalReservesRatio-1) / (pegReservesRatio-1)
      val x3_1 = one_min_fee0_min_krr + pegReservesRatio * k_rr / optimalReservesRatio
      val x3 = x3_1 / one_min_fee0
      val x4 = math.pow((x2*x3).toDouble, A2.toDouble)
      val x = rc0_min_N_div_rc0 * x1 * x4
      val p = (1 - (1 / pegReservesRatio)) / A1
      val Z = math.pow(x.toDouble, p.toDouble)
      val V = x3_1 / (pegReservesRatio*L)
      (Z * one_min_fee0_min_krr) / (V - Z * (krr_div_L_ropt))
    }

    def calculateRange4(): BigDecimal = {
      val V = (one_min_fee0_min_krr + r0 * krr_div_L_ropt) / (r0 - L)
      val Z = math.pow(rc0_min_N_div_rc0.toDouble, (1/A2).toDouble)
      (Z * one_min_fee0_min_krr + V*L) / (V - Z * (krr_div_L_ropt))
    }

    def calculateRange5(): BigDecimal = {
      val one_min_fee0 = 1 - baseFee
      val roptL_min_L = optimalReservesRatio * L - L
      val x1_1 = (r0 - L) / roptL_min_L
      val x1 = math.pow(x1_1.toDouble, (1/one_min_fee0).toDouble)
      val Z = math.pow((rc0_min_N_div_rc0 * x1).toDouble, (1/A2).toDouble)
      val V = one_min_fee0 / roptL_min_L
      (Z * one_min_fee0_min_krr + V*L) / (V - Z * (krr_div_L_ropt))
    }

    def calculateRange6(): BigDecimal = {
      val x1 = math.pow(rc0_min_N_div_rc0.toDouble, (1-baseFee).toDouble)
      val x2 = r0 - L
      x1 * x2 + L
    }

    /*
      Depending on the initial and new reserve ratios we need to use different formulas.
      Initial ratio is known in advance, so we can eliminate using inappropriate formulas, but
      new ratio isn't known so we need to calculate all possible variants and then, by analyzing the results,
      pick only the correct one. See more details in paper.
     */
    val initReserveRatio = r0 / L
    val newReserves =
      if (initReserveRatio < pegReservesRatio) {
        calculateRange1()
      }
      else if (pegReservesRatio <= initReserveRatio && initReserveRatio < optimalReservesRatio) {
        val newReserves4 = calculateRange4()
        val newReserveRatio4 = newReserves4 / L
        if (newReserveRatio4 >= pegReservesRatio)
          newReserves4
        else
          calculateRange2()
      }
      else {
        val newReserves6 = calculateRange6()
        val newReserveRatio6 = newReserves6 / L
        if (newReserveRatio6 >= optimalReservesRatio)
          newReserves6
        else {
          val newReserves5 = calculateRange5()
          val newReserveRatio5 = newReserves5 / L
          if (newReserveRatio5 >= pegReservesRatio)
            newReserves5
          else
            calculateRange3()
        }
      }

     reserves - newReserves
  }

  override def sellReservecoins(amountRC: N): Try[N] = Try {
    require(amountRC > 0)

    val baseCoinsToReturn = calculateBasecoinsForRedeemedReservecoins(amountRC)

    val newReserves = reserves - baseCoinsToReturn
    val newReservecoins = reservecoins - amountRC

    require(newReservecoins > 0)

    reserves = newReserves
    reservecoins = newReservecoins

    baseCoinsToReturn
  }
}
