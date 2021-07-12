package stablecoin

import stablecoin.Currency.{BaseCoin, PegCurrency}

import scala.util.Try

class StablecoinBankNewFormulas(address: Address, // bank's address
                                oracle: Oracle, // Oracle used by the bank
                                baseFee: N, // the base fee charged by the bank when buying and selling stablecoins and reservecoins
                                minReserveRatio: N, // the bank's minimum reserve ratio
                                maxReserveRatio: N, // the bank's maximum reserve ratio
                                val optimalReserveRatio: N, // the bank's optimal reserve ratio
                                reservecoinDefaultPrice: N, // default price of reservecoins, used when there are 0 reservecoins, 1 RC = 1 baseCoin * defaultPrice
                                val k_rm: N, // linear correlation coefficient for fee calculation for RC minting
                                val k_rr: N, // linear correlation coefficient for fee calculation for RC redeeming
                                val k_sm: N, // linear correlation coefficient for fee calculation for SC minting
                                val k_sr: N, // linear correlation coefficient for fee calculation for SC redeeming
                                initReserves: N = 0.0, // initial amount of basecoin reserves
                                initStablecoins: N = 0.0, // initial amount of stablecoins
                                initReservecoins: N = 0.0 // initial amount of reservecoins
                               )
  extends StablecoinBank(address, oracle, baseFee, minReserveRatio, maxReserveRatio,
                         reservecoinDefaultPrice, initReserves, initStablecoins, initReservecoins) {

  override def stablecoinNominalPrice(r: N, sc: N): N = {
    val p = oracle.conversionRate(PegCurrency, BaseCoin)
    if (sc == 0) p
    else {
      val ratio = reservesRatio(r, sc)
      val k = BigDecimal(1.0).min(ratio / minReserveRatio)
      k * p
    }
  }

  override def reservecoinNominalPrice(r: N, sc: N, rc: N): N = {
    if (rc != 0) {
      val liab = stablecoinNominalPrice(r, sc) * sc
      (r - liab) / rc
    }
    else reservecoinDefaultPrice
  }

  def calculateBasecoinsForMintedStablecoinsIter(amountSC: Int, accuracy: Int = 1) = {
    require(reservesRatio() > minReserveRatio)
    var newReserves = reserves
    var newStablecoins = stablecoins
    var totalAmountBaseToPay: N = 0.0

    def fee(r: N, sc: N): N = {
      val ratio = reservesRatio(r, sc)
      val additionalFee =
        if (ratio < optimalReserveRatio) {
          val optimalReserves = sc * oracle.conversionRate(PegCurrency, BaseCoin) * optimalReserveRatio
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
      require(reservesRatio(newReserves, newStablecoins) >= minReserveRatio)
      totalAmountBaseToPay += amountBaseToPay
    }

    totalAmountBaseToPay
  }

  def calculateBasecoinsForMintedStablecoins(amountSC: Int) = {
    require(reservesRatio() > minReserveRatio)
    val Pexch = oracle.conversionRate(PegCurrency, BaseCoin)

    def calculateAmountWithDynamicFee(sc0: N, r0: N, amount: N) = {
      val K = k_sm / optimalReserveRatio
      val one_plus_K = 1 + K
      val P = Pexch * (1 + baseFee + k_sm)
      val sc0_plus_N = sc0 + amount
      val C = (r0 - P * (sc0 / one_plus_K)) * math.pow(sc0.toDouble, K.toDouble)
      val x1 = P * (sc0_plus_N) / one_plus_K
      val x2 = math.pow(sc0_plus_N.toDouble, K.toDouble)
      x1 + (C / x2) - r0
    }

    val amountBasecoinsToPay =
      if (reservesRatio() > optimalReserveRatio) {
        // calculating how much SCs need to be bought to decrease ratio to optimal level
        val amountToOptimum = (reserves - optimalReserveRatio * Pexch * stablecoins) / (Pexch*(optimalReserveRatio-1-baseFee))
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
    require(newReserveRatio >= minReserveRatio, "The formulas are not suitable if ratio falls below optimum.")

    amountBasecoinsToPay
  }

  def buyStablecoins(amountSC: Int): Try[N] = Try {
    require(amountSC > 0)
    require(reservesRatio() > minReserveRatio)

    val baseCoinsToPay = calculateBasecoinsForMintedStablecoins(amountSC)

    val newReserves = reserves + baseCoinsToPay
    val newStablecoins = stablecoins + amountSC

    require(acceptableReserveChange(false, true, false, newReserves, stablecoins))
    require(reservesRatio(newReserves, newStablecoins) >= minReserveRatio)

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
        if (ratio > optimalReserveRatio) {
          val optimalReserves = sc * oracle.conversionRate(PegCurrency, BaseCoin) * optimalReserveRatio
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

  def calculateBasecoinsForRedeemedStablecoins(amountSC: Int) = {
    val Pexch = oracle.conversionRate(PegCurrency, BaseCoin)

    def calculateRange1(sc0: N, r0: N, amount: N) = {
      val K = k_sr / optimalReserveRatio
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
      val x1 = (1 - baseFee) / minReserveRatio
      val x2 = (sc0 - amount) / sc0
      val x = r0 * math.pow(x2.toDouble, x1.toDouble)
      r0 - x
    }

    def calculateRange4(sc0: N, r0: N, amount: N) = {
      val rounded_ratio = reservesRatio(r0, sc0).setScale(10, BigDecimal.RoundingMode.HALF_UP)
      require(rounded_ratio >= minReserveRatio)   // we do rounding to pass the check cause reservesRatio() might be slightly less due to rounding issues when we do BigDecimal->Double conversion in math.pow() on the previous step
      require(reservesRatio(r0, sc0) < optimalReserveRatio)
      // calculating how much SCs need to be redeemed to increase ratio to optimal level
      val amountToOptimum = (r0 - optimalReserveRatio * Pexch * sc0) / (Pexch*(1 - optimalReserveRatio-baseFee))
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
      require(reservesRatio(r0, sc0) < minReserveRatio)
      // calculating how much SCs need to be redeemed to increase ratio to minimum level
      val amountToMinimum = {
        val d = (1 - baseFee) / minReserveRatio
        val x1 = minReserveRatio * Pexch * math.pow(sc0.toDouble, d.toDouble) / r0
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
      if (reservesRatio() >= optimalReserveRatio)
        calculateRange1(stablecoins, reserves, amountSC)
      else {
        if (reservesRatio() < minReserveRatio)
          calculateRange5(stablecoins, reserves, amountSC)
        else
          calculateRange4(stablecoins, reserves, amountSC)
      }

    amountBasecoinsToReturn
  }

//  def calculateBasecoinsForRedeemedStablecoinsIter(amountSC: Int, accuracy: Int = 1) = {
//    require(amountSC <= stablecoins)
//    var newReserves = reserves
//    var newStablecoins = stablecoins
//    var totalAmountBaseToReturn: N = 0.0
//
//    for(i <- 0 until amountSC * accuracy) {
//      val price = stablecoinNominalPrice(newReserves, newStablecoins)
//      val amountBase = price / accuracy
//      val amountBaseToReturn = amountBase //* (1 - fee)
//      newReserves -= amountBaseToReturn
//      newStablecoins -= (1.0 / accuracy)
//      totalAmountBaseToReturn += amountBaseToReturn
//    }
//
//    totalAmountBaseToReturn
//  }
//
//  // TODO: fix formula to work with (k=1). Currently it works correctly only when k<1, e.g. when system is under-collateralized
//  def calculateBasecoinsForRedeemedStablecoins(amountSC: N) = {
//    require(amountSC <= stablecoins)
//    val t1 = 1 / minReserveRatio
//    val t2 = (stablecoins - amountSC) / stablecoins
//    val newReserves = reserves * math.pow(t2.doubleValue, t1.doubleValue)
//    reserves - newReserves
//  }
//
  def calculateReservecoinsForRedeemedStablecoinsIter(amountSC: Int, accuracy: Int = 1) = {
    require(amountSC <= stablecoins)
    var newStablecoins = stablecoins
    var newReservecoins = reservecoins
    var newReserves = reserves
    var totalAmountReservecoinsToReturn: N = 0.0

    def reservecoinsSwapAmount(r: N, sc: N, rc: N) = {
      val p = oracle.conversionRate(PegCurrency, BaseCoin)
      val k = min(1, reservesRatio(r, sc) / minReserveRatio)
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

    if (reservesRatio() < minReserveRatio) {
      val Pexch = oracle.conversionRate(PegCurrency, BaseCoin)
      val one_min_fee = 1 - baseFee
      val inv_rm = (1 / minReserveRatio)
      val d = inv_rm * one_min_fee

      val amountToMinimum = {
        val x1 = minReserveRatio * Pexch * math.pow(stablecoins.toDouble, d.toDouble) / reserves
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

  /**
   * New version of stablecoin redemption. If the system is under-collateralized (i.e., reserves ratio < minReserveRatio),
   * redemption is partially fulfilled in basecoins and the rest is returned in reservecoins
   * @param amountSC amount of stablecoins to redeem
   * @return returned number of Basecoins and Reservecoins
   */
  def sellStablecoinWithSwap(amountSC: Int): Try[(N,N)] = Try {
    require(amountSC > 0)

    val baseCoinsToReturn = calculateBasecoinsForRedeemedStablecoins(amountSC)
    val reserveCoinsToReturn = calculateReservecoinsForRedeemedStablecoins(amountSC)

    val newReserves = reserves - baseCoinsToReturn
    val newReservecoins = reservecoins + reserveCoinsToReturn
    val newStablecoins = stablecoins - amountSC
    require(acceptableReserveChange(false, false, false, newReserves, newStablecoins))

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
        if (ratio >= optimalReserveRatio) {
          val optimalReserves = sc * oracle.conversionRate(PegCurrency, BaseCoin) * optimalReserveRatio
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
    val A = optimalReserveRatio / (optimalReserveRatio * (one_plus_fee0 - k_rm) + k_rm)

    def calculateRange1() = {
      val X = (1 - 1/minReserveRatio) * one_plus_fee0
      math.pow(rc0_plus_N_div_rc0.toDouble, X.toDouble) * r0
    }

    def calculateRange2() = {
      val X = 1 / (1 - 1/minReserveRatio)
      val rmin_mul_L = minReserveRatio * L
      val x1 = math.pow(rc0_plus_N_div_rc0.toDouble, one_plus_fee0.toDouble)
      val x2 = math.pow((rmin_mul_L / r0).toDouble, (-X).toDouble)
      val x3 = rmin_mul_L - L
      x1 * x2 * x3 + L
    }

    def calculateRange3() = {
      val ropt_mul_l = optimalReserveRatio * L
      val X = 1 / ((1-1/minReserveRatio)*one_plus_fee0)
      val Y = 1 / one_plus_fee0
      val r0_pow_X = math.pow(r0.toDouble, X.toDouble)
      val rmin_min_one_pow_Y = math.pow((minReserveRatio - 1).toDouble, Y.toDouble)
      val ropt_min_one_pow_Y = math.pow((optimalReserveRatio - 1).toDouble, Y.toDouble)
      val rmin_mul_L_pow_X = math.pow((minReserveRatio * L).toDouble, X.toDouble)
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
      val ropt_mul_l = optimalReserveRatio * L
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
      val ropt_mul_l = optimalReserveRatio * L
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
      if (initReserveRatio < minReserveRatio) {
        val newReserves1 = calculateRange1()
        val newReserveRatio1 = newReserves1 / L
        lazy val newReserves2 = calculateRange2()
        lazy val newReserveRatio2 = newReserves2 / L
        lazy val newReserves3 = calculateRange3()
        lazy val newReserveRatio3 = newReserves3 / L

        if (newReserveRatio1 < minReserveRatio) {
          newReserves1
        }
        else if (minReserveRatio <= newReserveRatio2 && newReserveRatio2 < optimalReserveRatio) {
          assert(minReserveRatio <= newReserveRatio1)
          newReserves2
        }
        else {
          assert(optimalReserveRatio <= newReserveRatio2)
          assert(optimalReserveRatio <= newReserveRatio3)
          newReserves3
        }
      }
      else if (minReserveRatio <= initReserveRatio && initReserveRatio < optimalReserveRatio) {
        val newReserves4 = calculateRange4()
        val newReserveRatio4 = newReserves4 / L
        lazy val newReserves5 = calculateRange5()
        lazy val newReserveRatio5 = newReserves5 / L

        if (newReserveRatio4 < optimalReserveRatio)
          newReserves4
        else {
          assert(optimalReserveRatio <= newReserveRatio4)
          assert(optimalReserveRatio <= newReserveRatio5)
          newReserves5
        }
      }
      else {
        assert(optimalReserveRatio <= initReserveRatio)
        calculateRange6()
      }

    newReserves - reserves
  }

  def buyReservecoins(amountRC: Int, accuracy: Int = 10): Try[N] = Try {
    require(amountRC > 0)

    val baseCoinsToPay = calculateBasecoinsForMintedReservecoins(amountRC)

    val newReserves = reserves + baseCoinsToPay
    val newReservecoins = reservecoins + amountRC

    require(acceptableReserveChange(false, true, false, newReserves, stablecoins))

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
        if (ratio < optimalReserveRatio) {
          val optimalReserves = sc * oracle.conversionRate(PegCurrency, BaseCoin) * optimalReserveRatio
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
    val krr_div_L_ropt = k_rr/(L*optimalReserveRatio)
    val A1 = 1 / one_min_fee0_min_krr
    val A2 = optimalReserveRatio / (optimalReserveRatio*one_min_fee0_min_krr + k_rr)

    def calculateRange1() = {
      val V = (one_min_fee0_min_krr + r0*krr_div_L_ropt) / r0
      val p = (minReserveRatio - 1) / (minReserveRatio * A1)
      val Z = math.pow(rc0_min_N_div_rc0.toDouble, p.toDouble)
      (Z * one_min_fee0_min_krr) / (V - Z * (krr_div_L_ropt))
    }

    def calculateRange2() = {
      val rmin_mul_L = minReserveRatio * L
      val x1 = (one_min_fee0_min_krr + minReserveRatio*k_rr/optimalReserveRatio)
      val x2 = one_min_fee0_min_krr + r0*krr_div_L_ropt
      val x3 = (r0-L)*x1 / ((rmin_mul_L-L)*x2)
      val x4 = math.pow(x3.toDouble, A2.toDouble) * rc0_min_N_div_rc0
      val p = (1 - (1 / minReserveRatio)) / A1
      val Z = math.pow(x4.toDouble, p.toDouble)
      val V = x1 / rmin_mul_L
      (Z * one_min_fee0_min_krr) / (V - Z * (krr_div_L_ropt))
    }

    def calculateRange3() = {
      val ropt_min_one = optimalReserveRatio - 1
      val one_min_fee0 = 1 - baseFee

      val x1_1 = (r0-L) / (ropt_min_one*L)
      val x1 = math.pow(x1_1.toDouble, (1/one_min_fee0).toDouble)
      val x2 = (optimalReserveRatio-1) / (minReserveRatio-1)
      val x3_1 = one_min_fee0_min_krr + minReserveRatio * k_rr / optimalReserveRatio
      val x3 = x3_1 / one_min_fee0
      val x4 = math.pow((x2*x3).toDouble, A2.toDouble)
      val x = rc0_min_N_div_rc0 * x1 * x4
      val p = (1 - (1 / minReserveRatio)) / A1
      val Z = math.pow(x.toDouble, p.toDouble)
      val V = x3_1 / (minReserveRatio*L)
      (Z * one_min_fee0_min_krr) / (V - Z * (krr_div_L_ropt))
    }

    def calculateRange4(): BigDecimal = {
      val V = (one_min_fee0_min_krr + r0 * krr_div_L_ropt) / (r0 - L)
      val Z = math.pow(rc0_min_N_div_rc0.toDouble, (1/A2).toDouble)
      (Z * one_min_fee0_min_krr + V*L) / (V - Z * (krr_div_L_ropt))
    }

    def calculateRange5(): BigDecimal = {
      val one_min_fee0 = 1 - baseFee
      val roptL_min_L = optimalReserveRatio * L - L
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
      if (initReserveRatio < minReserveRatio) {
        calculateRange1()
      }
      else if (minReserveRatio <= initReserveRatio && initReserveRatio < optimalReserveRatio) {
        val newReserves4 = calculateRange4()
        val newReserveRatio4 = newReserves4 / L
        if (newReserveRatio4 >= minReserveRatio)
          newReserves4
        else
          calculateRange2()
      }
      else {
        val newReserves6 = calculateRange6()
        val newReserveRatio6 = newReserves6 / L
        if (newReserveRatio6 >= optimalReserveRatio)
          newReserves6
        else {
          val newReserves5 = calculateRange5()
          val newReserveRatio5 = newReserves5 / L
          if (newReserveRatio5 >= minReserveRatio)
            newReserves5
          else
            calculateRange3()
        }
      }

     reserves - newReserves
  }

  def sellReservecoins(amountRC: Int): Try[N] = Try {
    require(amountRC > 0)

    val baseCoinsToReturn = calculateBasecoinsForRedeemedReservecoins(amountRC)

    val newReserves = reserves - baseCoinsToReturn
    val newReservecoins = reservecoins - amountRC

    require(newReservecoins > 0)
    require(acceptableReserveChange(false, false, true, newReserves, stablecoins))

    reserves = newReserves
    reservecoins = newReservecoins

    baseCoinsToReturn
  }
}
