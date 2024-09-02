package com.tst.challenge

import com.tst.challenge.domain.CruisePriceService
import com.tst.challenge.model.{BestGroupPrice, CabinPrice, Rate}
import munit.FunSuite

class CruisePriceServiceSuite extends FunSuite {

  test("CruisePriceService should generate the best price for a single rate") {
    val rates = List(
      Rate("M1", "Military")
    )

    val cabinPrices = List(
      CabinPrice("CA", "M1", 200.00),
    )

    val expected = List(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
    )

    assertEquals(CruisePriceService.getBestGroupPrices(rates, cabinPrices), expected)
  }

  test("CruisePriceService should remove used rates once assigned and use the next best rate") {
    val rates = List(
      Rate("M1", "Military"),
      Rate("M1", "Military"),
    )

    val cabinPrices = List(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M1", 220.00),
      CabinPrice("CA", "M1", 230.00),
    )

    val expected = List(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CA", "M1", 220.00, "Military"),
    )

    assertEquals(CruisePriceService.getBestGroupPrices(rates, cabinPrices), expected)
  }

  test("CruisePriceService should find rates with different rate codes for the same rate group") {
    val rates = List(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
    )

    val cabinPrices = List(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M1", 210.00),
      CabinPrice("CA", "M2", 220.00),
    )

    val expected = List(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CA", "M1", 210.00, "Military"),
    )

    assertEquals(CruisePriceService.getBestGroupPrices(rates, cabinPrices), expected)
  }

  test("CruisePriceService should generate best price") {
    val rates = List(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val cabinPrices = List(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    val expected = List(
      BestGroupPrice("CA", "M1", 200.00, "Military"),
      BestGroupPrice("CA", "S1", 225.00, "Senior"),
      BestGroupPrice("CB", "M1", 230.00, "Military"),
      BestGroupPrice("CB", "S1", 245.00, "Senior"),
    )

    assertEquals(CruisePriceService.getBestGroupPrices(rates, cabinPrices), expected)
  }

}
