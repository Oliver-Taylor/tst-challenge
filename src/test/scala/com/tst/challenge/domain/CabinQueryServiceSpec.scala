package com.tst.challenge.domain

import munit.FunSuite
import com.tst.challenge.model.CabinPrice

class CabinQueryServiceSpec extends FunSuite {

  // Test case 1: Matching Cabins
  test("getCabinPrices should return only prices for the target cabin code") {
    val prices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CC", "M1", 250.00),
      CabinPrice("CA", "M2", 210.00)
    )
    val targetCabinCode = "CA"
    val expected = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "M2", 210.00)
    )
    val actual = CabinQueryService.getCabinPrices(prices, targetCabinCode)
    assertEquals(actual, expected)
  }

  // Test case 2: No Matching Cabins
  test("getCabinPrices should return an empty sequence if no cabins match the target code") {
    val prices = Seq(
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CC", "M1", 250.00)
    )
    val targetCabinCode = "CA"
    val expected = Seq.empty[CabinPrice]
    val actual = CabinQueryService.getCabinPrices(prices, targetCabinCode)
    assertEquals(actual, expected)
  }

  // Test case 3: Empty Input List
  test("getCabinPrices should return an empty sequence if the input list is empty") {
    val prices = Seq.empty[CabinPrice]
    val targetCabinCode = "CA"
    val expected = Seq.empty[CabinPrice]
    val actual = CabinQueryService.getCabinPrices(prices, targetCabinCode)
    assertEquals(actual, expected)
  }

  // Test case 4: Case Sensitivity
  test("getCabinPrices should be case-sensitive when matching cabin codes") {
    val prices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("ca", "M2", 210.00), // Different case
      CabinPrice("Ca", "S1", 225.00)  // Different case
    )
    val targetCabinCode = "CA"
    val expected = Seq(
      CabinPrice("CA", "M1", 200.00)
    )
    val actual = CabinQueryService.getCabinPrices(prices, targetCabinCode)
    assertEquals(actual, expected)
  }

  // Test case 5: Target code not present, but similar codes are (extra check for robustness)
  test("getCabinPrices should return an empty sequence when target code is not found, even with similar codes") {
    val prices = Seq(
      CabinPrice("CAX", "M1", 200.00),
      CabinPrice("XCA", "M2", 210.00)
    )
    val targetCabinCode = "CA"
    val expected = Seq.empty[CabinPrice]
    val actual = CabinQueryService.getCabinPrices(prices, targetCabinCode)
    assertEquals(actual, expected)
  }
}
