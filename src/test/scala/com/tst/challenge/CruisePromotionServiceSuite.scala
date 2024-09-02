package com.tst.challenge

import com.tst.challenge.domain.CruisePromotionService
import com.tst.challenge.model.{Promotion, PromotionCombo}
import munit.FunSuite

class CruisePromotionServiceSuite extends FunSuite {

  test("CruisePromotionService#allCombinablePromotions should combination with no exclusions") {
    val promotions =
      List(
        Promotion.of("P1"),
        Promotion.of("P2"),
      )

    val output = List(
      PromotionCombo.of("P1", "P2"),
    )

    assertEquals(CruisePromotionService.allCombinablePromotions(promotions), output)
  }

  test("CruisePromotionService#allCombinablePromotions should generate simple combination when there are exclusions") {
    val promotions =
      List(
        Promotion.of("P1", "P3"),
        Promotion.of("P2"),
        Promotion.of("P3", "P1"),
      )

    val output = List(
      PromotionCombo.of("P1", "P2"),
      PromotionCombo.of("P2", "P3"),
    )

    assertEquals(CruisePromotionService.allCombinablePromotions(promotions), output)
  }

  test("CruisePromotionService#allCombinablePromotions should generate the maximum subset of combinable promotions") {
    val promotions =
      List(
        Promotion.of("P1", "P3"),
        Promotion.of("P2"),
        Promotion.of("P3", "P1"),
        Promotion.of("P4")
      )

    val output = List(
      PromotionCombo.of("P1", "P2", "P4"),
      PromotionCombo.of("P2", "P3", "P4"),
    )

    assertEquals(CruisePromotionService.allCombinablePromotions(promotions), output)
  }

  test("CruisePromotionService#allCombinablePromotions should generate all combinable promotions") {
    val promotions =
      List(
        Promotion.of("P1", "P3"),
        Promotion.of("P2", "P4", "P5"),
        Promotion.of("P3", "P1"),
        Promotion.of("P4", "P2"),
        Promotion.of("P5", "P2")
      )

    val output = List(
      PromotionCombo.of("P3", "P4", "P5"),
      PromotionCombo.of("P2", "P3"),
      PromotionCombo.of("P1", "P4", "P5"),
      PromotionCombo.of("P1", "P2"),
    )

    assertEquals(CruisePromotionService.allCombinablePromotions(promotions), output)
  }

  test("CruisePromotionService#combinablePromotion should return all valid promotions for P1") {
    val promotions =
      List(
        Promotion.of("P1", "P3"),
        Promotion.of("P2", "P4", "P5"),
        Promotion.of("P3", "P1"),
        Promotion.of("P4", "P2"),
        Promotion.of("P5", "P2")
      )

    val output = List(
      PromotionCombo.of("P1", "P4", "P5"),
      PromotionCombo.of("P1", "P2"),
    )

    assertEquals(CruisePromotionService.combinablePromotions("P1", promotions), output)
  }

  test("CruisePromotionService#combinablePromotion should return all valid promotions for P3") {
    val promotions =
      List(
        Promotion.of("P1", "P3"),
        Promotion.of("P2", "P4", "P5"),
        Promotion.of("P3", "P1"),
        Promotion.of("P4", "P2"),
        Promotion.of("P5", "P2")
      )

    val output = List(
      PromotionCombo.of("P3", "P4", "P5"),
      PromotionCombo.of("P2", "P3"),
    )

    assertEquals(CruisePromotionService.combinablePromotions("P3", promotions), output)
  }
}
