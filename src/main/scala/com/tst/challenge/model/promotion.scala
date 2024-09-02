package com.tst.challenge.model

import cats.Show

final case class Promotion(code: String, notCombinableWith: Seq[String])
object Promotion {
  def of(code: String, notCombinableWith: String*): Promotion = Promotion(code, notCombinableWith)
}

final case class PromotionCombo(promotionCodes: Seq[String])
object PromotionCombo {

  implicit val show: Show[PromotionCombo] = Show.show { combo =>
    combo.promotionCodes.mkString("PromotionCombo(", ", ", ")")
  }

  def of(promotionCodes: String*): PromotionCombo = PromotionCombo(promotionCodes)
}
