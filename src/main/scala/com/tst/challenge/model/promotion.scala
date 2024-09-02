package com.tst.challenge.model

final case class Promotion(code: String, notCombinableWith: Seq[String])
object Promotion {
  def of(code: String, notCombinableWith: String*): Promotion = Promotion(code, notCombinableWith)
}

final case class PromotionCombo(promotionCodes: Seq[String])
object PromotionCombo {
  def of(promotionCodes: String*): PromotionCombo = PromotionCombo(promotionCodes)
}
