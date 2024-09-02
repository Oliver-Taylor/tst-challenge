package com.tst.challenge.model

import cats.Show

final case class Rate(rateCode: String, rateGroup: String)

final case class CabinPrice(cabinCode: String, rateCode: String, price: BigDecimal)

final case class BestGroupPrice(cabinCode: String, rateCode: String, price: BigDecimal, rateGroup: String)

object BestGroupPrice {
  implicit val show: Show[BestGroupPrice] =
    Show.show(a => s"""BestGroupPrice(${a.cabinCode}, ${a.rateCode}, ${f"${a.price}%1.2f"}, ${a.rateGroup})""")
}
