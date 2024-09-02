package com.tst.challenge.domain

import cats.syntax.show._
import cats.syntax.foldable._
import cats.effect.{IO, IOApp}
import com.tst.challenge.model.{BestGroupPrice, CabinPrice, Rate}

import scala.collection.mutable

object CruisePriceService extends IOApp.Simple {

  /**
    * Assumptions:
    *
    * - We will never run out of cabin prices to offer people
    * - The rate code found in the cabin prices will always be present in the rates
    */
  def getBestGroupPrices(rates: Seq[Rate], prices: Seq[CabinPrice]): Seq[BestGroupPrice] = {

    // Initial scan of rates allows us to build a join between rate group and rate code
    val rateCodesToRateGroup: Map[String, String] = rates.view.map(rate => rate.rateCode -> rate.rateGroup).toMap

    val rateGroupLookup = prices
      // Join cabin price with rate group
      // Has the potential to fail if the rate code is not present in the provided rates
      .map(price => rateCodesToRateGroup(price.rateCode) -> price)
      .groupMap(_._1)(_._2)
      .view
      .mapValues(_.sortBy(_.price))
      .to(mutable.Map)

    rates
      .map { rate =>
        // Has the potential to fail if we run out of cabin prices to offer
        val cabinPrice = rateGroupLookup(rate.rateGroup).head
        rateGroupLookup.updateWith(rate.rateGroup)(_.map(_.tail))
        BestGroupPrice(
          cabinCode = cabinPrice.cabinCode,
          rateCode = cabinPrice.rateCode,
          price = cabinPrice.price,
          rateGroup = rate.rateGroup
        )
      }
      .sortBy(_.price)
  }

  override def run: IO[Unit] = {
    val inputRates = List(
      Rate("M1", "Military"),
      Rate("M2", "Military"),
      Rate("S1", "Senior"),
      Rate("S2", "Senior")
    )

    val inputCabinPrices = List(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CA", "M2", 250.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CA", "S2", 260.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CB", "M2", 260.00),
      CabinPrice("CB", "S1", 245.00),
      CabinPrice("CB", "S2", 270.00)
    )

    IO(CruisePriceService.getBestGroupPrices(inputRates, inputCabinPrices))
      .flatMap(result => IO(println(result.mkString_("\n"))))

  }
}
