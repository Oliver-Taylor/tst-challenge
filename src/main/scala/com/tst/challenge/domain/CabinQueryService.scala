package com.tst.challenge.domain

import cats.effect.{IO, IOApp}
import com.tst.challenge.model.CabinPrice // Corrected import for CabinPrice

object CabinQueryService extends IOApp.Simple {

  /**
    * Filters a sequence of CabinPrice objects by cabin code.
    *
    * @param prices A sequence of CabinPrice objects.
    * @param cabinCode The cabin code to filter by.
    * @return A sequence of CabinPrice objects that match the provided cabinCode.
    */
  def getCabinPrices(prices: Seq[CabinPrice], cabinCode: String): Seq[CabinPrice] = {
    prices.filter(_.cabinCode == cabinCode)
  }

  /**
    * The main entry point for the IO application.
    */
  def run: IO[Unit] = {
    // 1. Create sample data
    val samplePrices = Seq(
      CabinPrice("CA", "M1", 200.00),
      CabinPrice("CB", "M1", 230.00),
      CabinPrice("CA", "S1", 225.00),
      CabinPrice("CC", "M1", 250.00),
      CabinPrice("CA", "M2", 210.00)
    )

    // 2. Define a sample cabinCode string to search for
    val targetCabinCode = "CA"

    // 3. Call getCabinPrices with the sample data and cabin code
    val result = getCabinPrices(samplePrices, targetCabinCode)

    // 4. Print the results to the console
    IO(println(s"Cabin prices for code '$targetCabinCode':")) *>
    IO(if (result.isEmpty) {
      println("No cabins found for this code.")
    } else {
      println(result.mkString("\n"))
    })
  }
}
