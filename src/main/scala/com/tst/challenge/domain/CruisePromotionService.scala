package com.tst.challenge.domain

import cats.effect.{IO, IOApp}
import com.tst.challenge.model.{Promotion, PromotionCombo}

object CruisePromotionService extends IOApp.Simple {

  /**
   * Assumptions:
   *
   * - The order of the output promotion combos do not matter
   * - The number of promotions are not expected to be large (currently the solution is O(n^n))
   *
   *
   */
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    val codes = allPromotions.map(_.code).toSet

    // Built up a lookup of all allowable promotions for each promotion
    val allAllowable = allPromotions.map(p => p.code -> ((codes - p.code) -- p.notCombinableWith)).toMap

    def validCombinations(remaining: Set[String], current: Seq[String]): Set[Seq[String]] =
      if (remaining.isEmpty && current.size > 1) Set(current.sorted)
      else if (remaining.isEmpty) Set.empty
      else {
        val head  = remaining.head
        val tail  = remaining.tail
        val valid = (allAllowable(head) + head) -- current

        valid.flatMap { next =>
          val notAllowed = current.flatMap(code => codes -- allAllowable(code)).toSet
          if (notAllowed.contains(next)) validCombinations(tail, current)
          else validCombinations(tail, next +: current)
        }
      }

    val allCombinations = codes
      .flatMap { code =>
        val maximumPossible = allAllowable(code)
        validCombinations(maximumPossible, Seq(code)).toSeq
      }

    val largestCombinations = allCombinations.filter { combination =>
      !allCombinations.exists(other => other.size > combination.size && combination.toSet.subsetOf(other.toSet))
    }

    largestCombinations.map(PromotionCombo(_)).toSeq
  }

  def combinablePromotions(promotionCode: String, allPromotions: Seq[Promotion]): Seq[PromotionCombo] =
    // Filter out promotions that are not combinable with the given promotion code
    allCombinablePromotions(allPromotions.filterNot(_.notCombinableWith.contains(promotionCode)))
      .filter(_.promotionCodes.contains(promotionCode))

  override def run: IO[Unit] = {

    val promotions =
      List(
        Promotion.of("P1", "P3"),
        Promotion.of("P2", "P4", "P5"),
        Promotion.of("P3", "P1"),
        Promotion.of("P4", "P2"),
        Promotion.of("P5", "P2")
      )

    for {
      _ <- IO(println("All combinable promotions:"))
      _ <- IO(allCombinablePromotions(promotions).foreach(println))
      _ <- IO(println("Combinable promotions for P1:"))
      _ <- IO(combinablePromotions("P1", promotions).foreach(println))
      _ <- IO(println("Combinable promotions for P3:"))
      _ <- IO(combinablePromotions("P3", promotions).foreach(println))
    } yield {}

  }
}
