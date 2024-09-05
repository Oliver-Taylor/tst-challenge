package com.tst.challenge.domain

import cats.syntax.foldable._
import cats.effect.{IO, IOApp}
import com.tst.challenge.model.{Promotion, PromotionCombo}

object CruisePromotionService extends IOApp.Simple {

  /**
    * Assumptions:
    *
    * - The order of the output promotion combos do not matter
    * - The number of promotions are not expected to be large (currently the solution is roughly O(2^n))
    * - There is enough memory to store all valid combinations
    */
  def allCombinablePromotions(allPromotions: Seq[Promotion]): Seq[PromotionCombo] = {
    // Cache previously calculated results in the valid combinations recursion
    val memoize = collection.mutable.Map.empty[(Set[String], Set[String]), Set[Set[String]]]

    // Recursively build up all valid combinations of promotions for a given code
    def validCombinations(remaining: Seq[Promotion], current: Set[String], excluded: Set[String]): Set[Set[String]] =
      // return pre-computed results
      if (memoize.contains((current, excluded))) memoize((current, excluded))
      // avoid returning a single promotion code
      else if (remaining.isEmpty && current.size > 1) Set(current)
      // we have reached the end of the list of promotions and have not found a valid combination
      else if (remaining.isEmpty) Set.empty
      else {
        val head = remaining.head
        val tail = remaining.tail

        val result = if (excluded.contains(head.code)) {
          validCombinations(tail, current, excluded)
        } else {
          validCombinations(tail, current + head.code, excluded ++ head.notCombinableWith) ++ validCombinations(
            tail,
            current,
            excluded
          )
        }

        memoize((current, excluded)) = result
        result
      }

    val allCombinations = validCombinations(allPromotions, Set.empty, Set.empty).toSeq

    // Filter out any combinations that are a subset of another combination
    val largestCombinations = allCombinations.filterNot { combination =>
      allCombinations.exists(other => other.size > combination.size && combination.subsetOf(other))
    }

    largestCombinations.map(promotions => PromotionCombo(promotions.toSeq))
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
      _ <- IO(println(allCombinablePromotions(promotions).mkString_("\n")))
      _ <- IO(println("Combinable promotions for P1:"))
      _ <- IO(println(combinablePromotions("P1", promotions).mkString_("\n")))
      _ <- IO(println("Combinable promotions for P3:"))
      _ <- IO(println(combinablePromotions("P3", promotions).mkString_("\n")))
    } yield {}

  }
}
