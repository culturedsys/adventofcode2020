package day21

import cats.parse.{Parser => P}
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util
import cats.implicits._
import cats.syntax._
import scala.annotation.tailrec

final case class I(name: String)
final case class A(name: String)

object Day21 {
    final case class State(ingredients: Set[I] = Set(), ingredientsForAllergens: Map[A, Set[I]] = Map())

    def mergeBy[A, B](m1: Map[A, B], m2: Map[A, B])(reducer: (B, B) => B) =
        (m1.toSeq ++ m2).groupMapReduce(_._1)(_._2)(reducer)


    @tailrec
    def ensureUniqueValues[A, B](m: Map[A, Set[B]]): Map[A, Set[B]] = {
        val (singletons, multiples) = m.partition { case (_, ingredients) => ingredients.size == 1 }

        if (multiples.isEmpty)
            singletons
        else {
            val withoutSingletons = singletons.values.foldLeft(multiples) { (acc, singleton) =>
                acc.view.mapValues(_ -- singleton).toMap
            }
            ensureUniqueValues(singletons ++ withoutSingletons)
        }
    }

    def identifyAllergens(input: Seq[(Set[I], Set[A])]): Map[I, Option[A]] ={
        val ingredients: Set[I] = input.map(_._1).reduce(_.union(_))

        val allergenToIngredients: Map[A, Set[I]] = input.map { 
            case (ingredients, allergens) => allergens.map((_, ingredients)).toMap
        }.foldLeft(Map[A, Set[I]]()) { (acc, map) =>
            mergeBy(acc, map)(_.intersect(_))
        }

        val result = ensureUniqueValues(allergenToIngredients)

        ingredients.map { i =>
            (i, result.find { case (_, is) => is == Set(i) }.map(_._1))
        }.toMap
    }

    val whitespace = P.charsWhile1(_.isWhitespace).void
    val commaSep = P.char(',') ~ whitespace
    val ingredient = P.charsWhile1(_.isLetter).map(I)
    val ingredients = P.rep1Sep(ingredient, 1, whitespace)
    val allergen = P.charsWhile1(_.isLetter).map(A)
    val allergens = ((P.string("(contains") ~ whitespace).with1 *> P.rep1Sep(allergen, 1, commaSep)) <* P.char(')')
    val food = ingredients ~ (whitespace *> allergens)

    def parse(input: String) = 
        food.parseAll(input)
            .map(t => (t._1.toList.toSet, t._2.toList.toSet))
}


object Part1 extends IOApp {
    import Day21._
    
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day21/input.txt")
        maybeFoods = input.toSeq.traverse(parse)
        exitcode <- Util.executeEither {
            maybeFoods.map { foods => 
                val ingredientToAllergen = identifyAllergens(foods)
                val ingredientsWithoutAllergens = ingredientToAllergen.filter((a: Option[A]) => a.isEmpty).keySet
                foods.map(food => food._1.count(ingredient => ingredientsWithoutAllergens.contains(ingredient)))
                    .sum
            }
        }
    } yield exitcode
}

object Part2 extends IOApp {
    import Day21._
    
    override def run(args: List[String]): IO[ExitCode] = for {
        input <- Util.readInput("day21/input.txt")
        maybeFoods = input.toSeq.traverse(parse)
        exitcode <- Util.executeEither {
            maybeFoods.map { foods => 
                val ingredientToAllergen = identifyAllergens(foods)
                ingredientToAllergen.toList
                    .collect {
                        case (i, Some(a)) => (i, a)
                    }.sortBy(_._2.name)
                    .map(_._1.name)
                    .mkString(",")
            }
        }
    } yield exitcode
}