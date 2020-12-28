package day21

import cats.parse.{Parser => P}
import cats.effect.IOApp
import cats.effect.{ExitCode, IO}
import util.Util
import cats.implicits._
import cats.syntax._

final case class I(name: String)
final case class A(name: String)

object Day21 {
    final case class State(ingredients: Set[I] = Set(), ingredientsForAllergens: Map[A, Set[I]] = Map())

    def identifyAllergens(input: Seq[(Set[I], Set[A])]): Map[I, Option[A]] ={
        val s = input.foldLeft(State()) { (state, line) =>
            val (ingredients, allergens) = line
            val updatedIngredientsForAllergens = allergens.foldLeft(state.ingredientsForAllergens) { (iForA, allergen) =>
                iForA.updatedWith(allergen)(oldIngredients => oldIngredients.map(_.intersect(ingredients)).orElse(Some(ingredients)))
            }

            State(state.ingredients ++ ingredients, updatedIngredientsForAllergens)
        }

        def removeFound(iForA: Map[A, Set[I]]): Map[A, Set[I]] = {
            val (singletons, multiples) = iForA.partition(_._2.size == 1)

            if (multiples.isEmpty)
                singletons
            else {
                val filtered = singletons.foldLeft(multiples) { (acc, s) =>
                    acc.map(t => (t._1, t._2 - s._2.head))
                }
                removeFound(singletons ++ filtered)
            }
        }

        val result = removeFound(s.ingredientsForAllergens)

        s.ingredients.map { i =>
            (i, result.find { case (_, is) => is == Set(i) }.map(_._1))
        }.toMap
    }

    val whitespace = P.charsWhile1(_.isWhitespace).void
    val ingredient = P.charsWhile1(_.isLetter).map(I)
    val allergen = P.charsWhile1(_.isLetter).map(A)
    val allergens = ((P.string("(contains") ~ whitespace).with1 *> P.rep1Sep(allergen, 1, P.char(',') ~ whitespace)) <* P.char(')') 
    val food = P.rep1Sep(ingredient, 1, whitespace) ~ (whitespace *> allergens)

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