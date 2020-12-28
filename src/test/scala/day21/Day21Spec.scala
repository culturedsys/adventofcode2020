package day21

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class Day21Spec extends AnyFlatSpec with Matchers {
    import Day21._

    "identifyAllergens" should "identify allergens for example" in {
        identifyAllergens(Seq(
            (Set(I("mxmxvkd"), I("kfcds"), I("sqjhc"), I("nhms")), Set(A("dairy"), A("fish"))),
            (Set(I("trh"), I("fvjkl"), I("sbzzf"), I("mxmxvkd")), Set(A("dairy"))),
            (Set(I("sqjhc"), I("fvjkl")), Set(A("soy"))),
            (Set(I("sqjhc"), I("mxmxvkd"), I("sbzzf")), Set(A("fish")))
        )) shouldBe Map(
            I("mxmxvkd") -> Some(A("dairy")),
            I("sqjhc") -> Some(A("fish")),
            I("fvjkl") -> Some(A("soy")),
            I("kfcds") -> None,
            I("nhms") -> None,
            I("trh") -> None,
            I("sbzzf") -> None
        )
    } 

    "parse" should "correctly parse example" in {
        parse("mxmxvkd kfcds sqjhc nhms (contains dairy, fish)") shouldBe
            Right((Set(I("mxmxvkd"), I("kfcds"), I("sqjhc"), I("nhms")), Set(A("dairy"), A("fish"))))
    }
}
