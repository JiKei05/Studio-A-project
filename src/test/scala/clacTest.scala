
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._
import Dash.Cards

class clacTest extends AnyFlatSpec with Matchers:

  val numbers = Seq(124.4, 136.76, 149, 183.55, 106, 155)
  val maxNumber = 183.55
  val minNumber = 106
  val sumNumber = 124.4 + 136.76 + 149 + 183.55 + 106 + 155
  val avgNumber: Double = (124.4 + 136.76 + 149 + 183.55 + 106 + 155) / 6
  val sdNumber: Double = 24.414463211155986


  "Max calculations" should "produce the matching results" in {
    Cards.maxCard(numbers) shouldEqual(maxNumber)
  }

  "Min calculations" should "produce the matching results" in {
    Cards.minCard(numbers) shouldEqual(minNumber)
  }

  "Avg calculations" should "produce the matching results" in {
    Cards.avgCard(numbers) shouldEqual(avgNumber)
  }

  "Sum calculations" should "produce the matching results" in {
    Cards.sumCard(numbers) shouldEqual(sumNumber)
  }

  "SD calculations" should "produce the matching results" in {
    Cards.SDCard(numbers) shouldEqual(sdNumber)
  }
