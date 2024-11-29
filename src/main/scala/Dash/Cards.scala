package Dash

import scalafx.scene.Node
import scalafx.scene.control.Label
import scalafx.scene.paint.Color
import scalafx.scene.layout.{Background, VBox}
import scalafx.scene.text.{Font, FontWeight}

import scala.math.*


object Cards:

  /** These are the calculation methods */
  def minCard(data: Seq[Double]): Double = data.min
  def maxCard(data: Seq[Double]): Double = data.max
  def sumCard(data: Seq[Double]): Double = data.sum
  def avgCard(data: Seq[Double]): Double = data.sum/data.length
  def SDCard(data: Seq[Double]): Double =
    val mean = avgCard(data)
    sqrt(data.map( a => pow(abs(mean - a), 2)).sum / data.size)


  class Card(title: String, kind: String, data: Seq[Double]) extends Component.Compo:
    val result = Map("Min" -> minCard(data), "Max" -> maxCard(data),
                     "Sum" -> sumCard(data), "Avg" -> avgCard(data), "SD" -> SDCard(data))
    private val graph = new VBox:
      prefHeight = 120
      prefWidth = 230
      background = Background.fill(Color.Turquoise)
      val label = new Label(title):
        font = Font("Montserrat", FontWeight.Bold, 30)
      val cardKind = new Label(kind):
        font = Font("Montserrat", FontWeight.Bold, 30)
      val numeral = new Label(result(kind).toString):
        font = Font("Montserrat", FontWeight.Bold, 30)
      children = Array(label, cardKind, numeral)

    def getGraph = this.graph
    def getTitle = this.title
    def getType = "Card"
    def getData = this.data
    def getKind = this.kind
