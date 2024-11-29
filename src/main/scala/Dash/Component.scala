package Dash

import javafx.scene.chart
import scalafx.collections.ObservableBuffer
import scalafx.geometry.Side
import scalafx.scene.chart.*
import scalafx.scene.control.Tooltip
import scalafx.scene.layout.Background
import scalafx.Includes.observableList2ObservableBuffer

object Component:

  /** Scatter, Bar, Pie, Card are of this trait */
  trait Compo:
    def getType: String
    def getGraph: scalafx.scene.Node
    def getTitle: String



  class Scatter(theTitle: String, theData: Seq[(String, Seq[(Double, Double)])]) extends Compo:
    private val graph = //This is the graph of this scatter object
      var datan = ObservableBuffer[javafx.scene.chart.XYChart.Series[Number, Number]]()
      def xySeriesScatter(name: String, data: Seq[(Double, Double)]) = javafx.scene.chart.XYChart.Series[Number, Number](
      name,
      ObservableBuffer.from(data.map({ case (x, y) =>
        XYChart.Data[Number, Number](x, y)
      })))
      for j <- theData do
        datan = datan :+ xySeriesScatter(j._1, j._2)
      new ScatterChart(NumberAxis("X", 100, 150, 2), NumberAxis("Y", 100, 150, 2)) {
            title = theTitle
            legendSide = Side.Right
            data = datan
          }
    for series <- graph.getData do
      for data <- series.getData do
        val tooltip = new Tooltip()
        tooltip.setText(String.format("Xasis: %2.1f - Yaxis: %2.1f", 
                                data.getXValue().doubleValue(), 
                                data.getYValue().doubleValue()))
        Tooltip.install(data.getNode, tooltip)
    def getGraph = this.graph
    def getTitle = this.theTitle
    def getData = this.theData
    def getType = "Scatter"


  
  class Pie(theTitle: String, theData: Seq[Double]) extends Compo:
    private val graph =
      var bla = Seq[String]()
        for i <- 1 until (theData.length + 1) do
          bla = bla :+ ("Day" + i.toString)
      val names =
        if theData.length == 4 then Seq("Low", "Open", "High", "Close")
        else bla
      val dataPairs = names zip theData
      new PieChart {
            title = theTitle
            clockwise = false
            data = ObservableBuffer.from(dataPairs.map({ case (x, y) =>
              PieChart.Data(x, y)
            }))
          }
    for data <- graph.getData do
      val tooltip = new Tooltip()
      tooltip.setText(data.getPieValue.toString)
      Tooltip.install(data.getNode, tooltip)
    def getGraph = this.graph
    def getTitle = this.theTitle
    def getData = this.theData
    def getType = "Pie"

  
  
  
  class Bar(theTitle: String, theData: Seq[(String, Seq[Double])]) extends Compo:
    private val graph =
      var date = Seq[String]()
      var datas = ObservableBuffer[javafx.scene.chart.XYChart.Series[String, Number]]()
      for i <- 1 until (theData.length + 1) do
        date = date :+ ("Day" + i.toString)
      def xySeriesBar(name: String, data: Seq[Double]) = {
        val series = date zip data
        XYChart.Series[String, Number](
          name,
          ObservableBuffer.from(series.map({ case (x, y) =>
            XYChart.Data[String, Number](x, y)
          }))
        )
      }
      for j <- theData do
        datas = datas :+ xySeriesBar(j._1, j._2)

      new BarChart(CategoryAxis(), NumberAxis("Y Values")) {
            title = theTitle
            data = datas
      }
    for series <- graph.getData do
      for data <- series.getData do
        val tooltip = new Tooltip()
        tooltip.setText(data.getYValue.toString)
        Tooltip.install(data.getNode, tooltip)
    def getGraph = this.graph
    def getTitle = this.theTitle
    def getData = this.theData
    def getType = "Bar"

