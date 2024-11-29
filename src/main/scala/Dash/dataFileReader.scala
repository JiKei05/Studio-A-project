package Dash

import cats.syntax.either.*
import io.circe.*
import io.circe.generic.auto.*

import java.io.File


object testExtractor:

  /** An input data file with the correct format is basically a Quote object */
  case class Quote(volume: Seq[Double], low: Seq[Double], open: Seq[Double], high: Seq[Double], close: Seq[Double])


  val defaultData = Quote(Seq(0, 0, 0, 0, 0), Seq(0, 0, 0, 0, 0), Seq(0, 0, 0, 0, 0), Seq(0, 0, 0, 0, 0), Seq(0, 0, 0, 0, 0))//When you start the program or uplad a faulty file, the program default to this
  var stock: Quote = defaultData //This is the variable that stores all the data


  /** This method handles file reading */
  def readFile(file: File): Quote =
    try
      val jsonString: String = ujson.read(file).toString
      val json = parser.parse(jsonString)
      val data = json
        .leftMap(err => err: Error)
        .flatMap(_.as[Quote])
      data match
        case Left(error) => throw Exception("fail to read")
        case Right(right) =>
          if right.open.size != 5 || right.close.size != 5 || right.high.size != 5 || right.low.size != 5 then
            defaultData
          else right
    catch
      case e: ujson.ParseException => defaultData
      case e: Exception => defaultData


  /** Each of the method here returns data for each type of graphs */
  def barChoose: Map[String, (String, Seq[Double])] =//for bar graphs
    Map( "[High]" -> ("High", stock.high), "[Low]" -> ("Low", stock.low),
         "[Open]" -> ("Open", stock.open), "[Close]" -> ("Close", stock.close))

  def pieDayChoice: Map[String, Seq[Double]] =//for pie graph by day
    Map("Day 1" -> Seq(stock.low(0), stock.open(0), stock.high(0), stock.close(0)),
        "Day 2" -> Seq(stock.low(1), stock.open(1), stock.high(1), stock.close(1)),
        "Day 3" -> Seq(stock.low(2), stock.open(2), stock.high(2), stock.close(2)),
        "Day 4" -> Seq(stock.low(3), stock.open(3), stock.high(3), stock.close(3)),
        "Day 5" -> Seq(stock.low(4), stock.open(4), stock.high(4), stock.close(4)))

  def pieTypeChoice: Map[String, Seq[Double]] =//for pie graph by type and cards
   Map("Low" -> stock.low.toSeq,
       "High" -> stock.high.toSeq,
       "Open" -> stock.open.toSeq,
       "Close" -> stock.close.toSeq)

  def pairData(xAxis: String, yAxis: String): Seq[(Double, Double)] =//for scatter graph
    val ex: Map[String, Seq[Double]] =
      Map("Low" -> stock.low.toSeq,
          "High" -> stock.high.toSeq,
          "Open" -> stock.open.toSeq,
          "Close" -> stock.close.toSeq)
    ex(xAxis) zip ex(yAxis)

