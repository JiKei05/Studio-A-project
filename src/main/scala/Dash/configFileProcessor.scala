package Dash

import cats.syntax.either.*
import io.circe.*
import io.circe.generic.semiauto.*
import io.circe.jawn.decode
import upickle.default.*

import java.io.{File, FileWriter}
import scala.collection.mutable


object Writer:


  case class Quote(volume: Seq[Double], low: Seq[Double], open: Seq[Double], high: Seq[Double], close: Seq[Double])

  /** These case classes serve as place holder for the classses in Component and Cards, they serve to hold the data then extracted
   * and put in to the classes in Components and Cards */
  case class Bar(theTitle: String, theData: Seq[(String, Seq[Double])]):
    def getTitle = this.theTitle
    def getData = this.theData
    def getType = "Bar"
  case class Pie(theTitle: String, theData: Seq[Double]):
    def getTitle = this.theTitle
    def getData = this.theData
    def getType = "Pie"
  case class Scatter(theTitle: String, theData: Seq[(String, Seq[(Double, Double)])]):
    def getTitle = this.theTitle
    def getData = this.theData
    def getType = "Scatter"
  case class Card(theTitle: String, kind: String, theData: Seq[Double]):
    def getTitle = this.theTitle
    def getData = this.theData
    def getType = "Cards"
    def getKind = this.kind

  /** A predefined file is basically a Confi object, this is just a class that holds all of the dashboard graphs and cards */
  case class Confi(bar: Seq[Bar], pie: Seq[Pie], scatter: Seq[Scatter], card: Seq[Card])


  object Quote :
    implicit val reader: Reader[Quote] = macroR
    implicit val writer: Writer[Quote] = macroW

  object Pie:
    implicit val reader: Reader[Pie] = macroR
    implicit val writer: Writer[Pie] = macroW

  object Bar:
    implicit val reader: Reader[Bar] = macroR
    implicit val writer: Writer[Bar] = macroW

  object Scatter:
    implicit val reader: Reader[Scatter] = macroR
    implicit val writer: Writer[Scatter] = macroW

  object Card:
    implicit val reader: Reader[Card] = macroR
    implicit val writer: Writer[Card] = macroW

  object Confi:
    implicit val reader: Reader[Confi] = macroR
    implicit val writer: Writer[Confi] = macroW

  /** Because I can't access the maps in the GUI from here so these variables are here to be updated from dashboardGUI then be used
   * to help create the json string of the dashboard state */
  var allBar: Seq[Component.Bar] = Seq()
  var allPie: Seq[Component.Pie] = Seq()
  var allScatter: Seq[Component.Scatter] = Seq()
  var allCard: Seq[Cards.Card] = Seq()

  var writtenData: String = ""//This is the string that stores the "state" of the GUI, later to be written to a Json file
  val defaultMap = (mutable.Map[String, Component.Bar](), mutable.Map[String, Component.Pie](),
                    mutable.Map[String, Component.Scatter](), mutable.Map[String, Cards.Card]())

  /** This takes all of the data from the 4 varialbles of Seq type above and turn it into a Confi object, then turn the object
   * into a Json string that is contained by the variable writtenData */
  def bringDataToList() =
    val barList = allBar.map( a => Bar(a.getTitle, a.getData) )
    val pieList = allPie.map( a => Pie(a.getTitle, a.getData) )
    val scatterList = allScatter.map( a => Scatter(a.getTitle, a.getData) )
    val cardList = allCard.map( a => Card(a.getTitle, a.getKind, a.getData) )
    writtenData = write[Confi](Confi(barList, pieList, scatterList, cardList))

  /** Reads the predefined files and turn it into a tuple4 of all the values for the 4 maps is dashboardGUI */
  def readConfi(file: File): (mutable.Map[String, Component.Bar], mutable.Map[String, Component.Pie],
                mutable.Map[String, Component.Scatter], mutable.Map[String, Cards.Card]) =
    try
      val jsonString = ujson.read(file).toString
      val confi = read[Confi](jsonString)
      (confi.bar.map( a => (a.getTitle, Component.Bar(a.getTitle, a.getData))).to(collection.mutable.Map),
       confi.pie.map( a => (a.getTitle, Component.Pie(a.getTitle, a.getData))).to(collection.mutable.Map),
       confi.scatter.map( a => (a.getTitle, Component.Scatter(a.getTitle, a.getData))).to(collection.mutable.Map),
       confi.card.map( a => (a.getTitle, Cards.Card(a.getTitle, a.getKind, a.getData))).to(collection.mutable.Map))
    catch
      case e: Exception => defaultMap

  /** Writes the writtenData string into a new file */
  def writeConfi(file: File) =
    val fileWriter = new FileWriter(file)
    fileWriter.write(writtenData)
    fileWriter.close()









