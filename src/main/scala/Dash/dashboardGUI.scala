package Dash

import javafx.event.ActionEvent
import javafx.scene.paint.Paint
import scalafx.application.JFXApp3
import scalafx.scene.Node.sfxNode2jfx
import scalafx.scene.Scene
import scalafx.scene.chart.{BarChart, CategoryAxis, NumberAxis}
import scalafx.scene.control.*
import scalafx.scene.paint.Color
import scalafx.scene.text._

import scala.collection.mutable
import scalafx.scene.layout.*
import scalafx.Includes.*
import scalafx.geometry.Orientation
import scalafx.scene.text.FontWeight
import scalafx.stage.FileChooser

object dashboardGUI extends JFXApp3:

  def start() =
    stage = new JFXApp3.PrimaryStage:
      title = "GUI test"
      width = 1500
      height = 1000



    /** This is the warning label at the bottom left of the screen that would display messages when the
     * user does something they're not supposed to */
    val warning = new Label("  Warning:"):
      font = Font("System", FontWeight.SemiBold, 14)

    val warningList = Map("space" -> "  Warning: Out of space",
      "no name" -> "  Warning: Element must have name",
      "same name" -> "  Warning: Element of the same type must have different name",
      "file error" -> "  Warning: The file you uploaded was faulty",
      "no name" -> "  Warning: This graph does not exist",
      "not enough" -> "  Warning: Fill in all of the needed field to generate your element")



  /** This is the "tiles" that we are going to add our components to, with the maximum number of 8 */
    val lowerPane = new SplitPane()
    lowerPane.orientation = Orientation.Horizontal

    val upperPane = new SplitPane()
    upperPane.orientation = Orientation.Horizontal

    val mainPane = new SplitPane()
    mainPane.orientation = Orientation.Vertical
    mainPane.getItems.addAll(upperPane, lowerPane)



    /** This is a helper method to add graphs and cards into the GUI */
    def increase(element: Component.Compo) =
      if upperPane.items.size + lowerPane.items.size < 8 then
        if upperPane.items.size < 4 then
          upperPane.getItems += element.getGraph
        else if lowerPane.items.size < 4 then
          lowerPane.getItems += element.getGraph
      else warning.text = "  Warning: Out of space"

    /** The space that the graphs/cards have taken up */
    val space = upperPane.items.size + lowerPane.items.size



    /** This is the Maps that contain all of the graphs that have been added into the GUI, 3 maps for 3
     * different types of graphs. The keys are the title of the graphs and the values are the graphs
     * themselves. They serve the purpose of keeping track and easier access when the user want to
     * delete, duplicate, hide elements.
     */
    var allBar: mutable.Map[String, Component.Bar] = mutable.Map()
    var allPie: mutable.Map[String, Component.Pie] = mutable.Map()
    var allScatter: mutable.Map[String, Component.Scatter] = mutable.Map()
    var allCard: mutable.Map[String, Cards.Card] = mutable.Map()

    /** This is the method that would add all of the graphs and cards from the predefined file into the
     * GUI and the 4 maps */
    def loadConfig(data: (mutable.Map[String, Component.Bar], mutable.Map[String, Component.Pie],
                mutable.Map[String, Component.Scatter], mutable.Map[String, Cards.Card])): Unit =
      allBar = data._1
      allPie = data._2
      allScatter = data._3
      allCard = data._4

      upperPane.items.clear()
      lowerPane.items.clear()
      allBar.foreach(a => increase(a._2))
      allPie.foreach(a => increase(a._2))
      allScatter.foreach(a => increase(a._2))
      allCard.foreach(a => increase(a._2))

    /**  The Menu, consist of 2 menu: 1 for file and 1 for action */
    val menuBar = new MenuBar
    val fileMenu = new Menu("File")

    val open = new MenuItem("Open data file")
    open.onAction = (e: ActionEvent) => {
        val fileChooser = new FileChooser
        val selectedFile = fileChooser.showOpenDialog(stage)
        val extracted = testExtractor.readFile(selectedFile)
        if extracted == testExtractor.defaultData then warning.text = warningList("file error")
        testExtractor.stock = extracted}

    val openCon = new MenuItem("Open config file") {
      onAction = (e: ActionEvent) => {
        val fileChooser = new FileChooser
        val selectedFile = fileChooser.showOpenDialog(stage)
        val extracted = Writer.readConfi(selectedFile)
        if extracted == Writer.defaultMap then warning.text = warningList("file error")
        loadConfig(extracted) }}

    val load = new MenuItem("Load")
    load.onAction = (e: ActionEvent) => {
      Writer.allBar = allBar.values.toSeq
      Writer.allPie = allPie.values.toSeq
      Writer.allScatter = allScatter.values.toSeq
      Writer.allCard = allCard.values.toSeq
      Writer.bringDataToList()}

    val save = new MenuItem("Save file")
    save.onAction = (e: ActionEvent) => {
      val fileChooser = new FileChooser
      val selectedFile = fileChooser.showSaveDialog(stage)
      Writer.writeConfi(selectedFile) }
    fileMenu.items = List(open, openCon, load, save)
    menuBar.menus = List(fileMenu)



    /** The user will type the name of the element (as keys) that they want to delete/hide/duplicate.
    * This will be how the program can access elements through the afore mentioned maps above */
    val actionEle = new TextField()
    actionEle.promptText = "Choose an element"



    /** The user choose the type of graph that they want to delete/duplicate/add. The program knows which map
     * to access through this */
    val actionAppliedType = new ComboBox(List("Bar", "Pie", "Scatter", "Card"))



    /** Delete button */
    val delete = new Button("Delete") {
      onAction = (e:ActionEvent) => {
        try
          if actionAppliedType.value.apply() == "Bar" then
            val toDel = allBar(actionEle.text.apply()).getGraph
            if upperPane.getItems.contains(toDel) then upperPane.getItems -= toDel
            else lowerPane.getItems -= toDel
            allBar -= actionEle.text.apply()
          else if actionAppliedType.value.apply() == "Pie" then
            val toDel = allPie(actionEle.text.apply()).getGraph
            if upperPane.getItems.contains(toDel) then upperPane.getItems -= toDel
            else lowerPane.getItems -= toDel
            allPie -= actionEle.text.apply()
          else if actionAppliedType.value.apply() == "Scatter" then
            val toDel = allScatter(actionEle.text.apply()).getGraph
            if upperPane.getItems.contains(toDel) then upperPane.getItems -= toDel
            else lowerPane.getItems -= toDel
            allScatter -= actionEle.text.apply()
          else if actionAppliedType.value.apply() == "Card" then
            val toDel = allCard(actionEle.text.apply()).getGraph
            if upperPane.getItems.contains(toDel) then upperPane.getItems -= toDel
            else lowerPane.getItems -= toDel
            allCard -= actionEle.text.apply()
          val typeOf = Map("Bar" -> allBar, "Pie" -> allPie, "Scatter" -> allScatter, "Card" -> allCard)
          typeOf(actionAppliedType.value.apply()) -= actionEle.text.apply()
        catch
          case e: NoSuchElementException  => warning.text = warningList("no name") }}



    /** Duplicate button */
    val duplication = new Button("Duplicate") {
      onAction = (e: ActionEvent) => {
        try
          if actionAppliedType.value.apply() == "Bar" then
            val dup = Component.Bar(allBar(actionEle.text.apply()).getTitle + " copy", allBar(actionEle.text.apply()).getData)
            if space < 8 && !allBar.contains(dup.getTitle) then
              allBar(dup.getTitle) = dup
              increase(dup)
            else if allBar.contains(dup.getTitle) then warning.text = warningList("same name")
          else if actionAppliedType.value.apply() == "Pie" then
            val dup = Component.Pie(allPie(actionEle.text.apply()).getTitle + "copy", allPie(actionEle.text.apply()).getData)
            if space < 8 && !allPie.contains(dup.getTitle) then
              allPie(dup.getTitle) = dup
              increase(dup)
            else if allPie.contains(dup.getTitle) then warning.text = warningList("same name")
          else if actionAppliedType.value.apply() == "Scatter" then
            val dup = Component.Scatter(allScatter(actionEle.text.apply()).getTitle + "copy", allScatter(actionEle.text.apply()).getData)
            if space < 8 && !allScatter.contains(dup.getTitle) then
              allScatter(dup.getTitle) = dup
              increase(dup)
            else if allScatter.contains(dup.getTitle) then warning.text = warningList("same name")
          else if actionAppliedType.value.apply() == "Card" then
            val a = allCard(actionEle.text.apply())
            val dup = Cards.Card(a.getTitle + "copy", a.getKind, a.getData)
            if space < 8 && !allCard.contains(dup.getTitle) then
              allCard(dup.getTitle) = dup
              increase(dup)
            else if allCard.contains(dup.getTitle) then warning.text = warningList("same name")
        catch
          case e: NoSuchElementException => warning.text = warningList("no name")}}



    /** Hide or unhide button. A hidden button can be unhidden by applying this to it if it's hidden */
    val changeVisibility = new Button("Change visibility") {
      onAction = (e: ActionEvent) => {
        try
          if actionAppliedType.value.apply() == "Bar" then
            val vis = allBar(actionEle.text.apply()).getGraph.visible.apply()
            allBar(actionEle.text.apply()).getGraph.setVisible(!vis)
          else if actionAppliedType.value.apply() == "Pie" then
            val vis = allPie(actionEle.text.apply()).getGraph.visible.apply()
            allPie(actionEle.text.apply()).getGraph.setVisible(!vis)
          else if actionAppliedType.value.apply() == "Scatter" then
            val vis = allScatter(actionEle.text.apply()).getGraph.visible.apply()
            allScatter(actionEle.text.apply()).getGraph.setVisible(!vis)
          else if actionAppliedType.value.apply() == "Card" then
            val vis = allCard(actionEle.text.apply()).getGraph.visible.apply()
            allCard(actionEle.text.apply()).getGraph.setVisible(!vis)
        catch
          case e: NoSuchElementException => warning.text = warningList("no name")}}



    /** This whole block of code is handles adding data points to the graph and adding the graph to the GUI */
    var chosenBar = mutable.Buffer[(String, Seq[Double])]() //contains all of the data points
    val barChoice = new ListView(List("Low", "High", "Open", "Close"))

    val addBarElement = new Button("Add bar element") {  //add the data point into the graph
      onAction = (e: ActionEvent) => {
        chosenBar.append(testExtractor.barChoose(barChoice.selectionModel.apply().getSelectedItems.toString)) }}

    val barName = new TextField()
    barName.promptText = "Name your graph"

    val addBarGraph = new Button("Add bar graph") {
      onAction = (e: ActionEvent) => {
        if allBar.contains(barName.text.apply()) then
          warning.text = warningList("same name")
        else if barName.text.apply() == "" then
          warning.text = warningList("no name")
        else if upperPane.items.size + lowerPane.items.size >= 8 then
          warning.text = warningList("space")
        else
          val bar = Component.Bar(barName.text.apply(), chosenBar.toSet.toSeq)
          increase(bar)
          allBar(bar.getTitle) = bar
          chosenBar = mutable.Buffer[(String, Seq[Double])]()
          warning.text = "  Warning:"}}



    /** Function kind of the same as the bar graph block, the user can choose data points by day */
    var chosenDayPie = Seq[Double]()
    val dayChoice = new ComboBox(List("Day 1", "Day 2", "Day 3", "Day 4", "Day 5"))
    dayChoice.onAction = (e:ActionEvent) => {
      chosenDayPie = testExtractor.pieDayChoice(dayChoice.value.apply())}

    val dayPieName = new TextField()
    dayPieName.promptText = "Name your graph"

    val addDayPieGraph = new Button("Add pie graph") {
      onAction = (e: ActionEvent) => {
        if allPie.contains(dayPieName.text.apply()) then
          warning.text = warningList("same name")
        else if dayPieName.text.apply() == "" then
          warning.text = warningList("no name")
        else if upperPane.items.size + lowerPane.items.size >= 8 then
          warning.text = warningList("space")
        else
          val pie = Component.Pie(dayPieName.text.apply(), chosenDayPie)
          increase(pie)
          allPie(pie.getTitle) = pie
          chosenDayPie = Seq[Double]()
          warning.text = "  Warning:"}}



    /** Same the as bloc above, but by type: low, high, close, open */
    var chosenTypePie = Seq[Double]()
    val TypeChoice = new ComboBox(List("Low", "High", "Close", "Open"))
    TypeChoice.onAction = (e:ActionEvent) => {
      chosenTypePie = testExtractor.pieTypeChoice(TypeChoice.value.apply())}

    val pieTypeName = new TextField()
    pieTypeName.promptText = "Name your graph"

    val addTypePieGraph = new Button("Add type pie graph") {
      onAction = (e: ActionEvent) => {
        if allPie.contains(pieTypeName.text.apply()) then
          warning.text = warningList("same name")
        else if pieTypeName.text.apply() == "" then
          warning.text = warningList("no name")
        else if upperPane.items.size + lowerPane.items.size >= 8 then
          warning.text = warningList("space")
        else
          val pie = Component.Pie(pieTypeName.text.apply(), chosenTypePie)
          increase(pie)
          allPie(pie.getTitle) = pie
          chosenTypePie = Seq[Double]()
          warning.text = "  Warning:"}}



    /** Also fucntion kind of the same as the bar graph block. The user choose to combine 2 type as the Xasis and Yaxis value
     * for a data point (doesn't make really much sense but if it works, it works) */
    var scatterP = mutable.Buffer[(String, Seq[(Double, Double)])]()
    val scatterDataName = new TextField
    val xPoint = new ComboBox(List("Low", "High", "Close", "Open"))
    val yPoint = new ComboBox(List("Low", "High", "Close", "Open"))

    val addPoint = new Button("Add a scatter graph data point") {
      onAction = (e: ActionEvent) => {
        try
          scatterP.append((scatterDataName.text.apply(),testExtractor.pairData(xPoint.value.apply(), yPoint.value.apply())))
        catch
          case e: NoSuchElementException => warning.text = warningList("not enough")} }

    val scatterName = new TextField()
    scatterName.promptText = "Name your graph"

    val addScatter = new Button("Add scatter graph") {
      onAction = (e: ActionEvent) => {
        try
          if allScatter.contains(scatterName.text.apply()) then
            warning.text = warningList("same name")
          else if scatterName.text.apply() == "" then
            warning.text = warningList("no name")
          else if upperPane.items.size + lowerPane.items.size >= 8 then
            warning.text = warningList("space")
          else
            val scatter = Component.Scatter(scatterName.text.apply(), scatterP.toSet.toSeq)
            increase(scatter)
            allScatter(scatter.getTitle) = scatter
            scatterP = mutable.Buffer[(String, Seq[(Double, Double)])]()
            warning.text = "  Warning:"
        catch
          case e: NoSuchElementException => warning.text = warningList("not enough")}}

    /** This block of code handles Cards */
    val cardType = new ComboBox(List("Min", "Max", "Avg", "Sum", "SD"))
    val cardData = new ComboBox(List("Low", "High", "Close", "Open"))

    val cardName = new TextField()
    cardName.promptText = "Name your card"

    val addCard = new Button("Add a card") {
        onAction = (e: ActionEvent) => {
          try
            if allCard.contains(cardName.text.apply()) then
              warning.text = warningList("same name")
            else if cardName.text.apply() == "" then
              warning.text = warningList("no name")
            else
              val card = Cards.Card(cardName.text.apply(), cardType.value.apply(), testExtractor.pieTypeChoice(cardData.value.apply()))
              increase(card)
              allCard(card.getTitle) = card
              warning.text = "  Warning:"
          catch
              case e: NoSuchElementException => warning.text = warningList("not enough")}}


    /** A bunch of tiltedPanes that contain the buttons, comboBox, textField and Listview for the user
     * to interact with the GUI */
    val barGenerate = new TitledPane
    barGenerate.text = "Bar graph"
    barGenerate.content = addBarElement
    barGenerate.content = new VBox:
      spacing = 10
      children = List(addBarElement, barChoice, barName, addBarGraph)

    val pieDayGenerate = new TitledPane
    pieDayGenerate.text = "Pie graph by day"
    pieDayGenerate.content = new VBox:
      spacing = 10
      children = List(dayChoice, dayPieName, addDayPieGraph)

    val pieTypeGenerate = new TitledPane
    pieTypeGenerate.text = "Pie graph by type"
    pieTypeGenerate.content = new VBox:
      spacing = 10
      children = List(TypeChoice, pieTypeName, addTypePieGraph)

    val scatterGenerate = new TitledPane
    scatterGenerate.text = "Scatter graph"
    scatterGenerate.content = new VBox:
      spacing = 10
      children = List(xPoint, yPoint, scatterDataName, addPoint, scatterName, addScatter, actionAppliedType, actionEle)

    val action = new TitledPane
    action.text = "Action"
    action.content = new VBox:
      spacing = 10
      children = List(actionAppliedType, actionEle, delete, duplication, changeVisibility)

    val cardGenerate = new TitledPane
    cardGenerate.text = "Card"
    cardGenerate.content = new VBox:
      spacing = 10
      children = List(cardType, cardData, cardName, addCard)

    val accord = new Accordion
    accord.panes = List(barGenerate, pieDayGenerate, pieTypeGenerate, scatterGenerate, cardGenerate, action)



    val bottom = new HBox()
    bottom.children = warning
    bottom.background = Background.fill(Color.LightGrey)
    bottom.prefHeight = 20

    val root = BorderPane(mainPane, menuBar, null, bottom, accord)


    stage.scene = Scene(parent = root)

  end start
end dashboardGUI


