import io.circe.generic.auto._
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, ChartUtils}
import org.jfree.data.category.DefaultCategoryDataset
import sttp.client3._
import sttp.client3.circe._
import java.io.{File, PrintWriter}
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.time.temporal.{ChronoUnit, TemporalAdjusters}
import java.util.concurrent.{Executors, TimeUnit}
import scala.io.{Source, StdIn}
import scala.util.Try

case class TimeSeriesData(datasetId: Int, startTime: String, endTime: String, value: Double)
case class ApiResponse(data: List[TimeSeriesData], pagination: Option[Pagination])
case class Pagination(currentPage: Int, lastPage: Int)

object Main extends App {
  def fetchDataAndStore(powerApi: PowerAPI, datasetId: String, formatter: DateTimeFormatter, now: ZonedDateTime): Try[Unit] = {
    val oneMonthAgo = now.minus(1, ChronoUnit.MONTHS)
    val dataTry = powerApi.fetchData(datasetId, formatter.format(oneMonthAgo), formatter.format(now))
    dataTry.map { data =>
      powerApi.storeData(data, s"${datasetId}Data.csv")
    }
  }

  val now = ZonedDateTime.now()
  val formatter = DateTimeFormatter.ISO_INSTANT
  val powerApi = new PowerAPI()

  // Fetch and store data for datasets
  val datasets = List("181", "191", "248")
  datasets.foreach(datasetId => fetchDataAndStore(powerApi, datasetId, formatter, now))

  // Start the thread that reads the API every minute
  val executor = Executors.newSingleThreadScheduledExecutor()
  val task = new Runnable {
    def run(): Unit = {
      val currentMinute = java.time.LocalDateTime.now().getMinute
      if (List(1, 16, 31, 46).contains(currentMinute)) {
        val now = java.time.LocalDateTime.now()
        val oneMonthAgo = now.minusMonths(1)
        val datasets = List("181", "191", "248")
        datasets.foreach { datasetId =>
          val data = powerApi.fetchData(datasetId, oneMonthAgo.toString, now.toString)
          data.foreach(d => powerApi.storeData(d, s"${datasetId}Data.csv"))
        }
      }
    }
  }
  executor.scheduleAtFixedRate(task, 0, 1, TimeUnit.MINUTES)

  menuLoop()

  @annotation.tailrec
  def menuLoop(): Unit = {
    println("1. Current status\n" +
      "2. Collect data\n" +
      "3. View energy generation and storage\n" +
      "4. Analyse data\n" +
      "5. Detect and handle issues\n" +
      "6. Monitor and Control energy sources\n" +
      "0. Exit")
    print("Enter selection: ")
    scala.io.StdIn.readLine() match {
      case "1" =>
        val datasets = Map("181" -> "Wind", "191" -> "Hydro", "248" -> "Solar")
        datasets.foreach { case (datasetId, energyType) =>
          val data = powerApi.readSecondLineFromFile(s"${datasetId}Data.csv")
          data.foreach(line => {
            val parts = line.split(",")
            val timeSeriesData = TimeSeriesData(parts(0).toInt, parts(1), parts(2), parts(3).toDouble)
            println(s"$energyType Production: ${timeSeriesData.value} MW/h at ${timeSeriesData.startTime}")
          })
        }
        println()
      case "2" =>
        datasets.foreach(datasetId => fetchDataAndStore(powerApi, datasetId, formatter, now))
        println("Data stored in renewable_energy_data.csv")
        println()
      case "3" =>
        viewEnergyGenerationAndStorage()
      case "4" =>
        println("Analyse data option selected.")
        val analyzeData = new AnalyzeData()
        val energyType = analyzeData.EnergyType()
        analyzeData.FilterData(energyType)
        println()
      case "5" =>
        detectAndHandleIssues()
      case "6" =>
        new EnergyController().controlEnergySource()
        println()
      case "0" =>
        println("Exiting...")
        System.exit(0)
      case _ =>
        println("Unrecognized command. Please try again.")
    }
    menuLoop()
  }

  def viewEnergyGenerationAndStorage(): Unit = {
    println("Viewing energy generation and storage...")
    powerApi.readDataFromCSV("renewable_energy_data.csv").foreach(powerApi.plotData)
  }

  def detectAndHandleIssues(): Unit = {
    println("Detecting and handling issues based on stored data...")
    powerApi.readDataFromCSV("renewable_energy_data.csv").map(powerApi.detectIssues)
  }
}

// 181. Wind power production - real time data (3 min) (https://data.fingrid.fi/en/datasets/181)
// 191. Hydro power production - real time data (3 min) (https://data.fingrid.fi/en/datasets/191)
// 248. Solar power generation forecast - updates every 15 minutes (https://data.fingrid.fi/en/datasets/248)
class PowerAPI {
  val apiKey: String = "f2ace58435cb48b593e949b5f5515c0b"
  val backend: SttpBackend[Identity, Any] = HttpURLConnectionBackend()

  def fetchData(datasets: String, startDate: String, endDate: String): Try[List[TimeSeriesData]] = {
    val baseUrl = s"https://data.fingrid.fi/api/data?datasets=$datasets&startTime=$startDate&endTime=$endDate&pageSize=10000"
    sendRequest(apiKey, baseUrl, backend).map(_.data)
  }

  def printData(data: List[TimeSeriesData]): Unit = {
    data.foreach(d => println(s"Dataset ID: ${d.datasetId}, Start Time: ${d.startTime}, End Time: ${d.endTime}, Value MW/h: ${d.value}"))
  }

  def storeData(data: List[TimeSeriesData], fileName: String): Unit = {
    val writer = new PrintWriter(new File(fileName))
    try {
      writer.println("Dataset ID,Start Time,End Time,Value MW/h")
      data.foreach(d => writer.println(s"${d.datasetId},${d.startTime},${d.endTime},${d.value}"))
    } finally {
      writer.close()
    }
  }

  def readDataFromCSV(filePath: String): Try[List[TimeSeriesData]] = Try {
    val source = Source.fromFile(filePath)
    try {
      source.getLines().drop(1).map { line =>
        val parts = line.split(",").map(_.trim)
        TimeSeriesData(parts(0).toInt, parts(1), parts(2), parts(3).toDouble)
      }.toList
    } finally {
      source.close()
    }
  }

  def plotData(data: List[TimeSeriesData]): Unit = {
    val dataset = new DefaultCategoryDataset()
    data.foreach(d => dataset.addValue(d.value, "MW/h", d.startTime))
    val chart = ChartFactory.createBarChart(
      "Energy Generation Over Time",
      "Start Time",
      "Value (MW/h)",
      dataset,
      PlotOrientation.VERTICAL,
      true,
      true,
      false
    )
    ChartUtils.saveChartAsJPEG(new File("EnergyGenerationChart.jpeg"), chart, 800, 600)
  }

  def detectIssues(data: List[TimeSeriesData]): Unit = {
    val issues = data.filter(_.value < lowOutputThreshold)
    if (issues.nonEmpty) {
      println(s"Detected ${issues.length} issues related to low energy output:")
      issues.foreach(issue => println(s"Dataset ID: ${issue.datasetId}, Time: ${issue.startTime} to ${issue.endTime}, Output: ${issue.value} MW/h"))
    } else {
      println("No issues detected.")
    }
  }

  def sendRequest(apiKey: String, url: String, backend: SttpBackend[Identity, Any]): Try[ApiResponse] = {
    Try {
      val response = basicRequest.header("x-api-key", apiKey).get(uri"$url").response(asJson[ApiResponse]).send(backend)
      response.body match {
        case Right(apiResponse) => apiResponse
        case Left(error) => throw new Exception(error.toString)
      }
    }
  }

  def readLastLineFromFile(fileName: String): Option[String] = {
    val source = Source.fromFile(fileName)
    try {
      val lines = source.getLines().toList
      lines.lastOption
    } catch {
      case e: Exception =>
        println(s"Error reading from file: $fileName")
        None
    } finally {
      source.close()
    }
  }

  def readFirstLineFromFile(fileName: String): Option[String] = {
    val source = Source.fromFile(fileName)
    try {
      val lines = source.getLines().toList
      lines.headOption
    } catch {
      case e: Exception =>
        println(s"Error reading from file: $fileName")
        None
    } finally {
      source.close()
    }
  }

  def readSecondLineFromFile(fileName: String): Option[String] = {
    val source = Source.fromFile(fileName)
    try {
      val lines = source.getLines().toList
      lines.lift(1)
    } catch {
      case e: Exception =>
        println(s"Error reading from file: $fileName")
        None
    } finally {
      source.close()
    }
  }

  private val lowOutputThreshold: Double = 200.0
}

// For the 1st option in the menu
class EnergyController {
  val solarPanelPosition: Int = 1   // In real application would read the position from a sensor and update properly
  val windTurbinePosition: Int = 1  // Just added to simulate how it would look like
  val hydropowerPosition: Int = 1

  def controlEnergySource(): Unit = {
    println("Please enter the energy source you wish to view (solar, wind, hydro): ")
    val energyType = scala.io.StdIn.readLine().toLowerCase
    energyType match {
      case "solar" | "wind" | "hydro" =>
        val fileName = energyType match {
          case "solar" => "248Data.csv"
          case "wind" => "181Data.csv"
          case "hydro" => "191Data.csv"
        }
        val (startTime, endTime) = ReadStartAndEndDates(fileName)
        println(s"The $energyType energy source started collecting energy on $startTime.")
        println(s"New data was last added to the $energyType energy source on $endTime.")

        val currentPosition = energyType match {
          case "solar" => solarPanelPosition
          case "wind" => windTurbinePosition
          case "hydro" => hydropowerPosition
        }
        println(s"The current position of $energyType is $currentPosition. Do you want to change the position? (yes/no)")
        scala.io.StdIn.readLine().toLowerCase match {
          case "yes" =>
            def askForNewPosition(): Unit = {
              println("Please enter the new position (1-5) or 'abort' to cancel: ")
              val newPosition = scala.io.StdIn.readLine().toLowerCase
              newPosition match {
                case "abort" => return
                case "1" | "2" | "3" | "4" | "5" =>
                  if (newPosition.toInt == currentPosition) {
                    println(s"The $energyType is already in position $newPosition. Please try again.")
                    askForNewPosition()
                  } else {
                    energyType match {
                      case "solar" => moveSolarPanel(newPosition.toInt)
                      case "wind" => moveWindTurbine(newPosition.toInt)
                      case "hydro" => moveHydropower(newPosition.toInt)
                    }
                  }
                case _ =>
                  println("Invalid position.")
                  askForNewPosition()
              }
            }
            askForNewPosition()
          case "no" => println(s"The position of $energyType remains at $currentPosition.")
          case _ =>
            println("Invalid response.")
            controlEnergySource()
        }
      case _ =>
        println("Invalid energy type. Please enter either 'solar', 'wind', or 'hydro'.")
        controlEnergySource()
    }
  }

  def ReadStartAndEndDates(fileName: String): (String, String) = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = source.getLines().drop(1).toList
    source.close()
    val firstLine = lines(0)
    val lastLine = lines.last
    val originalStartDate = lastLine.split(",")(1)
    val originalEndDate = firstLine.split(",")(1)
    val zonedStartDate = ZonedDateTime.parse(originalStartDate)
    val zonedEndDate = ZonedDateTime.parse(originalEndDate)
    val formatter = DateTimeFormatter.ofPattern("yyyy/MM/dd HH:mm:ss")
    (zonedStartDate.format(formatter), zonedEndDate.format(formatter))
  }

  def moveSolarPanel(newPosition: Int): Unit = {
    // solarPanelPosition = newPosition
    println(s"The position of the solar panel has been changed to $newPosition.")
  }

  def moveWindTurbine(newPosition: Int): Unit = {
    // windTurbinePosition = newPosition
    println(s"The position of the wind turbine has been changed to $newPosition.")
  }

  def moveHydropower(newPosition: Int): Unit = {
    // hydropowerPosition = newPosition
    println(s"The position of the hydropower has been changed to $newPosition.")
  }
}


class AnalyzeData { // Could add that user can choose to analyze data from a start and end date
  def EnergyType(): String = {
    println("Please enter the type of energy to analyze (solar, wind, hydro): ")
    val energyType = StdIn.readLine().toLowerCase

    energyType match {
      case "solar" | "wind" | "hydro" => energyType
      case _ =>
        println("Invalid energy type. Please enter either 'solar', 'wind', or 'hydro'.")
        EnergyType()
    }
  }

  def FilterData(energyType: String): Unit = {
    println("Please enter the interval for data analysis (hourly, daily, weekly, monthly): ")
    val interval = StdIn.readLine().toLowerCase

    val fileName = energyType match {
      case "solar" => "248Data.csv"
      case "wind" => "181Data.csv"
      case "hydro" => "191Data.csv"
    }

    val data = analyze(interval, fileName)
    printAnalysis(data, interval)
  }

  def analyze(interval: String, fileName: String): List[TimeSeriesData] = {
    val source = scala.io.Source.fromFile(fileName)
    try {
      val lines = source.getLines().drop(1)
      val data = lines.map { line =>
        val parts = line.split(",").map(_.trim)
        TimeSeriesData(parts(0).toInt, parts(1), parts(2), parts(3).toDouble)
      }.toList

      interval match {
        case "hourly" => aggregateHourly(data)
        case "daily" => aggregateDaily(data)
        case "weekly" => aggregateWeekly(data)
        case "monthly" => aggregateMonthly(data)
        case _ => data
      }
    } finally {
      source.close()
    }
  }

  def aggregateHourly(data: List[TimeSeriesData]): List[TimeSeriesData] = {
    val groupedData = data.groupBy(d => ZonedDateTime.parse(d.startTime).truncatedTo(ChronoUnit.HOURS))
    aggregate(groupedData)
  }

  def aggregateDaily(data: List[TimeSeriesData]): List[TimeSeriesData] = {
    val groupedData = data.groupBy(d => ZonedDateTime.parse(d.startTime).truncatedTo(ChronoUnit.DAYS))
    aggregate(groupedData)
  }

  def aggregateWeekly(data: List[TimeSeriesData]): List[TimeSeriesData] = {
    val groupedData = data.groupBy(d => ZonedDateTime.parse(d.startTime).`with`(TemporalAdjusters.previousOrSame(java.time.DayOfWeek.MONDAY)))
    aggregate(groupedData)
  }

  def aggregateMonthly(data: List[TimeSeriesData]): List[TimeSeriesData] = {
    val groupedData = data.groupBy(d => ZonedDateTime.parse(d.startTime).withDayOfMonth(1))
    aggregate(groupedData)
  }

  def aggregate(groupedData: Map[ZonedDateTime, List[TimeSeriesData]]): List[TimeSeriesData] = {
    groupedData.map { case (time, dataList) =>
      val averageValue = dataList.map(_.value).sum / dataList.size
      TimeSeriesData(dataList.head.datasetId, time.format(DateTimeFormatter.ISO_ZONED_DATE_TIME), "", averageValue)
    }.toList
  }

  def printAnalysis(data: List[TimeSeriesData], interval: String): Unit = {
    val values = data.map(_.value)
    if (values.nonEmpty) {
      val total = values.sum
      val count = values.size
      val mean = total / count

      val sortedValues = values.sorted
      val median = if (count % 2 == 0) (sortedValues(count / 2 - 1) + sortedValues(count / 2)) / 2.0 else sortedValues(count / 2)

      val mode = values.groupBy(identity).mapValues(_.size).maxBy(_._2)._1

      val range = values.max - values.min

      val midrange = (values.min + values.max) / 2

      println(s"The mean value for $interval data is $mean MW/h.")
      println(s"The median value for $interval data is $median MW/h.")
      println(s"The mode value for $interval data is $mode MW/h.")
      println(s"The range of values for $interval data is $range MW/h.")
      println(s"The midrange value for $interval data is $midrange MW/h.")
    } else {
      println("No data to analyze.")
    }
  }
}
