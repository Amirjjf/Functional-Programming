import io.circe.generic.auto._
import sttp.client3._
import sttp.client3.circe._

import java.io.{File, PrintWriter}
import java.text.SimpleDateFormat
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

  val initialPowerPlant = powerApi.PowerPlant(0.0)
  val updatedPowerPlant = powerApi.setMaximumStorageCapacity(initialPowerPlant, 100000.0)

  menuLoop()

  @annotation.tailrec
  def menuLoop(): Unit = {
    println("1. Current status\n" +
      "2. Control powerplant operations\n" +
      "3. Analyse data\n" +
      "0. Exit")
    print("Enter selection: ")
    scala.io.StdIn.readLine() match {
      case "1" =>
        println()
        println("Current energy production: ")
        val datasets = Map("181" -> "Wind", "191" -> "Hydro", "248" -> "Solar")
        val warningTreshhold = Map("Wind" -> 20000.0, "Hydro" -> 30000.0, "Solar" -> 500.0)
        var totalEnergyProduction = 0.0

        var solarEnergyProduction = 0.0
        var windEnergyProduction = 0.0
        var hydroEnergyProduction = 0.0

        datasets.foreach { case (datasetId, energyType) =>
          val data = powerApi.readSecondLineFromFile(s"${datasetId}Data.csv")
          data.foreach(line => {
            val parts = line.split(",")
            val timeSeriesData = TimeSeriesData(parts(0).toInt, parts(1), parts(2), parts(3).toDouble)
            var currentEnergyProduction = 0.0
            energyType match {
              case "Solar" =>
                currentEnergyProduction = timeSeriesData.value * 4
                solarEnergyProduction += currentEnergyProduction
                totalEnergyProduction += solarEnergyProduction
              case "Wind" =>
                currentEnergyProduction = timeSeriesData.value * 20
                windEnergyProduction += currentEnergyProduction
                totalEnergyProduction += windEnergyProduction
              case "Hydro" =>
                currentEnergyProduction = timeSeriesData.value * 20
                hydroEnergyProduction += currentEnergyProduction
                totalEnergyProduction += hydroEnergyProduction
            }
            println(s"$energyType Production: $currentEnergyProduction MW/h generated between ${timeSeriesData.startTime} and ${timeSeriesData.endTime}")
            if (currentEnergyProduction < warningTreshhold(energyType)) {
              println(Console.YELLOW + s"Warning: Low production value detected for $energyType" + Console.RESET)
            }
          })
        }
        println()
        val capacity = powerApi.getMaximumStorageCapacity(updatedPowerPlant)
        println(s"Total energy production: $totalEnergyProduction MWh")
        println(s"Maximum storage capacity: $capacity MWh")
        println(s"Percentage of maximum storage capacity used: ${(totalEnergyProduction / capacity) * 100}%")
        if(totalEnergyProduction > capacity) {
          println(Console.RED + "Warning: Maximum storage capacity exceeded" + Console.RESET)
        }
        println()
      case "2" =>
        new EnergyController().controlEnergySource()
        println()
      case "3" =>
        println("Analyse data option selected.")
        val analyzeData = new AnalyzeData()
        val energyType = analyzeData.readEnergyType()
        analyzeData.FilterData(energyType)
        println()
      case "0" =>
        println("Exiting...")
        System.exit(0)
      case _ =>
        println("Unrecognized command. Please try again.")
    }
    menuLoop()
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

  def storeData(data: List[TimeSeriesData], fileName: String): Unit = {
    val writer = new PrintWriter(new File(fileName))
    try {
      writer.println("Dataset ID,Start Time,End Time,Value MW/h")
      data.foreach(d => writer.println(s"${d.datasetId},${d.startTime},${d.endTime},${d.value}"))
    } finally {
      writer.close()
    }
  }

  case class PowerPlant(maximumStorageCapacity: Double)

  def setMaximumStorageCapacity(powerPlant: PowerPlant, value: Double): PowerPlant = {
    powerPlant.copy(maximumStorageCapacity = value)
  }

  def getMaximumStorageCapacity(powerPlant: PowerPlant): Double = {
    powerPlant.maximumStorageCapacity
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

object StartEndDatesFunction {
  def getStartAndEndDates(fileName: String, dateFormat: String): (String, String) = {
    val source = scala.io.Source.fromFile(fileName)
    val lines = source.getLines().drop(1).toList
    source.close()
    val firstLine = lines(0)
    val lastLine = lines.last
    val originalStartDate = lastLine.split(",")(1)
    val originalEndDate = firstLine.split(",")(1)
    val zonedStartDate = ZonedDateTime.parse(originalStartDate)
    val zonedEndDate = ZonedDateTime.parse(originalEndDate)
    val formatter = DateTimeFormatter.ofPattern(dateFormat)
    (zonedStartDate.format(formatter), zonedEndDate.format(formatter))
  }
}

class EnergyController {

  def askQuestionAndPerformAction(question: String, validResponses: Set[String], action: String => Unit): Unit = {
    println(question)
    val response = scala.io.StdIn.readLine().toLowerCase

    if (validResponses.contains(response)) {
      action(response)
    } else {
      println("Invalid response. Please try again.")
      askQuestionAndPerformAction(question, validResponses, action)
    }
  }

  def askForNewPosition(energyType: String): Unit = {
    val (question, validResponses) = energyType match {
      case "solar" => ("Please enter the new position (north, east, south, west, the sun) or 'abort' to cancel: ", Set("north", "east", "south", "west", "the sun", "abort"))
      case "wind" | "hydro" => ("Please enter the new position ('25%', '50%', '75%', '100%') or 'abort' to cancel: ", Set("25%", "50%", "75%", "100%", "abort"))
      case _ => ("Invalid energy type.", Set.empty[String]) // Ensure that this is a Set[String]
    }

    askQuestionAndPerformAction(question, validResponses, {
      case "abort" => return
      case newPosition if newPosition == getCurrentPosition(energyType) =>
        println(s"The $energyType is already at $newPosition. Please try again.")
        askForNewPosition(energyType)
      case newPosition =>
        energyType match {
          case "solar" => moveSolarPanel(newPosition)
          case "wind" => moveWindTurbine(newPosition)
          case "hydro" => moveHydroPower(newPosition)
        }
    })
  }

  def getCurrentPosition(energyType: String): String = energyType match { // On a real application this would be retrieve info from the devices
    case "solar" => "the sun"
    case "wind" => "50%"
    case "hydro" => "50%"
  }

  def AskYesOrNo(energyType: String): Unit = {
    val question = energyType match {
      case "solar" => s"The solar panel is currently facing ${getCurrentPosition(energyType)}. Do you want to change its direction? (yes/no)"
      case "wind" => s"The wind turbine is currently at ${getCurrentPosition(energyType)} pitch control. Do you want to adjust the pitch? (yes/no)"
      case "hydro" => s"The hydropower is currently at ${getCurrentPosition(energyType)} flow control. Do you want to adjust the flow? (yes/no)"
      case _ => "Invalid energy type."
    }

    askQuestionAndPerformAction(question, Set("yes", "no"), {
      case "yes" => askForNewPosition(energyType)
      case "no" => println(s"The set value of $energyType remains at ${getCurrentPosition(energyType)}.")
    })
  }

  def printEnergySourceInfo(energyType: String, startTime: String, endTime: String): Unit = {
    println(s"The $energyType energy source started collecting energy on $startTime.")
    println(s"New data was last added to the $energyType energy source on $endTime.")
  }

  def askEnergySource(): String = {
    println("Please enter the energy source you wish to view (solar, wind, hydro): ")
    val energyType = scala.io.StdIn.readLine().toLowerCase

    energyType match {
      case "solar" | "wind" | "hydro" => energyType
      case _ =>
        println("Invalid energy type. Please enter either 'solar', 'wind', or 'hydro'.")
        askEnergySource()
    }
  }

  def controlEnergySource(): Unit = {
    val energyType = askEnergySource()
    val fileName = energyType match {
      case "solar" => "248Data.csv"
      case "wind" => "181Data.csv"
      case "hydro" => "191Data.csv"
    }
    val dateFormat = "yyyy/MM/dd HH:mm:ss"
    val (startTime, endTime) = StartEndDatesFunction.getStartAndEndDates(fileName, dateFormat)
    printEnergySourceInfo(energyType, startTime, endTime)
    AskYesOrNo(energyType)
  }

  def moveSolarPanel(newPosition: String): Unit = {
    println(s"The direction of the solar panel has been updated to face $newPosition.")
    println("Handled as a simulation in the code. A real application would connect to the devices.")
  }

  def moveWindTurbine(newPosition: String): Unit = {
    println(s"The pitch control of the wind turbine has been updated to $newPosition.")
    println("Handled as a simulation in the code. A real application would connect to the devices.")
  }

  def moveHydroPower(newPosition: String): Unit = {
    println(s"The flow control of the hydropower has been updated to $newPosition.")
    println("Handled as a simulation in the code. A real application would connect to the devices.")
  }
}


class AnalyzeData {
  def readEnergyType(): String = {
    println("Please enter the type of energy to analyze (solar, wind, hydro): ")
    val energyType = StdIn.readLine().toLowerCase

    energyType match {
      case "solar" | "wind" | "hydro" => energyType
      case _ =>
        println("Invalid energy type. Please enter either 'solar', 'wind', or 'hydro'.")
        readEnergyType()
    }
  }

  def readInterval(): String = {
    println("Please enter the interval for data analysis (hourly, daily, weekly, monthly): ")
    val interval = StdIn.readLine().toLowerCase

    interval match {
      case "hourly" | "daily" | "weekly" | "monthly" => interval
      case _ =>
        println("Invalid interval. Please enter either 'hourly', 'daily', 'weekly', or 'monthly'.")
        readInterval()
    }
  }

  def readRangeChoice(): String = {
    println("Do you want to analyze the whole data set or a specific range? (whole/range)")
    val AnalyzeChoice = StdIn.readLine().toLowerCase
    AnalyzeChoice match {
      case "whole" | "range" => AnalyzeChoice
      case _ =>
        println("Invalid choice. Please enter either 'whole' or 'range'.")
        readRangeChoice()
    }
  }

  def FilterData(energyType: String): Unit = {
    val fileName = energyType match {
      case "solar" => "248Data.csv"
      case "wind" => "181Data.csv"
      case "hydro" => "191Data.csv"
    }

    val interval = readInterval()
    val rangeChoice = readRangeChoice()

    rangeChoice match {
      case "whole" =>
        val data = analyze(interval, fileName, None, None)
        printAnalysis(data, interval)
      case "range" =>
        val dateFormat = "yyyy-MM-dd"
        val (oldestEntry, newestEntry) = StartEndDatesFunction.getStartAndEndDates(fileName, dateFormat)
        println(s"The oldest entry in the data is from $oldestEntry and the newest entry is from $newestEntry")
        val (startDate, endDate) = getValidDates(oldestEntry, newestEntry)
        val data = analyze(interval, fileName, Some(startDate), Some(endDate))
        printAnalysis(data, interval)
      case _ =>
        println("Invalid choice. Please enter either 'whole' or 'range'.")
        FilterData(energyType)
    }
  }

  def getValidDates(oldestEntry: String, newestEntry: String): (String, String) = {
    val startDate = CheckDateFormat("Please enter the start date (yyyy-MM-dd): ")
    val endDate = CheckDateFormat("Please enter the end date (yyyy-MM-dd): ")
    if (isDateWithinRange(startDate, endDate, oldestEntry, newestEntry)) {
      (startDate, endDate)
    } else {
      println("The start date and end date must be within the range of the data. Please re-enter the dates.")
      getValidDates(oldestEntry, newestEntry)
    }
  }

  def isDateWithinRange(startDate: String, endDate: String, oldestEntry: String, newestEntry: String): Boolean = {
    val start = java.time.LocalDate.parse(startDate)
    val end = java.time.LocalDate.parse(endDate)
    val oldest = java.time.LocalDate.parse(oldestEntry)
    val newest = java.time.LocalDate.parse(newestEntry)
    !(start.isBefore(oldest) || end.isAfter(newest))
  }

  def CheckDateFormat(prompt: String): String = {
    try {
      println(prompt)
      val input = StdIn.readLine()
      java.time.LocalDate.parse(input)
      input
    } catch {
      case _: java.time.format.DateTimeParseException =>
        println("Invalid date format. Please enter the date in the format yyyy-MM-dd. For example, enter '2024-04-12' for April 12, 2024.")
        CheckDateFormat(prompt)
    }
  }

  def analyze(interval: String, fileName: String, startDate: Option[String], endDate: Option[String]): List[TimeSeriesData] = {
    val source = scala.io.Source.fromFile(fileName)
    try {
      val lines = source.getLines().drop(1)
      val data = lines.map { line =>
        val parts = line.split(",").map(_.trim)
        TimeSeriesData(parts(0).toInt, parts(1), parts(2), parts(3).toDouble)
      }.toList

      val filteredData = (startDate, endDate) match {
        case (Some(start), Some(end)) => data.filter(d => d.startTime >= start && d.endTime <= end)
        case _ => data
      }

      interval match {
        case "hourly" => aggregateHourly(filteredData)
        case "daily" => aggregateDaily(filteredData)
        case "weekly" => aggregateWeekly(filteredData)
        case "monthly" => aggregateMonthly(filteredData)
        case _ => filteredData
      }
    } finally {
      source.close()
    }
  }

  def aggregate(data: List[TimeSeriesData], grouper: ZonedDateTime => ZonedDateTime): List[TimeSeriesData] = {
    val groupedData = data.groupBy(d => grouper(ZonedDateTime.parse(d.startTime)))
    groupedData.map { case (time, dataList) =>
      val averageValue = dataList.map(_.value).sum / dataList.size
      TimeSeriesData(dataList.head.datasetId, time.format(DateTimeFormatter.ISO_ZONED_DATE_TIME), "", averageValue)
    }.toList
  }

  def aggregateHourly(data: List[TimeSeriesData]): List[TimeSeriesData] = {
    aggregate(data, _.truncatedTo(ChronoUnit.HOURS))
  }

  def aggregateDaily(data: List[TimeSeriesData]): List[TimeSeriesData] = {
    aggregate(data, _.truncatedTo(ChronoUnit.DAYS))
  }

  def aggregateWeekly(data: List[TimeSeriesData]): List[TimeSeriesData] = {
    aggregate(data, _.`with`(TemporalAdjusters.previousOrSame(java.time.DayOfWeek.MONDAY)))
  }

  def aggregateMonthly(data: List[TimeSeriesData]): List[TimeSeriesData] = {
    aggregate(data, _.withDayOfMonth(1))
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
