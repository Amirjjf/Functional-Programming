import io.circe.generic.auto._
import org.jfree.chart.plot.PlotOrientation
import org.jfree.chart.{ChartFactory, ChartUtils}
import org.jfree.data.category.DefaultCategoryDataset
import sttp.client3._
import sttp.client3.circe._
import java.io.{File, PrintWriter}
import scala.io.Source
import scala.util.Try

case class TimeSeriesData(datasetId: Int, startTime: String, endTime: String, value: Double)
case class ApiResponse(data: List[TimeSeriesData], pagination: Option[Pagination])
case class Pagination(currentPage: Int, lastPage: Int)

object Main extends App {
  val powerApi = new PowerAPI()
  menuLoop()

  @annotation.tailrec
  def menuLoop(): Unit = {
    println("1. Monitor/Control\n" +
      "2. Collect data\n" +
      "3. View energy generation and storage\n" +
      "4. Analyse data\n" +
      "5. Detect and handle issues\n" +
      "0. Exit")
    print("Enter selection: ")
    scala.io.StdIn.readLine() match {
      case "1" =>
        val data = powerApi.fetchData("245", "2024-05-01T00:00:00Z", "2024-05-02T00:00:00Z")
        data.foreach(powerApi.printData)
        println()
      case "2" =>
        val data = powerApi.fetchData("245", "2024-05-01T00:00:00Z", "2024-05-02T00:00:00Z")
        data.foreach(powerApi.printData)
        data.foreach(d => powerApi.storeData(d, "renewable_energy_data2.csv"))
        println()
      case "3" =>
        viewEnergyGenerationAndStorage()
      case "4" =>
        println("Analyse data option selected.")
      case "5" =>
        detectAndHandleIssues()
      case "0" =>
        println("Exiting...")
        return
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

  private val lowOutputThreshold: Double = 200.0
}
