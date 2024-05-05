import sttp.client3._
import sttp.client3.circe._
import io.circe.generic.auto._
import io.circe.Error
import java.io.{File, PrintWriter}
import scala.io.Source
import scala.io.StdIn.readLine
import org.jfree.chart.{ChartFactory, ChartUtils}
import org.jfree.chart.plot.PlotOrientation
import org.jfree.data.category.DefaultCategoryDataset

case class TimeSeriesData(datasetId: Int, startTime: String, endTime: String, value: Double)
case class ApiResponse(data: List[TimeSeriesData], pagination: Option[Pagination])
case class Pagination(currentPage: Int, lastPage: Int)

object Main {
  def main(args: Array[String]): Unit = {
    menuLoop() // Start the main menu loop

    def menuLoop(): Unit = {
      println("1. Monitor/Control\n" +
        "2. Collect data\n" +
        "3. View energy generation and storage\n" +
        "4. Analyse data\n" +
        "5. Detect and handle issues\n" +
        "0. Exit")
      print("Enter selection: ")
      val command = readLine()
      command match {
        case "1" =>
          PowerAPI.fetchAndDisplayData("245", "2024-05-01T00:00:00Z", "2024-05-02T00:00:00Z")
          println()
        case "2" =>
          PowerAPI.fetchAndStoreData("245", "2024-05-01T00:00:00Z", "2024-05-02T00:00:00Z")
          println()
        case "3" =>
          viewEnergyGenerationAndStorage()
        case "4" =>
        // Implementation for analyse data
        case "5" =>
          detectAndHandleIssues()
        case "0" =>
          return // Exit the menu loop
        case _ =>
          println("Unrecognized command. Please try again.")
      }
      menuLoop()
    }

  }

  def viewEnergyGenerationAndStorage(): Unit = {
    println("Viewing energy generation and storage...")
    val data = PowerAPI.readDataFromCSV("renewable_energy_data.csv")
    PowerAPI.plotData(data)
  }

  def detectAndHandleIssues(): Unit = {
    println("Detecting and handling issues based on stored data...")
    val data = PowerAPI.readDataFromCSV("renewable_energy_data.csv")
    PowerAPI.detectIssues(data)
  }
}

// 181. Wind power production - real time data (3 min) (https://data.fingrid.fi/en/datasets/181)
// 191. Hydro power production - real time data (3 min) (https://data.fingrid.fi/en/datasets/191)
// 248. Solar power generation forecast - updates every 15 minutes (https://data.fingrid.fi/en/datasets/248)
object PowerAPI {
  val apiKey = "f2ace58435cb48b593e949b5f5515c0b"
  val backend = HttpURLConnectionBackend()
  val lowOutputThreshold = 200.0  // Define threshold for low energy output detection

  def fetchData(datasets: String, startDate: String, endDate: String, currentPage: Int = 1, allData: List[TimeSeriesData] = List.empty[TimeSeriesData]): ApiResponse = {
    val baseUrl = s"https://data.fingrid.fi/api/data?datasets=$datasets&startTime=$startDate&endTime=$endDate&pageSize=10000"
    val pagedUrl = s"$baseUrl&page=$currentPage"
    val response = sendRequest(apiKey, pagedUrl, backend)
    val apiResponse = handleResponse(response)
    val newData = allData ++ apiResponse.data
    apiResponse.pagination match {
      case Some(pagination) if currentPage < pagination.lastPage =>
        fetchData(datasets, startDate, endDate, currentPage + 1, newData)
      case _ =>
        ApiResponse(newData, None)
    }
  }

  def fetchAndDisplayData(datasets: String, startDate: String, endDate: String): List[String] = {
    val data = fetchData(datasets, startDate, endDate)
    data.data.map(d => s"Dataset ID: ${d.datasetId}, Start Time: ${d.startTime}, End Time: ${d.endTime}, Value MW/h: ${d.value}")
  }

  def fetchAndStoreData(datasets: String, startDate: String, endDate: String): Unit = {
    val data = fetchData(datasets, startDate, endDate)
    val writer = new PrintWriter(new File("renewable_energy_data.csv"))
    writer.println("Dataset ID,Start Time,End Time,Value MW/h")
    data.data.foreach { d =>
      writer.println(s"${d.datasetId},${d.startTime},${d.endTime},${d.value}")
    }
    writer.close()
    println("Data has been stored to the CSV file.")
  }

  def readDataFromCSV(filePath: String): List[TimeSeriesData] = {
    val source = Source.fromFile(filePath)
    try {
      source.getLines().drop(1).map { line =>
        val Array(datasetId, startTime, endTime, value) = line.split(",").map(_.trim)
        TimeSeriesData(datasetId.toInt, startTime, endTime, value.toDouble)
      }.toList
    } finally {
      source.close()
    }
  }

  def plotData(data: List[TimeSeriesData]): Unit = {
    val dataset = new DefaultCategoryDataset()
    data.foreach { d =>
      dataset.addValue(d.value, "MW/h", d.startTime)
    }
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
    println("Energy generation chart saved as EnergyGenerationChart.jpeg")
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

  def sendRequest(apiKey: String, url: String, backend: SttpBackend[Identity, Any]): Response[Either[ResponseException[String, Error], ApiResponse]] = {
    basicRequest
      .header("x-api-key", apiKey)
      .get(uri"$url")
      .response(asJson[ApiResponse])
      .send(backend)
  }

  def handleResponse(response: Response[Either[ResponseException[String, Error], ApiResponse]]): ApiResponse = {
    response.body match {
      case Right(apiResponse) => apiResponse
      case Left(error) =>
        error match {
          case DeserializationException(body, e) =>
            println(s"Deserialization error with body: $body, error: ${e.getMessage}")
          case HttpError(body, statusCode) =>
            println(s"HTTP error $statusCode with body: $body")
          case otherError =>
            println(s"Other error: ${otherError.getMessage}")
        }
        ApiResponse(List(), None)
    }
  }
}
