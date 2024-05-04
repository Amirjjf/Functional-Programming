import sttp.client3._
import sttp.client3.circe._
import io.circe.generic.auto._
import io.circe.Error
import java.io.{File, PrintWriter}
import scala.io.StdIn.readLine

case class TimeSeriesData(datasetId: Int, startTime: String, endTime: String, value: Double)
case class ApiResponse(data: List[TimeSeriesData], pagination: Option[Pagination])
case class Pagination(currentPage: Int, lastPage: Int)

object Main {
  def main(args: Array[String]): Unit = {
    var continue = true
    while (continue) {
      println("Enter command (1.monitor/control, 2.collect data, 3.view energy generation and storage, 4.analyse data, 5.detect and handle issues, 6.exit):")
      val command = readLine()
      command match {
        case "1" =>
          WindPowerForecast.fetchAndDisplayData()
        case "2" =>
          WindPowerForecast.fetchAndStoreData()
        case "3" =>
          // Implementation for view energy generation and storage
        case "4" =>
          // Implementation for analyse data
        case "5" =>
          // Implementation for detect and handle issues
        case "6" =>
          continue = false
        case _ =>
          println("Unrecognized command. Please try again.")
      }
    }
  }
}

object WindPowerForecast {
  val apiKey = "f2ace58435cb48b593e949b5f5515c0b"
  val datasets = "245"
  val startDate = "2024-04-20T00:00:00Z"
  val endDate = "2024-05-03T00:00:00Z"
  val baseUrl = s"https://data.fingrid.fi/api/data?datasets=$datasets&startTime=$startDate&endTime=$endDate&pageSize=10000"
  val backend = HttpURLConnectionBackend()

  def fetchData(): ApiResponse = {
    var currentPage = 1
    var lastPage = 1
    var allData = List.empty[TimeSeriesData]

    do {
      val pagedUrl = s"$baseUrl&page=$currentPage"
      val response = sendRequest(apiKey, pagedUrl, backend)
      val apiResponse = handleResponse(response)
      apiResponse.data.foreach(data => allData :+= data)
      apiResponse.pagination.foreach { pagination =>
        currentPage = pagination.currentPage
        lastPage = pagination.lastPage
        currentPage += 1
      }
    } while (currentPage <= lastPage)

    ApiResponse(allData, None)
  }

  def fetchAndDisplayData(): Unit = {
    val data = fetchData()
    data.data.foreach(d => println(s"Dataset ID: ${d.datasetId}, Start Time: ${d.startTime}, End Time: ${d.endTime}, Value MW/h: ${d.value}"))
  }

  def fetchAndStoreData(): Unit = {
    val data = fetchData()
    val writer = new PrintWriter(new File("renewable_energy_data.csv"))
    writer.println("Dataset ID,Start Time,End Time,Value MW/h")
    data.data.foreach { d =>
      writer.println(s"${d.datasetId},${d.startTime},${d.endTime},${d.value}")
    }
    writer.close()
    println("Data has been stored to the CSV file.")
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
