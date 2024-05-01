import sttp.client3._
import sttp.client3.circe._
import io.circe.generic.auto._
import io.circe.Error
import java.time.ZonedDateTime
import java.time.format.DateTimeFormatter
import java.io.PrintWriter
import java.io.File

case class TimeSeriesData(datasetId: Int, startTime: String, endTime: String, value: Double)
case class ApiResponse(data: List[TimeSeriesData])

object Main extends App {
  val apiKey = "f2ace58435cb48b593e949b5f5515c0b"  // Your API key
  val datasets = "245"  // Dataset IDs you want to query
  val endDate = ZonedDateTime.now().format(DateTimeFormatter.ISO_INSTANT)
  val startDate = ZonedDateTime.now().minusDays(1).format(DateTimeFormatter.ISO_INSTANT)
  val requestUrl = s"https://data.fingrid.fi/api/datasets/$datasets/data?startTime=$startDate&endTime=$endDate" // Doesnt get data for the entire day

  val backend = HttpURLConnectionBackend()

  val response = sendRequest(apiKey, requestUrl, backend)
  handleResponse(response)

  def sendRequest(apiKey: String, url: String, backend: SttpBackend[Identity, Any]): Response[Either[ResponseException[String, Error], ApiResponse]] = {
    basicRequest
      .header("x-api-key", apiKey)
      .get(uri"$url")
      .response(asJson[ApiResponse])
      .send(backend)
  }

  def handleResponse(response: Response[Either[ResponseException[String, Error], ApiResponse]]): Unit = {
    response.body match {
      case Right(apiResponse) =>
        val writer = new PrintWriter(new File("renewable_energy_data.csv"))
        writer.write("Dataset ID,Start Time,End Time,Value MW/h\n") // Write the header
        apiResponse.data.foreach { data =>
          writer.write(s"${data.datasetId},${data.startTime},${data.endTime},${data.value}\n") // Write each line of data
        }
        writer.close()
      case Left(error) =>
        error match {
          case DeserializationException(body, e) =>
            println(s"Deserialization error with body: $body, error: ${e.getMessage}")
          case HttpError(body, statusCode) =>
            println(s"HTTP error $statusCode with body: $body")
          case otherError =>
            println(s"Other error: ${otherError.getMessage}")
        }
    }
  }
}
