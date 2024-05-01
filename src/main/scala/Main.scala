import sttp.client3._
import sttp.client3.circe._
import io.circe.generic.auto._
import io.circe.Error

case class TimeSeriesData(datasetId: Int, startTime: String, endTime: String, value: Double)
case class ApiResponse(data: List[TimeSeriesData])

object WindPowerForecast {
  def main(args: Array[String]): Unit = {
    val apiKey = "f2ace58435cb48b593e949b5f5515c0b"  // Your API key
    val datasets = "245,247"  // Dataset IDs you want to query
    val startDate = "2024-05-02T00:00:00Z"
    val endDate = "2024-05-03T00:00:00Z"
    val requestUrl = s"https://data.fingrid.fi/api/data?datasets=$datasets&startTime=$startDate&endTime=$endDate"

    val backend = HttpURLConnectionBackend()

    val response = sendRequest(apiKey, requestUrl, backend)
    handleResponse(response)
  }

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
        apiResponse.data.foreach(data => println(s"Dataset ID: ${data.datasetId}, Start Time: ${data.startTime}, End Time: ${data.endTime}, Value: ${data.value} MW/h"))
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
