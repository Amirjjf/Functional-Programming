import sttp.client3._
import sttp.client3.circe._
import io.circe.generic.auto._
import io.circe.Error
import java.io.PrintWriter
import java.io.File
import scala.io.StdIn.readLine

case class TimeSeriesData(datasetId: Int, startTime: String, endTime: String, value: Double)
case class ApiResponse(data: List[TimeSeriesData])

object Main {
  def main(args: Array[String]): Unit = {
    var continue = true
    while (continue) {
      println("Enter command (1.monitor/control, 2.collect data, 3.view energy generation and storage, 4.analyse data, 5.detect and handle issues, 6.exit):")
      val command = readLine()
      command match {
        case "1" =>
        // Code
        case "2" =>
          WindPowerForecast.main(Array.empty)
        case "3" =>
        // Code
        case "4" =>
        // Code
        case "5" =>
        // Code
        case "6" =>
          continue = false
        case _ =>
          println("Unrecognized command. Please try again.")
      }
    }
  }
}


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
        val writer = new PrintWriter(new File("renewable_energy_data.csv"))
        writer.write("Dataset ID,Start Time,End Time,Value MW/h\n") // Write the header
        writer.flush() // Flush the writer
        apiResponse.data.foreach { data =>
          writer.write(s"${data.datasetId},${data.startTime},${data.endTime},${data.value}\n") // Write each line of data
          writer.flush() // Flush the writer
        }
        writer.close()
        println("Data has been added to the CSV file.") // csv file appears after closing the program in intellij
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
