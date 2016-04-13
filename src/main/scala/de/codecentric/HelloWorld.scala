package de.codecentric

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import com.typesafe.config.ConfigFactory
import play.api.libs.json.{ Json, Format }
import play.api.libs.ws.{ WSAuthScheme, WSResponse }
import play.api.libs.ws.ahc.AhcWSClient
import scala.concurrent.{ Await, Future }
import scala.concurrent.duration.Duration

case class Token(value: String) extends AnyVal

case class UserInfo(fullname: String, timezone: String)

object UserInfo {
  implicit val userFormat: Format[UserInfo] = Json.format[UserInfo]
}

object HelloWorld {
  val config = ConfigFactory.load()
  val token = Token(config.getString("client.token"))

  def requestInfo(ws: AhcWSClient)(token: Token): Future[WSResponse] =
    ws.url("https://www.toggl.com/api/v8/me").
      withAuth(token.value,"api_token",WSAuthScheme.BASIC).
      get

  def main(args: Array[String]): Unit = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    implicit val system: ActorSystem = ActorSystem("flatmap")
    implicit val mat: ActorMaterializer = ActorMaterializer()
    val ws: AhcWSClient = AhcWSClient()

    try {

      val result = requestInfo(ws)(token).map { response =>
        println((response.json \ "data").validate[UserInfo])
      }

      Await.result(result, Duration.Inf)
    } finally {
      /*
       https://playframework.com/documentation/2.5.x/ScalaWS

       However, if you choose, you can instantiate a WSClient directly
       from code and use this for making requests or for configuring
       underlying AsyncHttpClient options. If you create a WSClient
       manually then you must call client.close() to clean it up when
       you’ve finished with it. Each client creates its own thread
       pool. If you fail to close the client or if you create too many
       clients then you will run out of threads or file handles -—
       you’ll get errors like “Unable to create new native thread” or
       “too many open files” as the underlying resources are consumed.
       */
      ws.close()
      mat.shutdown()
      system.terminate()
    }
  }
}
