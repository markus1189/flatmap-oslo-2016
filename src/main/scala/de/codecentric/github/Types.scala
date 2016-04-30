package de.codecentric.github

import akka.stream.Materializer
import play.api.libs.json.JsValue
import play.api.libs.ws.ahc.AhcWSClient
import scala.concurrent.Future

case class Issue(value: Int)

case class Url(value: String)

case class Owner(value: String)

case class UserLogin(value: String)

case class Repo(value: String)

case class Body(value: String) { override def toString = "<body not shown>" }

case class Comment(url: Url, body: Body, user: UserLogin)

case class User(login: String, name: String)

trait Endpoint[A] {
  def toUri(fa: A): String
}

object Endpoint {
  def apply[A](fa: A)(implicit E: Endpoint[A]): String = E.toUri(fa)
}

class Client(underlying: AhcWSClient) {

  // Don't do this, just for simplification purposes
  import scala.concurrent.ExecutionContext.Implicits.global

  def fetch(uri: String): Future[JsValue] = {
    underlying.url(uri).get.map(_.json)
  }

  def close(): Unit = underlying.close()
}

object Client {
  def ahcws(implicit mat: Materializer) = new Client(AhcWSClient())
}
