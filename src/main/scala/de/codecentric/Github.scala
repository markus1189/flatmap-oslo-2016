package de.codecentric

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.`~>`
import cats.free.FreeApplicative
import cats.std.future._
import play.api.libs.functional.syntax._
import play.api.libs.json._
import play.api.libs.ws.ahc.AhcWSClient
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }

trait GithubTypes {
  case class Issue(value: Int)
  implicit val readIssue: Reads[Issue] = Json.reads[Issue]

  case class Url(value: String)
  implicit val readUrl: Reads[Url] = Json.reads[Url]

  case class Owner(value: String)
  implicit val readUserLogin: Reads[Owner] = Json.reads[Owner]
  type UserLogin = Owner
  def UserLogin(value: String): UserLogin = Owner(value)

  case class Repo(value: String)
  implicit val readRepo: Reads[Repo] = Json.reads[Repo]

  case class Body(value: String) {
    override def toString = "<body>"
  }

  case class Comment(url: Url, body: Body, user: UserLogin)
  case class User(login: Owner, name: String)
}

trait GithubDsl extends Serializable with Types with GithubTypes {
  type GitHubA[A] = FreeApplicative[GitHub, A]

  sealed trait GitHub[A]
  private case class GetComments(owner: Owner, repo: Repo, issue: Issue) extends GitHub[List[Comment]]
  private case class GetUser(login: Owner) extends GitHub[User]

  def getComments(owner: Owner, repo: Repo, issue: Issue): GitHubA[List[Comment]] =
    FreeApplicative.lift(GetComments(owner, repo, issue))

  def getUser(login: Owner): GitHubA[User] =
    FreeApplicative.lift(GetUser(login))

  object interp {
    // TODO type class?
    def toUri(fa: GitHub[_]): String = "https://api.github.com" + (fa match {
      case GetComments(Owner(owner), Repo(repo), Issue(number)) =>
        s"/repos/$owner/$repo/issues/$number/comments"
      case GetUser(Owner(login)) =>
        s"/users/$login"
      case _ => throw new IllegalArgumentException(s"No url for: $fa")
    })

    def step(client: AhcWSClient)(implicit ec: ExecutionContext): GitHub ~> Future = new (GitHub ~> Future) {
      def apply[A](fa: GitHub[A]): Future[A] = fa match {
        case GetComments(Owner(owner), Repo(repo), Issue(number)) =>
          client.url(toUri(fa)).get.map { resp =>
            val objs = resp.json.validate[List[JsValue]].get
            objs.map { obj =>
              (for {
                url <- (obj \ "url").validate[String]
                body <- (obj \ "body").validate[String]
                login <- (obj \ "user" \ "login").validate[String]
              } yield Comment(Url(url),Body(body),UserLogin(login))).get
            }
          }
      }
    }
  }
}

object App {
  object app extends GithubDsl
  import app._
  def main(args: Array[String]): Unit = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    implicit val system: ActorSystem = ActorSystem("flatmap")
    implicit val mat: ActorMaterializer = ActorMaterializer()
    val ws: AhcWSClient = AhcWSClient()

    println(Await.result(
      getComments(Owner("scala"), Repo("scala"), Issue(5102)).foldMap(interp.step(ws)),
      Duration.Inf
    ))

    ws.close()
    mat.shutdown()
    system.terminate()
  }
}
