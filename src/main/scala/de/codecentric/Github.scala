package de.codecentric

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.`~>`
import cats.data.Coproduct
import cats.free.Free
import cats.free.FreeApplicative
import cats.std.list._
import cats.std.future._
import cats.syntax.traverse._
import play.api.libs.json._
import play.api.libs.ws.ahc.AhcWSClient
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }

case class Issue(value: Int)
case class Url(value: String)
case class Owner(value: String)
case class UserLogin(value: String)
case class Repo(value: String)
case class Body(value: String) {
  override def toString = "<body>"
}

case class Comment(url: Url, body: Body, user: UserLogin)
case class User(login: String, name: String)

object GitHubDsl extends Serializable {
  type GitHubApplicative[A] = FreeApplicative[GitHub, A]
  type GitHubMonadic[A] = Free[GitHub, A]
  type GitHubBoth[A] = Free[Coproduct[GitHub,GitHubApplicative,?],A]

  sealed trait GitHub[A]
  case class GetComments(owner: Owner, repo: Repo, issue: Issue) extends GitHub[List[Comment]]
  case class GetUser(login: UserLogin) extends GitHub[Option[User]]

  def getCommentsM(owner: Owner, repo: Repo, issue: Issue): GitHubBoth[List[Comment]] =
    Free.liftF[Coproduct[GitHub,GitHubApplicative,?],List[Comment]](
      Coproduct.left[GitHubApplicative](GetComments(owner, repo, issue)))

  def getComments(owner: Owner, repo: Repo, issue: Issue): GitHubApplicative[List[Comment]] =
    FreeApplicative.lift(GetComments(owner, repo, issue))

  def getUserM(login: UserLogin): GitHubBoth[Option[User]] =
    Free.liftF[Coproduct[GitHub,GitHubApplicative,?],Option[User]](
      Coproduct.left[GitHubApplicative](GetUser(login)))

  def getUser(login: UserLogin): GitHubApplicative[Option[User]] =
    FreeApplicative.lift(GetUser(login))

  def embed[A](p: GitHubApplicative[A]): GitHubBoth[A] =
    Free.liftF[Coproduct[GitHub,GitHubApplicative,?],A](Coproduct.right(p))
}

object GitHubInterp {
  import GitHubDsl._

  // TODO type class?
  def toUri(fa: GitHub[_]): String = "https://api.github.com" + (fa match {
    case GetComments(Owner(owner), Repo(repo), Issue(number)) =>
      s"/repos/$owner/$repo/issues/$number/comments"
    case GetUser(UserLogin(login)) =>
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

      case GetUser(UserLogin(owner)) =>
        client.url(toUri(fa)).get.map { resp =>
          val obj = resp.json

          (for {
            login <- (obj \ "login").validate[String]
            name <- (obj \ "name").validate[String]
          } yield User(login,name)).asOpt
        }
    }
  }

  def stepApplicative(client: AhcWSClient)(implicit ec: ExecutionContext): GitHubApplicative ~> Future =
    new (GitHubApplicative ~> Future) {
      def apply[A](fa: GitHubApplicative[A]): Future[A] = fa.monad.foldMap(step(client)(implicitly))
    }
}

object App {
  import GitHubDsl._

  def main(args: Array[String]): Unit = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    implicit val system: ActorSystem = ActorSystem("flatmap")
    implicit val mat: ActorMaterializer = ActorMaterializer()
    val ws: AhcWSClient = AhcWSClient()

    val program = for {
      comments <- getCommentsM(Owner("scala"), Repo("scala"), Issue(5102))
      users <- embed { comments.traverseU(comment => getUser(comment.user)) }
    } yield users

    println(Await.result(
      program.foldMap(GitHubInterp.step(ws).or(GitHubInterp.stepApplicative(ws))),
      Duration.Inf
    ))

    ws.close()
    mat.shutdown()
    system.terminate()
  }
}
