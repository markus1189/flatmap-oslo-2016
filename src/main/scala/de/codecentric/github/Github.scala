package de.codecentric.github

import cats.Applicative
import cats.`~>`
import cats.data.Coproduct
import cats.free.Free
import cats.free.FreeApplicative
import cats.std.future._
import cats.std.list._
import cats.std.set._
import cats.syntax.traverse._
import play.api.libs.json._
import play.api.libs.ws.ahc.AhcWSClient
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

sealed trait GitHub[A]
case class GetComments(owner: Owner, repo: Repo, issue: Issue)
    extends GitHub[List[Comment]]

case class GetUser(login: UserLogin) extends GitHub[Option[User]]

object GitHub {
  private val ghApi = "https://api.github.com"

  implicit def commentsEndpoint: Endpoint[GetComments] = {
    new Endpoint[GetComments] {
      def toUri(gc: GetComments) = gc match {
        case GetComments(Owner(owner), Repo(repo), Issue(number)) =>
          ghApi + s"/repos/$owner/$repo/issues/$number/comments"
      }
    }
  }

  implicit def userEndpoint: Endpoint[GetUser] = {
    new Endpoint[GetUser] {
      def toUri(gu: GetUser) = gu match {
        case GetUser(UserLogin(login)) => ghApi + s"/users/$login"
      }
    }
  }
}

object GitHubDsl extends Serializable {
  type GitHubApplicative[A] = FreeApplicative[GitHub, A]
  type GitHubMonadic[A] = Free[GitHub, A]
  type GitHubBoth[A] = Free[Coproduct[GitHub,GitHubApplicative,?],A]

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

  def step(client: AhcWSClient)(implicit ec: ExecutionContext): GitHub ~> Future = new (GitHub ~> Future) {
    def apply[A](fa: GitHub[A]): Future[A] = fa match {
      case ffa@GetComments(Owner(owner), Repo(repo), Issue(number)) =>
        println("Getting: " + Endpoint(ffa))
        client.url(Endpoint(ffa)).get.map { resp =>
          val objs = resp.json.validate[List[JsValue]].get
          objs.map { obj =>
            (for {
              url <- (obj \ "url").validate[String]
              body <- (obj \ "body").validate[String]
              login <- (obj \ "user" \ "login").validate[String]
            } yield Comment(Url(url),Body(body),UserLogin(login))).get
          }
        }

      case ffa@GetUser(UserLogin(owner)) =>
        println("Getting: " + Endpoint(ffa))
        client.url(Endpoint(ffa)).get.map { resp =>
          val obj = resp.json

          (for {
            login <- (obj \ "login").validate[String]
            name <- (obj \ "name").validate[String]
          } yield User(login,name)).asOpt
        }
    }
  }

  def stepAp(client: AhcWSClient)(implicit ec: ExecutionContext): GitHubApplicative ~> Future =
    new (GitHubApplicative ~> Future) {
      def apply[A](fa: GitHubApplicative[A]): Future[A] = fa.monad.foldMap(step(client)(implicitly))
    }

  def stepApPar(client: AhcWSClient)(implicit ec: ExecutionContext): GitHubApplicative ~> Future =
    new (GitHubApplicative ~> Future) {
      def apply[A](fa: GitHubApplicative[A]): Future[A] = fa.foldMap(step(client)(implicitly))
    }

  def stepApOpt(client: AhcWSClient)(implicit ec: ExecutionContext): GitHubApplicative ~> Future =
    new (GitHubApplicative ~> Future) {
      def apply[A](fa: GitHubApplicative[A]): Future[A] = {
        val userLogins: List[UserLogin] =
          fa.analyze(requestedUserNames).toList

        val fetchedP: GitHubApplicative[List[Option[User]]] =
          userLogins.traverseU(u=>getUser(u))

        val fetched: Future[List[Option[User]]] = fetchedP.foldMap(step(client))

        val futureMapping: Future[Map[UserLogin,User]] =
          fetched.map(userLogins.zip(_).collect { case (l,Some(x)) => (l,x) }.toMap)

        futureMapping.flatMap { mapping =>
          fa.foldMap(prefetchedUsers(mapping)(step(client)))
        }
      }
    }

  def naturalLogging[F[_]]: F ~> F = new (F ~> F) {
    def apply[A](fa: F[A]): F[A] = {
      println("*"*80)
      println(fa)
      println("*"*80)
      fa
    }
  }

  val requestedUserNames: GitHub ~> λ[α=>Set[UserLogin]] = {
    new (GitHub ~> λ[α=>Set[UserLogin]]) {
      def apply[A](fa: GitHub[A]): Set[UserLogin] = fa match {
        case GetComments(_,_,_) => Set.empty
        case GetUser(u) => Set(u)
      }
    }
  }

  def prefetchedUsers[F[_]:Applicative](prefetched: Map[UserLogin,User])(interp: GitHub ~> F): GitHub ~> F =
    new (GitHub ~> F) {
      def apply[A](fa: GitHub[A]): F[A] = fa match {
        case GetComments(_,_,_) => interp(fa)
        case ffa@GetUser(login) =>
          prefetched.get(login) match {
            case Some(user) => Applicative[F].pure(Some(user))
            case None => interp(ffa)
          }
      }
    }
}
