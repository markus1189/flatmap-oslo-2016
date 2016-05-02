package de.codecentric.github

import play.api.libs.json._
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
import scala.concurrent.Future

sealed trait GitHub[A]
case class GetComments(owner: Owner, repo: Repo, issue: Issue)
    extends GitHub[List[Comment]]

case class GetUser(login: UserLogin)
    extends GitHub[User]

case class ListIssues(owner: Owner, repo: Repo)
    extends GitHub[List[Issue]]

case class GetComment(owner: Owner, repo: Repo, id: Int)
    extends GitHub[Comment]

object GitHub {
  private val ghApi = "https://api.github.com"

  implicit val commentsEndpoint: Endpoint[GetComments] = {
    new Endpoint[GetComments] {
      def toUri(gc: GetComments) = gc match {
        case GetComments(Owner(owner), Repo(repo), Issue(number)) =>
          ghApi + s"/repos/$owner/$repo/issues/$number/comments"
      }
    }
  }

  implicit val userEndpoint: Endpoint[GetUser] = {
    new Endpoint[GetUser] {
      def toUri(gu: GetUser) = gu match {
        case GetUser(UserLogin(login)) => ghApi + s"/users/$login"
      }
    }
  }

  implicit val listIssuesEndpoint: Endpoint[ListIssues] = {
    new Endpoint[ListIssues] {
      def toUri(li: ListIssues) = li match {
        case ListIssues(Owner(owner),Repo(repo)) => ghApi + s"/repos/$owner/$repo/issues"
      }
    }
  }

  implicit val commentEndpoint: Endpoint[GetComment] = {
    new Endpoint[GetComment] {
      def toUri(gc: GetComment) = gc match {
        case GetComment(Owner(owner),Repo(repo),id) => ghApi + s"/repos/$owner/$repo/issues/comments/$id"
      }
    }
  }
}

object GitHubDsl extends Serializable {
  type GitHubApplicative[A] = FreeApplicative[GitHub, A]
  type GitHubMonadic[A] = Free[GitHub, A]
  type GitHubBoth[A] = Free[Coproduct[GitHub,GitHubApplicative,?],A]

  def getCommentsMonad(owner: Owner, repo: Repo, issue: Issue): GitHubMonadic[List[Comment]] =
    Free.liftF(GetComments(owner, repo, issue))

  def getCommentsM(owner: Owner, repo: Repo, issue: Issue): GitHubBoth[List[Comment]] =
    Free.liftF[Coproduct[GitHub,GitHubApplicative,?],List[Comment]](
      Coproduct.left[GitHubApplicative](GetComments(owner, repo, issue)))

  def getComments(owner: Owner, repo: Repo, issue: Issue): GitHubApplicative[List[Comment]] =
    FreeApplicative.lift(GetComments(owner, repo, issue))

  def getUserMonad(login: UserLogin): GitHubMonadic[User] =
    Free.liftF(GetUser(login))

  def getUserM(login: UserLogin): GitHubBoth[User] =
    Free.liftF[Coproduct[GitHub,GitHubApplicative,?],User](
      Coproduct.left[GitHubApplicative](GetUser(login)))

  def getUser(login: UserLogin): GitHubApplicative[User] =
    FreeApplicative.lift(GetUser(login))

  def listIssuesMonad(owner: Owner, repo: Repo): GitHubMonadic[List[Issue]] =
    Free.liftF(ListIssues(owner,repo))

  def listIssuesM(owner: Owner, repo: Repo): GitHubBoth[List[Issue]] =
    Free.liftF[Coproduct[GitHub,GitHubApplicative,?],List[Issue]](
      Coproduct.left[GitHubApplicative](ListIssues(owner,repo)))

  def listIssues(owner: Owner, repo: Repo): GitHubApplicative[List[Issue]] =
    FreeApplicative.lift(ListIssues(owner,repo))

  def embed[A](p: GitHubApplicative[A]): GitHubBoth[A] =
    Free.liftF[Coproduct[GitHub,GitHubApplicative,?],A](Coproduct.right(p))
}

object GitHubInterp {
  import scala.concurrent.ExecutionContext.Implicits.global // don't do this
  import GitHubDsl._

  def step(client: Client): GitHub ~> Future =
    new (GitHub ~> Future) {
      def apply[A](fa: GitHub[A]): Future[A] = {
        println(fa)
        fa match {
          case ffa@GetComments(_, _, _) => client.fetch(Endpoint(ffa)).map(parseComment)
          case ffa@GetUser(_) => client.fetch(Endpoint(ffa)).map(parseUser)
          case ffa@ListIssues(_,_) => client.fetch(Endpoint(ffa)).map(parseIssue)
          case ffa@GetComment(_,_,_) => client.fetch(Endpoint(ffa)).map(parseSingleComment)
        }
      }
    }

  def stepAp(client: Client): GitHubApplicative ~> Future =
    new (GitHubApplicative ~> Future) {
      def apply[A](fa: GitHubApplicative[A]): Future[A] = fa.monad.foldMap(step(client))
    }

  def stepApPar(client: Client): GitHubApplicative ~> Future =
    new (GitHubApplicative ~> Future) {
      def apply[A](fa: GitHubApplicative[A]): Future[A] = fa.foldMap(step(client))
    }

  def stepApOpt(client: Client): GitHubApplicative ~> Future =
    new (GitHubApplicative ~> Future) {
      def apply[A](fa: GitHubApplicative[A]): Future[A] = {
        val userLogins: List[UserLogin] =
          fa.analyze(requestedLogins).toList

        val fetched: Future[List[User]] =
          userLogins.traverseU(u=>getUser(u)).foldMap(step(client))

        val futureMapping: Future[Map[UserLogin,User]] =
          fetched.map(userLogins.zip(_).toMap)

        futureMapping.flatMap { mapping =>
          fa.foldMap(prefetchedUsers(mapping)(step(client)))
        }
      }
    }

  def naturalLogging[F[_]]: F ~> F = new (F ~> F) {
    def apply[A](fa: F[A]): F[A] = {
      println(fa)
      fa
    }
  }

  val requestedLogins: GitHub ~> λ[α=>Set[UserLogin]] = {
    new (GitHub ~> λ[α=>Set[UserLogin]]) {
      def apply[A](fa: GitHub[A]): Set[UserLogin] = fa match {
        case GetUser(u) => Set(u)
        case GetComments(_,_,_) => Set.empty
        case ListIssues(_,_) => Set.empty
        case GetComment(_,_,_) => Set.empty
      }
    }
  }

  val requestedIssues: GitHub ~> λ[α=>Map[(Owner,Repo),Int]] = {
    new (GitHub ~> λ[α=>Map[(Owner,Repo),Int]]) {
      def apply[A](fa: GitHub[A]): Map[(Owner,Repo),Int] = fa match {
        case GetComment(owner,repo,id) => Map((owner,repo) -> id)
        case GetUser(_) => Map.empty
        case ListIssues(_,_) => Map.empty
        case GetComments(_,_,_) => Map.empty
      }
    }
  }

  def prefetchedUsers[F[_]:Applicative](prefetched: Map[UserLogin,User])(interp: GitHub ~> F): GitHub ~> F =
    new (GitHub ~> F) {
      def apply[A](fa: GitHub[A]): F[A] = fa match {
        case GetComments(_,_,_) => interp(fa)
        case ListIssues(_,_) => interp(fa)
        case GetComment(_,_,_) => interp(fa)
        case ffa@GetUser(login) =>
          prefetched.get(login) match {
            case Some(user) =>
              Applicative[F].pure(user)
            case None =>
              interp(ffa)
          }
      }
    }

  private def parseComment(json: JsValue): List[Comment] = {
    val objs = json.validate[List[JsValue]].get
    objs.map { obj =>
      (for {
        url <- (obj \ "url").validate[String]
        body <- (obj \ "body").validate[String]
        login <- (obj \ "user" \ "login").validate[String]
      } yield Comment(Url(url),Body(body),UserLogin(login))).get
    }
  }

  private def parseSingleComment(obj: JsValue): Comment = {
    (for {
      url <- (obj \ "url").validate[String]
      body <- (obj \ "body").validate[String]
      login <- (obj \ "user" \ "login").validate[String]
    } yield Comment(Url(url),Body(body),UserLogin(login))).get
  }

  private def parseUser(json: JsValue): User = {
    (for {
      login <- (json \ "login").validate[String]
      name <- (json \ "name").validate[String] orElse (json \ "login").validate[String]
    } yield User(login,name)).asOpt.get
  }

  private def parseIssue(json: JsValue): List[Issue] = {
    val objs = json.validate[List[JsValue]].get
    objs.map(obj => (obj \ "number").validate[Int].map(Issue(_)).asOpt).flatten
  }
}
