package de.codecentric.github

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats._
import cats.`~>`
import cats.data.Coproduct
import cats.std.future._
import cats.std.list._
import cats.std.set._
import cats.syntax.cartesian._
import cats.syntax.traverse._
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

trait ApplicativePrograms {
  import GitHubDsl._
  def getUsers(logins: List[UserLogin]
  ): GitHubApplicative[List[User]] =
    logins.traverseU(getUser)

  def logins: GitHubApplicative[List[User]] =
    List(UserLogin("markus1189"), UserLogin("..."), ???).
      traverseU(login => getUser(login))

  val issuesConcat: GitHubApplicative[List[Issue]] =
    (listIssues(Owner("scala"),Repo("scala-dev"))
      |@|
      listIssues(Owner("scala"),Repo("slip"))
    ).map(_++_)

  val scalaIssues: GitHubApplicative[List[Issue]] =
    List("scala","scala-dev","slip","scala-lang").
      traverseU(repo =>
        listIssues(Owner("scala"),Repo(repo))).
      map(_.flatten)

  def extractLogins(p: GitHubApplicative[_]): Set[UserLogin] = {
    import GitHubInterp._
    p.analyze(requestedLogins)
  }

  def precompute[A,F[_]:Applicative](
    p: GitHubApplicative[A],
    interp: GitHub ~> F
  ): F[Map[UserLogin,User]] = {
    val userLogins = extractLogins(p).toList

    val fetched: F[List[User]] =
      userLogins.traverseU(getUser).foldMap(interp)

    Functor[F].map(fetched)(userLogins.zip(_).toMap)
  }

  def optimizeNat[F[_]:Applicative](
    mapping: Map[UserLogin,User],
    interp: GitHub ~> F
  ): GitHub ~> F = new (GitHub ~> F) {
    def apply[A](fa: GitHub[A]): F[A] = fa match {
      case ffa@GetUser(login) =>
        mapping.get(login) match {
          case Some(user) => Applicative[F].pure(user)
          case None => interp(ffa)
        }
      case _ => interp(fa)
    }
  }

  def interpret: GitHub ~> Future = ???
  def interpretOpt[A](p: GitHubApplicative[A])(implicit ec: scala.concurrent.ExecutionContext): Future[A] = {
    val mapping: Future[Map[UserLogin,User]] = analyzing(p,interpret)

    mapping.flatMap { m =>
      val betterNat = optimizeNat(m,interpret)
      p.foldMap(betterNat)
    }
  }
}

trait Programs {
  import GitHubDsl._

  def branching = Monad[GitHubMonadic].ifM {
    listIssuesMonad(Owner("foo"),Repo("bar")).map(_.nonEmpty)
  }(listIssuesMonad(Owner("foo"),Repo("baz"))   //if true
    ,listIssuesMonad(Owner("foo"),Repo("qux"))) //if false

  def allUsers(
    owner: Owner,
    repo: Repo
  ): GitHubMonadic[List[(Issue,List[(Comment,User)])]] = for {
    issues <- listIssuesMonad(owner,repo)

    issueComments <- issues.traverseU(issue =>
      getCommentsMonad(owner,repo,issue).map((issue,_)))

    users <- issueComments.traverseU { case (issue,comments) =>
      comments.traverseU(comment =>
        getUserMonad(comment.user).map((comment,_))).map((issue,_))
    }
  } yield users

  def allUsersM(owner: Owner, repo: Repo):
      GitHubBoth[List[(Issue,List[(Comment,User)])]] = for {

    issues <- listIssuesM(owner,repo)

        issueComments <- embed {
          issues.traverseU(issue =>
            getComments(owner,repo,issue).map((issue,_)))
        }

        users <- embed {
          issueComments.traverseU { case (issue,comments) =>
            comments.traverseU(comment =>
              getUser(comment.user).map((comment,_))).map((issue,_))
          }
        }
  } yield users

  def userNamesFromIssueComments(
    owner: Owner,
    repo: Repo,
    issue: Issue
  ): GitHubBoth[List[User]] = for {
    comments <- getCommentsM(owner, repo, issue)
    users <- embed(comments.traverseU(comment => getUser(comment.user)) )
  } yield users

  def userNamesFromAllIssuesComments(
    owner: Owner,
    repo: Repo
  ): GitHubBoth[List[List[User]]] = for {
    issues <- listIssuesM(owner,repo)
    issueComments <- embed(issues.traverseU(issue => getComments(owner,repo,issue)))
    users <- embed { getUsers(issueComments) }
  } yield users

  def getUsers(issueComments: List[List[Comment]]): GitHubApplicative[List[List[User]]] =
    issueComments.traverseU(comments =>
      comments.traverseU(comment =>
        getUser(comment.user)))
}

object Webclient {
  implicit val ec = scala.concurrent.ExecutionContext.Implicits.global // don't do this
  import GitHubDsl._

  private def withClient[A](f: Client => A): A = {
    implicit val sys: ActorSystem = ActorSystem(s"github-run-${util.Random.nextInt.abs}")
    implicit val mat: ActorMaterializer = ActorMaterializer()
    val client: Client = Client.ahcws

    try {
      f(client)
    } finally {
      client.close()
      mat.shutdown()
      sys.terminate()
    }
  }

  val timeOut: FiniteDuration = 5.minutes

  def applicative[A](p: GitHubApplicative[A]): A = {
    import GitHubInterp._
    withClient { client =>
      Await.result(p.foldMap(step(client)), 5.minutes)
    }
  }

  def monadic[A](p: GitHubMonadic[A]): A = {
    import GitHubInterp._
    withClient { client =>
      Await.result(p.foldMap(step(client)), 5.minutes)
    }
  }

  def both[A](p: GitHubBoth[A]): A = {
    import GitHubInterp._
    withClient { client =>
      Await.result(p.foldMap(step(client).or[GitHubApplicative](stepApOpt(client))),
        5.minutes)
    }
  }

  def apply[A](
    p: GitHubBoth[A],
    dur: FiniteDuration = timeOut,
    doApplicative: Boolean = false,
    doMonadic: Boolean = false,
    doOptimized: Boolean = false
  ): Unit = {
    withClient { client =>

      if (doApplicative) {
        val parallel: Coproduct[GitHub,GitHubApplicative,?] ~> Future = {
          import GitHubInterp._
          step(client).or[GitHubApplicative](stepApPar(client))
        }

        println("©"*80)
        println("Applicative program (sleeping a moment):")
        Thread.sleep(2000)
        val resA = p.foldMap(parallel)

        Await.result(resA,timeOut)
      }

      if (doMonadic) {
        val sequential: Coproduct[GitHub,GitHubApplicative,?] ~> Future = {
          import GitHubInterp._
          step(client).or[GitHubApplicative](stepAp(client))
        }
        println("©"*80)
        println("Monadic program:")
        val resM = p.foldMap(sequential)

        Await.result(resM,timeOut)
      }

      if (doOptimized) {
        val optimized: Coproduct[GitHub,GitHubApplicative,?] ~> Future = {
          import GitHubInterp._
          step(client).or[GitHubApplicative](stepApOpt(client))

        }

        println("©"*80)
        println("Applicative program OPTIMIZED (sleeping a moment):")
        Thread.sleep(2000)
        val resOpt = p.foldMap(optimized)

        Await.result(resOpt,timeOut)
      }
    }
  }
}

object MonadicDsl extends Programs {
  def main(args: Array[String]): Unit =
    println(Webclient.monadic(allUsers(Owner("scala"), Repo("scala"))))
}

object ApplicativeDsl extends ApplicativePrograms {
  def main(args: Array[String]): Unit =
    println(Webclient.applicative(scalaIssues))
}

object MixedDsl extends Programs {
  def main(args: Array[String]): Unit =
    println(Webclient.both(allUsersM(Owner("scala"),Repo("scala"))))
}
