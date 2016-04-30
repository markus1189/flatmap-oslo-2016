package de.codecentric.github

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.`~>`
import cats.data.Coproduct
import cats.std.future._
import cats.std.list._
import cats.syntax.traverse._
import cats.syntax.cartesian._
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

trait ApplicativePrograms {
  import GitHubDsl._
  def getUsers(logins: List[UserLogin]
              ): GitHubApplicative[List[User]] =
    logins.traverseU(getUser)

  val logins: GitHubApplicative[List[User]] =
  List(UserLogin("markus1189"), UserLogin("..."), ???).
    traverseU(login => getUser(login))

  val issuesConcat: GitHubApplicative[List[Issue]] =
    (listIssues(Owner("scala"),Repo("scala-dev"))
      |@|
      listIssues(Owner("scala"),Repo("slip"))
    ).map(_++_)
}

trait Programs {
  import GitHubDsl._

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
      Await.result(p.foldMap(naturalLogging andThen step(client)), 5.minutes)
    }
  }

  def monadic[A](p: GitHubMonadic[A]): A = {
    import GitHubInterp._
    withClient { client =>
      Await.result(p.foldMap(naturalLogging andThen step(client)), 5.minutes)
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
    Webclient.monadic(allUsers(Owner("scala"), Repo("scala")))
}

object ApplicativeDsl extends ApplicativePrograms {
  def main(args: Array[String]): Unit =
    Webclient.applicative(issuesConcat)
}


object App extends Programs {

  def main(args: Array[String]): Unit = {
    val response =
      Webclient(userNamesFromIssueComments(Owner("scala"),Repo("scala"),Issue(5102)))

    println(response)
  }
}
