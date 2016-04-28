package de.codecentric.github

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.`~>`
import cats.data.Coproduct
import cats.std.future._
import cats.std.list._
import cats.syntax.traverse._
import play.api.libs.ws.ahc.AhcWSClient
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

trait Programs {
  import GitHubDsl._
  def userNamesFromIssueComments(
    owner: Owner,
    repo: Repo,
    issue: Issue
  ): GitHubBoth[List[User]] = for {
    comments <- getCommentsM(owner, repo, issue)
    users <- embed(comments.traverseU(comment => getUser(comment.user)) )
  } yield users.flatten

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
        getUser(comment.user)).map(_.flatten))
}

object Webclient {
  import GitHubDsl._
  val timeOut: FiniteDuration = 5.minutes

  def apply[A](
    p: GitHubBoth[A],
    dur: FiniteDuration = timeOut,
    doApplicative: Boolean = false,
    doMonadic: Boolean = false,
    doOptimized: Boolean = false
  ): Unit = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    implicit val system: ActorSystem = ActorSystem("github-run-web")
    implicit val mat: ActorMaterializer = ActorMaterializer()
    val ws: AhcWSClient = AhcWSClient()

    try {
      if (doApplicative) {
        val parallel: Coproduct[GitHub,GitHubApplicative,?] ~> Future = {
          import GitHubInterp._
          step(ws).or[GitHubApplicative](stepApPar(ws))
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
          step(ws).or[GitHubApplicative](stepAp(ws))
        }
        println("©"*80)
        println("Monadic program:")
        val resM = p.foldMap(sequential)

        Await.result(resM,timeOut)
      }


      if (doOptimized) {
        val optimized: Coproduct[GitHub,GitHubApplicative,?] ~> Future = {
          import GitHubInterp._
          step(ws).or[GitHubApplicative](stepApOpt(ws))

        }

        println("©"*80)
        println("Applicative program OPTIMIZED (sleeping a moment):")
        Thread.sleep(2000)
        val resOpt = p.foldMap(optimized)

        Await.result(resOpt,timeOut)
      }

    } finally {
      ws.close()
      mat.shutdown()
      system.terminate()
    }
  }
}

object App2 extends Programs {
  def main(args: Array[String]): Unit = {
    val response = Webclient(
      userNamesFromAllIssuesComments(Owner("scala"), Repo("scala")),
      doOptimized = true
    )
    println(response)
  }
}

object App extends Programs {

  def main(args: Array[String]): Unit = {
    val response =
      Webclient(userNamesFromIssueComments(Owner("scala"),Repo("scala"),Issue(5102)))

    println(response)
  }
}
