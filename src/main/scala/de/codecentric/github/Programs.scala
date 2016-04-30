package de.codecentric.github

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.`~>`
import cats.data.Coproduct
import cats.std.future._
import cats.std.list._
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
  import GitHubDsl._
  val timeOut: FiniteDuration = 5.minutes

  def monadic[A](p: GitHubMonadic[A]): A = {
    import GitHubInterp._
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    implicit val system: ActorSystem = ActorSystem("github-run-web")
    implicit val mat: ActorMaterializer = ActorMaterializer()
    val client: Client = Client.ahcws

    try {
      Await.result(p.foldMap(naturalLogging andThen step(client)), 5.minutes)
    } finally {
      client.close()
      mat.shutdown()
      system.terminate()
    }
  }

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
    val client: Client = Client.ahcws

    try {
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

    } finally {
      client.close()
      mat.shutdown()
      system.terminate()
    }
  }
}

object App2 extends Programs {
  def main(args: Array[String]): Unit = {
    val response = Webclient.monadic(
      allUsers(Owner("scala"), Repo("scala"))
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
