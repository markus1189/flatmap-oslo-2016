package de.codecentric.github

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.`~>`
import cats.data.Coproduct
import cats.std.future._
import cats.std.list._
import cats.std.set._
import cats.syntax.traverse._
import play.api.libs.ws.ahc.AhcWSClient
import scala.concurrent.Future
import scala.concurrent.duration.FiniteDuration
import scala.concurrent.duration._
import scala.concurrent.{ Await, Future }

trait Programs {
  import GitHubDsl._
  def userNamesFromIssueComments(
    owner: Owner, repo: Repo, issue: Issue): GitHubBoth[List[User]] = for {
    comments <- getCommentsM(owner, repo, issue)
      users <- embed { comments.traverseU(comment => getUser(comment.user)) }
  } yield users.collect { case Some(u) => u }
}

object Webclient {
  import GitHubDsl._
  val timeOut: FiniteDuration = 30.seconds
  def apply[A](p: GitHubApplicative[A]): A = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    implicit val system: ActorSystem = ActorSystem("github-run-web-ap")
    implicit val mat: ActorMaterializer = ActorMaterializer()
    val ws: AhcWSClient = AhcWSClient()

    try {
      println("©"*80)
      println("Applicative program with OPTIMIZATION (sleeping a moment):")
      Thread.sleep(2000)
      val userLogins: List[UserLogin] = p.analyze(GitHubInterp.requestedUserNames).toList
      val fetchedP: GitHubApplicative[List[Option[User]]] =
        userLogins.traverseU(u=>getUser(u))

      val fetched: Future[List[Option[User]]] = fetchedP.foldMap(GitHubInterp.step(ws))

      val futureMapping: Future[Map[UserLogin,User]] =
        fetched.map(userLogins.zip(_).collect { case (l,Some(x)) => (l,x) }.toMap)

      val resOpt = futureMapping.flatMap { mapping =>
        p.foldMap(GitHubInterp.prefetchedUsers(mapping)(GitHubInterp.step(ws)))
      }

      Await.result(resOpt,timeOut)
    } finally {
      ws.close()
      mat.shutdown()
      system.terminate()
    }
  }

  def apply[A](p: GitHubBoth[A], dur: FiniteDuration = 30.seconds): A = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    implicit val system: ActorSystem = ActorSystem("github-run-web")
    implicit val mat: ActorMaterializer = ActorMaterializer()
    val ws: AhcWSClient = AhcWSClient()

    val parallel: Coproduct[GitHub,GitHubApplicative,?] ~> Future = {
      import GitHubInterp._
      step(ws).or[GitHubApplicative](stepApPar(ws))
    }

    val sequential: Coproduct[GitHub,GitHubApplicative,?] ~> Future = {
      import GitHubInterp._
      step(ws).or[GitHubApplicative](stepAp(ws))
    }

    try {
      println("©"*80)
      println("Monadic program:")
      val resM = p.foldMap(sequential)

      Await.result(resM,timeOut)

      println("©"*80)
      println("Applicative program (sleeping a moment):")
      Thread.sleep(2000)
      val resA =
        p.foldMap(parallel)

      Await.result(resA,timeOut)

    } finally {
      ws.close()
      mat.shutdown()
      system.terminate()
    }
  }
}

object App extends Programs {

  def main(args: Array[String]): Unit = {
    val response =
      Webclient(userNamesFromIssueComments(Owner("scala"),Repo("scala"),Issue(5102)))

    println(response)
  }
}
