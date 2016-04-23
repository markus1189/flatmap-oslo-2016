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
  def userNamesFromComments(owner: String, repo: String, issue: Int) = for {
    comments <- getCommentsM(Owner(owner), Repo(repo), Issue(issue))
    users <- embed { comments.traverseU(comment => getUser(comment.user)) }
  } yield users
}

object Webclient {
  import GitHubDsl._
  def apply[A](p: GitHubBoth[A], dur: FiniteDuration = 30.seconds): A = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    implicit val system: ActorSystem = ActorSystem("github-run-web")
    implicit val mat: ActorMaterializer = ActorMaterializer()
    val ws: AhcWSClient = AhcWSClient()

    val nat: Coproduct[GitHub,GitHubApplicative,?] ~> Future = {
      import GitHubInterp._
      step(ws).or[GitHubApplicative](stepApplicativePar(ws))
    }

    try {
      val res =
        p.foldMap(GitHubInterp.logging[Coproduct[GitHub,GitHubApplicative,?]].andThen(nat))

      Await.result(res,dur)
    } finally {
      ws.close()
      mat.shutdown()
      system.terminate()
    }
  }
}

object App extends Programs {

  def main(args: Array[String]): Unit = {
    val response = Webclient(userNamesFromComments("scala","scala",5102))

    println(response)
  }
}
