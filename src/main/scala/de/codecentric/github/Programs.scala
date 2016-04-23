package de.codecentric.github

import akka.actor.ActorSystem
import akka.stream.ActorMaterializer
import cats.std.list._
import cats.std.future._
import cats.syntax.traverse._
import play.api.libs.ws.ahc.AhcWSClient
import scala.concurrent.Future
import scala.concurrent.duration.Duration
import scala.concurrent.{ Await, Future }

trait Programs {
  import GitHubDsl._
  val userNamesFromComments = for {
    comments <- getCommentsM(Owner("scala"), Repo("scala"), Issue(5102))
    users <- embed { comments.traverseU(comment => getUser(comment.user)) }
  } yield users
}

object Webclient {
  import GitHubDsl._
  def apply[A](p: GitHubBoth[A]): Future[A] = {
    implicit val ec = scala.concurrent.ExecutionContext.Implicits.global
    implicit val system: ActorSystem = ActorSystem("github-run-web")
    implicit val mat: ActorMaterializer = ActorMaterializer()
    val ws: AhcWSClient = AhcWSClient()

    try {
      p.foldMap(GitHubInterp.step(ws).or(GitHubInterp.stepApplicative(ws)))
    } finally {
      ws.close()
      mat.shutdown()
      system.terminate()
    }
  }
}

object App extends Programs {

  def main(args: Array[String]): Unit = {
    val response = Webclient(userNamesFromComments)

    println(Await.result(response, Duration.Inf))
  }
}
