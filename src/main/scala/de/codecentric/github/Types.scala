package de.codecentric.github

case class Issue(value: Int)

case class Url(value: String)

case class Owner(value: String)

case class UserLogin(value: String)

case class Repo(value: String)

case class Body(value: String) { override def toString = "<body not shown>" }

case class Comment(url: Url, body: Body, user: UserLogin)

case class User(login: String, name: String)

trait Endpoint[A] {
  def toUri(fa: A): String
}

object Endpoint {

  def apply[A](fa: A)(implicit E: Endpoint[A]): String =
    "https://api.github.com" + E.toUri(fa)
}
