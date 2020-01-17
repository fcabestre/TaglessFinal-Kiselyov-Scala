package net

package object sigusr {

  sealed trait Ctx0
  final case object Pos extends Ctx0
  final case object Neg extends Ctx0

  sealed trait Tree
  final case class Leaf(value: String) extends Tree
  final case class Node(name: String, children: List[Tree]) extends Tree

  sealed trait Ctx1[+E]
  final case class Lca[E](e: E) extends Ctx1[E]
  final case object NonLca extends Ctx1[Nothing]

  def id[A](x: A): A = x
}
