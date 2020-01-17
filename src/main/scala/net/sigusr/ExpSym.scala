package net.sigusr

// Typeclass definition (equivalent to class … where … in Haskell)
trait ExpSym[R] {
  def lit(i: Int): R
  def neg(r: R): R
  def add(r1: R, r2: R): R
}

object ExpSymInt {
  implicit val expSymInt: ExpSym[Int] = new ExpSym[Int] {
    def lit(i: Int): Int = i
    def neg(r: Int): Int = -r
    def add(r1: Int, r2: Int): Int = r1 + r2
  }
}

object ExpSymString {
  implicit val expSymString: ExpSym[String] = new ExpSym[String] {
    def lit(i: Int): String = i.toString
    def neg(r: String): String = s"(-${r.toString})"
    def add(r1: String, r2: String): String = s"(${r1.toString} + ${r2.toString})"
  }
}

object ExpSymTree {
  implicit val expSymTree: ExpSym[Tree] = new ExpSym[Tree] {
    def lit(i: Int): Tree = Node("Lit", List(Leaf(i.toString)))
    def neg(r: Tree): Tree = Node("Neg", List(r))
    def add(r1: Tree, r2: Tree): Tree = Node("Add", List(r1, r2))
  }
}

object ExpSymPushNeg {
  def _instanceExpSymPushNeg[R: ExpSym]: ExpSym[Ctx0 => R] = new ExpSym[Ctx0 => R] {
    private val e = implicitly[ExpSym[R]]
    def lit(i: Int): Ctx0 => R = {
      case Pos => e.lit(i)
      case Neg => e.neg(e.lit(i))
    }
    def neg(r: Ctx0 => R): Ctx0 => R = {
      case Pos => r(Neg)
      case Neg => r(Pos)
    }
    def add(r1: Ctx0 => R, r2: Ctx0 => R): Ctx0 => R = (ctx: Ctx0) => e.add(r1(ctx), r2(ctx))
  }
  implicit val expSymPushNeg: ExpSym[Ctx0 => String] = {
    import ExpSymString.expSymString
    _instanceExpSymPushNeg
  }
}

object ExpSymFlata {
  def _instanceExpSymFlata[R: ExpSym]: ExpSym[Ctx1[R] => R] = new ExpSym[Ctx1[R] => R] {
    private val e = implicitly[ExpSym[R]]
    def lit(i: Int): Ctx1[R] => R = {
      case NonLca => e.lit(i)
      case Lca(r) => e.add(e.lit(i), r)
    }
    def neg(r: Ctx1[R] => R): Ctx1[R] => R = {
      case NonLca => e.neg(r(NonLca))
      case Lca(r1) => e.add(e.neg(r(NonLca)), r1)
    }
    def add(r1: Ctx1[R] => R, r2: Ctx1[R] => R): Ctx1[R] => R = (ctx: Ctx1[R]) => r1(Lca(r2(ctx)))
  }
  implicit val expSymFlata: ExpSym[Ctx1[String] => String] = {
    import ExpSymString.expSymString
    ExpSymFlata._instanceExpSymFlata

  }
}

object ExpSymSamples {
  def tf1[R: ExpSym]: R = {
    val e = implicitly[ExpSym[R]]
    e.add(e.lit(8), e.neg(e.add(e.lit(1), e.lit(2))))
  }
  def tf3[R: ExpSym]: R = {
    val e = implicitly[ExpSym[R]]
    e.add(tf1, e.neg(e.neg(tf1)))
  }
  val tf1_tree: Node = Node("Add", List(
    Node("Lit", List(Leaf("8"))),
    Node("Neg", List(Node("Add", List(Node("Lit", List(Leaf("1"))), Node("Lit", List(Leaf("2")))))))
  ))
}
