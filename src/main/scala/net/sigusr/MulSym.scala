package net.sigusr

trait MulSym[R] {
  def mul(r1: R, r2: R): R
}

object MulSymInt {
  implicit val mulSymInt: MulSym[Int] = (r1: Int, r2: Int) => r1 * r2
}

object MulSymString {
  implicit val mulSymString: MulSym[String] = (r1: String, r2: String) => s"($r1 * $r2)"
}

object MulSymPushNeg {
  def _instanceMulSymPushNeg[R: MulSym]: MulSym[Ctx0 => R] = new MulSym[Ctx0 => R] {
    private val m = implicitly[MulSym[R]]
    def mul(r1: Ctx0 => R, r2: Ctx0 => R): Ctx0 => R = {
      case Pos => m.mul(r1(Pos), r2(Pos))
      case Neg => m.mul(r1(Pos), r2(Neg))
    }
  }
  implicit val mulSymPushNeg: MulSym[Ctx0 => String] = {
    import MulSymString.mulSymString
    _instanceMulSymPushNeg
  }
}

object MulSymSamples {
  def tfm1[R:ExpSym: MulSym]: R = {
    val e = implicitly[ExpSym[R]]
    val m = implicitly[MulSym[R]]
    e.add(e.lit(7), e.neg(m.mul(e.lit(2), e.lit(1))))
  }

  def tfm2[R:ExpSym: MulSym]: R = {
    val e = implicitly[ExpSym[R]]
    val m = implicitly[MulSym[R]]
    m.mul(e.lit(7), ExpSymSamples.tf1)
  }
}
