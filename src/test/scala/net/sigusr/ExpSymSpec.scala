package net.sigusr

class ExpSymSpec extends org.specs2.Specification {

  def is = s2"""
An ExpSym expression when interpreted with the 'Int' evaluator
   add(lit(8), neg(add(lit(1),lit(2)))) must be 5 $e1
An ExpSym expression  when interpreted with the 'String' evaluator
   add(lit(8), neg(add(lit(1),lit(2)))) must be "(8 + (-(1 + 2)))" $e2
An ExpSym expression  when interpreted with the 'Tree' evaluator
   add(lit(8), neg(add(lit(1),lit(2)))) must be… a nice tree $e3
An ExpSym expression  when interpreted with the 'PushNeg' and then the 'String' evaluators
   add(lit(8), neg(add(lit(1),lit(2)))) must be "(8 + ((-1) + (-2)))" $e4
"""

  private def e1 = {
    import ExpSymInt.expSymInt
    ExpSymSamples.tf1 must_== 5
  }

  private def e2 = {
    import ExpSymString.expSymString
    ExpSymSamples.tf1 must_== "(8 + (-(1 + 2)))"
  }

  private def e3 = {
    import ExpSymTree.expSymTree
    ExpSymSamples.tf1 must_== ExpSymSamples.tf1_tree
  }

  private def e4 = {
    import ExpSymPushNeg.expSymPushNeg
    ExpSymSamples.tf1.apply(Pos) must_== "(8 + ((-1) + (-2)))"
  }
}