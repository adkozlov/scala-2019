package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.HList.HCons
import ru.spbau.jvm.scala.Peano.S

trait SplitAt[List <: HList, Idx <: Peano, Fir <: HList, Sec <: HList] {
  def apply(list: List, idx: Idx): (Fir, Sec)
}

object SplitAt {

  implicit def nil: SplitAt[Nil, _, Nil, Nil] =
    (_: Nil, _: Peano) => (HList.HNil, HList.HNil)

  implicit def consZeroIdx[List <: HList]: SplitAt[List, Z, List, Nil] =
    (list: List, _: Z) => (list, HList.HNil)

  implicit def cons[Head,
                    Tail <: HList,
                    Idx <: Peano,
                    Fir <: HList,
                    Sec <: HList](
    implicit splitAt: SplitAt[Tail, Idx, Fir, Sec]
  ): SplitAt[Cons[Head, Tail], S[Idx], Fir, Cons[Head, Sec]] =
    (list, sucIdx) => {
      val (HCons(head, tail), S(idx)) = (list, sucIdx)
      val (fir, sec) = splitAt(tail, idx)
      (fir, HCons(head, sec))
    }
}
