package ru.spbau.jvm.scala

import ru.spbau.jvm.scala.HList.HCons

trait Zip[Fir <: HList, Sec <: HList, Res <: HList] {
  def apply(fir: Fir, sec: Sec): Res
}

object Zip {

  implicit def nilNil: Zip[Nil, Nil, Nil] =
    (_: Nil, _: Nil) => HList.HNil

  implicit def consNil[Fir <: HList]: Zip[Fir, Nil, Nil] =
    (_: Fir, _: Nil) => HList.HNil

  implicit def nilCons[Sec <: HList]: Zip[Nil, Sec, Nil] =
    (_: Nil, _: Sec) => HList.HNil

  implicit def consCons[FHead,
                        FTail <: HList,
                        SHead,
                        STail <: HList,
                        Res <: HList](
    implicit zip: Zip[FTail, STail, Res]
  ): Zip[Cons[FHead, FTail], Cons[SHead, STail], Cons[(FHead, SHead), Res]] =
    (fir: Cons[FHead, FTail], sec: Cons[SHead, STail]) => {
      val (HCons(head1, list1), HCons(head2, list2)) = (fir, sec)
      HCons((head1, head2), zip(list1, list2))
    }
}
