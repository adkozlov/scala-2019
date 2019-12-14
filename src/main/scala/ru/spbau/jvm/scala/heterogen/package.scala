package ru.spbau.jvm.scala

package object heterogen {

  type ::[Head, Tail <: HList] = HCons[Head, Tail]
  type Nil = HNil.type
  type Zero = Zero.type

  trait SplitAt[
    List <: HList,
    Index <: Nat,
    Result <: (HList, HList)
  ] {
    def apply(list: List, index: Index): Result
  }

  object SplitAt {

    // splitAt 0 xs = ([], xs)
    implicit def splitAtZero[
      List <: HList,
      Index <: Zero,
    ]: SplitAt[List, Index, (HNil.type, List)] =
      (list, _) => (HNil, list)

    // splitAt (Suc n) (x :: xs) = let (ys, zs) = splitAt(n) in (x :: ys, xs)
    implicit def splitAtSuc[
      Head,
      List <: HList,
      ListLeft <: HList,
      ListRight <: HList,
      Index <: Nat
    ](implicit splitter: SplitAt[List, Index, (ListLeft, ListRight)])
    : SplitAt[Head :: List, Suc[Index], (Head :: ListLeft, ListRight)] =
      (list, idx) => {
        val HCons(head, tail) = list
        val (left, right) = splitter(tail, idx.pred)
        (head :: left, right)
      }
  }

  trait Zip[
    ListLeft <: HList,
    ListRight <: HList,
    Result <: HList
  ] {
    def apply(listLeft: ListLeft, listRight: ListRight): Result
  }

  object Zip {
    // zip [] [] => []
    implicit def zipNilNil: Zip[Nil, Nil, Nil] = (_, _) => HNil

    // zip [] (x::xs) => []
    implicit def zipNilCons[Head, List <: HList]: Zip[Nil, HCons[Head, List], Nil] = (_, _) => HNil

    // zip (x::xs) [] => []
    implicit def zipConsNil[Head, List <: HList]: Zip[HCons[Head, List], Nil, Nil] = (_, _) => HNil

    // zip (x::xs) (y::ys) = (x, y) :: zip xs ys
    implicit def zipCons[
      HeadLeft,
      HeadRight,
      List <: HList,
      ListLeft <: HList,
      ListRight <: HList,
    ](implicit zipper: Zip[ListLeft, ListRight, List])
    : Zip[HeadLeft :: ListLeft, HeadRight :: ListRight, (HeadLeft, HeadRight) :: List] =
      (left, right) => {
        val HCons(leftHead, leftTail) = left
        val HCons(rightHead, rightTail) = right
        (leftHead, rightHead) :: zipper(leftTail, rightTail)
      }
  }

  implicit class HListExt[List <: HList](private val list: List) extends AnyVal {
    def ::[Head](head: Head): HCons[Head, List] = HCons(head, list)

    def zip[
      WithList <: HList,
      Result <: HList
    ](withList: WithList)
     (implicit zip: Zip[List, WithList, Result]): Result =
      zip(list, withList)

    def splitAt[
      Index <: Nat,
      ListLeft <: HList,
      ListRight <: HList
    ](index: Index)
     (implicit splitAt: SplitAt[List, Index, (ListLeft, ListRight)]): (ListLeft, ListRight) =
      splitAt(list, index)
  }

}
