package hw03

trait Splittable[
List <: HList,
LeftSize <: NonNegative,
Result <: (HList, HList)
] {
    def apply(list: List, leftSize: LeftSize): Result
}

object Splittable {

    // split empty list only by Zero
    implicit def nilSplittable: Splittable[Nil, ZeroType, (Nil, Nil)] =
        (_: Nil, _: ZeroType) => (HNil, HNil)

    // split list with zero left size
    implicit def zeroSplittable[List <: HList]: Splittable[List, ZeroType, (Nil, List)] =
        (list: List, _: ZeroType) => (HNil, list)

    // split(Tail, leftSize) = (LPart, RPart)
    // split(Head :: Tail, Positive[leftSize] = (Head :: LPart, RPart)
    implicit def splittable[
    Head,
    LPart <: HList,
    RPart <: HList,
    Tail <: HList,
    LeftSize <: NonNegative
    ](implicit splittable: Splittable[Tail, LeftSize, (LPart, RPart)]):
    Splittable[Head :: Tail, Positive[LeftSize], (Head :: LPart, RPart)] =
        (list: Head :: Tail, leftSize: Positive[LeftSize]) => {
            val HCons(head, tail) = list
            val (lpart, rpart) = splittable(tail, leftSize.rest)
            (HCons(head, lpart), rpart)
        }
}
