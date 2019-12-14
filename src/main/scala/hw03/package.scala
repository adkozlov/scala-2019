package object hw03 {
    type ::[Head, Tail <: HList] = HCons[Head, Tail]
    type Nil = HNil.type
    type ZeroType = Zero.type

    implicit class HListExt[List <: HList](private val list: List) extends AnyVal {

        def ::[Head](head: Head): HCons[Head, List] = HCons(head, list)

        def :::[
        Prefix <: HList,
        Result <: HList
        ](prefix: Prefix)
         (implicit appendable: Appendable[Prefix, List, Result]): Result =
            appendable(prefix, list)

        def zip[
        Right <: HList,
        Result <: HList
        ](that: Right)
         (implicit zippable: Zippable[List, Right, Result]): Result =
            zippable(list, that)

        def split[
        LeftSize <: NonNegative,
        Result <: (HList, HList)
        ](leftSize: LeftSize)
         (implicit splittable: Splittable[List, LeftSize, Result]): Result =
            splittable(list, leftSize)
    }
}
