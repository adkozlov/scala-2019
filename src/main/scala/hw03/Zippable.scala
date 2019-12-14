package hw03

trait Zippable[
Left <: HList,
Right <: HList,
Result <: HList
] {
    def apply(left: Left, right: Right): Result
}

object Zippable {

    // Nil zip Nil = Nil
    implicit def doubleNilZippable: Zippable[Nil, Nil, Nil] = (_: Nil, _: Nil) => HNil

    // LTail zip RTail = Result
    // (LHead :: LTail) zip (RHead :: RTail) = (LHead, RHead) :: Result
    implicit def zippable[
    Left <: HList,
    Right <: HList,
    Result <: HList,
    LHead,
    LTail <: HList,
    RHead,
    RTail <: HList
    ](implicit zippable: Zippable[LTail, RTail, Result]):
    Zippable[LHead :: LTail, RHead :: RTail, (LHead, RHead) :: Result] =
        (left: LHead :: LTail, right: RHead :: RTail) => {
            val HCons(lhead, ltail) = left
            val HCons(rhead, rtail) = right
            HCons((lhead, rhead), zippable(ltail, rtail))
        }
}

