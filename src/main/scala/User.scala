class User(
              val name: String,
              val surname: String,
              val number: String,
              val id: Int
          ) {
    override def toString: String = s"$name $surname $number"

    override def equals(obj: Any): Boolean =
        obj match {
            case obj: User =>
                name.equals(obj.name) &&
                surname.equals(obj.surname) &&
                number.equals(obj.number) &&
                id.equals(obj.id)
            case _ => false
        }
}
