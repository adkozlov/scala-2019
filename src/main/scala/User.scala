class User(
              val name: String,
              val surname: String,
              val number: String,
              val id: Int
          ) {
    override def toString: String = s"$name $surname $number"
}
