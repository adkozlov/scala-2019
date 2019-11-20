package ru.spbau.jvm.scala.database

sealed trait DbType

case object DbTypeInt extends DbType

case object DbTypeFloat extends DbType

case object DbTypeString extends DbType

case object DbTypeDate extends DbType
