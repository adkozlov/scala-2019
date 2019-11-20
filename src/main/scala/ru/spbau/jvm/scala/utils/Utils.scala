package ru.spbau.jvm.scala.utils

import java.io.{File, FilenameFilter}

import scala.util.{Failure, Success, Try}

object Utils {
  def sequenceTry[T](list: List[Try[T]]): Try[List[T]] = {
    if (list.exists(elem => elem.isFailure))
      list.find(elem => elem.isFailure) match {
        case Some(Failure(ex)) => Failure(ex)
      }
    else
      Success(list.map(elem => elem.get))
  }

  def sequenceOption[T](list: List[Option[T]]): Option[List[T]] = {
    if (list.exists(_.isEmpty))
      None
    else
      Some(list.map(_.get))
  }

  def getTxtFilesFromDirectory(path: String): List[File] = {
      val file = new File(path)
      if (file == null || !file.exists || !file.isDirectory) {
        throw new IllegalArgumentException(s"Cannot access directory $path")
      }
      file.listFiles(new FilenameFilter {
        // Only need *.txt files.
        override def accept(file: File, s: String): Boolean = s.endsWith(".txt")
      }).toList
  }
}
