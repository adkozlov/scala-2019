package ru.spbau.jvm.scala

import java.io.{File, FileReader}
import java.nio.file.Path

import com.google.gson.Gson

class CallsBase(val name: String, val calls: Array[Call])

object CallsBase {
    def fromPath(path: Path): CallsBase = {
        val file: File = new File(path.toUri)
        val callsBase: CallsBase = new Gson().fromJson(new FileReader(file), classOf[CallsBase])
        callsBase
    }
}
