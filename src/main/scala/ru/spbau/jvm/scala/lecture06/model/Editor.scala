package ru.spbau.jvm.scala
package lecture06
package model

final class Editor(implicit project: Project) {

  def renderNode(node: ASTNode): Unit = {
    println(node.text)
  }
}
