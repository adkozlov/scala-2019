package ru.spbau.jvm.scala
package lecture06
package model

object Main {

  def main(args: Array[String]): Unit = {
    implicit val project: Project = new Project("name", "path")
    val foo = new ASTNode("foo")
    val bar = new ASTNode("bar")

    renderAll(
      foo,
      bar,
      bar.desugaredTree
    )
  }

  private def renderAll(nodes: ASTNode*)
                       (implicit editor: Editor): Unit =
    nodes.foreach(editor.renderNode)

  private implicit def createEditor(implicit project: Project): Editor = new Editor()
}
