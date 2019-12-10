package ru.spbau.jvm.scala
package lecture06
package model

final class ASTNode(val text: String)
                   (implicit project: Project) {

  def desugaredTree: ASTNode = new ASTNode(text)
}
