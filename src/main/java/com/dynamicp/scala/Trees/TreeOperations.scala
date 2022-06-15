package com.dynamicp.scala.Trees

import com.dynamicp.adt.GenericTreeNode

import scala.collection.mutable.ListBuffer

class TreeOperations[T](val root:GenericTreeNode[T]) {
  /**
    * Method To collect the path for given node
    * Return ListBuffer containg all nodes on a path to given node in reverse order.
    * Retruns null incase of root is null
    * Returns empty ListBuffer is node is not found
    * @param value
    * @param root
    * @param listBuffer
    * @tparam T
    * @return
    */
  def getPath[T](value:T,root:GenericTreeNode[T]=root,listBuffer: ListBuffer[T]): ListBuffer[T] ={
    var l:ListBuffer[T]=ListBuffer(listBuffer:_*)
    if(root==null) return l
    else if(root._value.equals(value)) {l.append(value);return l}
    else {
      if(root._left!=null) l=getPath(value,root._left,listBuffer)
      if(l.isEmpty && root._right!=null) l=getPath(value,root._right,listBuffer)
      if(!l.isEmpty) l.append(root._value)
    }
    l
  }

  /**
    * Collects all ancestors of given node in tree.
    * Returns null if root is null
    * Returns Empty ListBuffer is not found
    * @param value
    * @param root
    * @param listBuffer
    * @tparam T
    * @return
    */
  def getAncestors[T](value:T,root:GenericTreeNode[T]=root,listBuffer: ListBuffer[T]):(Boolean,ListBuffer[T])={
    if(root==null) return (false,listBuffer)
    else if(root._value.equals(value)) {return (true,listBuffer)}
    else {
      var a:(Boolean,ListBuffer[T])=(false,listBuffer)
      if(root._left!=null) {a = getAncestors(value,root._left,listBuffer)}
      if(!a._1 && root._right!=null) {a = getAncestors(value,root._right,listBuffer)}
      if(a._1) listBuffer.append(root._value)
      (a._1,listBuffer)
    }
  }
}


object TreeOperations{
  def main(args: Array[String]): Unit = {
    println(new TreeOperations[Int](root=new GenericTreeNode[Int](2,_left=GenericTreeNode[Int]
      (1,_left=GenericTreeNode[Int](8),_right = GenericTreeNode[Int](4)),
      GenericTreeNode[Int](3,_right = GenericTreeNode[Int](7)))).getAncestors(7,listBuffer=ListBuffer.empty[Int]))

    // working with generic tre operation
    println(new TreeOperations[String](root=new GenericTreeNode[String]("a",_left=GenericTreeNode[String]
      ("b",_left=GenericTreeNode[String]("c"),_right = GenericTreeNode[String]("d")),
      GenericTreeNode[String]("e",_right = GenericTreeNode[String]("f")))).getAncestors("d",listBuffer=ListBuffer.empty[String]))
  }
}


