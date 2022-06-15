package com.dynamicp.adt

case class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  var value: Int = _value
  var left: TreeNode = _left
  var right: TreeNode = _right
  var next: TreeNode = null
  override def toString: String = s"${this.value} ${if(this.left!=null) "\n"+this.left.toString else null} ${if(this.right!=null) "\t\n"+this.right.toString else null}"
}

case class GenericTreeNode[T](_value: T = 0, _left: GenericTreeNode[T] = null, _right: GenericTreeNode[T] = null) {
  var value: T = _value
  var left: GenericTreeNode[T] = _left
  var right: GenericTreeNode[T] = _right
  override def toString: String = s"${this.value} ${if(this.left!=null) "\n"+this.left.toString else null} ${if(this.right!=null) "\t\n"+this.right.toString else null}"
}