package com.problemsolving

import com.problemsolving.CheckMaxIslandArea.{calculateArea, matrix, maxArea, visited}
import com.problemsolving.ContainsDuplicate.m
import com.problemsolving.FloodFill.{image, startColor}

import scala.collection.mutable

object MaxBorders extends App {

val tests=scala.io.StdIn.readLine().toInt

for(i <- 0 until tests){
  val testsDim=scala.io.StdIn.readLine().split(" ")
  val rows=testsDim(0).toInt
  val cols=testsDim(1).toInt
  var startBorder=0
  var endBorder=0
  for( j <- 0 until rows){
    var bCount=0
    for(c <- scala.io.StdIn.readLine()){
      if (c.equals('#')){
        bCount=bCount+1
      }
    }
    if(startBorder==0) startBorder=bCount
    if(bCount!=0) endBorder=bCount
  }
  println(if(endBorder>startBorder) endBorder else startBorder)
}
}

object zoo extends App{
  import scala.collection.mutable.{HashMap=> m}
  val test=scala.io.StdIn.readLine()
  var map:m[Char,Int]=new m[Char,Int]()
  for(c <- test){
    if(map.contains(c)) map.put(c, map.get(c).get+1) else map.put(c,1)
  }
  val oCount=map.get('o').getOrElse(0)
  val zCount=map.get('z').getOrElse(0)
  if(oCount== 2 * zCount) println("Yes") else println("No")
}

object divisible extends App {
  val l=scala.io.StdIn.readLine().toInt
  var numStr=""
  val lastString=scala.io.StdIn.readLine().split(" ").last
  if(lastString.substring(lastString.length-1)=="0") println("Yes") else println("No")
}

object prize extends App {
  val tests=scala.io.StdIn.readLine().toInt
  for(i <- 0 until tests){
    val inStr=scala.io.StdIn.readLine().split(" ")
    val costArray = Array(inStr(0).toInt,inStr(1).toInt).sorted
    val parts=scala.io.StdIn.readLine().toInt
    var pr1Count=0
    var pr2Count=0
    for(j <- 0 until parts){
      val str=scala.io.StdIn.readLine().split(" ")
      pr1Count=pr1Count+str(0).toInt
      pr2Count=pr2Count+str(1).toInt
    }
    val countArray=Array(pr1Count,pr2Count).sorted
    println((costArray(0)*countArray(1)+costArray(1)*countArray(0)))
  }
}



object pallindrome extends App {
  val str=scala.io.StdIn.readLine()
  if(str.equals(str.reverse)) println("Yes") else println("No")
}


object magicSum extends App{
  val intSize=61
  val ints="862 327 443 -633 -75 712 -350 522 -883 -218 717 -478 27 921 -255 865 -679 -91 -354 -72 764 -454 -284 -577 701 182 -353 575 -864 -744 -130 -590 506 635 109 642 195 -340 129 351 -416 600 -250 -854 24 587 -913 667 914 -632 713 249 427 92 -858 386 -33 396 515 750 202".split(" ").map(_.toInt)
  var sumArray=Array.ofDim[Int](ints.length)
  var maxSum=Integer.MIN_VALUE
for(i <- 0 until(ints.length)){
  println(s"org Indexindex ${i}")
  var prevOffset=0
  var runSum=ints(i)
  var j=2
  while(j<intSize){
    println(s"j ${j}")
    println(s"prevOffset ${prevOffset}")
    println(s"startIndex ${i + prevOffset + 1}")
    println(s"endIndex ${i + j + prevOffset}")
    if(i + j + prevOffset < intSize){
      for(k <- i + prevOffset + 1  to i + j + prevOffset){
        runSum=runSum + ints(k)
      }
      prevOffset=i+j+prevOffset
    }else j=i + j + prevOffset
    j=j+1
  }
  println(s"run sum ${runSum}")
  if(runSum>maxSum) maxSum=runSum
}
  println(maxSum)
}

object twoSum extends App{
  val nums:Array[Int]=Array(2,7,11,15)
  val target=9
  var out:Array[Int]=null
  var i=0
  while(out==null && i<nums.length){
    println("in loop1")
    val abs=scala.math.abs(target-nums(i))
    var j=i+1
    while(out==null && j<nums.length){
      println("in loop2")
      if(abs==nums(j)) {
        println(s"found ${i} ${j}")
        out=Array(i,j)

      }
      j=j+1
    }
    i=i+1
  }
  println(out.toList)
}

object LongestCommonPrefix extends App{
  val strs = Array("flower","flow","flight")
  var prefix=strs(0)
  if(prefix=="") println("")
  var i=1
  while(!prefix.equals("") && i<strs.length){
    var looping,loopedon=""
    val buf:StringBuffer=new StringBuffer("")
    if(strs(i).length>prefix.length){looping=prefix;loopedon=strs(i)}
    else {looping=strs(i);loopedon=prefix}
    var k=0;var done=false;
    while(!done && k<looping.length){
      if(looping(k)==loopedon(k)) buf.append(looping(k))
      else done=true;
      k=k+1
    }
    prefix=buf.toString
    i=i+1
  }
}

object validParanthesis extends App{
  val s="{[()]}"
  var l:List[Char]=List()
  var notBalanced=false
  var i=0
  while(!notBalanced && i<s.length){
    var c=s.charAt(i)
    if(c=='{' || c=='[' || c=='('){
      l= c +: l
    }
    else if(c=='}'){
      if(!l.isEmpty &&l.head=='{') l=l.tail
      else {notBalanced=true;println(l)}
    } else if( c==']'){
      if(!l.isEmpty && l.head=='[') l=l.tail
      else {notBalanced=true;println(l)}
    } else if (c==')'){
      if(!l.isEmpty && l.head=='(') l=l.tail
      else {notBalanced=true;println(l)}
    }
    i=i+1
  }
  println(!notBalanced && l.length==0)
}

 class ListNode(_x: Int = 0, _next: ListNode = null) {
   var next: ListNode = _next
   var x: Int = _x
 }

object mergeList extends App{
  val list1:ListNode=null
  val list2:ListNode=null
  var smallHead:ListNode=null
  var largeHead:ListNode=null
  var temp:ListNode=null
  if(list1==null && list2!=null) list2
  if(list2==null && list1!=null) list1

  if(list1.x<=list2.x){
    smallHead=list1
    largeHead=list2
  }else {smallHead=list2;largeHead=list1}

  while(smallHead!=null && largeHead!=null){
    if(smallHead.next==null) smallHead.next=largeHead
    if(smallHead.next.x>=largeHead.x){
      temp=largeHead.next
      largeHead.next=smallHead.next
      smallHead.next=largeHead
      largeHead=temp
    }else smallHead=smallHead.next
  }
  smallHead
}

object lastWordLength extends App{
  val s="   fly me   to   the moon  "
  if(s==null || s.equals("")) println(0)
  val parts=s.split(" ")
  var result=Integer.MIN_VALUE
  var i=parts.length-1
  while(result==Integer.MIN_VALUE && i>=0){
    if(parts(i).length>0) {result=parts(i).length}
    i=i-1
  }
  println(result)
}

object addOne extends App{
  val digits: Array[Int]=Array(1,2,3)
  var carry=1
  var op:Array[Int]=Array.ofDim[Int](digits.length+1)
  for(i <- digits.length-1 to 0 by -1){
    val sum=digits(i)+carry
    carry=sum/10
    op(i+1)=sum%10
  }
  op(0)=carry
  if(op(0)==0) op=op.tail else op
}

object removeSortedDuplicates extends App{
  var nums:Array[Int]=Array(-3,-3,-2,-1,-1,0,0,0,0,0)
  var curr=0
  var nxt=1
  while(nums!=null && nxt<nums.length){
    println("*********")
    println(s" curr ${curr} nxt ${nxt}")
    println(s" currentele ${nums(curr)} nxtele ${nums(nxt)}")
    nums.foreach(print)
    println("*********")
    if(nums(curr)==nums(nxt)){
      if(nxt+1<nums.length) nums(curr+1)=nums(nxt+1)
      nxt=nxt+1
    } else {
      if(nxt>curr+1) nums(curr+1)=nums(nxt)
      curr=curr+1
      if(nxt==nums.length-1) nums(curr)=nums(nxt)
      nxt=nxt+1
    }
  }
  println(if(nums==null || nums.size==0) 0 else curr+1)
  nums.foreach(print)
}


object removeElement extends App {
  var nums:Array[Int]=Array(3,3)
  val `val`=5
  var curr=0
  var nxt=1
  var found=false
  if(nums==null || nums.length==0) println(0)
  if(nums.length==1) {if(nums(curr)==`val`) println (0) else println(1)}
  while(curr+nxt<nums.length){
    if(nums(curr)==`val`){
      found=true
      if(nums(curr+nxt)!=`val`){
        nums(curr)=nums(curr+nxt)
        nums(curr+nxt)=`val`
        curr=curr+1
      }else nxt=nxt+1
    }else curr=curr+1
  }
  if(curr==nums.length-1 && !found && nums(curr)==`val`) found=true
  println( if(found) curr else curr+1)
}

object removeDuplicatesInList extends App{
  val head:ListNode= new ListNode(1);
  val _2:ListNode=new ListNode(1);
  val _3:ListNode=new ListNode(2);
  head.next=_2
  _2.next=_3

  var current:ListNode=head
  if(current==null || current.next==null) current

  var next=current.next
  while(next!=null){
    if(current.x==next.x) next=next.next
    if(current.x!=next.x) {
      if (current.next != next) {
        current.next = next
        current = current.next
        next = current.next
      } else{
        current=current.next
        next=current.next
      }
    }
  }
  println(head)
}


 case class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
   var value: Int = _value
   var left: TreeNode = _left
   var right: TreeNode = _right
   var next: TreeNode = null
   override def toString: String = s"${this.value} ${if(this.left!=null) "\n"+this.left.toString else null} ${if(this.right!=null) "\t\n"+this.right.toString else null}"
 }

object TreeInorderTraversal extends App{
  var l:List[Int]= List.empty[Int]
  var root= TreeNode(1,null,null)
  func(root)
  def func(node: TreeNode):Unit = {
  var curn=node
  while(curn!=null) {
    func(curn._left)
    println(curn)
    func(curn._right)
  }
  }
}


object SymmetricTree extends App {
  var root= TreeNode(1,null,null)
  if(root!=null){
    preOrder(root.left).equals(postOrder(root.right).reverse)
  }

  def preOrder(root:TreeNode): String ={
    var s=""
    if(root!=null){
      s= s+ s"${root.value}"
      s= s+ preOrder(root.left)
      s= s+ preOrder(root.right)
    } else s=s + "n"
    s
  }
  def postOrder(root:TreeNode):String ={
    var s=""
    if(root!=null){
      s= s+ s"${root.value}"
      s= s+ postOrder(root.left)
      s= s+ postOrder(root.right)
    } else s=s + "n"
    s
  }
}

object RemoveNthNodeFromEnd extends App {
  var head= new ListNode(1,new ListNode(2,new ListNode(3,new ListNode(4,new ListNode(5)))))
  val n=2
  var prev:ListNode=null
  var curr=head
  var next=curr.next
  while(curr!=null){
    //check that current node is last numth Node
    var temp=curr
    var i=0
    while(temp!=null && i<n) {
      temp = temp.next
      i=i+1
    }
    if(i==n && temp==null){
      if(prev!=null){
        prev.next=curr.next
        curr=null
      }else{

      }
    }else{
      prev=curr
      curr=curr.next
      next=if(curr!=null) curr.next else null
      println(curr.x)
    }
  }
  while(head!=null){
    println(head.x)
    head=head.next
  }

  def removeNthFromEnd(head: ListNode, n: Int): ListNode = {
    var prev:ListNode=null
    var curr=head
    var next=curr.next
    while(curr!=null){
      //check that current node is last numth Node
      var temp=curr
      var i=0
      while(temp!=null && i<n) {
        temp = temp.next
        i=i+1
      }
      if(i==n && temp==null){
        if(prev!=null){
          prev.next=curr.next
          curr=null
        }else{
          return curr.next
        }
      }else{
        prev=curr
        curr=curr.next
        next=if(curr!=null) curr.next else null
      }
    }
    head
  }
}

object numberMAp extends App {
  val numMap=Map(2 -> Array("a","b","c"),
    3 -> Array("d","e","f"),
    4->Array("g","h","i"),
    5->Array("j","k","l"),
    6->Array("m","n","o"),
    7->Array("p","q","r","s"),
    8->Array("t","u","v"),
    9->Array("w","x","y","z"))
  val digits="2345"
  var s:List[String]= List.empty[String]
  var j=0
  for(i <- digits){
    j=j+1
    val c=numMap.get(i.toString.toInt).get
    var tempList=List(s:_*)
    if(tempList.isEmpty){
      c.foreach(x=> tempList = tempList :+ x)
    }else {
      c.foreach(x => tempList.foreach(y => {
        var z = y + x;
        if (z.size == j) tempList = tempList :+ z
      }))
    }
    s=tempList
  }
  println(s.filter(x=>x.size==digits.size))
  println(s.size)
}

/**
  * Definition for a binary tree node.
  * class TreeNode(_value: Int = 0, _left: TreeNode = null, _right: TreeNode = null) {
  *   var value: Int = _value
  *   var left: TreeNode = _left
  *   var right: TreeNode = _right
  * }
  */
object Solution extends App{
  def isValidBST(root: TreeNode): Boolean = {
    var result=true
    if(root!=null) {
      result=result &&
        {if(root.left!=null) root.left.value<root.value && isValidBST(root.left) else true} &&
        {if(root.right!=null) root.right.value>root.value && isValidBST(root.right) else true}
    }
    result
  }
}

object searchBinary extends App{
  val nums=Array(-1,0,3,5,9,12,16)
  val target= 8
  var r=nums.length-1
  var l=0
  var index = -1
  if(r==l) if(nums(r)==target) index=r
  while(r>l){
    var mid=(r+l+1)/2
    if(nums(mid)==target) {
      r=mid
      r=l
      index=mid
    } else {
      if(nums(r)==target) {mid=r;l=r;index=mid}
      else if(nums(l)==target) {mid=l;r=l;index=mid}
      else {
        if(r-l == 1) r=l
        else {
          if (nums(mid) > target) r = mid
          else if (nums(mid) < target) l = mid
        }
      }
    }
  }
  println(index)
}


object firstBad extends App {
  firstBadVersion(2126753390)
  def isBadVersion(i: Int) = {println(i);i>=10}
  def firstBadVersion(n: Int): Int = {
    var r:Int = n
    var l:Int = 1
    var index:Int = -1
    if (isBadVersion(l)) {
      index = l;
      r = l
    }
    while (l < r) {
      var sum:Double= (r.toDouble + l.toDouble)/2
      var mid = sum.toInt
      if (isBadVersion(mid)) {
        if (isBadVersion(l)) {
          mid = l
          r = mid
          index = mid
        } else {
          if (!isBadVersion(mid - 1)) {
            l = mid
            r = mid
            index = r
          } else r = mid - 1
        }
      } else {
        if (isBadVersion(mid + 1)) {
          l=mid+1
          r = mid+1
          index = mid+1
        } else l = mid + 1
      }
    }
    println(index)
    index
  }
}

object findIndex extends App{
  val nums=Array(-1,0,3,5,9,12,16)
  val target= 1
  var r=nums.length-1
  var l=0
  var index = -1
  if(r==l) if(nums(r)>target) index=0 else index=1
  while(r>l){
    var mid=(r+l+1)/2
    if(nums(mid)==target) {
      r=mid
      r=l
      index=mid
    } else {
      if(nums(r)==target) {mid=r;l=r;index=mid}
      else if(nums(l)==target) {mid=l;r=l;index=mid}
      else {
        if(r-l == 1) {
          if(r==nums.length-1 && nums(r)<target) index=r+1
          else if(l==0 && nums(l)>target)index=0
          else index=l+1
          r=l
        } else {
          if (nums(mid) > target) r = mid
          else if (nums(mid) < target) l = mid
        }
      }
    }
  }
  println(index)
}


object squareOfSortedArray extends App {
  val nums = Array(-10000,-9999,-7,-5,0,0,10000)
  var squared=Array.ofDim[Int](nums.length)
  val up=nums.length-1
  var l=0
  var r=nums.length-1
  while(l<=r){
    println("nums"+ nums.toList)
    println("squared"+ squared.toList)
    val sqL=Math.pow(nums(l),2).toInt
    val sqR=Math.pow(nums(r),2).toInt
    if(sqL>=sqR){
      squared(r-l)=sqL
      l=l+1
    }else{
      squared(r-l)=sqR
      r=r-1
    }
  }
  println(squared.toList)
}

object rotateArray extends App {
  val nums = Array(0,0,-7,-5,0,0,10000)
  var steps=0
  var i=0
  if(steps==0 || nums.length==1) println(nums.toList)
  if(steps>nums.length){
    if(steps%nums.length==0) steps=steps/nums.length
    else steps=steps/nums.length + steps%nums.length
  }
}

object shiftZeros extends App{
  var nums = Array(0,0,-7,-5,0,0,10000)
  var s=0
  var f=s+1
  while(f<nums.length){
    if(nums(s)==0){
      if(nums(f)==0){
        f=f+1
      }else{
        var temp=nums(s)
        nums(s)=nums(f)
        nums(f)=temp
        s=s+1
        f=f+1
      }
    }else {
      s=s+1
      f=f+1
    }
  }
  println(nums.toList)
}

object StringReverse extends App {
  var s: Array[Char]= Array('h','e','l','l')
  var temp:Char=Char.MinValue
  val up=s.length-1
  for(i <- 0 to up/2){
    var temp=s(i)
    s(i)=s(up-i)
    s(up-i)=temp
  }
  println(s.toList)
}

object WordsReverse extends App {
  val i=new StringBuffer("Let's take LeetCode contest")
  var start=0
  var end=i.indexOf(" ",start)
  while(end>=0){
    i.replace(start,end,i.substring(start,end).reverse)
    start=end+1
    end=i.indexOf(" ",start)
  }
  i.replace(start,i.length(),i.substring(start,i.length()).reverse)
  println(i.toString)
}


object ContainsDuplicate extends App {
  val nums = Array(1,2,1,4)
  var i=0
  val up=nums.length-1
  var m:mutable.Set[Int]=mutable.Set.empty
  var dups=false
  var rev=up-i
  while(i<up && up-i-i>=0){
    if(m.contains(nums(i))||m.contains(nums(up-i))){
      dups=true;i=Int.MaxValue-1
    } else{
      m.add(nums(i))
      if(i!=up-i && m.contains(nums(up-i))){
        dups=true;i=Int.MaxValue-1
      }else{
        m.add(nums(up-i))
      }
    }
    i+=1
    }
  println(dups)
}

object BoxBlur extends App{
  val image=Array(Array(7, 4, 0, 1),
    Array(5, 6, 2, 2),
  Array(6, 10, 7, 8),
  Array(1, 4, 2, 0))
  val r=image.length
  val c=image(0).length
  val radius=2
  var op=Array.ofDim[Array[Int]](r-radius) //each column of size c-2

  def getAvg(arr:Array[Array[Int]],r:Int,c:Int): Int ={
    (arr(r-1)(c-1) + arr(r-1)(c) + arr(r)(c-1) + arr(r)(c) + arr(r+1)(c-1) + arr(r+1)(c)
      +arr(r+1)(c+1)+ arr(r)(c+1)+ arr(r-1)(c+1))/9
  }

  for(i <- 1 to r-radius){
    var inner=Array.ofDim[Int](c-radius)
    for(j <- 1 to c-radius){
      val ele=getAvg(image,i,j)
      inner(j-1)=ele
    }
    op(i-1)=inner
  }
  op
}

object MineSweeper extends App{
  val matrix:Array[Array[Boolean]]=Array(
    Array(true,false),
    Array(true,false),
    Array(false,true),
    Array(false,false),
    Array(false,false)
  )
  var op:Array[Array[Int]]=Array.ofDim(matrix.length,matrix(0).length)
  for(i <- 0 until matrix.length){
    for(j <- 0 until matrix(0).length){
      if(matrix(i)(j)){
        for(k:Int <- i-1 to i+1 ){
            for(l:Int <- j-1 to j+1 ){
          if(k>=0 && k< matrix.length && l>=0 && l<matrix(0).length && (k,l)!=(i,j)){
            op(k)(l) += 1
          }
        }
        }
      }
    }
  }
op.foreach(x=>{x.foreach(y=>print(y+ " "));println()})
}

object ReplaceWithNext extends App{
  val alph="abcdefghijklmnopqrstuvwxyz"
  var inp="z"
  var op=""
  for(c <- inp){
    op=op+alph.charAt(if(alph.indexOf(c)<25) alph.indexOf(c)+1 else 0)
  }
  println(op)
}

object SameColorChessSquare extends App{
  val cell1="A1"
  val cell2="C3"
  val blackStartFiles="ACEG"
  if(blackStartFiles.contains(cell1.charAt(0))){
    if(blackStartFiles.contains(cell2.charAt(0))){
      if((cell1.charAt(1).toString.toInt%2==0 && cell2.charAt(1).toString.toInt%2==0) || cell1.charAt(1).toString.toInt%2!=0 && cell2.charAt(1).toString.toInt%2!=0){
        true
      }else false
    }else{
      if((cell1.charAt(1).toString.toInt%2==0 && cell2.charAt(1).toString.toInt%2!=0) || cell1.charAt(1).toString.toInt%2!=0 && cell2.charAt(1).toString.toInt%2==0){
        true
      }else false
    }
  }
  if(!blackStartFiles.contains(cell1.charAt(0))){
    if(!blackStartFiles.contains(cell2.charAt(0))){
      if((cell1.charAt(1).toString.toInt%2!=0 && cell2.charAt(1).toString.toInt%2!=0) || cell1.charAt(1).toString.toInt%2==0 && cell2.charAt(1).toString.toInt%2==0){
        true
      }else false
    }else{
      if((cell1.charAt(1).toString.toInt%2==0 && cell2.charAt(1).toString.toInt%2!=0) || cell1.charAt(1).toString.toInt%2!=0 && cell2.charAt(1).toString.toInt%2==0){
        true
      }else false
    }
  }

}

object MiddleOfLinkedList extends App {
  val head= new ListNode(1,new ListNode(2,new ListNode(3,new ListNode(4,new ListNode(5,new ListNode(6,null))))))
  var slow=head
  var fast=head.next
  while(fast!=null){
    slow=slow.next
    if(fast.next!=null){
      fast=fast.next.next
    }else{
      fast=fast.next
    }
  }
  slow
}

object lengthOfLongestSubstring extends App{
val s="dvdf"
var offset=0
var maxLength=if (s.length>1) 0 else s.length
val l=s.length
var i=0
while(i<l){
  if(s.substring(offset,i).contains(s.charAt(i))){
    offset=offset+1
    i=offset
  }else{
    i=i+1
    if(maxLength<s.substring(offset,i).length){
      maxLength=s.substring(offset,i).length
    }
  }
}
 println(maxLength)
}


object checkInclusion extends App{
  val s1: String="ab"
  val s2: String="eidboaoo"
  if(s1.length>s2.length) println(false)
  else {
    if(s2.sorted.contains(s1.sorted)) println(true) else false
  }
}

object FloodFill extends App{
  var image = Array(Array(1,1,1),Array(1,1,0),Array(1,0,1))
  val sr = 1
  val sc = 1
  val newColor = 2
  var visited:scala.collection.mutable.Set[(Int,Int)]=mutable.Set.empty
  val startColor=image(sr)(sc)
  changeColor(image,(sr,sc),newColor,startColor,visited)
  def changeColor(image:Array[Array[Int]],location:(Int,Int),newColor:Int,startColor:Int, visited:scala.collection.mutable.Set[(Int,Int)]): Array[Array[Int]] ={
      visited.add(location)
      image(location._1)(location._2)=newColor
      val up=(location._1-1,location._2)
      val down=(location._1+1,location._2)
      val left=(location._1,location._2-1)
      val right=(location._1,location._2+1)
      if(up._1>=0 && !visited.contains(up) && image(up._1)(up._2)==startColor) changeColor(image,up,newColor,startColor,visited)
      if(down._1<image.length && !visited.contains(down) && image(down._1)(down._2)==startColor) changeColor(image,down,newColor,startColor,visited)
      if(left._2>=0 && !visited.contains(left) && image(left._1)(left._2)==startColor) changeColor(image,left,newColor,startColor,visited)
      if(right._2<image(0).length && !visited.contains(right) && image(right._1)(right._2)==startColor) changeColor(image,right,newColor,startColor,visited)
    image
  }
  image.foreach(x=>{x.foreach(y=>print(y+" "));println()})
}

object CheckMaxIslandArea extends App{
  val matrix=Array(Array(0,0,1,0,0,0,0,1,0,0,0,0,0),Array(0,0,0,0,0,0,0,1,1,1,0,0,0),Array(0,1,1,0,1,0,0,0,0,0,0,0,0),Array(0,1,0,0,1,1,0,0,1,0,1,0,0),
    Array(0,1,0,0,1,1,0,0,1,1,1,0,0),Array(0,0,0,0,0,0,0,0,0,0,1,0,0),Array(0,0,0,0,0,0,0,1,1,1,0,0,0),Array(0,0,0,0,0,0,0,1,1,0,0,0,0))

  var maxArea=0
  var visited:scala.collection.mutable.Set[(Int,Int)]=mutable.Set.empty
  for(i<- 0 until matrix.length){
    for(j<- 0 until matrix(i).length){
      if(!visited.contains((i,j)) && matrix(i)(j)==1){
        val islandArea=calculateArea(matrix,(i,j),visited,0)
        if(islandArea>maxArea) maxArea=islandArea
        println(maxArea)
      }
    }
  }

  def calculateArea(matrix:Array[Array[Int]], location:(Int,Int), visited:scala.collection.mutable.Set[(Int,Int)], area:Int): Int ={
    visited.add(location)
    var subArea=1
    val up=(location._1-1,location._2)
    val down=(location._1+1,location._2)
    val left=(location._1,location._2-1)
    val right=(location._1,location._2+1)
    if(up._1>=0 && !visited.contains(up) && matrix(up._1)(up._2)==1) subArea=subArea+calculateArea(matrix,up,visited,subArea)
    if(down._1<matrix.length && !visited.contains(down) && matrix(down._1)(down._2)==1) subArea=subArea+calculateArea(matrix,down,visited,subArea)
    if(left._2>=0 && !visited.contains(left) && matrix(left._1)(left._2)==1) subArea=subArea+calculateArea(matrix,left,visited,subArea)
    if(right._2<matrix(0).length && !visited.contains(right) && matrix(right._1)(right._2)==1) subArea=subArea+calculateArea(matrix,right,visited,subArea)
    subArea
  }
}


object MergeTwoBinaryTrees extends App{
  val root1=new TreeNode(1,new TreeNode(3,new TreeNode(5)),new TreeNode(2))
  val root2=new TreeNode(2,new TreeNode(1,_right = new TreeNode(4)),new TreeNode(3,_right = new TreeNode(7)))
  merge(root1,root2)
  def merge(t1:TreeNode,t2:TreeNode): TreeNode ={
    if(t1==null) t2
    else if(t2==null) t1
    else {
      t1.value = t1.value + t2.value
      t1.left = merge(t1.left, t2.left)
      t1.right = merge(t1.right, t2.right)
      t1
    }
  }
  println(root1.toString)
}

object Connect extends App{
  def connectNext(treeNode: TreeNode,next:TreeNode): Unit ={
  if(next!=null) treeNode.next=next
  if(treeNode.left!=null) connectNext(treeNode.left,treeNode.right)
  if(treeNode.right!=null) connectNext(treeNode.right,if(treeNode.next!=null)treeNode.next.left else null)
  }
}

object RecordDistanceZero extends App{
  var mat = Array(Array(0,0,0),Array(0,1,0),Array(1,1,1))
  updateMatrix(mat)
  def updateMatrix(mat: Array[Array[Int]]): Array[Array[Int]] = {
    var visitedDistences:scala.collection.mutable.Map[(Int,Int),Int]=scala.collection.mutable.Map.empty
    var onGoing:scala.collection.mutable.Set[(Int,Int)]=scala.collection.mutable.Set.empty
    for(i<- 0 until mat.length) {
      for (j <- 0 until mat(i).length) {
        mat(i)(j)=recordDistance(mat,(i,j),visitedDistences,onGoing,true)
      }
    }
    visitedDistences=scala.collection.mutable.Map.empty
    onGoing=scala.collection.mutable.Set.empty
    mat.foreach(x=>{x.foreach(y=>print(y + " "));println()})
    for(i<- 0 until mat.length) {
      for (j <- 0 until mat(i).length) {
        mat(i)(j)=recordDistance(mat,(i,j),visitedDistences,onGoing,false)
      }
    }
    mat.foreach(x=>{x.foreach(y=>print(y + " "));println()})
    println(visitedDistences)
    println(onGoing)
    mat
  }

  def recordDistance(mat:Array[Array[Int]],location:(Int,Int),visited:scala.collection.mutable.Map[(Int,Int),Int],onGoing:scala.collection.mutable.Set[(Int,Int)],onlyUp:Boolean): Int ={
    onGoing.add(location)
    if(visited.contains(location)) {}
    else if(mat(location._1)(location._2)==0){visited.put(location,0)}
    else {
      var minDistance= Integer.MAX_VALUE
      var distance=Integer.MAX_VALUE
      if (onlyUp) {
        val up = (location._1 - 1, location._2)
        val left = (location._1, location._2 - 1)
        val upD = if (up._1 >= 0 && !onGoing.contains(up)) recordDistance(mat, up, visited, onGoing,true) else Integer.MAX_VALUE
        val leftD = if (left._2 >= 0 && !onGoing.contains(left)) recordDistance(mat, left, visited,onGoing,true) else Integer.MAX_VALUE
        minDistance=Math.min(upD,leftD)
        distance=if(minDistance<Integer.MAX_VALUE) minDistance+1 else Integer.MAX_VALUE
      }else{
        val thisLow=mat(location._1)(location._2)
        val down = (location._1 + 1, location._2)
        val right = (location._1, location._2 + 1)
        val downD = if (down._1 < mat.length && !onGoing.contains(down)) recordDistance(mat, down, visited, onGoing,false) else Integer.MAX_VALUE
        val rightD = if (right._2 < mat(0).length && !onGoing.contains(right)) recordDistance(mat, right, visited,onGoing,false) else Integer.MAX_VALUE
        minDistance=Math.min(Math.min(downD,rightD),thisLow)
        distance=if(minDistance<Integer.MAX_VALUE && minDistance<thisLow) minDistance+1 else thisLow
      }
      visited.put(location, distance)
    }
    onGoing.remove(location)
    visited.get(location).get
  }


  }