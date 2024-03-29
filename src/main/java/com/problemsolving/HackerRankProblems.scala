package com.problemsolving

import java.io._
import java.math._
import java.security._
import java.text._
import java.util
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._

import scala.collection.immutable._
import scala.collection.mutable._
import scala.collection.concurrent._
import scala.collection.mutable
import scala.concurrent._
import scala.io._
import scala.math._
import scala.sys._
import scala.util.matching._
import scala.reflect._

class HackerRankProblems  {

}


object HackerRankProblems extends App {
  def simpleArraySum(ar: Array[Int]): Int = {
    ar.reduce(_+_)
  }

  def diagonalDifference(arr: Array[Array[Int]]): Int = {
    // Write your code here
    var r=0
    var l=0
    val last=arr.length-1
    for(i <- 0 to last){
      l+=(arr(i)(i))
      r+=(arr(i)(last-i))
    }
    (l-r).abs
  }

  def plusMinus(arr: Array[Int]) {
    // Write your code here
    var holder:scala.collection.immutable.Map[String,Double]=scala.collection.immutable.Map("plus" -> 0.toDouble,"minus"->0.toDouble,"zero"->0.toDouble)
    for(i<-arr){
      var num=""
      if(i<0) num="minus"
      if(i==0) num="zero"
      if(i>0) num="plus"

      holder=holder + ((num,holder.get(num).get + 1))
    }
    println((((holder.get("plus").get/arr.length).toDouble * 1000000).round / 1000000.toDouble))
    println((((holder.get("minus").get/arr.length).toDouble * 1000000).round / 1000000.toDouble))
    println((((holder.get("zero").get/arr.length).toDouble * 1000000).round / 1000000.toDouble))
  }

  def staircase(n: Int) {
    // Write your code here
    for(i <- 1 to n){
      var x=for(j<-1 to n-i) yield " "
      var y=for(k<-1 to i) yield "#"
      println(x.mkString("")+y.mkString(""))
    }
  }

  def minMaxProblem(): Unit ={
    val arr=Array(1,2,3,4,5)
    var minValue:Long=Int.MaxValue
    var maxValue:Long=Int.MinValue
    var sum:Long=0
    for(i<- 0 until arr.length){
      if(arr(i)< minValue) minValue=arr(i)
      if(arr(i)>maxValue) maxValue=arr(i)
      sum=sum+arr(i)
    }
    println(s"${sum-maxValue}  ${sum-maxValue}")
  }

  def tallestCandles={
    val candles=Array(4,4,1,2,3)
    var tallest=Int.MinValue
    var tallestCount=1
    candles.foreach(i=>{ if(i>tallest) { tallest=i;tallestCount=1} else if(i==tallest) tallestCount=tallestCount+1})
    println(s"$tallest $tallestCount")
  }

  def gradingStudents(grades: Array[Int]): Array[Int] = {
    var j=0
    val ans=grades.map(i=> if(i>=38){
      val nextMultipleDiff=5*((i/5)+1)-i
      if(nextMultipleDiff<3) i+nextMultipleDiff
      else i
    }else i)
    ans
  }

  def caesarCipher(s: String ="!m-rB`-oN!.W`cLAcVbN/CqSoolII!SImji.!w/`Xu`uZa1TWPRq`uRBtok`xPT`lL-zPTc.BSRIhu..-!.!tcl!-U", k: Int=62): String = {
    val steppedDown=if(k>26) k%26 else k
    val sb=new scala.collection.mutable.StringBuilder()
    val alph=scala.collection.immutable.List(
      'A','B','C','D','E','F','G','H','I',
      'J','K','L','M','N','O','P','Q','R',
      'S','T','U','V','W','X','Y','Z')
    s.foreach(c=> {
      var charToappend:Char = Char.MinValue
      if(c.isLetter){
        if(alph.indexOf(c.toUpper) + steppedDown > alph.size-1)
          charToappend=alph(alph.indexOf(c.toUpper) + steppedDown - 26)
        else charToappend=alph(alph.indexOf(c.toUpper) + steppedDown)
      }else charToappend=c
      if(c.isLower) charToappend=charToappend.toLower

      sb.append(charToappend)
    })
    sb.toString()
  }

  def camelCase: Unit ={
    val words="saveTheCamel"
    var wordCount= if(!words.isEmpty) 1 else 0
    for(c <-words){
      if(c.isUpper) wordCount+=1
    }
  }

  def isPanagram(s:String="We promptly judged antique ivory buckles for the prize"):String={
    val m:scala.collection.mutable.Map[Char,Int]=scala.collection.mutable.Map.empty
    var i=0
    while(i < s.length && m.size<26){
      if(s.charAt(i).isLetter) {
        m.put(s.charAt(i).toLower, m.getOrElse(s.charAt(i).toLower, 0) + 1)
      }
      i+=1
    }
    if(m.size==26)"pangram" else "not pangram"
  }

  def extraLongFactorials(n: Int) {
    val arr=Array.ofDim[BigInt](n+1)
    for(i<- 0 to n){
      if(i==0) arr(i)=1
      else arr(i)=arr(i-1)*i
    }
    println(arr(arr.length-1))
  }

  def migratoryBirds(arr: Array[Int]) = {
    val sightings = scala.collection.mutable.Map.empty[Int,Int]
    var maxSightNum = 6
    var maxSightOccurance = 0
    for (ele <- arr) {
      val occu = sightings.getOrElse(ele,0)+1
      sightings.put(ele, occu)
      if (occu >= maxSightOccurance) {
        if (occu == maxSightOccurance && ele <= maxSightNum) maxSightNum = ele
        else if(occu>maxSightOccurance) maxSightNum = ele
        maxSightOccurance = occu
      }
    }
    maxSightNum
  }

  def formingMagicSquare(s: Array[Array[Int]]): Int = {
    // Write your code here
    val m=scala.collection.mutable.Map.empty[Int,Int]
    (1 to s.length*s(0).length).foreach(i=> m.put(i,0))
    var repeat=scala.collection.immutable.List.empty[Int]
    for(i<- 0 until s.length){
      for(j<- 0 until s(i).length){
        val occu=m.get(s(i)(j)).get+1
        m.put(s(i)(j),occu)
        if(occu>1) repeat=s(i)(j) :: repeat
      }
    }
    val missing=m.filter(x=> x._2==0).map(y=>y._1).toList
    repeat.sorted
    missing.sorted
    var minCost=0
    println(repeat)
    println(missing)
    for(i<- 0 until repeat.length){
      minCost= minCost + (repeat(i) - missing(i)).abs
    }
    println(minCost)
    minCost
  }

  def encrypt(s:String="haveaniceday"):String= {
    val s2=s.replace(" ","")
    val r=math.sqrt(s2.length).floor.toInt
    val c=math.sqrt(s2.length).ceil.toInt
    val mat=scala.collection.mutable.ArrayBuffer.empty[String]
    var i=0
    var offset=0
    while(i<=r){
      offset=i*c
      if(offset<s2.length) {
       mat.+=(s2.substring(offset, if (offset + c < s2.length) offset + c else s2.length).padTo(c, " ").mkString(""))
      }
      i=i+1
    }
    val sbM=new mutable.StringBuilder()
    for(l<- 0 until c) {
      val sb=new mutable.StringBuilder()
      for (k <- 0 until mat.length) {
        sb.append(mat(k)(l))
      }
      sbM.append(sb.toString().trim).append(" ")
    }
    println(sbM.toString().trim)
    ""
  }

  def climbingLeaderboard(ranked: Array[Int], player: Array[Int]): Array[Int] = {
    // Write your code here
    var op=Array.ofDim[Int](player.length)
    var i=player.length-1
    var j=0
    var rank=1
    while(i>=0){
      if(j<ranked.length && player(i)>=ranked(j)){
        op(i)=rank
        i=i-1
      }else{
        j=j+1
        while(j<ranked.length && ranked(j-1)==ranked(j)){
          j=j+1;
        }
        if(j<ranked.length){rank=rank+1}
        else {op(i)=rank+1; i=i-1}
      }
    }
    op.foreach(println)
    op
  }

  def gridSearch(G: Array[String], P: Array[String]): String = {
    var gRow=0
    var pRow=0
    var pIndex = -1
    var found=false
    while(gRow<G.length && pRow<P.length){
      var cIndex=G(gRow).indexOf(P(pRow),if(pIndex== -1) 0 else pIndex-1)
      if(cIndex!= -1) {
        if(pIndex== -1) {
          pRow = pRow + 1
          pIndex = cIndex
          gRow = gRow + 1
          found = true
        }
        else if(cIndex==pIndex) {
          pRow = pRow + 1
          pIndex = cIndex
          gRow = gRow + 1
          found = true
        }else {
          pRow=0;
          pIndex= -1
          found=false
        }
      }
      else{
        found=false
        if(pRow!=0){pRow=0;pIndex= -1 }
        else gRow=gRow+1
      }
    }
    println(found)
    if(found)"YES" else "NO"
  }

  def timeInWords(h: Int, m: Int): String = {
    // Write your code here
   //At 0 use o' clock.
   //1 minute
   //45,15 quarter
   //30 half
   val oClock="O'Clock"
   val minute="minute"
   val half="half"
   val quarter="quarter"
   var to=false
   var min=m
   var hr=h
   var time=""
   if(m>30){
     min=60-m
     to=true
     hr=h+1
   }

   if(m==0) time=s"${h} $oClock"
   if(to){
     time = s"${if(min==15) quarter else min+" "+ minute}${if(min!=1 && min!=15) "s"} to ${hr}"
   }else{
     time = s"${if(min==15) quarter else min+" "+minute}${if(min!=1 && min!=15) "s"} past ${hr}"
   }
   time
  }

  def countApplesAndOranges(s: Int, t: Int, a: Int, b: Int, apples: Array[Int], oranges: Array[Int]) {
    println(apples.map(ap => a+ap).filter(p=> p>=s && p<= t).length)
    println(oranges.map(or => b+or).filter(p=> p>=s && p<= t).length)
  }

  def sockMerchant(n: Int, ar: Array[Int]): Int = {
   val map=scala.collection.mutable.Map.empty[Int,Int]
   var pairs=0
   ar.foreach(color=> {
     val existing=map.getOrElse(color,0)
     if(existing==0) map.put(color,1)
     else if((existing+1)%2 == 0) {pairs=pairs+1;map.remove(color)}
     else map.put(color,existing+1)
   })
   pairs
  }

  def findDigits(n: Int): Int = {
  var l=new ListBuffer[Int]
  n.toString.foreach(c=>{
    val charInt=Integer.valueOf(c.toString)
    if(charInt !=0 && n%charInt==0){
       l.append(charInt)
    }
  })
  l.length
  }

  def hurdleRace(k: Int, height: Array[Int]): Int = {
    val max=height.max
    if(max>k) max-k else 0
  }

  def circularArrayRotation(a: Array[Int], k: Int, queries: Array[Int]): Array[Int] = {
    val rotationOffset=if(k<a.length) k else k%a.length
    val eles=for(i<-queries) yield {
      var finalEle=i+(rotationOffset-1)
      if(finalEle>=a.length) finalEle=finalEle-a.length
      if(rotationOffset==0) a(i)
      else a(finalEle)
    }
    eles
  }

  def kangaroo(x1: Int, v1: Int, x2: Int, v2: Int): String = {
    // Write your code here
    var r,q=0
    if((v2-v1)!=0) {
      r = (x1 - x2) % (v2 - v1)
      q = (x1 - x2) / (v2 - v1)
    }else {
      if((x1-x2)==0){
        r==0
        q==1
      }
    }
    if(r==0 && q>0) "YES" else "NO"
  }

  def pageCount(n: Int, p: Int): Int = {
    // Write your code here
    val pages=p/2
    val last=n/2
    min(pages,last-pages)
  }

  import java.util

  //rotate array by k steps in right
  def rotate(nums: util.List[Integer], offset: Int): util.List[Integer] = {
    val output = new util.ArrayList[Integer](nums)
    if (nums == null || nums.isEmpty || offset == 0 || offset == nums.size) return output
    val size = nums.size
    val finalOffset = offset % size
    var i = 0
    while ( i < nums.size) {
      if (i + finalOffset > size - 1) output.set(i + finalOffset - size, nums.get(i))
      else output.set(i + finalOffset, nums.get(i))
      i += 1
    }
    output
  }

  def countNonRepeatingSubStrings(s:String,count:Int):Int={
    var lCount=count
    var map:scala.collection.mutable.LinkedHashMap[Char,Int]=scala.collection.mutable.LinkedHashMap.empty
    var result=s.foreach(c=>{
      map.update(c,map.get(c).getOrElse(0)+1)
    })
    val repeat=map.filter( x => x._2>1)
    if(repeat.size==0) lCount=lCount+1
    if(s.length>1) {
      if(repeat.size>0){
        lCount=countNonRepeatingSubStrings(s.substring(s.indexOf(repeat.head._1)+1,s.length),lCount)
      }else {
        lCount=countNonRepeatingSubStrings(s.substring(1,s.length),lCount)
      }
    }
    lCount
  }

  def superReducedString(s:String):String={
    val stack:java.util.Stack[Char]=new util.Stack[Char]
    val deQueue:util.Deque[Char]=new util.LinkedList[Char]()
    val strBuf:StringBuffer=new StringBuffer()
    for(c <- s){
      if(deQueue.getLast==c){
        deQueue.removeLast()
      }else{
        stack.push(c)
      }
    }
    while(!stack.empty()){


    }
    ""
  }


  val mat=Array(Array(11,2,4),Array(4,5,6),Array(10, 8, -12))
  val ar=Array(-4, 3, -9, 0, 4, 1)
  //println(diagonalDifference(mat))
  //plusMinus(ar)
  //staircase(6)
  //tallestCandles
  //extraLongFactorials(30)
  //migratoryBirds(Array(1 ,2, 3, 4, 5, 4, 3, 2, 1, 3, 4))
  //formingMagicSquare(Array(Array(4, 5, 8),Array(2, 4, 1),Array(1, 9, 7)))
  //encrypt("feedthedog")
  //val ranks=Array(100, 90 ,90, 80 ,75 ,60)
  //val player=Array(50, 65, 77, 90, 102)
  val filename = "/home/kapil/workspace/DynamicProgramming/src/test/test.txt"
  val rd=Source.fromFile(filename).bufferedReader()
  val rankSize=rd.readLine().toInt
  val ranks=rd.readLine().split(" ").map(x=>x.toInt)
  val playerPoints=rd.readLine().toInt
  val player=rd.readLine().split(" ").map(x=>x.toInt)
  //climbingLeaderboard(ranks,player)
  val G=Array("7283455864","6731158619","8988242643","3830589324","2229505813","5633845374","6473530293","7053106601","0834282956","4607924137")
  val P=Array("9505","3845","3530")
  //gridSearch(G,P)
  //println(timeInWords(5,13))
  //println(findDigits(1012))
  //kangaroo(0,3,4,2)
  val s="bcada"
  var i=0;
  var count=0;
  for(i<- 0 until s.length){
    count=count+countNonRepeatingSubStrings(s.substring(0,i+1),0);
  }
  println(s"${count}")
}



import java.io._
import scala.io._

object Result {
  def circularArrayRotation(a: Array[Int], k: Int, queries: Array[Int]): Array[Int] = {
    val rotationOffset=if(k<a.length) k else k%a.length
    val eles=for(i<-queries) yield {
      var finalEle=i+(rotationOffset-1)
      if(finalEle>=a.length) finalEle=finalEle-a.length
      if(rotationOffset==0) a(i)
      else a(finalEle)
    }
    eles
  }

}

object Solution2 {
  def main(args: Array[String]) {
    val printWriter = new PrintWriter(sys.env("OUTPUT_PATH"))
    val firstMultipleInput = StdIn.readLine.replaceAll("\\s+$", "").split(" ")
    val n = firstMultipleInput(0).toInt
    val k = firstMultipleInput(1).toInt
    val q = firstMultipleInput(2).toInt
    val a = StdIn.readLine.replaceAll("\\s+$", "").split(" ").map(_.trim.toInt)
    val queries = Array.ofDim[Int](q)
    for (i <- 0 until q) {
      val queriesItem = StdIn.readLine.trim.toInt
      queries(i) = queriesItem
    }
    val result = Result.circularArrayRotation(a, k, queries)
    printWriter.println(result.mkString("\n"))
    printWriter.close()
  }
}
