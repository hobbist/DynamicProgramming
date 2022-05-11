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


  val mat=Array(Array(11,2,4),Array(4,5,6),Array(10, 8, -12))
  val ar=Array(-4, 3, -9, 0, 4, 1)
  //println(diagonalDifference(mat))
  //plusMinus(ar)
  //staircase(6)
  tallestCandles
}
