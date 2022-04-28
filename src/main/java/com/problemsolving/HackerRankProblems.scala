package com.problemsolving

import java.io._
import java.math._
import java.security._
import java.text._
import java.util._
import java.util.concurrent._
import java.util.function._
import java.util.regex._
import java.util.stream._
import scala.collection.immutable._
import scala.collection.mutable._
import scala.collection.concurrent._
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


  val mat=Array(Array(11,2,4),Array(4,5,6),Array(10, 8, -12))
  val ar=Array(-4, 3, -9, 0, 4, 1)
  //println(diagonalDifference(mat))
  //plusMinus(ar)
  staircase(6)
}
