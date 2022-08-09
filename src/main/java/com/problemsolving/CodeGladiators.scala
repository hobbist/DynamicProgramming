package com.problemsolving

object CodeGladiators extends App{
//missleTime 2022 - Round1
  printToHundredWithoutNumbers
  def missleTime={
    val launchTime= scala.io.StdIn.readLine().split(" ").map(x=>x.toInt)
    val timespan=scala.io.StdIn.readLine().split(" ").map(x=>x.toInt)
    val hitTime=Array.ofDim[Int](2)
    val minutesAdd=launchTime(1)+timespan(1)
    var hourAdd=launchTime(0)+timespan(0)
    var hourCarry=0
    if(minutesAdd>=60) {
      hitTime(1) = minutesAdd - 60
      hourCarry = 1
    }else{
      hitTime(1) = minutesAdd
    }

    hourAdd=hourAdd+hourCarry
    if(hourAdd>=24){
      hitTime(0)=hourAdd-24
    }else{
      hitTime(0)=hourAdd
    }

    println(hitTime.map(x=> if(x<10) "0"+x.toString else x).mkString(" "))
  }

  def buyGifts: Unit ={
    val testCases=scala.io.StdIn.readLine().toInt
    for(i<-0 until testCases){
      val giftsToBuy=scala.io.StdIn.readLine().toInt
      val numberofGifts=scala.io.StdIn.readLine().toInt
      val gifts=scala.io.StdIn.readLine().split(" ").map(z=>z.toDouble).sorted
      var minimumCost:Double=0
      for(j<-0 until giftsToBuy){
        minimumCost=minimumCost+gifts(j)
      }
      println(minimumCost.toString.replace(".0",""))
    }
  }

  def mthLargest={
    //o(mn)
    val inputSize="7".toInt
    val input="25 26 7 8 10 11 79".split(" ").map(_.toInt)
    val m="3".toInt
    val largest=Array.fill[Int](m)(Integer.MIN_VALUE)
    for(i <- 0 until inputSize){
      var temp=input(i)
      for(j<- 0 until largest.size){
        if(temp > largest(j)){
          var t=largest(j)
          largest(j)=temp
          temp=t
        }
      }
      println(largest(largest.size-1))
    }
  }

  def evenOddWar={
    val inputSize="7".toInt
    val input="74 32 31 91 77 88 96 44 23".split(" ").map(_.toInt)
    val map=scala.collection.mutable.Map[Int,Double](1->0,2->0)
    for(ele <- input){
      if(ele%2==0)map.put(1,map.get(1).get+ele)
      else map.put(2,map.get(2).get+ele)
    }
    if(map.get(1).get > map.get(2).get) println("Even")
    if(map.get(2).get > map.get(1).get) println("odd")
    if(map.get(2).get == map.get(1).get) println("Tied")
  }

  def findModalValue={
    val inputSize="7".toInt
    val input="6 3 9 6 6 5 9 3".split(" ").map(_.toInt)
    val map=scala.collection.mutable.Map.empty[Int,Double]
    for(ele<- input){
      map.put(ele,1 + map.getOrElse(ele,0.toDouble))
    }
    println(map.maxBy(x=>x._2)._1)
  }

  def printToHundredWithoutNumbers(): Unit ={
    val one="t".length
    val eight = one << one << one << one
    val ten = eight+one+one
    val hundred = ten * ten
    var i=one
    while(i<=hundred){
      println(i)
      i=i+one
    }
  }


}
