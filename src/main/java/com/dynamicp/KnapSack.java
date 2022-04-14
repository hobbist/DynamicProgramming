package com.dynamicp;

import java.util.ArrayList;
import java.util.List;

public class KnapSack {

    public static void main(String[] arr){
        int val[] = new int[] { 60, 100, 120 };
        int wt[] = new int[] { 10, 20, 30 };
        int W = 50;
        int n = val.length;
        List<Integer> l=new ArrayList<Integer>();
        System.out.println(maxWeightProfitRecurcive(val,wt,W,n,l));
        System.out.println(l);
    }

    public static int maxWeightProfitRecurcive(int[] val,int[] wt,int w, int n,List<Integer> l){
        int result=0;
        if(w==0 || n==0) return result;
        if(wt[n-1]>w)
            result = maxWeightProfitRecurcive(val,wt,w,n-1, l);
        else {result= Integer.max(maxWeightProfitRecurcive(val,wt,w,n-1,l), val[n-1]+ maxWeightProfitRecurcive(val,wt,w-wt[n-1],n-1,l));l.add(1);}
        return result;
    }

    public static int maxWeightProfit(int[] val,int[] wt,int w, int n,List<Integer> l){
      int[][] mat=new int[w+1][n+1];

        int result=0;
     if(w==0 || n==0) return result;
     if(wt[n-1]>w)
         result = maxWeightProfitRecurcive(val,wt,w,n-1, l);
     else {result= Integer.max(maxWeightProfitRecurcive(val,wt,w,n-1,l), val[n-1]+ maxWeightProfitRecurcive(val,wt,w-wt[n-1],n-1,l));l.add(1);}


        return 0;
 }

}
