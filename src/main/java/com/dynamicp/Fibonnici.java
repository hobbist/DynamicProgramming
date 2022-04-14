package com.dynamicp;

import java.util.ArrayList;
import java.util.Arrays;

public class Fibonnici {
    public static void main(String[] arr){
        int n=20;
        ArrayList<Integer> l=new ArrayList<>();
        for(int i=0;i<=n;i++){
            if(i==0) l.add(i,0);
            else if(i==1) l.add(i,1);
            else l.add(i,l.get(i-1)+l.get(i-2));
        }
     System.out.println(l);
    }

}
