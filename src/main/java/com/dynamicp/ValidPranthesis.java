package com.dynamicp;

public class ValidPranthesis {
public static void main(String[] arr){
    int n = 3;
    char[] str = new char[2 * n];
    printParenthesis(str, n);
    System.out.println(validate(str,str.length));
}
static boolean validate(char[] c,int n){
    boolean valid=true;
    int oCount=0;
    int clCount=0;
    System.out.println(n);
    for(int i=0;i<n;i++){
        if(c[i]=='{'){
            oCount=oCount+1;
            System.out.println("oCount "+oCount);
        }
        if(c[i]=='}'){
            clCount=clCount+1;
            System.out.println("oCount2 "+ oCount);
        }
    }
    return oCount-clCount==0;
}
    static void _printParenthesis(char str[], int pos, int n, int open, int close)
    {
        if(close == n)
        {
            // print the possible combinations
            for(int i=0;i<str.length;i++)
                System.out.print(str[i]);
            System.out.println();
            return;
        }
        else
        {
            if(open > close) {
                str[pos] = '}';
                _printParenthesis(str, pos+1, n, open, close+1);
            }
            if(open < n) {
                str[pos] = '{';
                _printParenthesis(str, pos+1, n, open+1, close);
            }
        }
    }

    // Wrapper over _printParenthesis()
    static void printParenthesis(char str[], int n)
    {
        if(n > 0)
            _printParenthesis(str, 0, n, 0, 0);
        return;
    }

}
