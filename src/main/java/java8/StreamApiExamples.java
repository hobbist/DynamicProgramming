package java8;

import java.util.*;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

public class StreamApiExamples {
    public static void main(String[] args) {
        new StreamApiExamples().mergeList();
    }

    public void mergeList(){
        //nlogn after ordering based on lowerbound and then by upper bound using curtom comparator
        ArrayList<ArrayList<Integer>> inputList=new ArrayList(
                Arrays.asList(new ArrayList(Arrays.asList(1,4)),
                        new ArrayList(Arrays.asList(3,5)),
                        new ArrayList(Arrays.asList(5,9)),
                        new ArrayList(Arrays.asList(10,13))));
        ArrayList<ArrayList<Integer>> outputList=new ArrayList<>();
        int i=0;
        while(i<inputList.size()){
            ArrayList<Integer> merged = outputList.size()==0 ? null: outputList.get(outputList.size()-1);
            ArrayList<Integer> currentMerge=inputList.get(i);
            //merge boundry Logic
            if(merged!=null){
                if(currentMerge.get(0)<=merged.get(1)){
                    currentMerge.set(0,merged.get(0));
                    currentMerge.set(1,Math.max(currentMerge.get(1),merged.get(1)));
                    outputList.set(outputList.size()-1,currentMerge);
                }else{
                    outputList.add(currentMerge);
                }
            }else{
                outputList.add(currentMerge);
            }
            i=i+1;
        }
        System.out.println(outputList);
    }





    public Map<String,List<String>> getCountryCities(){
        Set<String> country=new HashSet<>(Arrays.asList("INDIA","USA","UK"));
        Set<String> cities=new HashSet<>(Arrays.asList("INDIA=MUMBAI","INDIA=PUNE","INDIA=NAGPUR","BAN=DHAKA"));
        final Map<String,List<String>> countryCities=new HashMap<>();
        final Map<String,List<String>> countryCitiesViaStream=new HashMap<>();
        long startMillis=System.currentTimeMillis();
        //looping
        cities.forEach(x-> {
            String[] ar=x.split("=");
            if(country.contains(ar[0])){
                List<String> l=countryCities.getOrDefault(ar[0],new ArrayList<>());l.add(ar[1]);
                countryCities.put(ar[0],l);
            }
        });
        country.forEach(c-> countryCities.putIfAbsent(c,new ArrayList<>()));
        System.out.println(System.currentTimeMillis()-startMillis);
        System.out.println(countryCities);

        //Stream Api
        startMillis=System.currentTimeMillis();
        Stream<String> c=country.stream();
        c.forEach(x-> countryCitiesViaStream.put(x,cities.parallelStream().map(z->Arrays.asList(z.split("="))).filter(y->y.contains(x)).map(z-> z.get(1)).collect(Collectors.toList())));
        System.out.println(System.currentTimeMillis()-startMillis);
        System.out.println(countryCitiesViaStream);
        System.out.println(new String("a").hashCode());
        return countryCitiesViaStream;
    }

}
