package com.dynamicp;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

/**
 * Hello world!
 *
 */
public class App 
{
    public static void main( String[] args )
    {
        System.out.println( "Hello World!" );
        List<Employee> es=new ArrayList<>();
        es.add(new Employee("M",10));
        es.add(new Employee("M",12));
        es.add(new Employee("F",14));
        es.add(new Employee("M",1));
        es.add(new Employee("F",1));
        es.add(new Employee("M",3));
        es.add(new Employee("F",11));
        es.add(null);
        es.add(null);
        es.add(null);

        //funcational way
//        Collections.sort(es,Comparator.comparing(Employee::getGender).thenComparing(Employee::getTenure));

        //creating compartor with complete logic
        Collections.sort(es,new EmployeeComparator());

        System.out.println(es);
    }
}


class Employee{
    public Employee(String gender,Integer tenure){
        this.gender=gender;
        this.tenure=tenure;
    }

    public String getGender() {
        return gender;
    }

    public void setGender(String gender) {
        this.gender = gender;
    }

    public Integer getTenure() {
        return tenure;
    }

    public void setTenure(Integer tenure) {
        this.tenure = tenure;
    }

    private String gender;
    private Integer tenure;

    @Override
    public String toString() {
        return "{ gender="+this.getGender()+", tenure= "+ this.getTenure()+"}";
    }
}

class EmployeeComparator implements Comparator<Employee>{

    @Override
    public int compare(Employee employee, Employee t1) {
        if(employee==null || t1==null ) return -1;
        int g=employee.getGender().compareToIgnoreCase(t1.getGender());
        if(g==0){
            return employee.getTenure().compareTo(t1.getTenure());
        }
        return g;
    }
}