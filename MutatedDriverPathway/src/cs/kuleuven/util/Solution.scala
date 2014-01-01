/**
 * Created on October 24, 2013
 */
package cs.kuleuven.util

import oscar.cp.core.CPVarBool

//import java.io.PrintWriter
import java.io.DataOutputStream;
import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;



class Solution(names: List[String]) {

    val n = names.length
    val values 	 = new Array[scala.collection.mutable.HashSet[Int]](n)
    var objValue = new Integer(0)
      
    val nameMap  = names map { t => (t, names.indexOf(t)) } toMap
    
    def update(varName: String, cpVar: IndexedSeq[CPVarBool]) = {
        val pos = nameMap.get(varName).get
        val selectedValues = (0 until cpVar.size).filter(c => cpVar(c).getValue == 1)
        
        if (values(pos) == null) {
            values(pos) = new scala.collection.mutable.HashSet[Int]()            
            for(v <- selectedValues)
            	values(pos).add(v)
        } else {
            values(pos).clear
            for(v <- selectedValues)
            	values(pos).add(v)
        }
    }
    
    def setObjValue(objVal: Integer) = {
        objValue = objVal
    }
    
    def printVar(varName: String) = {
        val pos = nameMap.get(varName).get
        if(values(pos) == null) {
            println(varName + " is null")
        } else {
        	println( varName + ": " + values(pos) + " [" + values(pos).size + "]")
        }
                
    }
    
    def print() = {
        println("Objective value = " + objValue)
        for(name <- names)
            printVar(name)
    }
    
    def toString(varName: String, delimiter: String): String = {
        val pos = nameMap.get(varName).get
        var result: String = ""
            
        if (values(pos) == null) {
            ""
        } else {
            var i = 0
            for(v <- values(pos)) {
                if (i < values(pos).size - 1)
                	result += v + delimiter
                else
                    result += v
                i +=1
            }
            return result  
        }
    }
    
    def saveVar(varName: String, filename: String, delimiter: String, append: Boolean) = {
        
        var dos: DataOutputStream = null
        try {
        	var outFile = new File(filename);
        	if (append) {
        		dos = new DataOutputStream(new FileOutputStream(filename, true));
        	} else {
        		dos = new DataOutputStream(new FileOutputStream(outFile));
        	}
        	
        	val varValue = toString(varName, delimiter)
        	dos.writeBytes(varValue);
        	dos.writeBytes("\n")
        	dos.close();
        } catch {
            case ioe: IOException => throw new Exception(ioe.getMessage())        
        } 
        /*try{
        	val stream = new PrintWriter(filename)
        	val varValue = toString(varName, delimiter)
        	if (append) {
        	    stream.println()
        	} else {
        	    
        	}
        } catch {
            case ioe: IOException => throw new Exception(ioe.getMessage())
        }*/
    }
    
    
}