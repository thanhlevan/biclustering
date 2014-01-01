/**
 * Created on October 24, 2013
 */
package cs.kuleuven.util

import scala.util.Sorting

class TopKSolutions(k : Integer) {
    
    val solutions = new Array[Solution](k)
    
    def update(s: Solution) = {
        
        val nullSols = solutions.filter(s => s == null)
        
        if (nullSols.size > 0) {
            
            // There are null solutions. Select one of them
            val n = (0 until k).filter(i => solutions(i) == null).head
            solutions.update(n, s)            
        } else {
            // If there are solutions whose objective function value is smaller than the new one's,
            // kick the smallest one out
            
            val smallers = (0 until k).filter(i => solutions(i).objValue < s.objValue)
            if (smallers.size > 0) {
               val sortedSols = smallers.sortBy(i => solutions(i).objValue) 
               val smallestIndex = sortedSols.head
               solutions.update(smallestIndex, s)
            }            
        }
    }
    
    def print() = {
        val sortedSols = solutions.filter(s => s!=null).sortBy(s => s.objValue)
        for(s <- sortedSols) {
            if(s !=null)
                s.print()
        }
            
    }
    
    def save(outMutFile: String, outExpFile: String, outPatFile: String) = {
        
        val sortedSols = solutions.filter(s => s!=null).sortBy(s => s.objValue)
        var first = true;
        for(s <- sortedSols){
            if(s !=null) {
                if (first) {
                    s.saveVar("mutgene", outMutFile, "\t", false)
                    s.saveVar("expgene", outExpFile, "\t", false)
                    s.saveVar("exppat", outPatFile, "\t", false)
                    first = false;
                } else {
                    s.saveVar("mutgene", outMutFile, "\t", true)
                    s.saveVar("expgene", outExpFile, "\t", true)
                    s.saveVar("exppat", outPatFile, "\t", true)
                }
                s.print()
            }
        }
        
    }

}