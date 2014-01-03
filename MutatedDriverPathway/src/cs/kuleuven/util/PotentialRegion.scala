package cs.kuleuven.util

import cs.kuleuven.matrix.DenseMatrix
import cs.kuleuven.java.mdl.BiclusterMDLScore
import oscar.cp.core._
import oscar.cp.modeling._

/**
 * Created on December 27, 2013 
 * @author thanhle
 */

/**
 * 
 */
class PotentialRegion(binTdb: DenseMatrix, mulTdb: DenseMatrix) {
	// region(0): rows
	// region(1): cols
	val region 	 = new Array[Array[Int]](2)
	val density	 = new Array[Double](binTdb.colSize)
	val mdl = new BiclusterMDLScore() 
    mdl.setTdb(mulTdb); 
	
	def buildPotentialRegion(cp: CPSolver,
    				rows: IndexedSeq[CPVarBool], 
    				cols: IndexedSeq[CPVarBool],
    				//mdl: BiclusterMDLScore,
    				//binTdb: DenseMatrix,
    				//pRegion: PotentialRegion,
    				query: Array[Int]) = {
      
    	for(q <- query) {
    		cp.post(cols(q) == 1)
    	}
      
      	val selectedCols =  (0 until cols.size).filter(c => cols(c).getValue == 1)
        val possibleRows =  (0 until rows.size).filter(r => ((rows(r).isBound && rows(r).getValue == 1) ||
        													(!rows(r).isBound))).map(x => java.lang.Math.round(x/2))
        println("nSelectedCols: " + selectedCols.size)
        println("nPossibleRows: " + possibleRows.size)
        
        mdl.setBicIndexes(possibleRows.map(r => r:Integer).toArray,
        				selectedCols.map(c => c:Integer).toArray )
        
        var bestMDL = mdl.getMDLScore()
        var bStop = false
        while (!bStop) {
          var nextCol = mdl.getNextCompressedCol()
          println("Next column: " + nextCol)
          
          cp.post(cols(nextCol) == 1)
          val selectedCols2 =  (0 until cols.size).filter(c => cols(c).getValue == 1)
          val possibleRows2 =  (0 until rows.size).filter(r => ((rows(r).isBound && rows(r).getValue == 1) ||
        													(!rows(r).isBound))).map(x => java.lang.Math.round(x/2))
        
         println("nSelectedCols: "  + selectedCols2.size + " - nPossibleRows: " + possibleRows2.size)
         //println(selectedCols2)
         //println("nPossibleRows: "  + possibleRows2.size)
         //println(possibleRows2)
         
          mdl.setBicIndexes(possibleRows2.map(r => r:Integer).toArray,
        				selectedCols2.map(c => c:Integer).toArray )
          var newMDLScore = mdl.getMDLScore()
          if (newMDLScore < bestMDL) {
            bestMDL = newMDLScore
          }else {
            bStop = true
          }
        }
      	
      	//cp.run(0)
      	update(rows, cols)
    }
	
	def update(rows: IndexedSeq[CPVarBool], 
						cols:IndexedSeq[CPVarBool]) = {
		
		val posCols = (0 until cols.size).filter(c => (cols(c).isBound && cols(c).getValue == 1))
		val posRows = (0 until rows.size).filter(r => !(rows(r).isBound && rows(r).getValue == 0))
		val colDens = (0 until posCols.size).map(c => 1.0*colSum(c, posRows.toArray, binTdb)/posRows.length)
		
		region.update(0, posRows.toArray)
		region.update(1, posCols.toArray)
		
		for(c <- 0 until posCols.length) {
			density.update(posCols(c), colDens(c))
		}
		
		for(c <- 0 until binTdb.colSize) {
		  if(!posCols.contains(c))
		    density.update(c, 0.0)
		} 
		
	}
	
	def colSum(col: Int, rows: Array[Int], binTdb: DenseMatrix): Int = {
  	  var total = 0
  	  for(row <- rows) {
  	    total += binTdb.at(row, col)
  	  }
  	  return total
  	}
	
	def getOutsideCols(): IndexedSeq[Int] = {
	  val cols = (0 until binTdb.colSize).filter(x => !region(1).contains(x))
	  cols
	}
	
	def getPosCols(): IndexedSeq[Int] = {
	  val cols = region(1).toIndexedSeq
	  return cols
	}
	
	def getColDensity(col: Int): Double = {
	  return density(col)
	}
  	
  	def printRegion() = {
  	  val posCols = region(1)
  	  val posRows = region(0)
  	  
  	  print("Columns: ")  	  
  	  for (c <- posCols) {
  	    print(c + " ")
  	  }
  	  println(" [" + posCols.length + "]")
  	  print("Rows: ")
  	  for ( r <- posRows) {
  	    print( r + " ")
  	  }
  	  println(" [" + posRows.length + "]")    	 
  	}
  	
  	def printDensity() = {
  	  println("Density: ")
  	  for(c <- 0 until binTdb.colSize) { 
  	    println(c + ": " + density(c) )  	    
  	  }
  	}
  	  		
}