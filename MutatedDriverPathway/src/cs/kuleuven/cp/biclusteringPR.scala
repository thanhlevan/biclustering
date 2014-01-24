package cs.kuleuven.cp

/** 
 *  January 22sd, 2014
 */

import cs.kuleuven.java.mdl._
import cs.kuleuven.matrix._
import cs.kuleuven.constraints.IntWSum
import cs.kuleuven.java.mdl.BiclusterMDLScore
import cs.kuleuven.util.PotentialRegionBuilder
import cs.kuleuven.util.Solution
import cs.kuleuven.util.TopKSolutions
import cs.kuleuven.util.PotentialRegion
import oscar.cp.modeling._
import oscar.cp.core._
import breeze.stats.distributions.Bernoulli

/**
 * Mining fault-tolerant bi-clusters given column/row noise thresholds
 * @param dupFile
 * @param multiValuedFile 
 * @param queryFile 
 * @param removedRowsFile is a filename containing rows that should not be included in solutions.
 * @param rowThreshold
 * @param colTheshold
 * @param failureThreshold
 * @param restartThreshold
 * @param iteration
 */

class biclusteringPR(dupFile: String,
						multiValuedFile: String,
						queryFile: String,
						removedRowsFile: String, //zero-indexed row indexes of rows in multi-valued file
						workingDir: String,
						rowThreshold: Int,
						colThreshold: Int,
						failureThreshold: Int,
						restartThreshold: Int,
						iteration: Int,
						region: PotentialRegion
					) extends App{
  
  var bSolutionFound = false
  var bBetterMDLScore = false
  val delimiter = "\t"
    
  def execute(): Solution = {
	  
	  //   
	  val dupMatrix = new DenseMatrix(dupFile, delimiter)
	  val mulMatrix = new DenseMatrix(multiValuedFile, delimiter)
	  val queryMatrix = new DenseMatrix(queryFile, delimiter)	  
	  val query = (0 until queryMatrix.colSize).map(c => queryMatrix.at(0, c)).toArray
	   	  
	  val nR = dupMatrix.rowSize
	  val nC = dupMatrix.colSize	  	  
	  //parameters
	  
	  val Rows = 0 until nR
	  val Cols	= 0 until nC
	  
	   //decision variables
	  val cp = CPSolver()
	  val varRows =  Rows.map(i => CPVarBool(cp))
	  val varCols =  Cols.map(i => CPVarBool(cp))
	  
	  // removed rows 
	  val removedRows = region.getDupRowsRemoved.toSet
	  println("***")
	  println("RemovedRows: " + removedRows.toVector.sorted)
	  val consideredRows = if(removedRows == null) Rows else Rows.filter(r => !removedRows.contains(r))
	  val consideredVarRows = consideredRows.map(r => varRows(r))
	  // removed columns
	  val removedCols = Cols.toSet &~ region.getPosCols
	  val consideredCols = region.getPosCols.toIndexedSeq
	  val consideredVarCols = consideredCols.map(c => varCols(c))
	  
	  val bestRows = Array.fill(nR)(0)
	  val bestCols = Array.fill(nC)(0)
	  
	  val topK = new TopKSolutions(1)
	  val names = List("rows", "cols")
		  
	  //Randomizer
	  val rand = new scala.util.Random(0)
	  
	  //optimization criterion
	  val biclusterArea = sum((0 until nR).map(r => varRows(r))) * sum((0 until nC).map(c => varCols(c)))
	   
	  cp.maximize(biclusterArea)  subjectTo {
	    
		// 1.Noise constraints on rows
	    for(r <- consideredRows) {
	      val rWeight = (consideredCols).map(c => (100*dupMatrix.at(r,c) + rowThreshold - 100))	      
	      val coverageConstraint = new IntWSum(rWeight, consideredVarCols, 0, varRows(r))
	      cp.add(coverageConstraint)	      
	    }
	
	    // 2.Noise constraints on columns
	    for(c <- consideredCols) {	    
	      val cWeight = (consideredRows).map(r => (100*dupMatrix.at(r,c) + colThreshold - 100))
	      val aux = new CPVarBool(cp)
	      val colNoiseConstraint = new IntWSum(cWeight, consideredVarRows, 0, aux)
	      cp.add(colNoiseConstraint)
	      cp.add(varCols(c) ==> aux)
	    }
	    
	    // remove rows
	    if (removedRows !=null) {
	      for(r <- removedRows) {	    
	        cp.add(varRows(r) == 0)
	      }
	    }
	    
	   // remove cols
	    for (c <- removedCols) {
	      cp.add(varCols(c) == 0)
	    }
	    
	    // constraints for query
	    for (q <- query) {
	      cp.add(varCols(q) == 1)
	    }
	    
	    // 4. Number of rows >= 5
	  } exploration {
	  
	    cp.binaryFirstFail(varCols )
		    
	    bSolutionFound = true
	    println("\nSolution found:")
	    printSolution(varRows, varCols)
	    Rows.foreach(r => bestRows(r) = varRows(r).value)
		Cols.foreach(c => bestCols(c) = varCols(c).value)		
	    
	  } run (failureLimit = failureThreshold)
	  
	  // Large Neighborhood Search	 
	  val colRangeLimit = (0.8*nC).toInt
	  for (r <- 1 to restartThreshold) {
	    
		  println("\nLNS " + r)
		  cp.runSubjectTo(failureLimit = failureThreshold) {
		    
		    val selectedCols = Cols.filter(c => bestCols(c) == 1)
		    //println("accept previous solution: " + selectedCols)
		    
		    for (c <- selectedCols) {
		      cp.post(varCols(c) == 1)
		    }
		    
			for(c <- consideredCols.filter(c => !selectedCols.contains(c))){
				if(rand.nextInt(nC) < colRangeLimit)
			      cp.post(varCols(c) == bestCols(c))
			}
		  }
	  }
	  
	  // return the final solution
	  val s = new Solution(List("cols", "rows"))
	  val selRows = Rows.filter(r => bestRows(r) == 1).map(r => java.lang.Math.round(r/2)).toSet
	  val selCols = Cols.filter(c => bestCols(c) == 1).toSet
	  s.update("cols", selCols )
	  s.update("rows", selRows)
	  s.setObjValue(selRows.size * selCols.size)
	  
	  return s
  }	  
	
  	
  	def getColDensityInRows(col: Int, rows: IndexedSeq[Int], mulTdb: DenseMatrix): Double = {
	    val nonzeroValues = (rows).filter(r => mulTdb.at(r, col) != 0)
	    return ((nonzeroValues.size*1.0)/rows.size)    
  	}
  	
    def printSolution(rows: IndexedSeq[CPVarBool], cols: IndexedSeq[CPVarBool]) = {
	    val selectedCols = (0 until cols.size).filter(c => cols(c).getValue == 1)
	    val selectedRows = (0 until rows.size).filter(r => rows(r).getValue == 1)
	    val originalRows = selectedRows.map(r => java.lang.Math.round(r/2))
	    println()
	    println("Cols " + " [" + selectedCols.size + "/" + cols.size + "]: " + selectedCols.sorted)
	    println("Rows " + " [" + originalRows.size + "/" + rows.size + "]: " + originalRows.sorted )	    
	}
        
   
}