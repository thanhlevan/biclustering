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

class miningPotentialRegions(dupFile: String,
						multiValuedFile: String,
						queryFile: String,
						removedRowsFile: String, //zero-indexed row indexes of rows in multi-valued file
						workingDir: String,
						rowThreshold: Int,
						colThreshold: Int,
						failureThreshold: Int,
						restartThreshold: Int,
						iteration: Int
					) extends App{
  
  var bSolutionFound = false
  var bBetterMDLScore = false
  val delimiter = "\t"
    
  def execute(): Set[PotentialRegion] = {
	  
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
	  
	  // related to rows which are found in the previous iteration and 
	  // need to be removed 
	  val rmMatrix = if (removedRowsFile.isEmpty()) null else (new DenseMatrix(removedRowsFile, delimiter))
	  val rmRows = if (rmMatrix == null) null else (0 until rmMatrix.colSize).map(c => rmMatrix.at(0,c))
	  val rmEvenRows = if (rmMatrix == null) null else (0 until rmMatrix.colSize).map(c => 2*rmMatrix.at(0, c)).toSet
	  val rmOddRows  = if (rmMatrix == null) null else (0 until rmMatrix.colSize).map(c => (2*rmMatrix.at(0, c) + 1)).toSet
	  val removedRows = if (rmMatrix == null) null else (rmEvenRows union rmOddRows).toVector
	  if (!removedRowsFile.isEmpty()) {
		  println("Rows removed " + "[" + rmRows.size + "]: " + rmRows)
		  println("Duplicated rows removed " + "[" + removedRows.size + "]: " + removedRows)
	  }
	  //
	  val consideredRows = if(removedRows == null) Rows else Rows.filter(r => !removedRows.contains(r))
	  val consideredVarRows = consideredRows.map(r => varRows(r))
	  
	  //val bestRows = Array.fill(nR)(0)
	  //val bestCols = Array.fill(nC)(0)
	  val topK = new TopKSolutions(1)
	  val names = List("rows", "cols")
		  
	  val pRegionBuilder = new PotentialRegionBuilder(dupMatrix, mulMatrix)
	  
	  //optimization criterion
	  val biclusterArea = sum((0 until nR).map(r => varRows(r))) * sum((0 until nC).map(c => varCols(c)))
	   
	  cp.maximize(biclusterArea)  subjectTo {
	    
		// 1.Noise constraints on rows
	    for(r <- consideredRows) {
	      val rWeight = (0 until nC).map(c => (100*dupMatrix.at(r,c) + rowThreshold - 100))	      
	      val coverageConstraint = new IntWSum(rWeight, varCols, 0, varRows(r))
	      cp.add(coverageConstraint)	      
	    }
	
	    // 2.Noise constraints on columns
	    for(c <- Cols) {	    
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
	    
	    // constraints for query
	    for (q <- query) {
	      cp.add(varCols(q) == 1)
	    }
	    
	    // 4. Number of rows >= 5
	  } 
	  
	  // Check to see how the solver removes rows
	  val rmvRows = Rows.filter(r => varRows(r).isBound && varRows(r).getValue == 0)
	  val rmCols = Cols.filter(c => varCols(c).isBound && varCols(c).getValue == 0)
	  val slCols = Cols.filter(c => varCols(c).isBound && varCols(c).getValue == 1)
	   
	  println("After propagating constraints: ")
	  println("Removed rows [" + + rmvRows.size + "]: " + rmvRows.toVector)
	  println("Removed cols: " + rmCols.toVector)
	  println("Selected cols: " + slCols.toVector)
	  	  	    
	  println("\nConstructing potential regions ...\n")
	  val pRegions = pRegionBuilder.buildPotentialRegions(cp, varRows, varCols)	 
	  println("\nConstructing potential regions done.")
	  pRegionBuilder.printRunSummary(pRegions)
	  	  
	  cp.stop
	  
	  return pRegions	  
  }
   
 }