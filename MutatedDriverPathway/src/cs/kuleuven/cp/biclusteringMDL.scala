package cs.kuleuven.cp

/** 
 *  December 23rd, 2013
 */

import cs.kuleuven.java.mdl._
import cs.kuleuven.matrix._
import cs.kuleuven.constraints.IntWSum
import cs.kuleuven.java.mdl.BiclusterMDLScore
import cs.kuleuven.util.PotentialRegionBuilder
import oscar.cp.modeling._
import oscar.cp.core._
import breeze.stats.distributions.Bernoulli

class biclusteringMDL(dupFile: String,
						multiValuedFile: String,
						queryFile: String,
						rowThreshold: Int,
						colThreshold: Int,
						failureThreshold: Int,
						restartThreshold: Int
					) extends App {
  
  var bSolutionFound = false
   
  def execute() = {
	
	  val delimiter = "\t"
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
	  val bestRows = Array.fill(nR)(0)
	  val bestCols = Array.fill(nC)(0)
		  
	  val pRegionBuilder = new PotentialRegionBuilder(dupMatrix, mulMatrix)
	  
	  //optimization criterion
	  val biclusterArea = sum((0 until nR).map(r => varRows(r))) * sum((0 until nC).map(c => varCols(c))) 
	  
	  cp.maximize(biclusterArea)  subjectTo {
	    
		// 1.Noise constraints on rows
	    for(r <- Rows) {
	      val rWeight = (0 until nC).map(c => (100*dupMatrix.at(r,c) + rowThreshold - 100))	      
	      val coverageConstraint = new IntWSum(rWeight, varCols, 0, varRows(r))
	      cp.add(coverageConstraint)	      
	    }
	
	    // 2.Noise constraints on columns
	    for(c <- Cols) {	    
	      val cWeight = (0 until nR).map(r => (100*dupMatrix.at(r,c) + colThreshold - 100))
	      val aux = new CPVarBool(cp)
	      val colNoiseConstraint = new IntWSum(cWeight, varRows, 0, aux)
	      cp.add(varCols(c) ==> aux)
	    }
	    
	    // 3. Number of rows >= 5
	  }
	  
	  println("Constructing potential regions ...")
	  val pRegions = pRegionBuilder.buildPotentialRegions(cp, varRows, varCols, query)	 
	  println("Constructing potential regions done.")
	  pRegionBuilder.printRunSummary(pRegions)
	  
	  cp.exploration {
	    
	    while(!allBounds(varCols) ) {
	      val unBoundedVars = (0 until varCols.length).filter(c => !varCols(c).isBound)
	      
	      val col = unBoundedVars.head
	      //println("next item: " + col + " - unBounded = " + unBoundedVars.length)
	      cp.branch {cp.post(varCols(col) == 0)} {cp.post(varCols(col) == 1)}
	    }
	    println("\nSolution found:") 
	    printSolution(varRows, varCols)
	    bSolutionFound = true
	    //store the current best solution 
        Rows.foreach(r => bestRows(r) = varRows(r).value)
        Cols.foreach(c => bestCols(c) = varCols(c).value)
	    
	  } run(failureLimit = failureThreshold)
	  
	  for(region <- pRegions) {
		  println("\n************************************************************")
		  println("Set the potential region to ")
		  region.printStatistics
		  
		  for(i <- 0 until restartThreshold) {
		    println("Iteration " + i + "th")
		  
		    cp.runSubjectTo(failureLimit = failureThreshold) {
		    	val outCols = region.getOutsideCols
		       	for(c <- outCols) {
		    		cp.post(varCols(c) == 0)
		    	}
		    	
		    	val posCols 	 = region.getPosCols
		    	val sortedCols 	 = posCols.toVector.sorted
		    	val colDens 	 = (sortedCols).map(c => region.getColDensity(c))		    	
		    	val highProbCols = sortedCols.filter(c => (new Bernoulli(region.getColDensity(c))).sample == true )
		    	
		    	println("Column densities: " + colDens)
		    	println("High prob cols: " + highProbCols)
		    	
		    	for(c <- highProbCols) { 
		    	  cp.post(varCols(c) == 1)	    	  	    	  
		    	}
			  }
		  }
	  
	  }
	  
	  if (bSolutionFound) {
	    println("\nFinal solution: ")
	    printSolution(varRows, varCols)
	    cp.printStats
	  } else {	    
	    println("No solution found in the potential region! ")
	    println("You might consider to increase the thresholds of failures and restarts.")
	    println("Current failure threshold: " + failureThreshold)
	    println("Current restart threshold: " + restartThreshold)
	  }

  }
   
    def printSolution(rows: IndexedSeq[CPVarBool], cols:IndexedSeq[CPVarBool]) = {
	    val selectedCols = (0 until cols.size).filter(c => cols(c).getValue == 1)
	    val selectedRows = (0 until rows.size).filter(r => rows(r).getValue == 1)
	    val originalRows = selectedRows.map(r => java.lang.Math.round(r/2))
	    println()
	    println("Cols = " + selectedCols.sorted + " [" + selectedCols.size + "/" + cols.size + "]")
	    println("Rows = " + originalRows.sorted + " [" + originalRows.size + "/" + rows.size + "]")	    
	}     
}