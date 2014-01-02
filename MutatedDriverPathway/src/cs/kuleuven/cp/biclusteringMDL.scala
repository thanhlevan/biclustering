package cs.kuleuven.cp

/** 
 *  December 23rd, 2013
 */

import cs.kuleuven.java.mdl._
import cs.kuleuven.matrix._
import cs.kuleuven.constraints.IntWSum
import cs.kuleuven.java.mdl.BiclusterMDLScore
import cs.kuleuven.util.PotentialRegion
import oscar.cp.modeling._
import oscar.cp.core._
import breeze.stats.distributions.Bernoulli
import scala.util.continuations.{cps, cpsParam}

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
	  //expMatrix.printMatrix
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
		  
	  val pRegion = new PotentialRegion(dupMatrix, mulMatrix)
	  
	  //optimization criterion
	  val biclusterArea = sum((0 until nR).map(r => varRows(r))) * sum((0 until nC).map(c => varCols(c))) 
	  
	  cp.maximize(biclusterArea)  subjectTo {
	    
		// 1.Noise constraints on rows
	    for(r <- Rows) {
	      val rWeight = (0 until nC).map(c => (100*dupMatrix.at(r,c) + rowThreshold - 100))	      
	      val coverageConstraint = new IntWSum(rWeight, varCols, 0, varRows(r))
	      cp.add(coverageConstraint)	      
	    }
	
	    //2.Noise constraints on columns
	    for(c <- Cols) {
	      //val colSum = sum((0 until nR).map(r => varRows(r) * (100*dupMatrix.at(r,c) + colThreshold - 100) ))
	      //cp.add(varCols(c) ==> (colSum >== 0))
	      val cWeight = (0 until nR).map(r => (100*dupMatrix.at(r,c) + colThreshold - 100))
	      val aux = new CPVarBool(cp)
	      val colNoiseConstraint = new IntWSum(cWeight, varRows, 0, aux)
	      cp.add(varCols(c) ==> aux)
	    }
	  }
	  
	  println("Constructing the potential region ...")
	  pRegion.buildPotentialRegion(cp, varRows, varCols, query)	 
	  println("...done")
	  pRegion.printRegion
	  pRegion.printDensity
	  
	  cp.exploration {
	    
	    while(!allBounds(varCols) ) {
	      val unBoundedVars = (0 until varCols.length).filter(c => !varCols(c).isBound)
	      
	      val col = unBoundedVars.head
	      println("next: " + col + " - unBounded = " + unBoundedVars.length)
	      cp.branch {cp.post(varCols(col) == 0)} {cp.post(varCols(col) == 1)}
	    }
	     
	    printSolution(varRows, varCols)
	    bSolutionFound = true
	    //store the current best solution 
        Rows.foreach(r => bestRows(r) = varRows(r).value)
        Cols.foreach(c => bestCols(c) = varCols(c).value)
	    
	  } run(failureLimit = 100)
	  
	  for(i <- 0 until restartThreshold) {
	    println("Restart " + i + "th")
	  
	    cp.runSubjectTo(failureLimit = failureThreshold) {
	    	val outCols = pRegion.getOutsideCols
	       	for(c <- outCols) {
	    		cp.post(varCols(c) == 0)
	    	}
	    	
	    	val posCols = pRegion.getPosCols	    		  
	    	//val sortedDenCols = unBoundedCols.sortBy(c => pRegion.getColDensity(c))(Ordering[Double].reverse)	    	
	    	val highProbCols: scala.collection.mutable.Set[Int] = new scala.collection.mutable.HashSet()
	    	for(c <- posCols) {
	    	  val bern = new Bernoulli(pRegion.getColDensity(c))
	    	  if (bern.sample)
	    	    highProbCols += c
	    	}
	    	
	    	println("High prob cols: " + highProbCols)
	    	for(c <- highProbCols) { 
	    	  cp.post(varCols(c) == 1)	    	  	    	  
	    	}
		  }
	  }
	  
	  if (bSolutionFound) {
	    println("Final solution: ")
	    printSolution(varRows, varCols)
	    cp.printStats
	  } else {
	    
	    println("No solution found in the potential region! ")
	    println("You might consider to increase the failture threshold and number of restarting times")
	    println("Potential region:")
	    pRegion.printRegion
	    
	  }

  }
   
    def printSolution(rows: IndexedSeq[CPVarBool], cols:IndexedSeq[CPVarBool]) = {
	    val selectedCols = (0 until cols.size).filter(c => cols(c).getValue == 1)
	    val selectedRows = (0 until rows.size).filter(r => rows(r).getValue == 1)
	    println()
	    println("Cols = " + selectedCols + " [" + selectedCols.size + "/" + cols.size + "]")
	    println("Rows = " + selectedRows + " [" + selectedRows.size + "/" + rows.size + "]")	    
	}     
}