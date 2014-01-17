package cs.kuleuven.cp

/** 
 *  December 23rd, 2013
 */

import cs.kuleuven.java.mdl._
import cs.kuleuven.matrix._
import cs.kuleuven.constraints.IntWSum
import cs.kuleuven.java.mdl.BiclusterMDLScore
import cs.kuleuven.util.PotentialRegionBuilder
import cs.kuleuven.util.Solution
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

class biclusteringMDL(dupFile: String,
						multiValuedFile: String,
						queryFile: String,
						removedRowsFile: String, //zero-indexed row indexes of rows in multi-valued file
						rowThreshold: Int,
						colThreshold: Int,
						failureThreshold: Int,
						restartThreshold: Int,
						iteration: Int
					) extends App {
  
  var bSolutionFound = false
  var bBetterMDLScore = false
  val delimiter = "\t"
    
  def execute() = {
	  
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
	  
	  val bestRows = Array.fill(nR)(0)
	  val bestCols = Array.fill(nC)(0)
		  
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
	      cp.add(varCols(c) ==> aux)
	    }
	    
	    // remove rows
	    if (removedRows !=null) {
	      for(r <- removedRows) {	    
	        cp.add(varRows(r) == 0)
	      }
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
	  
	  println("Constructing potential regions ...")
	  val pRegions = pRegionBuilder.buildPotentialRegions(cp, varRows, varCols, query)	 
	  println("Constructing potential regions done.")
	  pRegionBuilder.printRunSummary(pRegions)
	  
	  if (pRegions.size == 0) {
	    println("\n No potential regions found. Stop.")
	    cp.stop	    
	  }    
	      
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
		
		  // TODOs: modify to restart until either solutions found or 
		  //		the number of restart exceeds a threshold
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
	    
	    //check MDL score to determine whether to continue or not
	    if (iteration == 1) {
	      bBetterMDLScore = true
	      saveSolution(varRows, varCols, iteration)
	    } else if (iteration > 1){
	      bBetterMDLScore = bContinue(iteration, "/home/thanh/test/", pRegionBuilder, varRows, varCols)
	      if (bBetterMDLScore) {
	        saveSolution(varRows, varCols, iteration)
	      }
	    }
	    
	  } else {	    
	    println("No solution found in the potential region! ")
	    println("You might consider to increase the thresholds of failures and restarts.")
	    println("Current failure threshold: " + failureThreshold)
	    println("Current restart threshold: " + restartThreshold)
	  }

  }
   
    def printSolution(rows: IndexedSeq[CPVarBool], cols: IndexedSeq[CPVarBool]) = {
	    val selectedCols = (0 until cols.size).filter(c => cols(c).getValue == 1)
	    val selectedRows = (0 until rows.size).filter(r => rows(r).getValue == 1)
	    val originalRows = selectedRows.map(r => java.lang.Math.round(r/2))
	    println()
	    println("Cols = " + selectedCols.sorted + " [" + selectedCols.size + "/" + cols.size + "]")
	    println("Rows = " + originalRows.sorted + " [" + originalRows.size + "/" + rows.size + "]")	    
	}
    
    def saveSolution(rows: IndexedSeq[CPVarBool], cols: IndexedSeq[CPVarBool], iterationTh: Int) = {
      
      val solu = new Solution(List("rows","cols"))
      solu.update("rows", rows)
      solu.update("cols", cols)
      
      val directory = "/home/thanh/test/"
      val delimiter = "\t"  
      val rowFileName = directory + "rows_" + iterationTh + ".txt"
      val colFileName = directory + "cols_" + iterationTh + ".txt"
      
      solu.saveVar2("rows", rowFileName, delimiter, false) //to convert to the original indexes
      solu.saveVar("cols", colFileName, delimiter, false)
      
      val rmRowsFileName = directory + "removedRows.txt"
      val fileHandler = new java.io.File(rmRowsFileName)
	  if (!fileHandler.exists()) {
	      solu.saveVar2("rows", rmRowsFileName, delimiter, false)
	  } else {
		  val reader = new DenseMatrix(rmRowsFileName, delimiter)
		  val prevResult = (0 until reader.colSize).map(c => reader.at(0, c)).toSet		  
		  val curResult = (0 until rows.size).filter(r => rows(r).getValue == 1).map(r => java.lang.Math.round(r/2)).toSet
		  val uniResult = prevResult union curResult
		  val writer = new java.io.PrintWriter(new java.io.File(rmRowsFileName))
		  uniResult.toVector.sorted.foreach(f => writer.write(f.toString + "\t"))	
		  writer.close()
	  }
    }
    
    def isSolutionFound() = bSolutionFound
    
    def bContinue(iteration: Int, 
    			workingDir: String,
    			pRegionBuilder: PotentialRegionBuilder,
    			curSolRows: IndexedSeq[CPVarBool], 
    			curSolCols: IndexedSeq[CPVarBool]): Boolean = {
      // row indexes saved in solution file are converted to the original values
      // which are 
      val rowSets = (1 until iteration).toVector.map(iter => readSolution(workingDir, iter, "rows", delimiter))
      val colSets = (1 until iteration).toVector.map(iter => readSolution(workingDir, iter, "cols", delimiter))
      
      val prevMDL = pRegionBuilder.getMDLOfNonOverlappingBiclusters(rowSets, colSets)
      
      // current row indexes -> indexes in multi-valued dataset 
      val curRows = (0 until curSolRows.size).filter(r => curSolRows(r).getValue == 1).map(r => java.lang.Math.round(r/2)).toSet
      val curCols = (0 until curSolCols.size).filter(c => curSolCols(c).getValue == 1).toSet
      val newRowSets = rowSets ++ Vector(curRows)
      val newColSets = colSets ++ Vector(curCols)
      
      val curMDL = pRegionBuilder.getMDLOfNonOverlappingBiclusters(newRowSets, newColSets)
      
      if(curMDL < prevMDL) true else false
      
    }
    
    def readSolution(workingDir: String,
    			iteration: Int,
    			prefix: String, 
    			delimiter: String): Set[Int] = {
     
      val filename = workingDir + prefix + "_" + iteration + ".txt"
      val sol = new DenseMatrix(filename, delimiter)
      (0 until sol.colSize).map(x => sol.at(0,x)).toSet
    }
}