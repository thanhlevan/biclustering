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
	    /*while(!allBounds(varCols) ) {
	      
	      val unBoundedCols = (0 until varCols.length).filter(c => !varCols(c).isBound)
	      val unBoundedRows = Rows.filter(r => !varRows(r).isBound || (varRows(r).isBound && varRows(r).getValue == 1))
	      
	      if (unBoundedRows.size >= region.getPosRows.size) {
	    	  val sortedDenCols = unBoundedCols.toVector.sortBy(col => region.getColDensity(col))(Ordering[Double].reverse)
		      val c = sortedDenCols.head
		  
		      val sampledValue = (new Bernoulli(region.getColDensity(c))).sample
		      val lhsValue = if (sampledValue == true) 1 else 0
		      val rhsValue = if (sampledValue == true) 0 else 1
		      println("From PR - next col: " + c + " - " + region.getColDensity(c) + " - sampledValue = " + sampledValue + " - nPossibleRows = " + unBoundedRows.size)		  
		      cp.branch {cp.post(varCols(c) == lhsValue)} {cp.post(varCols(c) == rhsValue)}
	      } else {
	        val convertedRows = unBoundedRows.map(r => java.lang.Math.round(r/2))
	        val colDens = unBoundedCols.map(c => getColDensityInRows(c, convertedRows, mulMatrix))
	        val sortedDenCols = (0 until unBoundedCols.size).sortBy(col => colDens(col))(Ordering[Double].reverse)
	        val c = unBoundedCols(sortedDenCols.head)
	        val den = colDens(sortedDenCols.head)
		    
	        val lhsValue = if (den*100 >= (100 - 0.9*colThreshold)) 1 else 0
		    val rhsValue = if (den*100 >= (100 - 0.9*colThreshold)) 0 else 1
		    println("next col: " + c + " - " + den + " - lhsValue = " + lhsValue + " - nPossibleRows = " + unBoundedRows.size)//sampledValue = " + sampledValue)
		    cp.branch {cp.post(varCols(c) == lhsValue)} {cp.post(varCols(c) == rhsValue)}
	      }
	    }*/
	    
	    bSolutionFound = true
	    println("\nSolution found:")
	    printSolution(varRows, varCols)
	    Rows.foreach(r => bestRows(r) = varRows(r).value)
		Cols.foreach(c => bestCols(c) = varCols(c).value)
		
	    
	  } run (failureLimit = failureThreshold)
	  
	  val s = new Solution(List("rows", "cols"))
	  s.update("rows", Rows.filter(r => bestRows(r) == 1).toSet)
	  s.update("cols", Cols.filter(c => bestCols(c) == 1).toSet)
	  
	  return s
  }	  
	  
   
  	def restart(cp: CPSolver) = {
  	  cp.startSubjectTo(failureLimit = 1) {
  	    cp.fail
  	  }
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
        
    def saveSolution(directory: String, solu: Solution, iterationTh: Int) = {
      
      val delimiter = "\t"
      val rowFileName = directory + "dup_rows_" + iterationTh + ".txt"  
      val rowFileName2 = directory + "rows_" + iterationTh + ".txt"
      val colFileName = directory + "cols_" + iterationTh + ".txt"
                  
      solu.saveVar("rows", rowFileName, delimiter, false)
      solu.saveVar("cols", colFileName, delimiter, false)
      
      // convert to the original indexes
      val solu2 = new Solution(List("rows", "cols"))
      val rows2 = solu.getVarValue("rows").map(r => java.lang.Math.round(r/2)).toSet
      solu2.update("rows", rows2)
      solu2.saveVar("rows", rowFileName2, delimiter, false)
      
      //
      val rmRowsFileName = directory + "removedRows.txt"
      val fileHandler = new java.io.File(rmRowsFileName)
	  if (!fileHandler.exists()) {
	      solu2.saveVar("rows", rmRowsFileName, delimiter, false)
	  } else {
		  val reader = new DenseMatrix(rmRowsFileName, delimiter)
		  val prevResult = (0 until reader.colSize).map(c => reader.at(0, c)).toSet		  
		  //val curResult = (0 until rows.size).filter(r => rows(r).getValue == 1).map(r => java.lang.Math.round(r/2)).toSet
		  var curResult = solu.getVarValue("rows").map(r => java.lang.Math.round(r/2))
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