package cs.kuleuven.cp

import oscar.cp.modeling._
import oscar.cp.core._
import cs.kuleuven.matrix.DenseMatrix
import cs.kuleuven.constraints.IntWSum

object biclusteringLNS extends App {
  def execute = {
	  val delimiter = "\t"
      //val dupFile = "/home/thanh/data/biclusters/8modules/data/noise_0.05/dup_bg_0.05_bic.0.05_8modules.txt"
	  val dupFile = "/home/thanh/data/1staircase/data/step_0.10/dup_bgnoise_0.05_step_0.10.txt"
	  //   
	  val dupMatrix = new DenseMatrix(dupFile, delimiter)	
	 	   
	  //expMatrix.printMatrix
	  val nR = dupMatrix.rowSize
	  val nC = dupMatrix.colSize	  	  
	  //parameters
	  val rowThreshold = 25
	  val colThreshold = 25	    
	  //
	  val Rows = 0 until nR
	  val Cols	= 0 until nC	  	  
	  //decision variables
	  val cp = CPSolver()
	  val varRows =  Rows.map(i => CPVarBool(cp))
	  val varCols =  Cols.map(i => CPVarBool(cp))
	  val bestRows = Array.fill(nR)(0)
	  val bestCols = Array.fill(nC)(0)
	  //Randomizer
	  val rand = new scala.util.Random(0)
	  //optimization criterion
	  val biclusterArea = sum((0 until nR).map(r => varRows(r))) * sum((0 until nC).map(c => varCols(c))) 
  
	  cp.maximize(biclusterArea)  subjectTo {
    
	    //1.Noise constraints on rows
	    for(r <- Rows){
	      //val rowSum = sum((0 until nC).map(c => varCols(c) * (100*dupMatrix.at(r,c) + rowThreshold - 100) ))	      
	      //cp.add(varRows(r) === (rowSum >== 0))
	      val rWeight = (0 until nC).map(c => (100*dupMatrix.at(r,c) + rowThreshold - 100))	      
	      val coverageConstraint = new IntWSum(rWeight, varCols, 0, varRows(r))
	      cp.add(coverageConstraint)	      
	    }
    
	    //2.Noise constraints on columns
	    for(c <- Cols){
	      //val colSum = sum((0 until nR).map(r => varRows(r) * (100*dupMatrix.at(r,c) + colThreshold - 100) ))
	      //cp.add(varCols(c) ==> (colSum >== 0))
	      val cWeight = (0 until nR).map(r => (100*dupMatrix.at(r,c) + colThreshold - 100))
	      val aux = new CPVarBool(cp)
	      val colNoiseConstraint = new IntWSum(cWeight, varRows, 0, aux)
	      cp.post(varCols(c) ==> aux)
	    }
	  }exploration{
	    cp.binaryFirstFail(varCols)
	    printSolution(varRows, varCols)
	    //store the current best solution 
        Rows.foreach(r => bestRows(r) = varRows(r).value)
        Cols.foreach(c => bestCols(c) = varCols(c).value)
	  }run(1)
	  
	  //large neighborhood search
	  var limit = 1400 // set the limit to 100 backtracks for LNS restarts
	  val colRangeLimit = (0.8*nC).toInt
	  for (r <- 1 to 100) {
		  // adapt the backtrack limit for next run *2 is previous run reached the limit /2 otherwise
		  //limit = if (cp.explorationCompleted) limit/2 else limit*2
		  //println("set backtrack limit to "+limit)     
		  // relax randomly 50% of the variables and run again
		  cp.runSubjectTo(failureLimit = limit) {
			  //cp.post((Cols).filter(c => rand.nextInt(nC) < colRangeLimit).map(i => varCols(i) == cp.lastSol(varCols(i))))
			  //cp.post( (Cols).filter(i => rand.nextInt(nC) < 50).map(i => varCols(i) == bestCols(i) ))		    
			  for(c <- Cols){
			    if(rand.nextInt(nC) < colRangeLimit)
			      cp.post(varCols(c) == bestCols(c))
			  }
		  }
	  }
	  
	  cp.printStats()
  }  
	  def printSolution(rows: IndexedSeq[CPVarBool], cols:IndexedSeq[CPVarBool]) = {
	    val selectedCols = (0 until cols.size).filter(c => cols(c).getValue == 1)
	    val selectedRows = (0 until rows.size).filter(r => rows(r).getValue == 1)
	    println()
	    println("Cols = " + selectedCols + " [" + selectedCols.size + "/" + cols.size + "]")
	    println("Rows = " + selectedRows + " [" + selectedRows.size + "/" + rows.size + "]") 
	    
	  }
}