package cs.kuleuven.cp

import oscar.cp.modeling._
import oscar.cp.core._
import cs.kuleuven.matrix.DenseMatrix
import cs.kuleuven.constraints.IntWSum

object biclustering extends App {

	  val delimiter = "\t"
      val dupFile = "/home/thanh/Rlib/MutDriverPathways/Generators/dup_expression_data.txt"	  
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
	  
	  //optimization criterion
	  val biclusterArea = sum((0 until nR).map(r => varRows(r))) * sum((0 until nC).map(c => varCols(c)))
	  
	  
	  cp.maximize(biclusterArea)  subjectTo {
    
	    //1.Noise constraints on rows
	  
	    for(r <- Rows){
	      //val rowSum = sum((0 until nC).map(c => varCols(c) * (100*dupMatrix.at(r,c) + rowThreshold - 100) ))	      
	      //cp.add(varRows(r) === (rowSum >== 0))
	    	val rWeight = (0 until nC).map(c => (100*dupMatrix.at(r,c) + rowThreshold - 100))
	    	val coverageConstraint = new IntWSum(rWeight, varCols, 0, varRows(r))
	    	cp.post(coverageConstraint)	
	    }
    
	    //2.Noise constraints on columns
	    for(c <- Cols){
	      //val colSum = sum((0 until nR).map(r => varRows(r) * (100*dupMatrix.at(r,c) + colThreshold - 100) ))
	      //cp.add(varCols(c) ==> (colSum >== 0))
	      val cWeight = (0 until nR).map(r => (100*dupMatrix.at(r,c) + colThreshold - 100))
	      val aux = new CPVarBool(cp)
	      val colNoiseConstraint = new IntWSum(cWeight, varRows, 0, aux)
	       cp.post(colNoiseConstraint)
	      cp.post(varCols(c) ==> aux)
	    }
	  }exploration{
	    cp.binaryFirstFail(varCols)
	    //   println("Start exploring ...")
	      // println(varRows)
	       //labelling(cp, varCols)
	       printSolution(varRows, varCols)
	  }run()
	  
	  cp.printStats()
	  println(varRows)
	  println("IsFail = " + cp.isFailed)
	  
	  def printSolution(rows: IndexedSeq[CPVarBool], cols:IndexedSeq[CPVarBool]) = {
	    val selectedCols = (0 until cols.size).filter(c => cols(c).getValue == 1)
	    val selectedRows = (0 until rows.size).filter(r => rows(r).getValue == 1)
	    println()
	    println("Cols = " + selectedCols + " [" + selectedCols.size + "/" + nC + "]")
	    println("Rows = " + selectedRows + " [" + selectedRows.size + "/" + nR + "]")	    
	  }
	   
	  def labelling(cp: CPSolver, vars: IndexedSeq[CPVarBool]) = {
	       println(vars)
	       println("All bound:" + allBounds(vars))
		  while(!allBounds(vars)){
		    val unBoundedVar = (0 until vars.size).filter(v => !vars(v).isBound).toSet
		    
		    val next = unBoundedVar.head
		    cp.branch{
		        print("var " + next + " = 0")
		        cp.post(vars(next) == 0 )
		      
		    } {
		        println("var " +next + " = 1")
		        cp.post(vars(next) == 1)		        
		    }
		  }
	} 
  
}