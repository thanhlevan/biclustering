package cs.kuleuven.cp


import cs.kuleuven.util.PotentialRegionBuilder
import cs.kuleuven.util.Solution
import cs.kuleuven.util.PotentialRegion
import cs.kuleuven.matrix.DenseMatrix

//import cs.kuleuven.util.TopKSolutions

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

class CBic (dupFile: String,
						multiValuedFile: String,
						queryFile: String,
						//removedRowsFile: String, //zero-indexed row indexes of rows in multi-valued file
						workingDir: String,
						rowThreshold: Int,
						colThreshold: Int,
						mdlRowThreshold: Int,
						mdlColThreshold: Int,
						failureThreshold: Int,
						restartThreshold: Int//,
						//iteration: Int
					) extends App {
  
  val delimiter = "\t"
  
  
  def mineMultiBiclusters() = {
    //var rmRowsFileName 	= "" ;
    var bStop = false
    var iter = 0
    
    while (!bStop) {
    	iter = iter + 1
    	val rmRowsFileName = if (iter == 1) "" else (workingDir + "removedRows.txt")
    	// Mine a single bicluster
    	val sol = mineSingleBicluster(rmRowsFileName, iter)
    	val bSolFound = if (sol.objValue > 0) true else false
    	
    	if (bSolFound) {
    	    // Check if the MDL Score is better when we add the new solution
    		val bGoodMDLScore = if (bSolFound) bContinue(iter, workingDir, sol) else false
    		if (bGoodMDLScore) {
    			saveSolution(workingDir, sol, iter)
    			
    		} else {
    			println("\nThe MDL score will get worse when we add the new solution found in this iteration!")
    			println("Stop.")   
    			bStop = true
    		}
    	} else {
    		bStop = true
    		println("\nNo solution found. Stop.")
    		
    	}
    	
    }
    
  }
  
  def mineSingleBicluster(removedRowsFile: String, iteration: Int): Solution = {
  
	  val regionBuilder = new miningPotentialRegions (dupFile,
							multiValuedFile,
							queryFile,
							removedRowsFile, //zero-indexed row indexes of rows in multi-valued file
							workingDir,
							mdlRowThreshold,
							mdlColThreshold,
							failureThreshold,
							restartThreshold,
							iteration)
	  //what happens if no region found (or there is no minimal solution)
	  val regions	= regionBuilder.execute
	  
	  if (regions.size == 0) {
		  val s = new Solution(List("rows", "cols"))
		  s.setObjValue(0)
		  return s
		  
	  } else {
		  savePotentialRegionResults(regions, workingDir, iteration)
		  
		  val solutions = regions.map(region => mineBicInPR(region, removedRowsFile, iteration))
		  val optSolu	= solutions.toVector.sortBy(s => s.objValue)(Ordering[Integer].reverse).head
		  
		  if (optSolu.objValue > 0) {
			  println("\n\n******************************************************************************************************")
			  println("\nFinal solution in the iteration " + iteration)
			  optSolu.print
			  println("******************************************************************************************************\n\n")
		  } else {
			  println("No solution found in the detected potential regions")
			  println("You might consider to increase the column noise level as well as the thresholds for failures and restarts")
			  println("Current column noise level: " + colThreshold)
			  println("Current failure threshold: " + failureThreshold)
			  println("Current restart threshold: " + restartThreshold)			  
		  }
		  
		  return optSolu
	  }
  }
  
  def mineBicInPR(region: PotentialRegion, removedRowsFile: String, iteration: Int ): Solution = {
    	  
    println("\nSet potential region to:")
    region.printStatistics
    
    val bicProg = new biclusteringPR (dupFile,
						multiValuedFile,
						queryFile,
						removedRowsFile, //zero-indexed row indexes of rows in multi-valued file
						workingDir,
						rowThreshold,
						colThreshold,
						failureThreshold,
						restartThreshold,
						iteration,
						region)
    
    // Note that row indexes returned by the biclusteringPR are those in the multi-valued matrix
    val solu = bicProg.execute
    println("\nFinal solution when searching in the potential region:")
    solu.print
    solu	  
  }
  
   def saveSolution(directory: String, solu: Solution, iterationTh: Int) = {
      
      val delimiter = "\t"
  
      val rowFileName = directory + "rows_" + iterationTh + ".txt"
      val colFileName = directory + "cols_" + iterationTh + ".txt"
                  
      solu.saveVar("rows", rowFileName, delimiter, false)
      solu.saveVar("cols", colFileName, delimiter, false)
  
      //
      val rmRowsFileName = directory + "removedRows.txt"
      val fileHandler = new java.io.File(rmRowsFileName)
      
	  if (!fileHandler.exists()) {
	      solu.saveVar("rows", rmRowsFileName, delimiter, false)
	      
	  } else {
		  val reader = new DenseMatrix(rmRowsFileName, delimiter)
		  
		  val prevResult = (0 until reader.colSize).map(c => reader.at(0, c)).toSet
		  val curResult = solu.getVarValue("rows").toSet
		  val uniResult = prevResult union curResult
		  
		  val writer = new java.io.PrintWriter(new java.io.File(rmRowsFileName))
		  uniResult.toVector.sorted.foreach(f => writer.write(f.toString + "\t"))	
		  
		  writer.close()
	  }
    }
    
    
    
    def bContinue(iteration: Int, 
    			workingDir: String,    			
    			curSol: Solution): Boolean = {
      // row indexes saved in solution file are converted to the original values
      // which are 
      
      if (iteration == 1) {
        return true
      } else {
	      val rowSets = (1 until iteration).toVector.map(iter => readSolution(workingDir, iter, "rows", delimiter))
	      val colSets = (1 until iteration).toVector.map(iter => readSolution(workingDir, iter, "cols", delimiter))
	      
	      println("rowSets: " + rowSets)
	      println("colSets: " + colSets)
	      
	      val dupMatrix = new DenseMatrix(dupFile, delimiter)
	      val mulMatrix = new DenseMatrix(multiValuedFile, delimiter)
	      val pRegionBuilder = new PotentialRegionBuilder(dupMatrix, mulMatrix)
	      
	      if (pRegionBuilder == null) {
	        println("pRegionBuilder is null")
	      }
	      
	      val prevMDL = pRegionBuilder.getMDLOfNonOverlappingBiclusters(rowSets, colSets)
	      
	      // current row indexes -> indexes in multi-valued dataset 
	      val curRows = curSol.getVarValue("rows").toSet
	      val curCols = curSol.getVarValue("cols").toSet
	      val newRowSets = rowSets ++ Vector(curRows)
	      val newColSets = colSets ++ Vector(curCols)
	      
	      val curMDL = pRegionBuilder.getMDLOfNonOverlappingBiclusters(newRowSets, newColSets)
	      
	      return if(curMDL < prevMDL) true else false
      
      }
      
    }
    
    def readSolution(workingDir: String,
    			iteration: Int,
    			prefix: String, 
    			delimiter: String): Set[Int] = {
     
      val filename = workingDir + prefix + "_" + iteration + ".txt"
      val sol = new DenseMatrix(filename, delimiter)
      (0 until sol.colSize).map(x => sol.at(0,x)).toSet
    }
    
    def savePotentialRegionResults(regions: Set[PotentialRegion], dir: String, iter: Int) = {
      var i = 1
      
      for (region <- regions) {
    	  val rowFile = dir + "iteration_" + iter + "_pr_" + i + "_rows.txt"
    	  val colFile = dir + "iteration_" + iter + "_pr_" + i + "_cols.txt"
    	  val scoFile = dir + "iteration_" + iter + "_pr_" + i + "_score.txt"
    	  val indFile = dir + "iteration_" + iter + "_pr_" + i + "_index.txt"
    	  
    	  region.save(rowFile, colFile, scoFile, indFile, delimiter)
    	  i = i + 1
      }
      
    }
}