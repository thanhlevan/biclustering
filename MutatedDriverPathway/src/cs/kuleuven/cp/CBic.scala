package cs.kuleuven.cp


import cs.kuleuven.util.PotentialRegionBuilder
import cs.kuleuven.util.Solution

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
						removedRowsFile: String, //zero-indexed row indexes of rows in multi-valued file
						workingDir: String,
						rowThreshold: Int,
						colThreshold: Int,
						failureThreshold: Int,
						restartThreshold: Int,
						iteration: Int
					) extends App {
  
  def execute() = {
  
	  val regionBuilder = new buildPotentialRegions (dupFile,
							multiValuedFile,
							queryFile,
							removedRowsFile, //zero-indexed row indexes of rows in multi-valued file
							workingDir,
							rowThreshold,
							colThreshold,
							failureThreshold,
							restartThreshold,
							iteration)
	  
	  val regions = regionBuilder.execute
	  
	  for (region <- regions) {
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
	    
	    val solu = bicProg.execute
	    println("\nFinal solution:")
	    solu.print
	  }
  }
}