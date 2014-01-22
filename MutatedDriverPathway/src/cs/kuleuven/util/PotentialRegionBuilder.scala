package cs.kuleuven.util

import cs.kuleuven.matrix.DenseMatrix
import cs.kuleuven.java.mdl.BiclusterMDLScore
import oscar.cp.core._
import oscar.cp.modeling._
import scala.collection.immutable.HashSet
import scala.collection.immutable.Vector
import cs.kuleuven.util.PotentialRegion
import cs.kuleuven.util.Solution
/**
 * Created on December 27, 2013 
 * @author thanhle
 */

/**
 * 
 */
class PotentialRegionBuilder(binTdb: DenseMatrix, mulTdb: DenseMatrix) {

	val mdl = new BiclusterMDLScore() 
    mdl.setTdb(mulTdb); 

	val scores = Array.fill[Int](binTdb.colSize)(0)
    val regionRows = new Array[Set[Int]](binTdb.colSize)
    val regionCols = new Array[Set[Int]](binTdb.colSize)
	
	/**
	 * Construct potential regions, each of which is a region 
	 * where the MDL score is at a local minimum
	 */
	def buildPotentialRegions(cp: CPSolver,
    				rows: IndexedSeq[CPVarBool], 
    				cols: IndexedSeq[CPVarBool]    				
    				/*query: Array[Int]*/): Set[PotentialRegion] = {
      
    	//for(q <- query) {
    	//	cp.post(cols(q) == 1)
    	//}
      
      	val selectedCols =  (0 until cols.size).filter(c => cols(c).getValue == 1)
        val possibleRows =  (0 until rows.size).filter(r => ((rows(r).isBound && rows(r).getValue == 1) ||
        													(!rows(r).isBound))).map(x => java.lang.Math.round(x/2))
        println("nSelectedCols: " + selectedCols.size)
        println("nPossibleRows: " + possibleRows.size)
        
        mdl.setBicIndexes(possibleRows.map(r => r:Integer).toArray,
        				selectedCols.map(c => c:Integer).toArray )
        
        var curMDL = mdl.getMDLScore()
        var curTrend = "up"
        val length = cols.size - selectedCols.size + 1
        
        
        println("length = " + length + " - scores size = " + scores.size)
        println("col size:" + cols.size)
        println("selectedCols size: " + selectedCols.size)
      	var index = 0
      	
      	scores.update(index, curMDL)      	
      	regionRows.update(index, possibleRows.toSet)
      	regionCols.update(index, selectedCols.toSet)
        
        while (!allBounds(cols) && !allBounds(rows) && !cp.isFailed) {
          var nextCol = mdl.getNextCompressedCol()
                    
          cp.post(cols(nextCol) == 1)
          val selectedCols2 =  (0 until cols.size).filter(c => cols(c).getValue == 1)           
          val possibleRows2 =  (0 until rows.size).filter(r => ((rows(r).isBound && rows(r).getValue == 1) ||
        													(!rows(r).isBound))).map(x => java.lang.Math.round(x/2))
                
          if (possibleRows2.size > 0) {   
        	mdl.setBicIndexes(possibleRows2.map(r => r:Integer).toArray,
        				selectedCols2.map(c => c:Integer).toArray )
        	var newMDLScore = mdl.getMDLScore()
        	index = index + 1
        	
        	println(index + " - Next column: " + nextCol + " - nCols = " + selectedCols2.size + " - nRows = " + possibleRows2.size + " - MDLScore: " + newMDLScore)
          
        	scores.update(index, newMDLScore)
        	regionRows.update(index, possibleRows2.toSet)
        	regionCols.update(index, selectedCols2.toSet)
          }          
        }
      	      	
      	val localMinimums = getLocalMinimums(scores.toVector)
      	val bestRegions = localMinimums.map(l => new PotentialRegion(regionRows(l), 
      											regionCols(l), 
      											scores(l), 
      											mulTdb,
      											l))
      	return bestRegions.toSet      	
    }
	
	def getLocalMinimums(scores: Vector[Int]): Vector[Int] = {
	  
		val nRequiredUp = 15
		val nRequiredDown = 15
		val locals = (0 until scores.length).filter(i => isLocalMinimum(i, scores, nRequiredUp, nRequiredDown)).toVector
		
		return locals	  
	}
	
	def isLocalMinimum(i: Int, scores: Vector[Int], nRequiredDown: Int, nRequiredUp: Int): Boolean = {
	
	  val element = scores(i)
	  val bInRange = if ((i >= nRequiredDown)&&(i + nRequiredUp < scores.length)) true else false 
	  val vLeft = scores.slice(i - nRequiredDown, i + 1)
	  
	  //val isLeftDescending = isDescendingOrdered(vLeft.toList)
	  val bLeft = if ((countElementsGreater(element, vLeft.toList)*1.0/vLeft.size) > 0.92) true else false
	  
	  val vRight = scores.slice(i, i + nRequiredUp + 1)
	  val bRight = if ((countElementsGreater(element, vRight.toList)*1.0/vLeft.size) > 0.92) true else false
	  //val isRightAscending = isAscendingOrdered(vRight.toList)
	  
	  //return bInRange && isLeftDescending && isRightAscending
	  return bInRange && bLeft && bRight
	}
	
	def countElementsGreater(e: Int, l: List[Int]): Int = l match {
	  case Nil => 0
	  case x :: Nil => if (x > e) 1 else 0
	  case x :: xs => if (x > e) 1 + countElementsGreater(e, xs) else (countElementsGreater(e, xs))
	}
	
	def isAscendingOrdered(l:List[Int]): Boolean = l match {
		case Nil => true
		case x :: Nil => true
		case x :: xs => x < xs.head && isAscendingOrdered(xs)
	}
	
	def isDescendingOrdered(l:List[Int]): Boolean = l match {
		case Nil => true
		case x :: Nil => true
		case x :: xs => x > xs.head && isDescendingOrdered(xs)
	}
	
	def printRunSummary(regions: Set[PotentialRegion]) = {
	  val mdlScores = scores.toSeq
	  val scores2 = mdlScores.filter(s => s > 0)
	  println("\nSummary results of constructing potential regions:\n")
	  println("+ MDL scores: " + scores2 +"\n")
	  println("+ " + regions.size + " potential regions found: \n ")
	  var i = 0
	  for(region <- regions) {
	    i = i + 1
	    println("Potential region " + i)
	    region.printStatistics
	  }
	  
	}
	
	/** 
	 *  Calculate the MDL score of a model defined by a set of bi-clusters M = (B1,..Bn)
	 *  @param rowSets is a set vector containing row indexes of the bi-clusters.
	 *  		Row indexes are those in the multi-valued matrix rather than in the duplicated matrix
	 *  @param colSets is a set vector containing col indexes of the bi-clusters
	 *  @return MDL score L(D;M)
	 */
	
	def getMDLOfNonOverlappingBiclusters(rowSets: Vector[Set[Int]],
	    colSets: Vector[Set[Int]]): Int = {
	  
	  var modelLen 		= 0.0
	  var dataLenInside = 0.0
	  var coveredArea 	= 0.0
	  
	  val bicDensities = Array.fill[Int](mdl.getDBDensityMapSize())(0)
	  
	  for(i <- 0 until rowSets.length) {
	    val bicRows = rowSets(i).toArray
	    val bicCols = colSets(i).toArray
	    mdl.setBicIndexes(bicRows.map(x => x:Integer), 
	        bicCols.map(x => x:Integer))
	    
	    modelLen = modelLen + mdl.getModelLength()
	    coveredArea = coveredArea + mdl.getBicArea()
	    dataLenInside = dataLenInside + mdl.getDataLengthInside()
	    
	    // calculate accumulated symbol densities in all of the bi-clusters
	    for(t <- 0 until mdl.getSymbolSize()) {
	      var den = bicDensities(t)
	      bicDensities.update(t, bicDensities(t) + mdl.getBicSymbolDensity(t))
	    }
	        
	  }
	  
	  // calculate data length outside bi-clusters
	  val remain = mdl.getTdb().rowSize * mdl.getTdb().colSize - coveredArea
	  var dataLenOutside = 0.0
	  for(i <- 0 until bicDensities.size) {
	    val symDen = mdl.getDBSymbolDensity(i) - bicDensities(i)
	    dataLenOutside = dataLenOutside + symDen * mdl.log(remain*1.0/symDen, mdl.getLogBase())
	  }
	  return (modelLen + dataLenInside + dataLenOutside).toInt
	  
	}
	
	/**
	 * Save potential regions in files
	 * @param regions set of potential regions that need to be saved 
	 * @rowsFile filename contains row indexes
	 * @colFile filename contains col indexes
	 * @mdlScoreFile filename contains mdl scores of the potential regions
	 * @indexFile filename contains the indexes in the mdl score list where these regions were found
	 */
	
	def savePotentialRegions(regions: Set[PotentialRegion],
	    rowsFile: String,
	    colsFile: String,
	    mdlScoreFile: String,
	    indexFile: String,
	    delimiter: String) = {
	  
	  
	  var first = true
	  
	  for (region <- regions) {
	    val s = new Solution(List("rows", "cols", "mdl", "index"))
	    s.update("rows", region.getPosRows.toSet)
	    s.update("cols", region.getPosCols.toSet)
	    s.update("mdl", scala.collection.immutable.HashSet(region.getMDLScore))
	    s.update("index", scala.collection.immutable.HashSet(region.getRegionIndex))
	    
	    if (first) {
	      s.saveVar("rows", rowsFile, delimiter, false)
	      s.saveVar("cols", colsFile, delimiter, false)
	      s.saveVar("mdl", mdlScoreFile, delimiter, false)
	      s.saveVar("index", indexFile, delimiter, false)
	    } else {
	      s.saveVar("rows", rowsFile, delimiter, false)
	      s.saveVar("cols", colsFile, delimiter, false)
	      s.saveVar("mdl", mdlScoreFile, delimiter, false)
	      s.saveVar("index", indexFile, delimiter, false)
	    }
	    
	  }	  
	  
	}
	
	
  		
}