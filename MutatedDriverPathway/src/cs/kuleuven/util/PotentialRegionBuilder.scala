package cs.kuleuven.util

import cs.kuleuven.matrix.DenseMatrix
import cs.kuleuven.java.mdl.BiclusterMDLScore
import oscar.cp.core._
import oscar.cp.modeling._
import scala.collection.immutable.HashSet
import scala.collection.immutable.Vector
import cs.kuleuven.util.PotentialRegion
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
    				cols: IndexedSeq[CPVarBool],    				
    				query: Array[Int]): Set[PotentialRegion] = {
      
    	for(q <- query) {
    		cp.post(cols(q) == 1)
    	}
      
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
        
        while (!allBounds(cols) && !allBounds(rows)) {
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
        	
        	println("Next column: " + nextCol + " - nCols = " + selectedCols2.size + " - nRows = " + possibleRows2.size + " - MDLScore: " + newMDLScore)
          
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
	  println("\nSummary results of constructing potential regions:")
	  println("MDL scores: " + scores2)
	  println("There are " + regions.size + " potential regions ")
	  var i = 0
	  for(region <- regions) {
	    i = i + 1
	    println("Potential region " + i)
	    region.printStatistics
	  }
	  
	}
  		
}