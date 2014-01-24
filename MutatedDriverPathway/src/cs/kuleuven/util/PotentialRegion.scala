package cs.kuleuven.util

import cs.kuleuven.matrix.DenseMatrix

/**
 * Created on Jan 07 2014
 */

/**
 * posRows: indexes in the original matrix (not those in the duplicated matrix)
 * 			Rows are selected if either they are bounded to true or unbounded
 * posCols: indexes of columns
 * mdlScore: MDLScore of the potential region
 * mulTdb: the original (multi-valued) matrix
 * posIndex: an integer value to remember where it is in the scores array.
 * 			When we visualize the scores, the posIndex should be a point where 
 *    		the MDL score is at a local minimum 
 */

class PotentialRegion(posRows: Set[Int], 
    posCols: Set[Int],    
    mdlScore: Int,
    mulTdb: DenseMatrix,
    posIndex: Int) {
  
  def getOutsideCols(): Set[Int] = {
    val cols = (0 until mulTdb.colSize).filter(x => !posCols.contains(x)).toSet
    return cols
  }
  
  def getPosCols() = posCols
  
  def getPosRows() = posRows
  
  def getMDLScore() = mdlScore
  
  def getColDensity(col: Int): Double = {
    val nonzeroValues = (posRows.toVector).filter(r => mulTdb.at(r, col) != 0)
    return ((nonzeroValues.size*1.0)/posRows.size)    
  }
  
  def printStatistics() = {
    println("Index position: " + posIndex)
    println("nCols = " + posCols.size + "/" + mulTdb.colSize + 
        " - nRows = " + posRows.size + "/" + mulTdb.rowSize +
        " - MDLScore = " + mdlScore)
    println("Column & row indexs are zero-indexed values")  
    println("Row indexes are those in the multi-valued matrix")
    println("Cols = " + posCols.toVector.sorted + "")
    println("Rows = " + posRows.toVector.sorted + "\n")
  }
  
  def getRowsRemoved(): Set[Int] = {
    val allRows = (0 until mulTdb.rowSize).map(x => x).toSet
    allRows &~ posRows.toSet
  }
  
  def getDupRowsRemoved(): Set[Int] = {
    val allRows = (0 until mulTdb.rowSize).map(x => x).toSet
    val rmRows = allRows &~ posRows.toSet
    val evenRows = rmRows.map(x => 2*x)
    val oddRows = rmRows.map(x => 2*x + 1)
    evenRows ++ oddRows
  }
  
  def getRegionIndex() = posIndex
  
  def save(rowFile: String, colFile: String, scoreFile: String, indexFile: String, delimiter: String) = {
    
    val s = new Solution(List("rows", "cols", "mdl", "index"))
	
    s.update("rows", getPosRows.toSet)
	s.update("cols", getPosCols.toSet)
	s.update("mdl", scala.collection.immutable.HashSet(getMDLScore))
	s.update("index", scala.collection.immutable.HashSet(getRegionIndex))
	
	s.saveVar("rows", rowFile, delimiter, false)
    s.saveVar("cols", colFile, delimiter, false)
    s.saveVar("mdl", scoreFile, delimiter, false)
    s.saveVar("index", indexFile, delimiter, false)
  }

}