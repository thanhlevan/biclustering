/**
 *
 */
package cs.kuleuven.matrix

/**
 * @author thanh
 *
 */

import scala.io.Source

class DenseMatrix(filename: String, delimiter: String) {
  
   
  def readTable(file: String, delimiter: String): Array[Array[Int]] = {
	   Source.fromFile(file).getLines().map(line => line.split('\t').map(_.toInt ).toArray).toArray
  }
  
  val denseMatrix = readTable(filename, delimiter)
  
  def rowSize(): Int = denseMatrix.size 
  
  def colSize(): Int = if(denseMatrix.size <=0 ) 0 else denseMatrix(0).size
    
  def at(i: Int, j: Int): Int = {
    if(i < 0 || i > rowSize)
      throw new IllegalArgumentException("row index is beyond the boundary")
    if(j < 0 || j > colSize)
      throw new IllegalArgumentException("column index is beyond the boundary")
    
    denseMatrix(i)(j)
    
  }
  
  def setValue(i: Int, j: Int, value: Int) = {
    denseMatrix(i)(j) = value
  }
  
  def printMatrix() = {
    for(a <- denseMatrix){
      for(i <- a) {
        print(i + "\t")
      }
      println()
    } 
  }

}