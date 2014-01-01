package cs.kuleuven.graph

import scala.collection.immutable.Set
import cs.kuleuven.matrix._

class UndirectedGraph(filename: String, delimiter: String) {
	
	val adjMatrix = new DenseMatrix(filename, delimiter)
	
	def getVerticeSize(): Int = adjMatrix.rowSize()
	
	def isConnected(u: Int, v: Int): Boolean =  if(adjMatrix.at(u,v) > 0) true else false
  
	def getNeighbours(u: Int): scala.collection.immutable.Set[Int] = (0 until getVerticeSize()).toSet.filter(v => isConnected(u,v))
	
	//////////////////////////////////////////////////////////////////////////////////////
    //Given a graph and a source node v, get a set of vertices, to which v is connected
    //Parameters:
	//	1. u: source node
    //Return: 
    //////////////////////////////////////////////////////////////////////////////////////
	def getConnectedComponent(u: Int): scala.collection.mutable.Set[Int] = {
		
		var lifo: scala.collection.mutable.Stack[Int] = scala.collection.mutable.Stack()
		var comp: scala.collection.mutable.Set[Int]  = scala.collection.mutable.Set()//scala.collection.mutable.Set[Int]()
		var explored: scala.Array[Boolean] = Array.fill[Boolean](getVerticeSize())(false)
		
		lifo.push(u)
		
		while(lifo.length >0){
		  var v: Int = lifo.pop()		  
		  comp.add(v)
		  if(explored(v)==false){
		    explored(v) = true
		    val neighbours = getNeighbours(v)
		    for(n <- neighbours)
		      lifo.push(n)
		  }
		}
		return comp
	}
	
	//////////////////////////////////////////////////////////////////////////////////////
    //Given a graph and a source node v, get a set of vertices, to which v is connected
    //Parameters:
	//	v: source node
	//	r: set of removed vertices
    //Return: set of vertices
    //////////////////////////////////////////////////////////////////////////////////////
    def getConnectedComponent(u: Int, r: scala.collection.immutable.Set[Int]): scala.collection.mutable.Set[Int] = {
		
		var lifo: scala.collection.mutable.Stack[Int] = scala.collection.mutable.Stack()
		var comp: scala.collection.mutable.Set[Int]  = scala.collection.mutable.Set()
		var explored: scala.Array[Boolean] = Array.fill[Boolean](getVerticeSize())(false)
		
		lifo.push(u)
		
		while(lifo.length >0){
		  var v: Int = lifo.pop()		  
		  comp.add(v)
		  if(explored(v)==false){
		    explored(v) = true
		    val neighbours = getNeighbours(v)
		    for(n <- neighbours){
		      if(!r.contains(n))
		        lifo.push(n)
		    }
		  }
		}
		return comp
	}
    
    
}