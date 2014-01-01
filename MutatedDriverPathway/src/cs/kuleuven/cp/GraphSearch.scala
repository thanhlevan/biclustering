package cs.kuleuven.cp

import oscar.cp.modeling._
import oscar.cp.core._
import cs.kuleuven.graph._
import scala.util.continuations.{cps, cpsParam}

class GraphSearch(filename: String, delimiter: String) {
  
	val graph = new UndirectedGraph(filename, delimiter)
	val vertexSize: Int = graph.getVerticeSize()
	
	def isAllBound(vars: IndexedSeq[CPVarBool]):Boolean = vars.filter(!_.isBound).isEmpty
	def printVarStatus(vars: IndexedSeq[CPVarBool]) = {
	  print("(")
	  for(v <- vars){
	    if(!v.isBound)
	      print("[0,1],")
	    else
	      print(v.getValue + ",")
	  }
	  println(")")
	}
	
	
	def labelConnectedSubGraph2(cp: CPSolver, vertices: IndexedSeq[CPVarBool]) = {
	  
		var lifo: scala.collection.mutable.Stack[Int] = scala.collection.mutable.Stack()		
		var explored: scala.Array[Boolean] = Array.fill[Boolean](graph.getVerticeSize())(false)
		val Vertices = 0 until vertices.length
		println("vertices length= " + vertices.length)
		//Randomly pickup one node
		val initNode = scala.util.Random.shuffle(Vertices.toList).head
		//println("Initially selected node: " + initNode)
		lifo.push(initNode)
		
		while(!isAllBound(vertices)){	
			
			if(lifo.length == 0){
				
				//case 2: have just finished one round of DFS and now it backs track
				//Note: all unbounded vertices should be marked unexplored.
			   
			    //print("Before propagations")
			    //printVarStatus(vertices)
			    var nextVertexId  = - 1 
				var unBoundIndexes = Vertices.filter(v => !vertices(v).isBound)
				for(v <- unBoundIndexes){
				  //print(v)
				  explored(v) = false
				}
				//println("Unbounded vertices" + unBoundedIndexes)
				//do manual propagations to remove nodes that cannot connect with the currently selected nodes
				val unBoundedVertices = Vertices.filter(v => !vertices(v).isBound).toSet 
				var unBoundedVertices2 = scala.collection.mutable.Set(unBoundedVertices.toSeq:_*)
				val selectedVertices  = Vertices.filter(v => vertices(v).isBound && vertices(v).isBoundTo(1) ).toSet
				val removedVertices   = Vertices.filter(v => vertices(v).isBound && vertices(v).isBoundTo(0)).toSet
				
				val existSelectedVertices = if(selectedVertices.size > 0) true else false
				val existRemovedVertices  = if(removedVertices.size > 0) true else false
				
								
				if(!existSelectedVertices && existRemovedVertices){
					
					nextVertexId = unBoundedVertices.head
					
				}else if(existSelectedVertices && !existRemovedVertices){					
					//Look for a vertex whose value is unassigned and it is a neighbor of one of vertices assigned 1
					var temp = scala.collection.mutable.Set(selectedVertices.toSeq:_*)//convert to mutable set
					var bFound = false					
					while (!bFound && (temp.size > 0)){
						var v = temp.head
						temp.remove(v)
						
						var neighbors = graph.getNeighbours(v)						
						var candidates = neighbors &  unBoundedVertices2
						if(candidates.size > 0){
						  bFound = true
						  nextVertexId = candidates.head
						  //println("bFound = " + nextVertexId)
						}
					}
				}else if(existSelectedVertices && existRemovedVertices){
					//NOTE THAT ALL VERTICES ASSIGNED TO 1 ARE CONNECTED WHEN THE PROGRAM BACKTRACKS
				  
					//1.Find all possible connected components of unbounded vertices
					var compsOfUnbounded = scala.collection.mutable.Set[scala.collection.mutable.Set[Int]]()
					var temp = scala.collection.mutable.Set(unBoundedVertices.toSeq:_*)//convert to mutable set
					while(temp.size > 0){
						var conComp = graph.getConnectedComponent(temp.head, removedVertices)
						compsOfUnbounded.add(conComp)
						temp = temp.diff(conComp)
					}
					var compOfSelectedVertices = graph.getConnectedComponent(selectedVertices.head, removedVertices)
					//3. Do propagations:
					//	if any connected components of unbounded vertices is disjoint with the connected component of nodes assigned to 1, 
					//	its vertices (elements of connected components of unbounded vertices) will be assigned 0
					for(comp <- compsOfUnbounded){
						if(!comp.equals(compOfSelectedVertices)){
							for(v <- comp){
								explored(v) = true
								cp.post(vertices(v) == 0)
							}
						}
					}
					// if there are still unbounded vertices after the manual propagations,
					// we will choose a neighbour of any selected vertices
					var newUnBoundedSet = scala.collection.mutable.Set(Vertices.filter(v => !vertices(v).isBound).toSeq:_*)
					if(newUnBoundedSet.size > 0){
					  //var s = newUnBoundedSet & compOfSelectedVertices
					  //nextVertexId = s.head
					  	var temp2 = scala.collection.mutable.Set(selectedVertices.toSeq:_*)//convert to mutable set
						var bFound = false					
						while (!bFound && (temp2.size > 0)){
							var v = temp2.head
							temp2.remove(v)
							
							var neighbors = graph.getNeighbours(v)						
							var candidates = neighbors &  newUnBoundedSet
							if(candidates.size > 0){
							  bFound = true
							  nextVertexId = candidates.head
							  //println("bFound2 = " + nextVertexId)
							}
						}
					}
					
				  
				}else if(!existSelectedVertices && !existRemovedVertices){
					println("Do not expect to have all of nodes unbounded!!!" )
				}
				
				if(nextVertexId > -1)
					lifo.push(nextVertexId)
				//print("After propagations: ")
				//printVarStatus(vertices)
				//println("Node pushed to stack: " + nextVertexId)
				
				/*else{
					println("Cannot find next vertex to put in stack")
					println("Unbounded Vertices: " + unBoundedVertices)
					println("Selected vertices: " + selectedVertices)
					println("Unselected vertices: " + removedVertices )
				}*/
				
				cpsunit()
				
			}else{ //lifo.size > 0
				/*var v = lifo.pop();
					
				explored(v) = true				
				var neighbors = graph.getNeighbours(v)				
				for(i <- neighbors){
					//!explored: to prevent loop in cycle
					//!vertices.isBound to select only unassigned vertices
					if(!explored(i) && !vertices(i).isBound) lifo.push(i)
				}
								
				cp.branch {cp.post(vertices(v) == 1)} {cp.post(vertices(v) == 0)}				
				cpsunit()*/
				//println("Stack: " + lifo)
				var v = lifo.pop();
				//println("Vertex popped: " + v )
				
				if(!explored(v)){	
					explored(v) = true				
					var neighbors = graph.getNeighbours(v)				
					for(i <- neighbors){
						//!explored: to prevent loop in cycle
						//!vertices.isBound to select only unassigned vertices
						if(!explored(i) && !vertices(i).isBound) lifo.push(i)
					}
									
					cp.branch {cp.post(vertices(v) == 1)} {cp.post(vertices(v) == 0)}
				}else
				cpsunit()
				
			}//end else 
		}//end while
	  
	}

		
	/*
	def labelConnectedSubGraph(cp: CPSolver, vertices: IndexedSeq[CPVarBool]) = {
	  
		var lifo: scala.collection.mutable.Stack[Int] = scala.collection.mutable.Stack()		
		var explored: scala.Array[Boolean] = Array.fill[Boolean](graph.getVerticeSize())(false)
		val Vertices = 0 until vertices.length
		
		//println("At beginning:" + vertices)
		
		while(!isAllBound(vertices)){	
			//println("start with while ")
			
			if(lifo.length == 0){
				//There are two cases
				var nUnBounded = Vertices.filter(v => !vertices(v).isBound).length
				//println(nUnBounded)
				if(vertices.length == nUnBounded){
					//case 1: completely new
					var v = scala.util.Random.shuffle(Vertices.toList).head
					println("Node selected: " + v)
					lifo.push(v)
				}else{
					//case 2: have just finished one round of DFS and now it backs track
					//Note: all unbounded vertices should be marked unexplored.
				    //println("case 2")
				    printVarStatus(vertices)
					var unBoundIndexes = Vertices.filter(v => !vertices(v).isBound)
					for(v <- unBoundIndexes){
					  //print(v)
					  explored(v) = false
					}
					//println("Unbounded vertices" + unBoundedIndexes)
					//do manual propagations to remove nodes that cannot connect with the currently selected nodes
					val unBoundedVertices = Vertices.filter(v => !vertices(v).isBound).toSet 
					val selectedVertices  = Vertices.filter(v => vertices(v).isBound && vertices(v).isBoundTo(1) ).toSet
					val removedVertices   = Vertices.filter(v => vertices(v).isBound && vertices(v).isBoundTo(0)).toSet
					
					//find possible connected components of unbounded vertices
					var connectedSet = scala.collection.mutable.Set[scala.collection.mutable.Set[Int]]()
					var s = scala.collection.mutable.Set(unBoundedVertices.toSeq:_*)//convert to mutable set
					while(s.size > 0){
						var conComp = graph.getConnectedComponent(s.head, removedVertices)
						connectedSet.add(conComp)
						s = s.diff(conComp)
					}
					//There are also two cases for propagations
					
					if(selectedVertices.size > 0){
						//Propagation 1: There are some vertices assigned to 1
						//println("come here")
						//println("connected set: " + connectedSet)
						var curConnectedComp = graph.getConnectedComponent(selectedVertices.head, removedVertices)
						for(s <- connectedSet){
							if(!s.equals(curConnectedComp)){
								for(i <- s){
									cp.post(vertices(i) == 0)
									explored(i) = true
								}
							}
						}
												
						var index = 0
						for(v <- selectedVertices){
							var neighbours = graph.getNeighbours(v)
							var unBounds = neighbours.filter(i => !vertices(i).isBound).toSet
							if(unBounds.size > 0){
								index = unBounds.head								
							}
						}
						
						if(index>0)
							lifo.push(index);
					}else {
						//Propagation 2: There is NO vertex assigned to 1
						var atRank0 = connectedSet.head
						for(i <- atRank0)
							lifo.push(i)
					}				  
				}
				
				cpsunit()
				
			}else{ //lifo.size > 0
				var v = lifo.pop();
				println("node pop: " + v)
				var neighbors = graph.getNeighbours(v)
				
				for(i <- neighbors){
					if(explored(i) == false){
						lifo.push(i)
						explored(i) = true
					}
				}
				
				if(explored(v) == false){
					explored(v) = true
					cp.branch {cp.post(vertices(v) == 1)} {cp.post(vertices(v) == 0)}
				} else {
					cpsunit()
				}
			}//end else 
		}//end while
	  
	}*/

  def cpsunit(): Unit @cps[Unit] = ()
}