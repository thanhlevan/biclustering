package cs.kuleuven.cp

import oscar.cp.modeling._
import oscar.cp.core._


object testGraphSearch extends App{
  
	//def execute = {
		val filename = "/home/thanh/Rlib/MutDriverPathways/Generators/test_small_tree.txt"
		val delimiter = "\t"
		val	gSearchController = new GraphSearch(filename, delimiter)
		
		val cp = CPSolver()
		val nV = 0 until gSearchController.vertexSize
		val vertices = (nV).map(i => CPVarBool(cp))
		var nSols = 0
		
		cp.solve subjectTo {
		  
		}exploration{	  
			gSearchController.labelConnectedSubGraph2(cp, vertices)
			print(nSols + ". ")
			printSolution(vertices)
			nSols = nSols + 1
		}run()	
	
	//}
	//function definitions
	def printSolution(vars: IndexedSeq[CPVarBool]) = {
	  val selected = (0 until vars.length).filter((i:Int) => (vars(i).getValue == 1))
	  println(selected)
	}
}