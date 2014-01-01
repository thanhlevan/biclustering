package cs.kuleuven.cp

import oscar.cp.modeling._
import oscar.cp.core._

object testBranching extends App{
  
	val cp = CPSolver()
	val nG = 3
	val genes =  (0 until nG).map(i => CPVarBool(cp))
	
	
	
	cp.solve subjectTo {
	  
	}exploration{
	  
	  labelling(cp, genes)
	  
	  /*while(!allBounds(genes)){
	    val unBoundedVar = (0 until 3).filter(v => !genes(v).isBound).toSet
	    
	    val next = unBoundedVar.head
	    cp.branch{
	      cp.post(genes(next) == 0 )
	    } {
	        cp.post(genes(next) == 1)
	    }
	  }*/
	  println(genes)
	}run()
	
	
	def labelling(cp: CPSolver, vertices: IndexedSeq[CPVarBool]) = {
	  while(!allBounds(genes)){
	    val unBoundedVar = (0 until 3).filter(v => !genes(v).isBound).toSet
	    
	    val next = unBoundedVar.head
	    cp.branch{
	      cp.post(genes(next) == 0 )
	    } {
	        cp.post(genes(next) == 1)
	    }
	  }
	}
}