package cs.kuleuven.constraints

import oscar.cp.core.CPOutcome
import oscar.cp.core.CPPropagStrength
import oscar.cp.core.CPVarInt
import oscar.cp.core.Constraint
import oscar.cp.core.CPVarBool
import oscar.cp.util.ArrayUtils;
//import oscar.reversible.ReversibleInt
import java.security.InvalidParameterException

/**
 * Sum Constraint: w[0]*x[0]+w[1]*x[1]+...+w[n]*x[n] >= b
 * @param w weighted vector
 * @param x decision vector
 * @param b lower bound value
 * @param r reified variable
 */

class IntWSum(val w: Array[Int], val x: Array[CPVarBool], val b: Int, val r: CPVarBool) extends Constraint(x(0).s, "IntWSum") {
  
		if (w.size != x.size) throw new InvalidParameterException("w and x must have the same size")
		
		val pos = (0 until w.size).filter(i => w(i) >= 0)
		val neg = (0 until w.size).filter(i => w(i) < 0)
		
		val wpos = pos.map(i => w(i))
		val wneg = neg.map(i => w(i))		
		
		def wsum(x: IndexedSeq[Int], y: IndexedSeq[Int]): Int = {
		  // Q: can we perform sum operation in parallel??
			var total = 0;
 			for(i <- 0 until x.size)
 				total = total + x(i)*y(i)
 			return total
		}  
				
		override def setup(l: CPPropagStrength): CPOutcome =  {
				if( propagate() == CPOutcome.Failure)
				  return CPOutcome.Failure;
				// register for x
				for(i <- 0 until x.size) {
					if( !x(i).isBound)
					  x(i).callPropagateWhenBoundsChange(this)
				}
				// register for reified variable
				if(!r.isBound)
				    r.callPropagateWhenBind(this)
				   // r.callPropagateWhenBoundsChange(this)
				// return    
				CPOutcome.Suspend
		}
		
       override def propagate(): CPOutcome = {
    		   //println("propagate")
    		   val xposmax = pos.map(i => x(i).getMax)
    		   val xposmin = pos.map(i => x(i).getMin)
    		   val xnegmin = neg.map(i => x(i).getMin)
    		   val xnegmax = neg.map(i => x(i).getMax)    		   
    		   
    		   val maxsum = wsum(wpos, xposmax) + wsum(wneg, xnegmin)
    		   val minsum = wsum(wpos, xposmin) + wsum(wneg, xnegmax)
    		   
    		   if(maxsum < b) {
    		         if (!r.isBound) {
    		        	 if( r.updateMax(0) == CPOutcome.Failure) {
    		        	     return CPOutcome.Failure    		        	  
    		        	 } else {
    		        	     //return CPOutcome.Suspend
    		        	     return CPOutcome.Success
    		        	 }
    		         } else if ( r.isBound && r.getValue == 1) {
    		             return CPOutcome.Failure
    		         } else if (r.isBound && r.getValue == 0){
    		             //return CPOutcome.Suspend
    		             return CPOutcome.Success
    		         }   		
    			     //return CPOutcome.Suspend?
    		   }
    		     
    		   if(minsum >= b) {
    			   if (!r.isBound) {
    		        	 if( r.updateMin(1) == CPOutcome.Failure) {
    		        	     return CPOutcome.Failure    		        	  
    		        	 }else {
    		        	     //return CPOutcome.Suspend
    		        	     return CPOutcome.Success
    		        	 }
    		       } else if (r.isBound && r.getValue == 0) {
    		            return CPOutcome.Failure
    		       } else if (r.isBound && r.getValue == 1) {
    		            //return CPOutcome.Suspend
    		             return CPOutcome.Success
    		       }   		
    			     //return CPOutcome.Suspend
    		   }    		     
    		   
    		   //if( (!r.isBound) || (r.isBound && r.getValue == 1) ) {
    		   if( r.isBound && r.getValue == 1) {
    		       // Perform constraint propagations 
    			   for(i <- 0 until w.size) {
	    			   if(maxsum < (b + w(i))) {
	    				   // println("update " + i)
	    				   // remove {0}
	    			       if( x(i).updateMin(1) == CPOutcome.Failure)
	    			         return CPOutcome.Failure
	    			   }else if(maxsum < (b - w(i))) {
	    				   // println("update " + i)
	    				   // remove {1}
	    				   if( x(i).updateMax(0) == CPOutcome.Failure)
	    				     return CPOutcome.Failure
	    			   }
    			   }	
    		   } else if (r.isBound && r.getValue == 0) {
    		         // Perform constraint propagations    		         
    		   } 
    		   return CPOutcome.Suspend
       }
        
}