package cs.kuleuven.test.constraints

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers

import oscar.cp.constraints._
import oscar.cp.core._
import oscar.cp.modeling._

import cs.kuleuven.constraints._


class TestIntWSum extends FunSuite with ShouldMatchers{
    
    test("Weighted Sum 1") {
        // This test case from Tias et. al., Itemset Mining: a Constraint Programming Perspective, 2011
        val cp = CPSolver()
        val w = Array(2, 4, 8)                     
        val y = (0 until 3).map(i => CPVarBool(cp))
        
        val aux = new CPVarBool(cp)              
        val weightConstraint = new IntWSum(w, y, 7, aux)
                                                 
        cp.post(y(0) == 1)                       
        cp.post(weightConstraint)                
        cp.post(aux == 1)  
        
        y(2).getValue should be (1)
    }
    
    test("Weighted Sum 2") {
     
        val cp = CPSolver()
        val w = Array(1, 1, -1)                     
        val y = (0 until 3).map(i => CPVarBool(cp))
        
        val aux = new CPVarBool(cp)              
        val weightConstraint = new IntWSum(w, y, 2, aux)
                            
        cp.post(weightConstraint)                
        cp.post(aux == 1)  
        
        y(0).getValue should be (1)
        y(1).getValue should be (1)
        y(2).getValue should be (0)
    }

}