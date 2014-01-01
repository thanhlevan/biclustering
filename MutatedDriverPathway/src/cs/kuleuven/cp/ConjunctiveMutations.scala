/**
 * Created on November 21, 2013
 */
package cs.kuleuven.cp

import oscar.cp.modeling._
import oscar.cp.core._
import cs.kuleuven.matrix.DenseMatrix
import cs.kuleuven.constraints.IntWSum
import cs.kuleuven.util._


class ConjunctiveMutations(mutFile: String,
        					outMutFile: String,        					
        					mRowThreshold: Integer,
        					mColThreshold: Integer,
        					mutThreshold: Integer        					
        ) extends App {
    
    val delimiter = "\t"
    //   
    val mutMatrix = new DenseMatrix(mutFile, delimiter)
    val nP = mutMatrix.colSize
    val nM = mutMatrix.rowSize
    //
    val Patients = 0 until nP        
    val Mutations = 0 until nM
  
    // solver
    val cp = CPSolver()
        
    // decision variables
    val mutGenes = (0 until nM).map(i => CPVarBool(cp))
    val mutPatients = (0 until nP).map(i => CPVarBool(cp))
  
    // optimization criterion        
    val optCriterion = sum((0 until nP).map(p => mutPatients(p))) * sum((0 until nM).map(m => mutGenes(m)))
    
    /************************************************************************
     * Perform complete search
     * 
     */
    def execute() = {
        
        /////////////////////////////////////////////////////////////////////////////////////////////
        // declare the cp model
        //
        ////////////////////////////////////////////////////////////////////////////////////////////
        cp.maximize( optCriterion )  subjectTo {
    
        	// 1.Noise constraints on rows of the mutation matrix
        	for(m <- Mutations){
        		val rWeight = (Patients).map(p => (100*mutMatrix.at(m, p) + mRowThreshold - 100))
        		val aux = new CPVarBool(cp)
        		val rowConstraint = new IntWSum(rWeight, mutPatients, 0, aux)
        		cp.add(rowConstraint)
        		cp.add(mutGenes(m) ==> aux)
        	}
    
        	// 2.Noise constraints on columns of the mutation matrix
        	for(p <- Patients){
        		val cWeight = (Mutations).map(m => (100*mutMatrix.at(m, p) + mColThreshold - 100))
        		val mutCoverConstraint = new IntWSum(cWeight, mutGenes, 0, mutPatients(p))        		
        		cp.add(mutCoverConstraint)
        	}
        	
        	// 3.Number of mutations should be greater than 0
        	val w1 = Mutations.map(m => 1)
        	val aux1 = new CPVarBool(cp)
        	cp.add(aux1 == 1)
        	val totalMutatedGenes = new IntWSum(w1, mutGenes, 0, aux1)
        	cp.add(totalMutatedGenes)
        	 
        	// 4. Limit of number of mutations   
        	val w2 = Mutations.map(m => -1)
        	val aux2 = new CPVarBool(cp)
        	cp.add(aux2 === 1)
        	val totalMutations = new IntWSum(w2, mutGenes, -1*mutThreshold, aux2) 
        	cp.add(totalMutations)
        	
        	// 5.Number of patients should be greater than 1
        	val w3 = Patients.map(m => 1)
        	val aux3 = new CPVarBool(cp)
        	cp.add(aux3 == 1)
        	val totalPatients = new IntWSum(w3, mutPatients, 1, aux3)
        	cp.add(totalPatients)
        	  
        } exploration {
            cp.binaryFirstFail(mutGenes)
            
            val selMutPatients = (0 until mutPatients.size).filter(m => mutPatients(m).getValue == 1)
            val selMutGenes = (0 until mutGenes.size).filter(r => mutGenes(r).getValue == 1)
            println("MutGenes: " + selMutGenes)
            println("MutPatients: " + selMutPatients)
            
        } run()
	  
        cp.printStats()
    } // end execute()
    
    
    /************************************************************************************
     * Perform large neighbourhood search
     * 
     */
    def executeLNS() = {
    	
        //Randomizer
    	val rand = new scala.util.Random(0)
    	val bestRows = Array.fill(nM)(0)
    	val bestCols = Array.fill(nP)(0)
        
        cp.maximize( optCriterion )  subjectTo {
    
        	// 1.Noise constraints on rows of the mutation matrix
        	for(m <- Mutations){
        		val rWeight = (Patients).map(p => (100*mutMatrix.at(m, p) + mRowThreshold - 100))
        		val aux = new CPVarBool(cp)
        		val rowConstraint = new IntWSum(rWeight, mutPatients, 0, aux)
        		cp.add(rowConstraint)
        		cp.add(mutGenes(m) ==> aux)
        	}
    
        	// 2.Noise constraints on columns of the mutation matrix
        	for(p <- Patients){
        		val cWeight = (Mutations).map(m => (100*mutMatrix.at(m, p) + mColThreshold - 100))
        		val mutCoverConstraint = new IntWSum(cWeight, mutGenes, 0, mutPatients(p))        		
        		cp.add(mutCoverConstraint)
        	}
        	
        	// 3.Number of mutations should be greater than 0
        	val w1 = Mutations.map(m => 1)
        	val aux1 = new CPVarBool(cp)
        	cp.add(aux1 == 1)
        	val totalMutatedGenes = new IntWSum(w1, mutGenes, 0, aux1)
        	cp.add(totalMutatedGenes)
        	 
        	// 4. Limit of number of mutations   
        	val w2 = Mutations.map(m => -1)
        	val aux2 = new CPVarBool(cp)
        	cp.add(aux2 === 1)
        	val totalMutations = new IntWSum(w2, mutGenes, -1*mutThreshold, aux2) 
        	cp.add(totalMutations)
        	
        	// 5.Number of patients should be greater than 1
        	val w3 = Patients.map(m => 1)
        	val aux3 = new CPVarBool(cp)
        	cp.add(aux3 == 1)
        	val totalPatients = new IntWSum(w3, mutPatients, 1, aux3)
        	cp.add(totalPatients)
        	  
        } exploration {
            cp.binaryFirstFail(mutGenes)
            
            val selMutPatients = (0 until mutPatients.size).filter(m => mutPatients(m).getValue == 1)
            val selMutGenes = (0 until mutGenes.size).filter(r => mutGenes(r).getValue == 1)
            println("MutGenes: " + selMutGenes)
            println("MutPatients: " + selMutPatients)
            
            //store the current best solution 
            Mutations.foreach(r => bestRows(r) = mutGenes(r).value)
            Patients.foreach(c => bestCols(c) = mutPatients(c).value)
            
        } run(1)
        
        // large neighborhood search
        val limit = 500 // set the limit to 100 backtracks for LNS restarts
        val colRangeLimit = (0.5*nM).toInt
        for (r <- 1 to 20) {
        	// relax randomly 50% of the variables and run again
            println(r + "th restart")
        	cp.runSubjectTo(failureLimit = limit) {        		
        		for(m <- Mutations){
        			if(rand.nextInt(nM) < colRangeLimit)
        				cp.post(mutGenes(m) == bestRows(m))
        		}
        	}
        }
	  
        cp.printStats()
        
    }

}