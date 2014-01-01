/**
 * Created on October 25, 2013
 */
package cs.kuleuven.cp

import oscar.cp.modeling._
import oscar.cp.core._
import cs.kuleuven.matrix.DenseMatrix
import cs.kuleuven.constraints.IntWSum
import cs.kuleuven.util._


class topKMutatedPathways(mutFile: String,
        					expFile: String,
        					graphFile: String,
        					outMutFile: String,
        					outExpFile: String,
        					outPatFile: String,
        					rowThreshold: Integer,
        					colThreshold: Integer,
        					mutThreshold: Integer,
        					gSearch: Boolean
        ) extends App {

def execute() = {
    
  val delimiter = "\t"
        
  //val mutFile = "/home/thanh/data/cancer/mutations/breast/post/comet_mutation_ds.txt"
  //val expFile = "/home/thanh/data/cancer/mutations/breast/post/comet_dup_exp_ds.txt"
  //val graphFile = "/home/thanh/data/cancer/mutations/breast/post/comet_network.txt"  
  //   
  val expMatrix = new DenseMatrix(expFile, delimiter)
  val mutMatrix = new DenseMatrix(mutFile, delimiter)
  
  // expMatrix.printMatrix
  val nE = expMatrix.rowSize
  val nP = expMatrix.colSize
  val nM = mutMatrix.rowSize
  
  // parameters
 // val rowThreshold1 = 25
  //val rowThreshold2 = 25
  //val colThreshold = 25
  //val mutThreshold = 5
  
  //
  val Patients = 0 until nP
  val Genes	= 0 until nE
  val Mutations = 0 until nM
  
  // solver
  val cp = CPSolver()
  val	gSearchController = if(gSearch) new GraphSearch(graphFile, delimiter) else null
  
  // decision variables
  val expGenes =  (0 until nE).map(i => CPVarBool(cp))
  //val expGenes2 =  (0 until nE).map(i => CPVarBool(cp))
  val expPatients = (0 until nP).map(i => CPVarBool(cp))
  val mutGenes = (0 until nM).map(i => CPVarBool(cp))
  val mutPatients = (0 until nP).map(i => CPVarBool(cp))
  
  // optimization criterion
  // val biclusterArea = sum((0 until nE).map(e => expGenes(e))) * sum((0 until nP).map(p => expPatients(p)))
  val optCriterion = sum((0 until nP).map(p => (expPatients(p)*2 - mutPatients(p)) )) * sum((0 until nE).map(e => expGenes(e)))
  
  // storing top-k solutions
  val topK = new TopKSolutions(100)
  val names = List("mutgene", "mutpat", "expgene", "exppat")
  
  cp.solve subjectTo{
  //cp.maximize( optCriterion )  subjectTo {
    
	  // 1.Noise constraints on rows of the expression matrix
	  for(e <- Genes){
		  val rWeight = (Patients).map(p => (100*expMatrix.at(e, p) + rowThreshold - 100))	      
		  val coverageConstraint = new IntWSum(rWeight, mutPatients, 0, expGenes(e))
		  cp.add(coverageConstraint)
	  }
    
	  // 2.Noise constraints on columns of the expression matrix
	  for(p <- Patients){
		  val cWeight = (Genes).map(e => (100*expMatrix.at(e, p) + colThreshold - 100))
		  val aux = new CPVarBool(cp)
		  val colConstraint = new IntWSum(cWeight, expGenes, 0, aux)
		  cp.add(colConstraint)
		  cp.add(mutPatients(p) ==> (aux === expPatients(p)))
	  }
    
	  // One more round
	  //for(e <- Genes){
	  //val rWeight = (Patients).map(p => (100*expMatrix.at(e, p) + rowThreshold2 - 100))	      
	  // val coverageConstraint = new IntWSum(rWeight, expPatients, 0, expGenes2(e))
	  //cp.add(coverageConstraint)
	  //}
	   
	   // prevent false negative
	   for(p <- Patients){
		   cp.add(expPatients(p) <== mutPatients(p))
	   }       
	  // 3. Each patient has at least one mutated gene   
	  for(p <- Patients){
	      val wMutations = (Mutations).map(m => mutMatrix.at(m, p))
	      val mutCoverageConstraint = new IntWSum(wMutations, mutGenes, 1, mutPatients(p))
	      cp.add(mutCoverageConstraint)
	  }
    
	  // 4. Each mutation is covered by at least one patient  
	  for(m <- Mutations){
	      //val nPatients = sum((0 until nP).map(p => mutPatients(p) * mutMatrix.at(m, p)))
	      //cp.add(mutGenes(m) ==> (nPatients >== 1))
	       val wPatients = (Patients).map(p => mutMatrix.at(m, p))
	       val aux = new CPVarBool(cp)
	       val patientCoverage = new IntWSum(wPatients, mutPatients, 1, aux)
	       cp.add( mutGenes(m) ==> aux)  
	  }
    
	  // 5. The number of mutations is small
	  for(p <- Patients){
		  //val nMutations = sum((0 until nM).map(m => mutGenes(m) * mutMatrix.at(m,p)))
		  //cp.add(mutPatients(p) ==> (nMutations <== mutThreshold))
	       val negWeight = Mutations.map(m => -1*mutMatrix.at(m,p))
	       val aux = new CPVarBool(cp)
	       val smallMutConstraint = new IntWSum(negWeight, mutGenes, -1*mutThreshold, aux)
	       cp.add(mutPatients(p) ==> aux)
	  }
	  
	   // Do we need this anymore?  
	  //cp.add( sum((0 until nM).map(m => mutGenes(m))) < mutThreshold)
	  val w0 = Mutations.map(m => -1)
	  val aux0 = new CPVarBool(cp)
	  cp.add(aux0 === 1)
	  val totalMutations = new IntWSum(w0, mutGenes, -1*mutThreshold, aux0) 
	  cp.add(totalMutations)
	   
	   
	  // Total expressed genes > 0 
	  // cp.add( sum((0 until nE).map(e => expGenes(e))) > 0)
	  val w1 = Genes.map(e => 1)
	  val aux1 = new CPVarBool(cp)
	  cp.add(aux1 === 1)	  
	  val totalExpressedGenes = new IntWSum(w1, expGenes, 1, aux1)
	  cp.add(totalExpressedGenes)
	  
	  // Total mutated patients >0 
	  // cp.add( sum((0 until nP).map(p => mutPatients(p))) > 0)
	  val w2 = Patients.map(p => 1)
	  val aux2 = new CPVarBool(cp)
	  cp.add(aux2 == 1)
	  val totalMutatedPatients = new IntWSum(w2, mutPatients, 1, aux2)
	  cp.add(totalMutatedPatients)
    
	   // Total mutated genes >= 1
	   val w3 = Mutations.map(m => 1)
	   val aux3 = new CPVarBool(cp)
	   cp.add(aux3 == 1)
	   val totalMutatedGenes = new IntWSum(w3, mutGenes, 1, aux3)
	   cp.add(totalMutatedGenes) 
	   
	   //for(m <- 15 until nM){
	   //      cp.add(mutGenes(m) === 0)
	   //}
  }exploration{
     
      if (gSearch) {
    	  gSearchController.labelConnectedSubGraph2(cp, mutGenes)
      } else {
          cp.binaryFirstFail(mutGenes)
      }
	  val solution = new Solution(names)
	  solution.update("mutgene", mutGenes)
	  solution.update("mutpat", mutPatients)
	  solution.update("expgene", expGenes)
	  solution.update("exppat", expPatients)
	  
	  val selMutPatients = (0 until mutPatients.size).filter(m => mutPatients(m).getValue == 1)
	  val selExpPatients = (0 until expPatients.size).filter(r => expPatients(r).getValue == 1)
	  val selExpGenes	 = (0 until expGenes.size).filter(g => expGenes(g).getValue == 1)
	  val objValue = (2*selExpPatients.size - selMutPatients.size) * selExpGenes.size
	  
	  solution.setObjValue(objValue)
	  topK.update(solution)
	  
	  println("---------------------------------\n")
	  //topK.print
	  topK.save(outMutFile, outExpFile, outPatFile)
	  println()
	  cp.printStats()
    
  }run()
  
  cp.printStats()
}   
  
  
}