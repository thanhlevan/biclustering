package cs.kuleuven.cp

import cs.kuleuven.graph._
import oscar.cp.modeling._
import oscar.cp.core._
import cs.kuleuven.constraints.IntWSum
import cs.kuleuven.util._
import cs.kuleuven.java.mdl._
import cs.kuleuven.matrix._
import java.lang.Math._
import breeze.stats.distributions._

object ws {

	//val filename = "/home/thbreezeanh/Rlib/MutDriverPathways/Generators/graph_adjacency.txt"
	//val g: UndirectedGraph = new UndirectedGraph(filename,"\t")
  
  /*val b = g.isConnected(2,6)
  var rm = scala.collection.mutable.Set(3)
  
  val c = g.getConnectedComponent(7,rm)
  
  
  val nE = 3
  val cp = CPSolver()
  val expGenes =  (0 until nE).map(i => CPVarBool(cp))
  
 
 
  
  //var t:IndexedSeq[Int] = IndexedSeq(1,2,5,7,0,9)
  //var r = scala.util.Random.shuffle(t).head
  
  var s = scala.collection.mutable.Set[scala.collection.mutable.Set[Int]]()
  var s1 = scala.collection.mutable.Set(1,2,3,5)
  var s2 = scala.collection.mutable.Set(5,6)
  s.add(s1)
  s.add(s2)
  s1 = s1.diff(s2)
  var l = s1
 	
 */
 /*
 val s1 = Set(1,3,4)
 var s2 = scala.collection.mutable.Set(s1.toSeq:_*)
 s2.add(5)
 println(s1)
 println(s2)
 
 var s = scala.collection.mutable.Set(1,3,5)
 s.remove(3)
 println(s)
 val bestRows = Array.fill(5)(0)
 bestRows(2) = 1
 val b = bestRows
 
 val c = (0.7*21).toInt
 //g.getConnectedComponent(s1.head,s2)
 */
 val cp = CPSolver()                              //> cp  : oscar.cp.modeling.CPSolver = SearchNode: nbPushed0 currentTrailSize:1
                                                  //| 
 /*val w = Array(1, 1, -1)
 val y = (0 until 3).map(i => CPVarBool(cp))
        
	val aux = new CPVarBool(cp)
	val weightConstraint = new IntWSum(w, y, 2, aux)
  //cp.post(y(0) == 1)
  cp.post(weightConstraint)
  cp.post(aux == 1)
  print(y(0))
  print(y(1))
  print(y(2))
  
 val xposmax = pos.map(i => x(i).getMax)
 val xnegmin = neg.map(i => x(i).getMin)
 
 def wsum(x: Array[Int], y: Array[Int]): Int = {
 		var total = 0;
 		for(i <- 0 until x.size)
 			total = total + x(i)*y(i)
 		return total
 }
 
 val maxsum = wsum(wpos, xposmax) + wsum(wneg, xnegmin)
 */
 /*
 val Cols = 0 until 20
 val rowThreshold = 25
 val row1 = Array(1,	1,	1,	1,	1,	1,	1,	1,	1,	1,	0,	0,	0,	0,	0,	0,	0,	0,	0,	0)
 val w = Cols.map(i => row1(i)*100 + rowThreshold - 100)
 val x = (0 until 20).map(i => CPVarBool(cp))
 val varRow = new CPVarBool(cp)
 //cp.post(x(0) == 1)
 val c = new IntWSum(w, x, 0, varRow)
 cp.post(c)
 
 //cp.post(rei === 1)
 val v = varRow
val x = (0 until 4).map(i => CPVarBool(cp))
cp.post(x(0) )
cp.post(x(3) )
cp.post(!x(1))
cp.post(!x(2))

val sols = new Array[Solution](3)
val names = List("mut", "pat", "exp")
val s1 = new Solution(names)
val s2 = new Solution(names)
val s3 = new Solution(names)
val s4 = new Solution(names)
s1.setObjValue(1)
s2.setObjValue(5)
s3.setObjValue(6)
s4.setObjValue(11)

s1.update("mut", x)
val t = s1.toString("mut", "\t")
s1.saveVar("mut", "/home/thanh/test_saving.txt", "\t", true)

sols.update(0, s1)
sols.update(1, s2)
sols.update(2, s3)

val topK = new TopKSolutions(2)
topK.update(s1)
topK.update(s2)
topK.print()
topK.update(s3)
topK.print()
topK.update(s4)
topK.print()
*/

/*	val Mutations = 0 until 10
	val x = 0 until 10
	val t = Mutations.map(m => 1 + x(m))
 */


//var t = mdl.buildDBDensityMap();
//val x = java.lang.Math.round(3/2)

/*
 def test() = {
        val delimiter = "\t"
        //val filename = "D:\\repos\\data\\biclusters\\test_tdb.txt"
        val filename = "D:\\repos\\data\\biclusters\\8modules\\noise_10\\bg_0.10_bic_0.10_8modules.txt"
        val matrix = new DenseMatrix(filename, delimiter)
        var mdl = new BiclusterMDLScore()
        mdl.setTdb(matrix);
        var r = mdl.getTdb().rowSize
        val bicRows = (1499 until 1800).map(x => x:Integer).toArray//Array(0,1,2).map(x => x: Integer)
        val bicCols = (0 until 10).map(x => x:Integer).toArray//Array(0,1,2).map(x => x: Integer)
        //val bicRows = (0 until 200).map(x => x:Integer).toArray//Array(0,1,2).map(x => x: Integer)
        //val bicCols = (0 until 100).map(x => x:Integer).toArray//Array(0,1,2).map(x => x: Integer)
        //val bicRows = Array(1,2,3).map(x => x: Integer)
        //val bicCols = Array(0,1,2).map(x => x: Integer)
        val query = Array(1, 3, 5)
        
        //var t = mdl.buildDBDensityMap();
        mdl.setBicIndexes(bicRows, bicCols)
        
        //mdl.getBicRowDefaults()
        //mdl.buildBicDensityMaps()
        mdl.printBicDensityMaps()
        println("model length = " + mdl.getModelLength())
        println("data length = " + mdl.getDataLength())
        println("MDL score = " + mdl.getMDLScore())
        val c = mdl.getNextCompressedCol()
        println("Next column: " + c)
    }
*/
//val sortedDenCols = unBoundedCols.sortBy(c => pRegion.getColDensity(c))(Ordering[Double].reverse)
//val bernouli = new Bernoulli(0.5)
//val samples = bernouli.sample(20)

//val v = Vector(Set(1,3,1),Set(3,4,5))
//val v2 = v ++ Vector(Set(4,5,6))

		  //val curResult = (0 until rows.size).filter(r => rows(r).getValue == 1).toSet
		  //val uniResult = prevResult union curResult
	//val v = Vector(2, 3, 4, 5, 6)
	//val den = Vector(0.6, 0.2, 0.8, 0.1, 0.9)
	
	//val sortedV = (0 until v.size).sortBy(c => den(c))(Ordering[Double].reverse)
	val s1 = scala.collection.immutable.HashSet(1, 2, 3)
                                                  //> s1  : scala.collection.immutable.HashSet[Int] = Set(1, 2, 3)
	val s2 = scala.collection.immutable.HashSet(1, 4, 6)
                                                  //> s2  : scala.collection.immutable.HashSet[Int] = Set(1, 6, 4)
	val s3 = s2 &~ s1                         //> s3  : scala.collection.immutable.HashSet[Int] = Set(6, 4)
	
 
}