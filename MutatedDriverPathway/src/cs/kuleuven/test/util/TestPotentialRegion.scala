package cs.kuleuven.test.util

import org.scalatest.FunSuite
import org.scalatest.matchers.ShouldMatchers
import cs.kuleuven.util.PotentialRegionBuilder
import cs.kuleuven.matrix.DenseMatrix


class TestPotentialRegion extends FunSuite with ShouldMatchers{
  val binFileName = "D:\\repos\\data\\biclusters\\8modules\\noise_10\\bg_0.10_bic_0.10_8modules.txt"
  val mulFileName = "D:\\repos\\data\\biclusters\\8modules\\noise_10\\bg_0.10_bic_0.10_8modules.txt"
  val delimiter = "\t"
  val binDB = new DenseMatrix(binFileName, delimiter)
  val mulDB = new DenseMatrix(mulFileName, delimiter)
  val pRegion = new PotentialRegionBuilder(binDB, mulDB)
  
	test("get MDL local minimums") {
	  
	  val l = List(1, 3, 5, 8, 10)
	  val isAscending = pRegion.isAscendingOrdered(l)
	  isAscending should be (true)
	}
}