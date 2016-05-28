package midterm2

/*
 val file = new java.io.File("src/resources/dat/poll.dat")
 val dat = breeze.linalg.csvread(file)
 */

object Midterm2 {

  import breeze.stats.distributions._
  import breeze.stats._
  import breeze.linalg._
  import breeze.numerics._
  import breeze.linalg._
  import breeze.linalg.{DenseMatrix=>Dmat,DenseVector=>Dvec}

  val R = org.ddahl.rscala.callback.RClient()
  R.eval("raw_dat <- read.csv('src/resources/dat/poll.dat',header=TRUE,sep='\t')")
  R.evalD2("print(raw_dat)")
  
  
}
