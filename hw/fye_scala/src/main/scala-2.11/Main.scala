package fyeScala

/** Suggestions:
 *    - R eval "some action" should print the output by default
 *    - R eval "some action" should be simplified to R""" some action"""
 */
object Main extends App {
  import fyeScala.Gprior
  import breeze.linalg.{DenseMatrix=>Dmat, DenseVector=>Dvec}
  val R = org.ddahl.rscala.callback.RClient()

  R eval ".libPaths(c(.libPaths(), '~/lib/R_lib'))"
  R eval "source('../fye/code/plotmap.R')"
  R eval "source('../quiz/mypairs.R',chdir=TRUE)"

  R eval "dat <- read.table('../fye/dat/ozone.dat',header=TRUE)"
  R eval "y <- log(dat[,1])"
  R eval "logdat <- cbind(y,dat[,-1])"
  R eval "colnames(logdat)[1] <- 'log_ozone'"
  //R eval "my.pairs(logdat)"

  val y = Dvec(R evalD1 "y")
  val Xtmp = R evalD2 "as.matrix(logdat[,-1])"
  val X = Dmat.tabulate(Xtmp.length,Xtmp(0).length){(i,j) => Xtmp(i)(j)}

  println(Console.GREEN)

  //R eval """mod1lm <- lm(log_ozone~.,data=logdat); print(summary(mod1lm))"""
  val lm1 = R evalR "lm(log_ozone~.,data=logdat)"
  R eval s"print(summary(${lm1}))"
  R eval "mm <- model.matrix(log_ozone~.^2,data=logdat)"
  val mmXtmp = R evalD2 "as.matrix(mm)"
  val mmX = Dmat.tabulate(mmXtmp.length,mmXtmp(0).length){(i,j) => mmXtmp(i)(j)}

  val mod1 = Gprior.sample(y=y,X=X,B=2000,gSetter=X.rows)
  println()
  println(mod1._1(1 to 10))
  println()
  println(mod1._2(1 to 10, ::))

  println(Console.RESET)
}
