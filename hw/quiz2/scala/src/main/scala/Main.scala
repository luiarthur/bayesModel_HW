package midterm2

object Main extends App {
  import Model._
  import Timer.timer
  import breeze.linalg.{DenseMatrix=>Dmat, DenseVector=>Dvec}

  // Explore
  val R = org.ddahl.rscala.callback.RClient()
  //R.eval("plot(rnorm(100))")
  R eval """
    source('../../quiz/mypairs.R',chdir=TRUE)
    histo <- function(x,color='grey',bordercol='white',...) 
               hist(x,prob=TRUE,col=color,border=bordercol,...)
    dat <- read.csv('src/resources/dat/poll.dat',header=TRUE,sep='\t')
    n <- nrow(dat)
    print( head(dat) )
    leave <- dat[,"LeavePerc"]
    stay <- dat[,"RemainPerc"]
    date <- sapply(dat[,"Date"],as.character)
    date_ind <- n:1
    src <- sapply(dat[,"Source"],as.character)
    meth <- sapply(dat[,"Method"],as.character)
    X.mod <- model.matrix(~date_ind+src+meth)
    X <- as.matrix(X.mod)
    X_colnames <- colnames(X)
    logit <- function(p) log(p/(1-p))
    invlogit <- function(w) 1/(1+exp(-w))
    p <- leave / (stay+leave)
    y <- logit(p)


    # Plots
    pairs(dat,col="orange",pch=20,bty='n',fg='black')
    par(bty="o",fg="grey40",col.axis="grey30")
    pairs(cbind(y,dat[,-c(2,3)]),pch=20,col='orange')
  """
 
  // I want to do Bayesian Lasso
  def logit(p: Double): Double = math.log(p / (1-p)) // - math.log(1-p)
  def invlogit(w: Double): Double = 1.0 / (1.0 + math.pow(math.E,-w))

  val leave = R.getD1("leave")
  val stay = R.getD1("stay")
  val p = (leave zip stay).map( tup => tup._1 / (tup._1+tup._2) )
  val y = p.map(x => logit(x))
  val date = R.getS1("date")
  val src = R.getS1("src")
  val meth = R.getS1("meth")

  /*
      R.eval("print(head(dat))")
      R.eval("print((X))")
      R.eval("print(X_colnames <- colnames(X))")
   */
}
