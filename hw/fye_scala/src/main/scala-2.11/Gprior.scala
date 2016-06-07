package fyeScala

object Gprior {
  import breeze.linalg.{DenseMatrix=>Dmat, DenseVector=>Dvec, inv}
  import breeze.stats.distributions.{MultivariateGaussian=>MVN,Gamma}
  import breeze.numerics.{pow}

  def sample(y: Dvec[Double], X: Dmat[Double], B: Int, gSetter:Double=0, addIntcpt:Boolean=true): 
    (Dvec[Double], Dmat[Double]) = {
    val g = if (gSetter == 0.0) X.rows.toDouble else gSetter

    val (n,k) = (X.rows,X.cols)
    val XX = X.t * X
    val XXi = inv(XX)
    val Xy = X.t * y
    val bHat = XX \ Xy
    val c = g / (1+g)

    val sse = pow(y-X*bHat,2).sum

    val phiShape = (n-1)/2.0
    val phiRate = (sse/2.0 + bHat.t *XX* bHat / (2.0*(1+g)))

    def sampBeta(phi: Double): Dvec[Double] =  MVN(c*bHat, c/phi*XXi).sample
    def sampPhi(N: Int): Dvec[Double] = Gamma(phiShape, 1/phiRate).samplesVector(N)

    val newPhi = sampPhi(B)
    val tmpBeta = newPhi.map(sampBeta(_))
    val newBeta = Dmat.tabulate(tmpBeta.length,tmpBeta(0).length){(i,j) => tmpBeta(i)(j)} 
    //Map("phi" -> newPhi, "beta" -> newBeta)
    (newPhi, newBeta)
  }
}
