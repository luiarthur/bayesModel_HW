set.seed(207)
library(rjulia)
source("../../quiz/mypairs.R",chdir=TRUE)
system("mkdir -p ../report/figs")

# Data:
dat <- read.table("../dat/ozone.dat",header=TRUE)
nrow(dat)
colnames(dat)
# ozone concentration: ppb
#     solar radiation: 0-low; 1-moderate to high
#      daily max temp: F
#          wind speed: mph


# EXPLORATORY ANALYSIS & FINDINGS
pdf("../report/figs/pairs.pdf")
  my.pairs(dat)
dev.off()

y <- log(dat$ozone)
log_dat <- cbind(y,dat[,-1])
colnames(log_dat)[1] <- "log_ozone"
head(log_dat)

pdf("../report/figs/log_ozone_pairs.pdf")
  my.pairs(log_dat)
dev.off()

linear.mod <- lm(log_ozone ~ ., data = log_dat)
summary( linear.mod )


# MODEL FITTING:

# Julia Settings:
julia_init()
julia_void_eval('include("julia/g-prior.jl")')
r2j(y,"y")
r2j(log_dat[,-1],"X_tmp")
julia_void_eval("X = convert(Matrix,X_tmp)")
julia_void_eval("blas_set_num_threads(1)")

# Fit Model:
julia_void_eval("@time out = gprior(y,X,2000, add_intercept=true,
                setseed=207)")
post.phi <- j2r("out[:phi]")
post.beta <- j2r("out[:beta]")

pdf("../report/figs/posts.pdf")
simple.plot.posts(cbind(post.phi,post.beta),ma=2,tckdig=2,
                  cnames=c("phi","Intercept",colnames(log_dat[,-1])))
dev.off()
