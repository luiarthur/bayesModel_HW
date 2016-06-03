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
# y | X, b ~ MVN(Xb, Sigma)
#
#
