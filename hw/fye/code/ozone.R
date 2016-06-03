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
