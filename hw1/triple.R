dat <- read.table("triple.dat",header=TRUE)

y <- dat$dist_m
x <- dat$wind_m_per_s

plot(x,y)

mod <- lm(y~x)
summary(mod)
abline(mod)

### Conclusion
# Apparently, wind speed doesn't affect jumping distance.
