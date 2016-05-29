using RCall

# Read Data:
R"""
raw_dat <- read.csv('../dat/poll.dat',sep="\t",header=TRUE)
head(raw_dat)
"""

@rget raw_dat
leave = raw_dat[:LeavePerc]
stay = raw_dat[:RemainPerc]

#=
include("poll.jl")
=#


