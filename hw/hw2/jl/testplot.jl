using RCall, Distributions, PyPlot
rgb = R"rgb"

rgb(.5,.5,.5,.5)

subplot()
subplot(221)
for color in ["red", "lime", "cornflowerblue"]
    n = 750
    sims = randn(n,2)
    x = sims[:,1]; y = sims[:,2]
    scale = 200 * rand(n)
    scatter(x, y, c=color, s=scale, label=color,
            alpha=0.3, edgecolors="none")
end

legend()
PyPlot.xlabel("X")
PyPlot.ylabel("my Y")
grid(true)


subplot(222)
x = randn(100)
PyPlot.plt[:hist](x,color="cornflowerblue",edgecolor="white")


subplot(223)
plot(x)

subplot(224)
PyPlot.plt[:hist](x,color="cornflowerblue",edgecolor="white")
# Have to use kde

#=
include("testplot.jl")
=#
