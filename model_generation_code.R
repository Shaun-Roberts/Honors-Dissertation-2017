library(sna)
library(igraph)
library(ergm)
library(intergraph)
#Faux.mesa.high model
data("faux.mesa.high")
ud <- faux.mesa.high
gplot(faux.mesa.high)
m1 = ergm(ud ~ edges + cycle(3:4))
summary(m1)

#Kapferer Model
data('kapferer')
ud = kapferer2
ud$gal$directed = F
m1 = ergm(ud ~ edges + isolates + cycle(3:5)) # unconstrained
m2 = ergm(ud ~ edges + isolates + cycle(3:4)) # unconstrained
m3 = ergm(ud ~ isolates + cycle(3:4)) #using this one
c(-Inf, 0.52494, -0.09838) #all sig
m4 = ergm(ud ~ isolates + cycle(3:5)) #unconstrained.

#Samplk2 Model
data('samplk')
ud = samplk2
ud$gal$directed = F
plot(ud)
m1 = ergm(ud ~ isolates + cycle(3:6))
summary(m1)
m2 = ergm(ud ~ isolates + cycle(3) + cycle(6))
summary(m2)
c(-Inf, 0.55372, -0.04207)
m3 = ergm(ud ~ isolates + cycle(3) + cycle(4))
summary(m3)

#Sampson model generation
data('sampson')
ud = samplike
ud$gal$directed = F
m1 = ergm(ud ~ isolates + cycle(3:6))
summary(m1)
plot(ud)
?cycle
summary(m1)
m2 = ergm(ud ~ isolates + cycle(4))
summary(m2)
m3 = ergm(ud ~ isolates + cycle(3:4))
summary(m3)
c(-Inf, 0.65250, -0.14087)