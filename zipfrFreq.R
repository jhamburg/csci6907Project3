require(zipfR)
require(languageR)
##zipfR Execution

zipfr_work <- function(document){
#vectorize a specific corpus document
text <- strsplit(document$content, " ")[[1]]
##create spc object
this.spc <- text2spc.fnc(text)
summary(this.spc)
#plot histogram of frequency classes
plot(this.spc)
#plot first 50 elements
plot(this.spc, log="x")
#plot frequency spectrum
with(this.spc, plot(m, Vm, main="Frequency Spectrum"))
#create growth object
#this_growth<- growth.fnc(text)
#summary(this_growth)
#create vgc from growth
#this_vgc <- growth2vgc.fnc(this_growth)
#summary(this_vgc)
#plot vocabulary growth
#plot(this_vgc, add.m=1)
#plot interpolated vocabulary growth
#this_bin_vgc <- vgc.interp(this.spc, N(this_vgc), m.max=1)
#estimating V and other 
#this.fzm <- lnre("fzm", this.spc, exact=FALSE)
#summary(this.fzm)
#this.fzm.spc <- lnre.spc(this.fzm, N(this.fzm))
#plot(this.spc, this.fzm.spc, legend=c("observed", "fZM"))
#this_zm <- lnre("zm", this.spc)
#this.zm.spc <- lnre.spc(this_zm, 10e6)
#this.zm.spc





}