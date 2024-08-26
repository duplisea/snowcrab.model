#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$
ALL THE PLOTTING FUNCTIONS FOR THE OUTPUT OF SIMULATIONS
#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

analysis=list()

##############################################################
# main plotting fucntion of abundance by groups of stages over time
##############################################################
analysis$plots.f= function(dataset,plot.type=1,years.removed=100){

# dataset = output list from a simulation run
# plot.type = the kind of plot to make (i.e. which instars to select to plot)
# no.years.remove = number of initial years to exclude in the plot
half.years.removed=years.removed*2
data= dataset$n.crab.6months[,-(1:half.years.removed)]

# types of plots to make are stored in a vector
type.vector=c("TM.males","TM.females","fishable.males","prime.fishable.males",
  "softshell.males.9.10.11","all","primiparous.females","multiparous.females",
  "instar1","instar2","instar3","instar4","instar5","instar6","instar7","instar8",
  "instar9","instar10","instar11","instar12","instar13","instar14","CW","wgt",
  "TM.mal.2to4","TM.mal.2to4.legal")
stages= type.vector[plot.type]

# makes the plot though matrix multiplication on the binary vector. This selects
# only the instars of interest (1 in the binary matrix) and sums over them for a
#time step and then plots over the simulation period
data.to.plot= t(data)%*%stage.binaries.f(stages)
# take the average in adjacent 6 month periods to get annual average
data.to.plot= (data.to.plot[seq(2,length(data.to.plot),by=2)]+data.to.plot[seq(2,length(data.to.plot),by=2)-1])/2

layout(matrix(1:3,3,1))
# plot of the abundance of selected instars
plot((years.removed+1):(length(data.to.plot)+years.removed),data.to.plot,
  type="l", xlab="Simulation time step (year)", ylab=paste("Mean abundance of ", stages))

# power spectrum
tmp.ps= spectrum(data.to.plot,taper=0,detrend=T,plot= T)
period= 1/tmp.ps$freq[match(max(tmp.ps$spec),tmp.ps$spec)]

# plot of catch, reported annually so need to account for this in month model if
# using month removed
catch.data= apply(dataset$catch,1,sum)[-(1:(years.removed))]
plot((years.removed+1):(length(catch.data)+years.removed),catch.data,
  type="l",xlab="Simulation time step (year)", ylab="Catch (Kg)")
par(new = TRUE)
plot((years.removed+1):(length(catch.data)+years.removed),
  dataset$realised.exp.rate.TAC[-(1:years.removed),1], axes = FALSE, type="l",
  ylim=c(0,1),xlab = "", ylab = "",lty=2)
axis(4)

# various indicators
max.min.ratio= max(data.to.plot)/min(data.to.plot)

sim.indicators=list()
sim.indicators[[1]]=c(years.removed,stages)
sim.indicators[[2]]=period
sim.indicators[[3]]=max.min.ratio
names(sim.indicators)= c("analysis","dominant period (annual)","max/min abundance ratio")
sim.indicators
}





##############################################################
#  Percent abundance by groups of stages over time
##############################################################
analysis$percent.plots.f= function(dataset,plot.type=1,years.removed=100){

# the catch data has one less column than the population data as the first
# column of the population data is the input starting population

if (plot.type==1){
  #1 = proportion of TM males population which is fishable (legal)
  pop.data= dataset$n.crab.year[,-(1:years.removed)]
  TM.male.bin= stage.binaries.f("TM.males")
  fishable.male.bin= stage.binaries.f("fishable.males")
  prop.fishable= round(t(pop.data)%*%fishable.male.bin / t(pop.data)%*%TM.male.bin, 2)
  plot((years.removed+1):(length(prop.fishable)+years.removed),prop.fishable,
    type="l", xlab="Simulation time (year)", ylab="Proportion TM males which are legal"
    )#, ylim=c(0,1))
  }
if (plot.type==2){
  #1 = proportion of fishable (legal) population which was fished
  pop.data= dataset$n.crab.year[,-(1:years.removed)]
  fished.data= dataset$catch[-(1:years.removed),]
  total.catch= apply(fished.data,1,sum)
  fishable.male.bin= stage.binaries.f("fishable.males")
  prop.fished= round(total.catch / (t(pop.data)%*%fishable.male.bin)[-1], 2)
  plot((years.removed+1):(length(prop.fished)+years.removed),prop.fished,
    type="l", xlab="Simulation time (year)", ylab="Proportion legal males fished"
    )#, ylim=c(0,1))
  }
if (plot.type==3){
  #1 = proportion of TM males population which was fished
  pop.data= dataset$n.crab.year[,-(1:years.removed)]
  fished.data= dataset$catch[-(1:years.removed),]
  TM.male.bin= stage.binaries.f("TM.males")
  total.catch= apply(fished.data,1,sum)
  prop.fished= round(total.catch / (t(pop.data)%*%TM.male.bin)[-1], 2)
  plot((years.removed+1):(length(prop.fished)+years.removed),prop.fished,
    type="l", xlab="Simulation time (year)", ylab="Proportion TM males fished"
    )#, ylim=c(0,1))
  }
if (plot.type==4){
  #1 = proportion reproductive females which are multiparous
  months.removed= years.removed*2
  pop.data= dataset$n.crab.6month[,-(1:months.removed)]
  primi.bin= stage.binaries.f("primiparous.females")
  multi.bin= stage.binaries.f("multiparous.females")
  prop.multi= round(t(pop.data)%*%multi.bin / (t(pop.data)%*%multi.bin +
                                               t(pop.data)%*%primi.bin),2)
  # remove every other time step as it always=1 because of the molting times of
  # females. i.e. females molt only one per year and therefore primi = 0 every
  # other time step
  prop.multi= prop.multi[prop.multi<1]
  plot(seq((years.removed+1),(length(prop.multi)+ years.removed),length=length(prop.multi)),prop.multi,
    type="l", xlab="Simulation time (year)",
    ylab="Proportion multiparous reproductive females"
    )#,ylim=c(0,1))
  }
}





##############################################################
# Plots of sex ratio over time
##############################################################
analysis$sex.ratio.f= function(dataset,years.removed=100){

# dataset = output list from a simulation run
# plot.type = the kind of plot to make (i.e. which instars to select to plot)
# no.years.remove = number of initial years to exclude in the plot

layout(matrix(1:3,3,1))
###### Average size in the sea

  months.removed= years.removed
  data= dataset$n.crab.year[,-(1:months.removed)]


### ratio with legal TM males condition 2 to 4
  male.abund= t(data)%*%stage.binaries.f("TM.mal.2to4.legal")
  fem.abund= t(data)%*%stage.binaries.f("TM.females")
  sex.ratio= logb(fem.abund/male.abund,10)
  plot((months.removed+1):(length(sex.ratio)+months.removed),sex.ratio,
    type="l", main="TM females to condition 2-4 TM legal males",
    xlab="Simulation time step (year)", ylab="log10 sex ratio")

### ratio with sublegal TM males condition 2 to 4
  # ie. sublegal component is the whole 2-4 TM males minus the legal ones
  male.abund= (t(data)%*%stage.binaries.f("TM.mal.2to4"))-male.abund
  fem.abund= t(data)%*%stage.binaries.f("TM.females")
  sex.ratio= logb(fem.abund/male.abund,10)
  plot((months.removed+1):(length(sex.ratio)+months.removed),sex.ratio,
    type="l", main="TM females to condition 2-4 TM sublegal males",
    xlab="Simulation time step (year)", ylab="log10 sex ratio")

### ratio with all TM males condition 2 to 4
  male.abund= t(data)%*%stage.binaries.f("TM.mal.2to4")
  fem.abund= t(data)%*%stage.binaries.f("TM.females")
  sex.ratio= logb(fem.abund/male.abund,10)
  plot((months.removed+1):(length(sex.ratio)+months.removed),sex.ratio,
    type="l", main="TM females to condition 2-4 TM males",
    xlab="Simulation time step (year)", ylab=" log10 sex ratio")

}




##############################################################
# Plots abundance by stage over time
##############################################################
analysis$stage.by.stage.f= function(dataset,plot.instars=1:20,time.steps.removed=100){

data= dataset[[1]][,-(1:time.steps.removed)]
type.vector=c("all","instar1","instar2","instar3","instar4",
  "instar5","instar6","instar7","instar8","instar9","instar10","instar11",
  "instar12","instar13","instar14","TM.males","TM.females","fishable.males",
  "prime.fishable.males","softshell.males.9.10.11")

layout(matrix(1:20,nrow=5,ncol=4))

for (plot.type in plot.instars){
stages= type.vector[plot.type]
data.to.plot= t(data)%*%stage.binaries.f(stages)
plot((time.steps.removed+1):(ncol(data)+time.steps.removed),data.to.plot,
  type="l", xlab="Simulation time step (6 month)", ylab=stages)
  }
}

analysis$stage.by.stage.f(tmp,,0)



##############################################################
# Power spectrum and autocorrelation plots
##############################################################
analysis$power.f= function(model.output){

# plot the power spectrum for fishable males, both in the sea and
# in the catch

#n.adults.m.9 = apply(model.output[[2]][26:35,],2,sum)
#n.adults.m.10 = apply(model.output[[2]][38:47,],2,sum)
#n.adults.m.11 = apply(model.output[[2]][50:61,],2,sum)
n.adults.m.12 = apply(model.output[[2]][64:75,],2,sum)
n.adults.m.13 = apply(model.output[[2]][78:91,],2,sum)
n.adults.m.14 = apply(model.output[[2]][94:107,],2,sum)

# n.small.adults.m = n.adults.m.9 + n.adults.m.10 + n.adults.m.11
n.big.adults.m = n.adults.m.12 + n.adults.m.13 + n.adults.m.14
span.size= c(21,21)

layout(matrix(1:6,3,2))
tmp=spectrum(n.big.adults.m,spans=span.size,plot=F)
dominant.freq= tmp$freq[match(max(tmp$spec),tmp$spec)]
#plot smoothed power spectrum
  spectrum(n.big.adults.m,spans=span.size)
  lines(c(dominant.freq,dominant.freq),range(tmp$spec),col=2)
  text(dominant.freq,max(tmp$spec)/2,as.character(round(1/dominant.freq,1)),col=2,cex=3)
#plot cumulative periodiogram
  cpgram(n.big.adults.m)
#plot autocorrelation
  acf(n.big.adults.m)
  dominant.freq
}




##############################################################
# Plots of average sizes of TM males in sea and catch
##############################################################
analysis$TM.male.sizes.f= function(dataset,years.removed=100){

# dataset = output list from a simulation run
# plot.type = the kind of plot to make (i.e. which instars to select to plot)
# no.years.remove = number of initial years to exclude in the plot

layout(matrix(1:4,2,2))
###### Average size in the sea

  months.removed= years.removed
  data= dataset$n.crab.year[,-(1:months.removed)]

  stages= "TM.males"

  # makes the plot though matrix multiplication on the binary vector. This selects
  # only the instars of interest (1 in the binary matrix) and sums over them for a
  #time step and then plots over the simulation period
  fished.stages= stage.binaries.f(stages)
  data.to.plot= t(data)%*%fished.stages

  #caparapace width
  CW= stage.binaries.f("CW")*fished.stages
  mean.CW.sea=(t(data)%*%CW)/data.to.plot

  plot((months.removed+1):(length(data.to.plot)+months.removed),mean.CW.sea,
    type="l", xlab="Simulation time step (year)", ylab="mean CW-TM males in sea (mm)")

  # body weight

  wgt= stage.binaries.f("wgt")*fished.stages
  mean.wgt.sea=(t(data)%*%wgt)/data.to.plot

  plot((months.removed+1):(length(data.to.plot)+months.removed),mean.wgt.sea,
    type="l", xlab="Simulation time step (year)", ylab="mean weight-TM males in sea (g)")

###### Average size in catch

  # carapace width
  reduced.catch.data= as.matrix(dataset$catch[-(1:years.removed),])
  catch.data= apply(reduced.catch.data,1,sum)
  CW.catch= c(50.7,50.7,50.7,50.7,50.7,50.7,64.5,64.5,64.5,64.5,64.5,64.5,
          79.7,79.7,79.7,79.7,79.7,79.7,96.7,96.7,96.7,96.7,96.7,96.7,
          115,115,115,115,115,115,115,115,115,115,115,
          47.2,47.2,47.2,47.2,47.2,47.2,
          56.4,56.4,56.4,56.4,56.4,56.4,
          68,68,68,68,68)
  mean.CW.catch= (reduced.catch.data%*%CW.catch)/catch.data
  plot((years.removed+1):(length(mean.CW.catch)+years.removed),mean.CW.catch,
    type="l", xlab="Simulation time step (year)", ylab="mean CW-TM males in catch (mm)")

  # body weight
  wgt.catch= c(51.987,51.987,51.987,51.987,51.987,51.987,108.8,108.8,108.8,108.8,108.8,108.8,
          208.4,208.4,208.4,208.4,208.4,208.4,377.1,377.1,377.1,377.1,377.1,377.1,
          642,642,642,642,642,642,642,642,642,642,642,
          35.9,35.9,35.9,35.9,35.9,35.9,
          58,58,58,58,58,58,
          96,96,96,96,96)
  mean.wgt.catch= (reduced.catch.data%*%wgt.catch)/catch.data
  plot((years.removed+1):(length(mean.wgt.catch)+years.removed),mean.wgt.catch,
    type="l", xlab="Simulation time step (year)", ylab="mean weight-TM males in catch (g)")

}
# TM.male.sizes.f(xx25)

