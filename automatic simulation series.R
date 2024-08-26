runsims.f= function(){

breadth=1:5
offset=1:5
refuge=7:10

no.simulations=length(breadth)*length(offset)*length(refuge)
indicator.output= matrix(ncol=5,nrow=no.simulations)
simulation.output=list()
counter=0
for (b1 in breadth){
  for (o1 in offset){
    for (r1 in refuge){
      counter=counter+1
      simulation.output[[counter]]= main.f(300,b1,o1,r1)  # here 300
      indicator.output[counter,]= c(b1,o1,r1,
              indicators.f(simulation.output[[counter]],100))   # and here 100
      }
    }
  }

indicator.output=as.data.frame(indicator.output)
names(indicator.output)= c("breadth","offset","refuge","period","max.min.ratio")
output= list()
output[[1]]= expand.grid(refuge,offset,breadth)[,3:1]
names(output[[1]])=c("breadth","offset","refuge")
output[[2]]= indicator.output
output[[3]]= simulation.output
names(output)=c("run.suite","indicators","simulations")
output
}
#sims.noF.r=runsims.f()

indicators.f= function(dataset,years.removed=100){
months.removed= years.removed
data= dataset$n.crab.year[,-(1:months.removed)]
stages= "TM.males"
data.to.plot= t(data)%*%stage.binaries.f(stages)
tmp.ps= spectrum(data.to.plot,taper=0,detrend=T,plot= F)
period= 1/tmp.ps$freq[match(max(tmp.ps$spec),tmp.ps$spec)]
max.min.ratio= max(data.to.plot)/min(data.to.plot)
out.indicators= c(period,max.min.ratio)
out.indicators
}

