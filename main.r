main.f=function(time, breadth=params$breadth, offset=params$offset, refuge=params$refuge) {
  #main.f - calculate a vector of abundance for each six months
  #Input t : the number of years simulation
  #	  params (created by create.params)
  # breadth, offset and refuge and related to parameterising cannibalism

  #function required:
  #       matrix.f
  #				skip.f
  #				TM.f
  #				egg.mort.f
  #				sperm.lim.f
  #				cannibalism.f
  #				calc.val
  #				fishing.f
  #				larv.surv.f
  #				graph.total
  #				graph.adults.m
  #Authors: Daniel Duplisea, Red Méthot, Jean-Francois Roy, Denis Gilbert
  #Maurice Lamontagne Institute, Dept. of Fisheries and Oceans Canada
##############################################################################

  # make available parameters that were orignally in function argument but now in parameters file
    m= matrix(params$starting.abundance,ncol=1) #starting abundance
    temp= params$temp #bottom water temperature
    exp.rate= params$exploitation.rate  #exploitation rate of fishery

  # The base leslie matrix (survivorship matrix)
    A.base= Leslie.matrix.f()

  # calculate cannibalism matrix (list), this really should be changed to a matrix not a list.
    simple.cannibalism= eaters.f(breadth,offset,refuge)

  # setup the output matrices to which results will be written inside the loop
    #catch will contain the biomass fished for each condition and instar crab see fishing.f
      catch=matrix(0,nrow=time,ncol=52)
      catch= as.data.frame(catch)
      names(catch)= c("9.1","9.2", "9.3", "9.4", "9.5", "9ado", "10.1", "10.2", "10.3", "10.4", "10.5", "10ado",
      "11.1", "11.2", "11.3", "11.4", "11.5", "11ado", "12.1","12.2", "12.3", "12.4", "12.5", "12ado",
      "13.1", "13.2", "13.3", "13.4", "13.5", "13ado", "14.1", "14.2", "14.3", "14.4", "14.5",
      "9.1fem", "9.2fem", "9.3fem", "9.4fem", "9.5fem", "9juv.fem",
      "10.1fem", "10.2fem", "10.3fem", "10.4fem", "10.5fem", "10juv.fem",
      "11.1fem", "11.2fem", "11.3fem", "11.4fem", "11.5fem" )
      realised.exp.rate.TAC=matrix(nrow=time,ncol=3)
    #n.crab.year will contain the vector of abundance for each year
      n.crab.year = matrix(0,nrow(m),ncol=time+1)
      n.crab.year[,1] = m
    #n.crab.6months will contain the vector of abundance for each 6 months
      n.crab.6months = matrix(0,nrow(m),ncol=(time*2)+1)  
      n.crab.6months[,1] = m

#########################  Temporal simulation loop ##############################
  for (i in 1:time)		{
		  #two time-steps per year
		  for (j in 1:2)	{
      AA=A.base #use basic survivorship matrix at start of sim

      # aggregate numbers at age and stage into 14 males and 14 female stages
        # need to remove 1 and 15 as they are dummay holders for first stage of males and females respectively
        stage.m= aggregate(m, by=list(params$stages), FUN=sum)[-c(1,15),2]
  
      #skip and terminal molting only occur at first j step of the year
      #vérifier si j=1 pour tous les stades, des stades de 6 mois pourraient skiper à j=2??
        if (params$skip==1)	{
        #calculate de percentage of skip molting
        out.skip=skip.f(m,AA,stage.m,j,simple.cannibalism)
        AA=out.skip[[1]]
      }
        else if (params$skip==0)	{
        #dummy matrix and vectors of 0
        out.skip=skip.f(m,AA,stage.m,j,simple.cannibalism)
        out.skip[[1]]=(AA+1)/(AA+1)-1
        out.skip[[2]]=(m+1)/(m+1)-1
      }
        if (j==1)	{
        #the base matrix is change to reflect the molting choice (terminal or not)
        #! AA takes on negative values after this step. Corrected in TM.f DD 17 June 2005
        AA=TM.f(m,AA,temp)
    	}
        #the fecundity could be modified by sexe-ratio (sperm could be limited)
        if (params$sperm.lim==1 & j==1)	{
        out.lim=sperm.lim.f(m, AA, out.lim, i, temp)
        AA=out.lim[[1]]
      }
        #the fecundity could be modified by sexe-ratio (sperm could be limited)
        # when no sperm limitation then the sperm limitation is calculated as in
        # the first step, i.e. no sperm limitation in 1st step (i=1) and therefore
        # all the percentages are set to 0 and primi- multi-parous are calculated
        # from the sex ratio.
        else if (params$sperm.lim==0 & j==1)	{
        out.lim=sperm.lim.f(m, AA, out.lim, 1, temp)
        AA=out.lim[[1]]
      }


      # egg.mort.f calculate the number of eggs lost before the stage larvae
      out.egg.mort=egg.mort.f(m,AA,temp,out.lim,out.egg.mort, i,j)
      AA=out.egg.mort[[1]]

        #if cannibalism == 1, we calculate the factor of cannibalism
        if (params$cannibalism == 1){
        AA=cannibalism.f(m,AA,j,stage.m,temp,simple.cannibalism)
      }
        #Fishing occur only at second 6 months time-step
        if (params$fishing==1 & j==2)	{
        out.fish=fishing.f(AA,m,exp.rate,out.skip,i,catch)
        AA=out.fish[[1]]
        catch=out.fish[[2]]
        realised.exp.rate.TAC[i,]=out.fish[[3]]
      }

    #Larvae survivorship and population recruitment
       # pelagic temperature is draw form log-normal with mean=9.7
       surface.temperature= eval(params$random.pelagic.temperature)
       larvae.survival= larv.surv.f(AA,surface.temperature)
       #modify the Leslie matrix according to larval survivorship
       AA[5,4] = larvae.survival
       AA[112,111] = larvae.survival

    # Leslie matrix calculation. Why is this not at the end of the loop? DD 30 April 06
      m= AA %*% m
      
      
    #limit of settlement for Instar 1 is 300 000 000
      m[5] [m[5] > 300000000] = params$lim.settle.instar1 #male recruitment
      m[112] [m[112] > 300000000] = params$lim.settle.instar1 #female recruitment

      #to take out the "fractions" of indivual
      #! This was changed so that when fractional it is set to 1 not 0.
      #! DD 17 June 2005
      #m[m<0.5] = 0
      m[m<1 & m>0] = 1

    # this is a multiplier of recruitment drawn from a log normal function with mean = 0
      R.multiplier= eval(params$stochastic.recruitment.multiplier)
      m[4]= m[4] * R.multiplier # males
      m[111]= m[111] * R.multiplier # females

    # write the results of the loop to the data storage structure
      if (j==1)	{
        n.crab.6months[,(i*2)] = m
        }
      if (j==2)	{
        n.crab.6months[,(i*2)+1] = m
        }
		
    }
    #at the end of the loop, we have 1 time step calculated
    #so we keep the m for this year and go to the time step
    n.crab.year[,i+1] = m
    #barplot(aggregate(stage.m,list(rep(1:14,2)),sum)$x) #plots results during the simulation
    }

#! the model is a 6 month model and the 1 year output is just for every other
#! time step. So the output for the year model in step t corresponds to the
#! output in teh 6month model at 2*t-1. e.g. column 114 of the year output for
#! a simulation longer than 114 years will have the exact same population size
#! stage for the 6 month model in column 227.  DD 16 June 2005
#! that is, this is actually a model with a 6 month time step and the 1 year model
#! just records output on every other step.

cannibalism.matrix= matrix(0,ncol=length(simple.cannibalism)-1,nrow=length(simple.cannibalism)-1)
for (i in 1:(length(simple.cannibalism)-1)) { cannibalism.matrix[i,simple.cannibalism[[i]]]=1}

n.crab=list()
n.crab[[1]]=n.crab.6months
#n.crab[[2]]=n.crab.year
n.crab[[2]]=catch #kg by fished cohort
n.crab[[3]]=as.data.frame(realised.exp.rate.TAC)
n.crab[[4]]=params
n.crab[[5]]=as.data.frame(cannibalism.matrix)
names(n.crab)= c("n.crab.6months", "catch","realised.exp.rate.TAC","scenario","cannibalism.matrix")
names(n.crab$realised.exp.rate.TAC)=c("realised.pop.exp.rate","TAC","fishable.numbers")
names(n.crab$cannibalism.matrix)=c(paste("pred.instar",1:14,sep=""))
n.crab
}

