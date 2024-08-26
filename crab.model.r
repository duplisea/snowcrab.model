##################################################
# SNOW CRAB POPULATION STAGE-AGE MATRIX MODEL
# Daniel Duplisea, Red Methot, Jean-Francois Gosselin, Denis Gilbert, Bernard Sainte-Marie
# 2002 - 2007
# Needs complete recoding - probably full of errors 17 Oct 2007, DD
##################################################




############
# A list with all the model functions
############
crab.model= list()




############
#Main function calling subfunctions
############
crab.model$main.f=function(time=25, breadth=inputs$params$breadth, offset.instar=inputs$params$offset.instar, refuge=inputs$params$refuge) {
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
    m= matrix(inputs$params$starting.abundance,ncol=1) #starting abundance
    temp= inputs$params$temp #bottom water temperature
    exp.rate= inputs$params$exploitation.rate  #exploitation rate of fishery

  # The base leslie matrix (survivorship matrix)
    A.base= crab.model$Leslie.matrix.f()

  # calculate cannibalism matrix (list), this really should be changed to a matrix not a list.
    simple.cannibalism= crab.model$eaters.f(breadth,offset.instar,refuge)

  # setup the output matrices to which results will be written inside the loop
    #catch will contain the biomass fished for each condition and instar crab see fishing.f
      catch=matrix(0, nrow= time,ncol=52)
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
        stage.m= aggregate(m, by=list(inputs$params$stages), FUN=sum)[-c(1,15),2]

      #skip and terminal molting only occur at first j step of the year
      #vérifier si j=1 pour tous les stades, des stades de 6 mois pourraient skiper à j=2??
        if (inputs$params$skip==1)	{
        #calculate de percentage of skip molting
        out.skip=crab.model$skip.f(m,AA,stage.m,j,simple.cannibalism)
        AA=out.skip[[1]]
      }
        else if (inputs$params$skip==0)	{
        #dummy matrix and vectors of 0
        out.skip=crab.model$skip.f(m,AA,stage.m,j,simple.cannibalism)
        out.skip[[1]]=(AA+1)/(AA+1)-1
        out.skip[[2]]=(m+1)/(m+1)-1
      }
        if (j==1)	{
        #the base matrix is change to reflect the molting choice (terminal or not)
        #! AA takes on negative values after this step. Corrected in TM.f DD 17 June 2005
        AA=crab.model$TM.f(m,AA,temp)
    	}
        #the fecundity could be modified by sexe-ratio (sperm could be limited)
        if (inputs$params$sperm.lim==1 & j==1)	{
        out.lim=crab.model$sperm.lim.f(m, AA, out.lim, i, temp)
        AA=out.lim[[1]]
      }
        #the fecundity could be modified by sexe-ratio (sperm could be limited)
        # when no sperm limitation then the sperm limitation is calculated as in
        # the first step, i.e. no sperm limitation in 1st step (i=1) and therefore
        # all the percentages are set to 0 and primi- multi-parous are calculated
        # from the sex ratio.
        else if (inputs$params$sperm.lim==0 & j==1)	{
        out.lim=crab.model$sperm.lim.f(m, AA, out.lim, 1, temp)
        AA=out.lim[[1]]
      }


      # egg.mort.f calculate the number of eggs lost before the stage larvae
      out.egg.mort=crab.model$egg.mort.f(m,AA,temp,out.lim,out.egg.mort, i,j)
      AA=out.egg.mort[[1]]

        #if cannibalism == 1, we calculate the factor of cannibalism
        if (inputs$params$cannibalism == 1){
        AA=crab.model$cannibalism.f(m,AA,j,stage.m,temp,simple.cannibalism)
      }
        #Fishing occur only at second 6 months time-step
        if (inputs$params$fishing==1 & j==2)	{
        out.fish=crab.model$fishing.f(AA,m,exp.rate,out.skip,i,catch)
        AA=out.fish[[1]]
        catch=out.fish[[2]]
        realised.exp.rate.TAC[i,]=out.fish[[3]]
      }

    #Larvae survivorship and population recruitment
       # pelagic temperature is draw form log-normal with mean=9.7
       surface.temperature= eval(inputs$params$random.pelagic.temperature)
       larvae.survival= crab.model$larv.surv.f(AA,surface.temperature)
       #modify the Leslie matrix according to larval survivorship
       AA[5,4] = larvae.survival
       AA[112,111] = larvae.survival

    # Leslie matrix calculation. Why is this not at the end of the loop? DD 30 April 06
      m= AA %*% m


    #limit of settlement for Instar 1 is 300 000 000
      m[5] [m[5] > 300000000] = inputs$params$lim.settle.instar1 #male recruitment
      m[112] [m[112] > 300000000] = inputs$params$lim.settle.instar1 #female recruitment

      #to take out the "fractions" of indivual
      #! This was changed so that when fractional it is set to 1 not 0.
      #! DD 17 June 2005
      #m[m<0.5] = 0
      m[m<1 & m>0] = 1

    # this is a multiplier of recruitment drawn from a log normal function with mean = 0
      R.multiplier= eval(inputs$params$stochastic.recruitment.multiplier)
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

n.crab=list()
n.crab[[1]]=n.crab.6months
#n.crab[[2]]=n.crab.year
n.crab[[2]]=catch # in biomass units
n.crab[[3]]=as.data.frame(realised.exp.rate.TAC)
n.crab[[4]]=inputs$params
n.crab[[5]]=simple.cannibalism
names(n.crab)= c("n.crab.6months", "catch","realised.exp.rate.TAC","scenario","cannibalism.matrix")
names(n.crab$realised.exp.rate.TAC)=c("realised.pop.exp.rate","TAC","fishable.numbers")
n.crab
}


############
# Leslie matrix of survival rates
############


crab.model$Leslie.matrix.f= function(){

  #extract the parameters from the list params
  larvs.pct = inputs$params$larvs.pct
  ext.death = inputs$params$ext.death
  CW.9 = inputs$params$CW.9
  CW.10 = inputs$params$CW.10
  CW.11 = inputs$params$CW.11
  stages.lst = inputs$params$stages.lst

  #A is the basic Leslie survivorship matrix
  #To keep track of each cohort, at each time step every cohorts move to another stage. For example,
  #a crab of Instar X at terminal molt will be X-TM1 , X-TM2 the next time step...
  #males 1:107, females 108:214

  A = matrix(0,nrow=length(stages.lst),ncol=length(stages.lst))

  #########################################################
  ######		survival rates			#########
  #########################################################
  #P is the proportion of the survivors from non-cannibalistic M, for each stage
  #see also ext_death.xls

  #P is the proportion of the survivors from non-cannibalistic M, for each stage
  #P is elevated at the power 1/2 because there are 2 treatments a year
  P = (1 - (ext.death)) ^ (1/2)

  #0.1 is attributed to molting, thus TM do not suffer this mortality
  #P2 is apply to adult (TM)
  P2 = (1 - (ext.death-0.1)) ^ (1/2)


  #P for males
  # part to change to modify mortality rates

  A[2,1] = 1						#egg(1) to egg(2)
  A[3,2] = 1						#egg(2) to egg(3)
  A[4,3] = 1						#egg(3) to larvae
  A[4,1] = 1						#egg(1) to larvae
  A[5,4] = larvs.pct				#larvae to Instar I (will be modified by the function larv.perc)
  A[6,5] = 0						#Instar I to I-skip(1)
  A[7,6] = P2[1,2]					#Instar I-skip(1) to I-skip(2)
  A[8,7] = P[1,2]					#Instar I-skip() to II
  A[8,5] = P[1,2]					#Instar I to II
  A[9,8] = 0						#Instar II to II-skip(1)
  A[10,9] = P2[2,2]					#Instar II-skip(1) to II-skip(2)
  A[11,10] = P[2,2]					#Instar II-skip(2) to III
  A[11,8] = P[2,2]					#Instar II to III
  A[12,11] = 0					#Instar III to III-skip(1)
  A[13,12] = P2[3,2]				#Instar III-skip(1) to III-skip(2)
  A[14,13] = 	P[3,2]				#Instar III-skip(2) to IV
  A[14,11] = P[3,2]					#Instar III to IV
  A[15,14] = 0					#Instar IV to IV-skip(1)
  A[16,15] = P2[4,2]				#Instar IV-skip(1) to IV-skip(2)
  A[17,16] = P[4,2]					#Instar IV-skip(2) to V
  A[17,14] = P[4,2]					#Instar IV to V
  A[18,17] = 0					#Instar V to V-skip(1)
  A[19,18] = P2[4,2]				#Instar V-skip(1) to V-skip(2)
  A[20,19] = P[4,2]					#Instar V-skip(2) to VI (1)
  A[20,17] = P[4,2]					#Instar V to VI(1)

  A[21,20] = P[6,2]					#Instar VI(1) to VI(2)
  A[22,21] = P[6,2]					#Instar VI(2) to VII(1)
  A[23,22] = P[7,2]					#Instar VII(1) to VII(2)
  A[24,23] = P[7,2]					#Instar VII(2) to VIII(1)
  A[25,24] = P[8,2]					#Instar VIII(1) to VIII(2)
  A[26,25] = P[8,2]					#Instar VIII(2) to IX-TM1
  A[27,26] = P2[9,2]				#Instar IX-TM1 to IX-TM2
  A[28,27] = P2[9,2]				#Instar IX-TM2 to IX-TM3
  A[29,28] = (1-0.60)^0.5				#Instar IX-TM3 to IX-TM4 (0.60 is a annual mortality rate)
  A[30,29] = (1-0.80)^0.5				#Instar IX-TM4 to IX-TM5 (P is manually assigned)
  A[31,30] = (1-0.80)^0.5				#Instar IX-TM5 to IX-TM6 (P is manually assigned)
  A[32,31] = P2[9,2]				#Instar IX-TM6 to IX-TM7
  A[33,32] = (1-0.60)^0.5				#Instar IX-TM7 to IX-TM8
  A[34,33] = (1-0.80)^0.5				#Instar IX-TM8 to IX-TM9 (P is manually assigned)
  A[35,34] = (1-0.80)^0.5				#Instar IX-TM9 to IX-TM10 (P is manually assigned)
  A[36,25] = P[8,2]					#Instar VIII(2) to IX(1)
  A[37,36] = P[9,2]					#Instar IX(1) to IX(2)
  A[38,37] = P[9,2]					#Instar IX(2) to X-TM1
  A[39,38] = P2[10,2]				#Instar X-TM1 to X-TM2
  A[40,39] = P2[10,2]				#Instar X-TM2 to X-TM3
  A[41,40] = (1-0.60)^0.5				#Instar X-TM3 to X-TM4
  A[42,41] = (1-0.80)^0.5				#Instar X-TM4 to X-TM5 (P is manually assigned)
  A[43,42] = (1-0.80)^0.5				#Instar X-TM5 to X-TM6 (P is manually assigned)
  A[44,43] = P2[9,2]				#Instar X-TM6 to X-TM7
  A[45,44] = (1-0.60)^0.5				#Instar X-TM7 to X-TM8
  A[46,45] = (1-0.80)^0.5				#Instar X-TM8 to X-TM9 (P is manually assigned)
  A[47,46] = (1-0.80)^0.5				#Instar X-TM9 to X-TM10 (P is manually assigned)

  A[48,37] = P[9,2]					#Instar IX(2) to X(1)
  A[49,48] = P[10,2]				#Instar X(1) to X(2)
  A[50,49] = P[10,2]				#Instar X(2) to XI-TM1
  A[51,50] = P2[11,2]				#Instar XI-TM1 to XI-TM2
  A[52,51] = P2[11,2]				#Instar XI-TM2 to XI-TM3
  A[53,52] = P2[11,2]				#Instar XI-TM3 to XI-TM4
  A[54,53] = (1-0.60)^0.5				#Instar XI-TM4 to XI-TM5
  A[55,54] = (1-0.60)^0.5				#Instar XI-TM5 to XI-TM6
  A[56,55] = (1-0.80)^0.5				#Instar XI-TM6 to XI-TM7 (P is manually assigned)
  A[57,56] = (1-0.80)^0.5				#Instar XI-TM7 to XI-TM8 (P is manually assigned)
  A[58,57] = P2[9,2]				#Instar XI-TM8 to XI-TM9
  A[59,58] = (1-0.60)^0.5				#Instar XI-TM9 to XI-TM10
  A[60,59] = (1-0.80)^0.5				#Instar XI-TM10 to XI-TM11 (P is manually assigned)
  A[61,60] = (1-0.80)^0.5				#Instar XI-TM11 to XI-TM12 (P is manually assigned)
  A[62,49] = P[10,2]				#Instar X(2) to XI(1)
  A[63,62] = P[11,2]				#Instar XI(1) to XI(2)
  A[64,63] = P[11,2]				#Instar XI(2) to XII-TM1
  A[65,64] = P2[12,2]				#Instar XII-TM1 to XII-TM2
  A[66,65] = P2[12,2]				#Instar XII-TM2 to XII-TM3
  A[67,66] = P2[12,2]				#Instar XII-TM3 to XII-TM4
  A[68,67] = (1-0.60)^0.5				#Instar XII-TM4 to XII-TM5
  A[69,68] = (1-0.60)^0.5				#Instar XII-TM5 to XII-TM6
  A[70,69] = (1-0.80)^0.5				#Instar XII-TM6 to XII-TM7 (P is manually assigned)
  A[71,70] = (1-0.80)^0.5				#Instar XII-TM7 to XII-TM8 (P is manually assigned)
  A[72,71] = P2[9,2]				#Instar XII-TM8 to XII-TM9
  A[73,72] = (1-0.60)^0.5				#Instar XII-TM9 to XII-TM10
  A[74,73] = (1-0.80)^0.5				#Instar XII-TM10 to XII-TM11 (P is manually assigned)
  A[75,74] = (1-0.80)^0.5				#Instar XII-TM11 to XII-TM12 (P is manually assigned)
  A[76,63] = P[11,2]				#Instar XI(2) to XII(1)
  A[77,76] = P[12,2]				#Instar XII(1) to XII(2)
  A[78,77] = P[12,2]				#Instar XII(2) to XIII-TM1
  A[79,78] = P2[13,2]				#Instar XIII-TM1 to XIII-TM2
  A[80,79] = P2[13,2]				#Instar XIII-TM2 to XIII-TM3
  A[81,80] = P2[13,2]				#Instar XIII-TM3 to XIII-TM4
  A[82,81] = P2[13,2]				#Instar XIII-TM4 to XIII-TM5
  A[83,82] = (1-0.50)^0.5				#Instar XIII-TM5 to XIII-TM6
  A[84,83] = (1-0.60)^0.5				#Instar XIII-TM6 to XIII-TM7
  A[85,84] = (1-0.60)^0.5				#Instar XIII-TM7 to XIII-TM8
  A[86,85] = (1-0.80)^0.5				#Instar XIII-TM8 to XIII-TM9 (P is manually assigned)
  A[87,86] = (1-0.80)^0.5				#Instar XIII-TM9 to XIII-TM10 (P is manually assigned)
  A[88,87] = P2[9,2]				#Instar XIII-TM10 to XIII-TM11
  A[89,88] = (1-0.60)^0.5				#Instar XIII-TM11 to XIII-TM12
  A[90,89] = (1-0.80)^0.5				#Instar XIII-TM12 to XIII-TM13 (P is manually assigned)
  A[91,90] = (1-0.80)^0.5				#Instar XIII-TM13 to XIII-TM14 (P is manually assigned)
  A[92,77] = P[12,2]				#Instar XII(2) to XIII(1)
  A[93,92] = P[13,2]				#Instar XIII(1) to XIII(2)
  A[94,93] = P[13,2]				#Instar XIII(2) to XIV-TM1
  A[95,94] = P2[14,2]				#Instar XIV-TM1 to XIV-TM2
  A[96,95] = P2[14,2]				#Instar XIV-TM2 to XIV-TM3
  A[97,96] = P2[14,2]				#Instar XIV-TM3 to XIV-TM4
  A[98,97] = (1-0.40)^0.5				#Instar XIV-TM4 to XIV-TM5
  A[99,98] = (1-0.50)^0.5				#Instar XIV-TM5 to XIV-TM6
  A[100,99] = (1-0.60)^0.5				#Instar XIV-TM6 to XIV-TM7
  A[101,100] = (1-0.70)^0.5				#Instar XIV-TM7 to XIV-TM8
  A[102,101] = (1-0.80)^0.5				#Instar XIV-TM8 to XIV-TM9 (P is manually assigned)
  A[103,102] = (1-0.80)^0.5				#Instar XIV-TM9 to XIV-TM10 (P is manually assigned)
  A[104,103] = P2[9,2]				#Instar XIV-TM10 to XIV-TM11
  A[105,104] = (1-0.60)^0.5				#Instar XIV-TM11 to XIV-TM12
  A[106,105] = (1-0.80)^0.5				#Instar XIV-TM12 to XIV-TM13 (P is manually assigned)
  A[107,106] = (1-0.80)^0.5				#Instar XIV-TM13 to XIV-TM14 (P is manually assigned)



  #P for females

  A[109,108] = 1					#egg(1) to egg(2)
  A[110,109] = 1					#egg(2) to egg(3)
  A[111,110] = 1					#egg(3) to larvae
  A[111,108] = 1					#egg(1) to larvae
  A[112,111] = larvs.pct				#larvae to Instar I
  A[113,112] = 0					#Instar I to I-skip(1)
  A[114,113] = P2[1,1]				#Instar I-skip(1) to I-skip(2)
  A[115,114] = P[1,1]				#Instar I-skip(2) to II
  A[115,112] = P[1,1]				#Instar I to II
  A[116,115] = 0					#Instar II to II-skip(1)
  A[117,116] = P2[2,1]				#Instar II-skip(1) to II-skip(2)
  A[118,117] = P[2,1]				#Instar II-skip(2) to III
  A[118,115] = P[2,1]				#Instar II to III
  A[119,118] = 0					#Instar III to III-skip(1)
  A[120,119] = P2[3,1]				#Instar III-skip(1) to III-skip(2)
  A[121,120] = P[3,1] 				#Instar III-skip(2) to IV
  A[121,118] = P[3,1]				#Instar III to IV
  A[122,121] = 0					#Instar IV to IV-skip(1)
  A[123,122] = P2[4,1]				#Instar IV-skip(1) to IV-skip(2)
  A[124,123] = P[4,1]				#Instar IV-skip(2) to V
  A[124,121] = P[4,1]				#Instar IV to V
  A[125,124] = 0					#Instar V to V-skip(1)
  A[126,125] = P2[5,1]				#Instar V-skip(1) to V-skip(2)
  A[127,126] = P[5,1]				#Instar V-skip(2) to VI(1)
  A[127,124] = P[5,1]					#Instar V to VI(1)
  A[128,127] = P[6,1]					#Instar VI(1) to VI(2)
  A[129,128] = P[6,1]					#Instar VI(2) to VII(1)
  A[130,129] = P[7,1]					#Instar VII(1) to VII(2)
  A[131,130] = P[7,1]					#Instar VII(2) to VIII(1)
  A[132,131] = P[8,1]					#Instar VIII(1) to VIII(2)
  A[133,132] = P[8,1]					#Instar VIII(2) to IX-TM1
  A[134,133] = P2[9,1]				#Instar IX-TM1 to IX-TM2
  A[135,134] = P2[9,1]				#Instar IX-TM2 to IX-TM3
  A[136,135] = P2[9,1]				#Instar IX-TM3 to IX-TM4
  A[137,136] = (1-0.80)^0.5				#Instar IX-TM4 to IX-TM5 (P is manually assigned)
  A[138,137] = (1-0.80)^0.5				#Instar IX-TM5 to IX-TM6 (P is manually assigned)
  A[139,138] = P2[9,2]				#Instar IX-TM6 to IX-TM7
  A[140,139] = (1-0.60)^0.5				#Instar IX-TM7 to IX-TM8
  A[141,140] = (1-0.80)^0.5				#Instar IX-TM8 to IX-TM9 (P is manually assigned)
  A[142,141] = (1-0.80)^0.5				#Instar IX-TM9 to IX-TM10 (P is manually assigned)
  A[143,132] = P[8,1]					#Instar VIII(2) to IX(1)
  A[144,143] = P[9,1]					#Instar IX(1) to IX(2)
  A[145,144] = P[9,1]					#Instar IX(2) to X-TM1
  A[146,145] = P2[10,1]				#Instar X-TM1 to X-TM2
  A[147,146] = P2[10,1]				#Instar X-TM2 to X-TM3
  A[148,147] = P2[10,1]				#Instar X-TM3 to X-TM4
  A[149,148] = (1-0.80)^0.5			#Instar X-TM4 to X-TM5 (P is manually assigned)
  A[150,149] = (1-0.80)^0.5			#Instar X-TM5 to X-TM6 (P is manually assigned)
  A[151,150] = P2[9,2]				#Instar X-TM6 to X-TM7
  A[152,151] = (1-0.60)^0.5				#Instar X-TM7 to X-TM8
  A[153,152] = (1-0.80)^0.5				#Instar X-TM8 to X-TM9 (P is manually assigned)
  A[154,153] = (1-0.80)^0.5				#Instar X-TM9 to X-TM10 (P is manually assigned)
  A[155,144] = P[9,1]				#Instar IX(2) to X(1)
  A[156,155] = P[10,1]				#Instar X(1) to X(2)
  A[157,156] = P[10,1]				#Instar X(2) to XI-TM1
  A[158,157] = P2[11,1]				#Instar XI-TM1 to XI-TM2
  A[159,158] = P2[11,1]				#Instar XI-TM2 to XI-TM3
  A[160,159] = P2[11,1]				#Instar XI-TM3 to XI-TM4
  A[161,160] = P2[11,1]				#Instar XI-TM4 to XI-TM5
  A[162,161] = P2[11,1]				#Instar XI-TM5 to XI-TM6
  A[163,162] = (1-0.80)^0.5			#Instar XI-TM6 to XI-TM7 (P is manually assigned)
  A[164,163] = (1-0.80)^0.5			#Instar XI-TM7 to XI-TM8 (P is manually assigned)
  A[165,164] = P2[9,2]				#Instar XI-TM8 to IX-TM9
  A[166,165] = (1-0.60)^0.5				#Instar XI-TM9 to IX-TM10
  A[167,166] = (1-0.80)^0.5				#Instar XI-TM10 to IX-TM11 (P is manually assigned)
  A[168,167] = (1-0.80)^0.5				#Instar XI-TM11 to IX-TM12 (P is manually assigned)

  #for the fecundity of the Instar 9,10 and 11
  #we take the mean carapace width of each stage
  #we calculate the number of eggs produced by a female at this CW
  #we calculate a number for primiparous females and one for multiparous females
  #then, we calculate a mean number of eggs, with the proportion
  #of primiparous and multiparous females

  sexe.ratio=0.5		#females/total
  #the mortality of female is included in the calcul of fecundity
  #! THIS IS WHERE A TAKES ON VALUES > 1
  primi = crab.model$fecundity.f(CW.9,"primiparous",)
  A[1,133] = round(primi*(1-sexe.ratio)*A[134,133])
  A[108,133]= round(primi*(sexe.ratio)*A[134,133])

  multi = crab.model$fecundity.f(CW.9,"multiparous",)
  A[1,135] = round(multi*(1-sexe.ratio)*A[136,135])
  A[108,135] = round(multi*sexe.ratio*A[136,135])
  A[1,137] = round(multi*(1-sexe.ratio)*A[138,137])
  A[108,137] = round(multi*sexe.ratio*A[138,137])
  A[1,139] = round(multi*(1-sexe.ratio)*A[140,139]*0.6)		#the 0.6 is added because old female have less fecundity
  A[108,139] = round(multi*sexe.ratio*A[140,139]*0.6)		#the 0.6 is added because old female have less fecundity

  primi = crab.model$fecundity.f(CW.10,"primiparous",)
  A[1,145] = round(primi*(1-sexe.ratio)*A[146,145])
  A[108,145] = round(primi*sexe.ratio*A[146,145])

  multi = crab.model$fecundity.f(CW.10,"multiparous",)
  A[1,147] = round(multi*(1-sexe.ratio)*A[148,147])
  A[108,147] = round(multi*sexe.ratio*A[148,147])
  A[1,149] = round(multi*(1-sexe.ratio)*A[150,149])
  A[108,149] = round(multi*sexe.ratio*A[150,149])
  A[1,151] = round(multi*(1-sexe.ratio)*A[152,151]*0.6)		#the 0.6 is added because old female have less fecundity
  A[108,151] = round(multi*sexe.ratio*A[152,151]*0.6)		#the 0.6 is added because old female have less fecundity

  primi = crab.model$fecundity.f(CW.11,"primiparous",)
  A[1,157] = round(primi*(1-sexe.ratio)*A[158,157])
  A[108,157]= round(primi*sexe.ratio*A[158,157])

  multi = crab.model$fecundity.f(CW.11,"multiparous",)
  A[1,159] = round(multi*(1-sexe.ratio)*A[160,159])
  A[108,159]=round(multi*sexe.ratio*A[160,159])
  A[1,161] = round(multi*(1-sexe.ratio)*A[162,161])
  A[108,161]=round(multi*sexe.ratio*A[162,161])
  A[1,163] = round(multi*(1-sexe.ratio)*A[164,163]*0.6)	#the 0.6 is added because old female have less fecundity
  A[108,163]=round(multi*sexe.ratio*A[164,163]*0.6)	#the 0.6 is added because old female have less fecundity
  A[1,165] = round(multi*(1-sexe.ratio)*A[166,165]*0.4)	#the 0.4 is added because old female have less fecundity
  A[108,165]=round(multi*sexe.ratio*A[166,165]*0.4)	#the 0.4 is added because old female have less fecundity

  #A_base is now the complete basic survivorship leslie matrix
  A.base = A
  A.base
  }
  #shite=Leslie.matrix.f()
  #write.table(shite,"d:/tmp/shite.csv",sep=";")




  
############
# Cannibalism foodweb matrix
############
crab.model$eaters.f= function(breadth=inputs$params$breadth,offset=inputs$params$offset,refuge.instar=inputs$params$refuge){

  #! This creates the foodweb matrix for cannibalism, i.e who eats whom.
  # changed to a function with a knife edge predator prey cannibalism function
  # offset will control the minimum instar distance between predators and prey,
  # breadth control the number of predator instars on each prey instar and
  # refuge is the instar upon which there is no cannibalism (there and thereafter)

  # simple cannibalism matrix. To be used for all seasons and situations.

  # the last vector [[15]] is created to solve the technical
  # problem of null vector but is not use in the model

  simple.cannibalism1=offset + (1:breadth)
  simple.cannibalism2=simple.cannibalism1 + 1
  simple.cannibalism3=simple.cannibalism1 + 2
  simple.cannibalism4=simple.cannibalism1 + 3
  simple.cannibalism5=simple.cannibalism1 + 4
  simple.cannibalism6=simple.cannibalism1 + 5
  simple.cannibalism7=simple.cannibalism1 + 6
  simple.cannibalism8=simple.cannibalism1 + 7
  simple.cannibalism9=simple.cannibalism1 + 8
  simple.cannibalism10=simple.cannibalism1 + 9
  simple.cannibalism11=simple.cannibalism1 + 10
  simple.cannibalism12=simple.cannibalism1 + 11
  simple.cannibalism13=simple.cannibalism1 + 12
  simple.cannibalism14=simple.cannibalism1 + 13
  simple.cannibalism15=c(0)

  # puts the food web into a list and truncates at the largest
  # predator instar (ie. 14)
  simple.cannibalism=list()
  simple.cannibalism[[1]]=simple.cannibalism1[simple.cannibalism1<15]
  simple.cannibalism[[2]]=simple.cannibalism2[simple.cannibalism1<14]
  simple.cannibalism[[3]]=simple.cannibalism3[simple.cannibalism1<13]
  simple.cannibalism[[4]]=simple.cannibalism4[simple.cannibalism1<12]
  simple.cannibalism[[5]]=simple.cannibalism5[simple.cannibalism1<11]
  simple.cannibalism[[6]]=simple.cannibalism6[simple.cannibalism1<10]
  simple.cannibalism[[7]]=simple.cannibalism7[simple.cannibalism1<9]
  simple.cannibalism[[8]]=simple.cannibalism8[simple.cannibalism1<8]
  simple.cannibalism[[9]]=simple.cannibalism9[simple.cannibalism1<7]
  simple.cannibalism[[10]]=simple.cannibalism10[simple.cannibalism1<6]
  simple.cannibalism[[11]]=simple.cannibalism11[simple.cannibalism1<5]
  simple.cannibalism[[12]]=simple.cannibalism12[simple.cannibalism1<4]
  simple.cannibalism[[13]]=simple.cannibalism13[simple.cannibalism1<3]
  simple.cannibalism[[14]]=simple.cannibalism14[simple.cannibalism1<2]
  simple.cannibalism[[15]]=simple.cannibalism15

  # the last vector [[15]] is created to solve the technical
  # problem of null vector but is not use in the model

  names(simple.cannibalism)= c( "simple.cannibalism1", "simple.cannibalism2",
    "simple.cannibalism3", "simple.cannibalism4", "simple.cannibalism5",
    "simple.cannibalism6", "simple.cannibalism7", "simple.cannibalism8",
    "simple.cannibalism9", "simple.cannibalism10", "simple.cannibalism11",
    "simple.cannibalism12", "simple.cannibalism13", "simple.cannibalism14","crap")

  # remove the values for the refuge instars, or when the difference between predator
  # and prey instar exceeds 14. i.e. there are no predators larger than 14 as there are
  # no individuals in the population larger than 14.

  max.instar.eaten= (15-offset)
  if (refuge.instar<max.instar.eaten) min.instar=refuge.instar else min.instar=max.instar.eaten
    for (i in min.instar:15){
      simple.cannibalism[[i]]= 0
    }

  simple.cannibalism
}




############
# Function to make decision on skip molting
############
crab.model$skip.f=function(m,AA,stage.m,j,simple.cannibalism)		{
  #what happen to stage with no cannibalism, only competiton then how much stages must be included???
  #this function determine de % of crab of each stage that skip a molt
  #when a crab skip a molt, it stay to actual stage or returns to anterior stage if Instar have two stages
  #e.g. stage from stage XIII(2) will return to stage XIII(1)
  #
  #The percentage of skip molt is determine by ratio of abundance of actual Instar / abondance of
  #other stages that interact with actual Instar by competition and/or cannibalism. The other stages
  # are every stage higher until the last stage that can be cannibalistic.
  #
  #Call by calc.t
  #Input: 	m : abundance vector
  #		AA : base leslie matrix
  #		params (create from create.params)
  #Output: list out.skip= AA : base matrix modified by skip percentage
  #				skip.m : abundance of each skipped molt stage (used in fishing.f)
  #
  #Red Méthot - January 2005

  #mid is the number of stages
  mid = length(stage.m)/2
  #this function is used to get male and female at the same stage

  #merge male and female in an abundance matrix
  instar.m=matrix(0,nrow = (length(stage.m)/2), ncol=1)
  for (i in 1:(length(stage.m)/2))	{
  instar.m[i]=stage.m[i] + stage.m[i+mid]
  }


  #ratio is calculated for each stages
  perc.skip=matrix(0,nrow=length(instar.m),ncol=1)

  for (k in 1:mid)	{
  #we extract the cannibalism stage that affect each stages
  #following the 6 months time-step, there's different predator stages
  if (inputs$params$simple.cannibalism.on==0){
    if (j==1) {
    eaters.exp=eaters.summer.exp[[k]]
      }

    else if (j==2) {
    eaters.exp=eaters.winter.exp[[k]]
      }
    }

  else if (inputs$params$simple.cannibalism.on==1){
      eaters.exp=simple.cannibalism[[k]]
    }

  if (length(eaters.exp) != 0){
  ratio=instar.m[k]/ sum(instar.m[(k+1):(max(eaters.exp))])
  ratio [ sum(instar.m[(k+1):(max(eaters.exp))])==0  ]=1
    }
  #For the Instar not affected by canibalism only competition from 3 Instars higher will affect skip molt
  if (length(eaters.exp) == 0){
  	if (k <= 11)	{
  	ratio=instar.m[k]/sum(instar.m[(k+1):(k+3)])
  	ratio [ sum(instar.m[(k+1):(k+3)])==0  ]=1
  	}
  	else if (k >= 11)	{
  	ratio=instar.m[k]/sum(instar.m[(k+1):14])
  	ratio [ sum(instar.m[(k+1):14])==0  ]=1
  	}

  }
  #perc.skip could be between 0 and 0.2
  #no skip if ratio higher then 0.05
  # max if ratio go close to zero
  perc.skip[k]= -4*ratio + 0.2

  perc.skip[k][perc.skip[k] < 0] = 0

  }


  #the leslie matrix is directly modified to allow skip molting
  AA[6,5]=perc.skip[1]*AA[8,5]
  AA[9,8]=perc.skip[2]*AA[12,8]
  AA[12,11]=perc.skip[3]*AA[14,11]
  AA[15,14]=perc.skip[4]*AA[17,14]
  AA[18,17]=perc.skip[5]*AA[20,17]
  AA[20,21]=perc.skip[6]*AA[22,21]
  AA[22,23]=perc.skip[7]*AA[24,23]
  AA[24,25]=perc.skip[8]*AA[26,25]
  AA[36,37]=perc.skip[9]*AA[38,37]
  AA[48,49]=perc.skip[10]*AA[50,49]
  AA[62,63]=perc.skip[11]*AA[64,63]
  AA[76,77]=perc.skip[12]*AA[78,77]
  AA[92,93]=perc.skip[13]*AA[94,93]

  AA[113,112]=perc.skip[1]*AA[115,112]
  AA[116,115]=perc.skip[2]*AA[118,115]
  AA[119,118]=perc.skip[3]*AA[120,118]
  AA[122,121]=perc.skip[4]*AA[124,121]
  AA[125,124]=perc.skip[5]*AA[127,124]
  AA[127,128]=perc.skip[6]*AA[129,128]
  AA[129,130]=perc.skip[7]*AA[131,130]
  AA[131,132]=perc.skip[10]*AA[133,132]
  AA[143,144]=perc.skip[8]*AA[145,144]
  AA[155,156]=perc.skip[9]*AA[157,156]

  #the percentage of molting crabs (TM or not) is modified
  #males

  AA[8,5] = AA[8,5]*(1-perc.skip[1])
  AA[12,8] = AA[12,8]*(1-perc.skip[2])
  AA[14,11] = AA[14,11]*(1-perc.skip[3])
  AA[17,14] = AA[17,14]*(1-perc.skip[4])
  AA[20,17] = AA[20,17]*(1-perc.skip[5])
  AA[22,21] = AA[22,21]*(1-perc.skip[6])
  AA[24,23] = AA[24,23]*(1-perc.skip[7])
  AA[26,25] = AA[26,25]*(1-perc.skip[8])
  AA[38,37] = AA[38,37]*(1-perc.skip[9])
  AA[48,37] = AA[38,37]
  AA[50,49] = AA[50,49]*(1-perc.skip[10])
  AA[62,49] = AA[50,49]
  AA[64,63] = AA[64,63]*(1-perc.skip[11])
  AA[76,63] = AA[64,63]
  AA[78,77] = AA[78,77]*(1-perc.skip[12])
  AA[92,77] = AA[78,77]
  AA[94,93] = AA[94,93]*(1-perc.skip[13])



  #females


  AA[115,112] = AA[115,112]*(1-perc.skip[1])
  AA[118,115] = AA[118,115]*(1-perc.skip[2])
  AA[120,118] = AA[120,118]*(1-perc.skip[3])
  AA[124,121] = AA[124,121]*(1-perc.skip[4])
  AA[127,124] = AA[127,124]*(1-perc.skip[5])
  AA[129,128] = AA[129,128]*(1-perc.skip[6])
  AA[131,130] = AA[131,130]*(1-perc.skip[7])
  AA[133,132] = AA[133,132]*(1-perc.skip[8])
  AA[143,132] = AA[133,132]
  AA[145,144] = AA[145,144]*(1-perc.skip[9])
  AA[155,144] = AA[145,144]
  AA[157,156] = AA[157,156]*(1-perc.skip[10])



  #keep the number of skipped-molt crabs (will be use in fishing.f) from Instar XIII to XIV
  skip.m=matrix(0,nrow=9,ncol=1)

  #males
  skip.m[1,1]=AA[24,25]*m[25]
  skip.m[2,1]=AA[36,37]*m[37]
  skip.m[3,1]=AA[48,49]*m[49]
  skip.m[4,1]=AA[62,63]*m[63]
  skip.m[5,1]=AA[76,77]*m[77]
  skip.m[6,1]=AA[92,93]*m[93]
  #females
  skip.m[7,1]=AA[131,132]*m[132]
  skip.m[8,1]=AA[143,144]*m[144]
  skip.m[9,1]=AA[155,156]*m[156]


  out.skip=list()
  out.skip[[1]]=AA
  out.skip[[2]]=skip.m
  names(out.skip)= c("AA", "skip.m")
  out.skip
}





############
# Function to make the terminal molt decision
############
crab.model$TM.f=function(m,AA,temp){
  #Molt decision
  #the "decision" will be function of regime temperature and abundance of females
  # and dominant males
  #see excel sheets: partitioning


  #this function determine de % of crab of each stage that do terminal molt or not
  #See also Ratio_TM.xls
  #A terminal molt occur from stage XIII(2) to XIII(2)
  #Call by calc.t
  #Input: 	m : abundance vector
  #		AA : matrix modified by skip molting
  #		temp : water temperature - will affect terminal Molt decision
  #		params (create from create.params)
  #
  #Output: AA : matrix modified by molting "decision"
  #
  #Red Méthot - January 2005



  #number of adult females (TM)
  n.fem.TM=sum(m[133:142]) + sum(m[145:154]) + sum(m[157:168])

  #weigthed number of males at each Instar (14 having more weigth then 8)
  val=inputs$params$weigth.male

  n.mal.8 = sum(m[24:25])*val[8]
  n.mal.9 = sum(m[26:37])*val[9]
  n.mal.10 = sum(m[38:49])*val[10]
  n.mal.11 = sum(m[45:63])*val[11]
  n.mal.12 = sum(m[64:77])*val[12]
  n.mal.13 = sum(m[78:93])*val[13]
  n.mal.14 = sum(m[94:107])*val[14]

  #ratio
  #each Instar is "preoccupied" by number of males of equal or larger size
  ratio.8=n.fem.TM/(n.mal.8 + n.mal.9 + n.mal.10 + n.mal.11 + n.mal.12 + n.mal.13 + n.mal.14)
  ratio.8[(n.mal.8 + n.mal.9 + n.mal.10 + n.mal.11 + n.mal.12 + n.mal.13 + n.mal.14)==0] = 5.1

  ratio.9=n.fem.TM/(n.mal.9 + n.mal.10 + n.mal.11 + n.mal.12 + n.mal.13 + n.mal.14)
  ratio.9[(n.mal.9 + n.mal.10 + n.mal.11 + n.mal.12 + n.mal.13 + n.mal.14)==0] = 5.1

  ratio.10=n.fem.TM/sum(n.mal.10 + n.mal.11 + n.mal.12 + n.mal.13 + n.mal.14)
  ratio.10[(n.mal.10 + n.mal.11 + n.mal.12 + n.mal.13 + n.mal.14)==0] = 5.1

  ratio.11=n.fem.TM/sum(n.mal.11 + n.mal.12 + n.mal.13 + n.mal.14)
  ratio.11[sum(n.mal.11 + n.mal.12 + n.mal.13 + n.mal.14)==0] = 5.1

  ratio.12=n.fem.TM/(n.mal.12 + n.mal.13 + n.mal.14)
  ratio.12[(n.mal.12 + n.mal.13 + n.mal.14)==0] = 5.1

  #the possible percentages to do TM is function of temperature, when temperature is cold
  #TM will be earlier
  #see TM.xls

  if (temp < 0.8) {

  #At ratio <= 0.1, the cohort is minimum proportion of  TM
  #At ratio >= 5, the % maximum of the cohort do TM

  #Instar VIII (min=0.2 max=0.4)
  perc.TM=0.051*ratio.8 + 0.0449
  perc.TM [ratio.8 <= 0.1]= 0.2
  perc.TM [ratio.8 > 5]= 0.4

  AA[26,25]=AA[26,25]*perc.TM
  AA[36,25]=AA[36,25]*(1-perc.TM)


  #Instar IX (min=0.5, max=0.8)
  perc.TM= 0.051*ratio.9 + 0.0949
  perc.TM[ratio.9 <= 0.1]= 0.5
  perc.TM[ratio.9 > 5]= 0.8

  AA[38,37]=AA[38,37]*perc.TM
  AA[48,37]=AA[48,37]*(1-perc.TM)


  #Instar X  (min=0.5, max=0.8)
  perc.TM= 0.0816*ratio.10 + 0.1918
  perc.TM [ratio.10 <= 0.1] = 0.5
  perc.TM [ratio.10 > 5] = 0.8

  AA[50,49]=AA[50,49]*perc.TM
  AA[62,49]=AA[62,49]*(1-perc.TM)


  #Instar XI  (min=0.5, max=0.8)
  perc.TM= 0.0204*ratio.11 + 0.048
  perc.TM [ratio.11 <= 0.1] = 0.5
  perc.TM [ratio.11 > 5] = 0.8

  AA[64,63]=AA[64,63]*perc.TM
  AA[76,63]=AA[76,63]*(1-perc.TM)


  #Instar XII   (min=0.6, max=0.8)
  perc.TM= 0.0204*ratio.12 + 0.048
  perc.TM [ratio.12 <= 0.1] = 0.6
  perc.TM [ratio.12 > 5] = 0.8

  AA[78,77]=AA[78,77]*perc.TM
  AA[92,77]=AA[92,77]*(1-perc.TM)



  #females - ratio of females / all males (=ratio.8)

  #if ratio.8 < 0.5, % minimum of the cohort do TM
  #if ratio.8 >= 1, % maximum of the cohort do TM

  #Instar VIII      (min=0.4, max=0.6)
  perc.TM.fem = 0.4*ratio.8 + 0.2
  perc.TM [ratio.8 < 0.5]= 0.4
  perc.TM [ratio.8 >= 1] = 0.6

  AA[133,132]=AA[133,132]*perc.TM
  AA[143,122]=AA[143,132]*(1-perc.TM)


  #Instar IX       (min=0.6, max=0.8)
  perc.TM.fem = 0.4*ratio.8 + 0.4
  perc.TM [ratio.8 < 0.5]= 0.6
  perc.TM [ratio.8 >= 1] = 0.8

  AA[145,144]=AA[145,144]*perc.TM
  AA[155,124]=AA[155,144]*(1-perc.TM)
  }


  ##############################################################################################

  else if (temp >= 0.8) {

  #At ratio <= 0.1, the % minimum of the cohort do TM
  #At ratio >= 5, the % maximum of the cohort do TM

  #Instar VIII    (min=0, max=0.1)
  #! this function can create negative values in the Leslie matrix as the intercept
  #! is negative. Therefore a switch is set to make the value 0 if less than 0.
  #! DD 17 June 2005
  perc.TM = 0.0204*ratio.8 - 0.002
  perc.TM[perc.TM<0]=0 #! this is the added switch DD 17 June 2005
  perc.TM[ratio.9 <= 0.1]=0
  perc.TM[ratio.9 > 5]= 0.1

  AA[26,25]=AA[26,25]*perc.TM
  AA[36,25]=AA[36,25]*(1-perc.TM)


  #Instar IX    (min=0.05, max=0.25)
  perc.TM= 0.0408*ratio.9 + 0.0459
  perc.TM[ratio.9 <= 0.1]=0.05
  perc.TM[ratio.9 > 5]= 0.25

  AA[38,37]=AA[38,37]*perc.TM
  AA[48,37]=AA[48,37]*(1-perc.TM)


  #Instar X   (min=0.2, max=0.6)
  perc.TM=0.0816*ratio.10 + 0.1918
  perc.TM [ratio.10 <= 0.1] = 0.2
  perc.TM [ratio.10 > 5] = 0.6

  AA[50,49]=AA[50,49]*perc.TM
  AA[62,49]=AA[62,49]*(1-perc.TM)


  #Instar XI     (min=0.4, max=0.8)
  perc.TM= 0.0816*ratio.11 + 0.3918
  perc.TM [ratio.11 <= 0.1] = 0.4
  perc.TM [ratio.11 > 5] = 0.8

  AA[64,63]=AA[64,63]*perc.TM
  AA[76,63]=AA[76,63]*(1-perc.TM)


  #Instar XII   (min=0.6, max=0.8)
  perc.TM= 0.0408*ratio.12 + 0.5959
  perc.TM [ratio.12 <= 0.1] = 0.6
  perc.TM [ratio.12 > 5] = 0.8

  AA[78,77]=AA[78,77]*perc.TM
  AA[92,77]=AA[92,77]*(1-perc.TM)

  ##########
  ###females - ratio of females / all males (=ratio.8)
  #########

  #if ratio.8 < 0.5, % minimum of the cohort do TM
  #if ratio.8 >= 1, % maximum of the cohort do TM


  #Instar VIII   (min=0.1, max=0.3)
  perc.TM.fem = 0.4*ratio.8 - 0.1
  perc.TM [ratio.8 < 0.5]= 0.1
  perc.TM [ratio.8 >= 1] = 0.3

  AA[133,132]=AA[133,132]*perc.TM
  AA[143,132]=AA[143,132]*(1-perc.TM)

  #Instar IX    (min=0.5, max=0.8)
  perc.TM.fem = 0.6*ratio.8 + 0.2
  perc.TM [ratio.8 < 0.5]= 0.5
  perc.TM [ratio.8 >= 1] = 0.8

  AA[145,144]=AA[145,144]*perc.TM
  AA[155,124]=AA[155,144]*(1-perc.TM)

  }
  AA
}






############
# Function that determines how sperm limited is the population
############
crab.model$sperm.lim.f=function(m,AA,out.lim,i,temp){

  #this function determine de % of recruitment lost because of sperm limitation
  #The percentage of lost is determine by ratio of abundance of females / abondance of males
  #for multiparous and by a ratio of dominant male / subordinate male for primiparous
  #Call by calc.t
  #Input: 	m : abundance vector
  #		AA : base leslie matrix
  #		out.lim: indications of sperm limitation from anterior reproducion
  #
  #Output: list out.lim= 	AA : base matrix modified by skip percentage
  #				lim.primi: indicator of sperm limitation at primiparous reproduction
  #				lim.multi.1: indicator of sperm limitation at first multiparous reproduction
  #				lim.multi.2: indicator of sperm limitation at second multiparous reproduction
  #				lim.multi.3: indicator of sperm limitation at third multiparous reproduction
  #! NOTE DANIEL, there is not sperm limitation in 1st year so when the option is turned
  #! off (=0) in the parameters file then the sperm.lim function is calculated in every
  #! time step as though it were the first time step. 1 June 2005
  #Red Méthot - January 2005
  #! pct.primi and pct.multi are not the percentages of females in each category but the
  #! of eggs unfertilised in each category owing to incomplete fertilisation. DD 15 June 2005


  #those parameters are first set to 0 by default (no sperm limitation the years before)
  if (i==1)	{
  lim.primi = 0
  lim.multi.1 = 0
  lim.multi.2 = 0
  lim.multi.3 = 0
  }

  if (i > 1)	{
  lim.primi = out.lim[[2]]
  lim.multi.1 = out.lim[[3]]
  lim.multi.2 = out.lim[[4]]
  lim.multi.3 = out.lim[[5]]
  }

  #multiparous



  n.multi = sum(m[135:139]) + sum(m[147:151]) +  sum(m[159:165])

  #only male able to mate (the soft-shell and moribund crabs are not included)
  n.mal = sum(m[28:33]) + sum(m[40:45]) +  sum(m[52:59])
   + sum(m[66:73]) + sum(m[80:89]) +  sum(m[96:105])

  sex.ratio = n.mal/(n.multi + n.mal)
  sex.ratio[n.multi + n.mal==0] = 0
  #if sex.ratio is bellow 0.1 sperm.limitation occurs
      #values of sex.ratio can be between 0 and 0.1
      #depending on sex.ratio, pct will be between 0.5 and 1
      pct.multi = 1.67*sex.ratio + 0.5
      pct.multi[pct.multi >= 1] = 1
      #we multiply the fecundity of the matrix by pct.multi
      #the lower sex.ratio is, the lower the fecundity will become




  if (pct.multi != 1 & temp >= 0.8)  {

  #if the sperm was limited last year the fecundity of multiparous 4 will be affected by sex.ratio
  if (lim.multi.3==1)	{
  AA[1,165] = AA[1,165] * pct.multi

  AA[108,165] = AA[108,165] * pct.multi
  }

  #if the sperm was limited last year the fecundity of multiparous 3 will be affected by sex.ratio
  lim.multi.3=0			#to keep track of the sperm limitation (0=none, 1=limitation)
  if (lim.multi.2==1)	{
  AA[1,139] = AA[1,139] * pct.multi
  AA[1,151] = AA[1,151] * pct.multi
  AA[1,163] = AA[1,163] * pct.multi

  AA[108,139] = AA[108,139] * pct.multi
  AA[108,151] = AA[108,151] * pct.multi
  AA[108,163] = AA[108,163] * pct.multi
  #keep track of the limitation for next year
  lim.multi.3=1
  }

  #if the sperm was limited last year the fecundity of multiparous 2 will be affected by sex.ratio
  lim.multi.2=0			#to keep track of the sperm limitation (0=none, 1=limitation)
  if (lim.multi.1==1)	{
  AA[1,137] = AA[1,137] * pct.multi
  AA[1,149] = AA[1,149] * pct.multi
  AA[1,161] = AA[1,161] * pct.multi

  AA[108,137] = AA[108,137] * pct.multi
  AA[108,149] = AA[108,149] * pct.multi
  AA[108,161] = AA[108,161] * pct.multi
  #keep track of the limitation for next year
  lim.multi.2=1
  }

  #if the sperm was limited last year the fecundity of multiparous 1 will be affected by sex.ratio
  lim.multi.1=0			#to keep track of the sperm limitation (0=none, 1=limitation)
  if (lim.primi==1)	{
  AA[1,135] = AA[1,135] * pct.multi
  AA[1,147] = AA[1,147] * pct.multi
  AA[1,159] = AA[1,159] * pct.multi

  AA[108,135] = AA[108,135] * pct.multi
  AA[108,147] = AA[108,147] * pct.multi
  AA[108,159] = AA[108,159] * pct.multi
  #keep track of the limitation for next year
  lim.multi.1=1
  }
  }


  if (temp < 0.8){

  #if the sperm was limited 2nd last years the fecundity of multiparous 2 will be affected by sex.ratio
  if (lim.multi.1==1)	{
  AA[1,165] = AA[1,165] * pct.multi
  AA[108,155] = AA[108,165] * pct.multi
  }

  #if the sperm was limited 2nd last year the fecundity of multiparous 1 will be affected by sex.ratio
  lim.multi.1=0			#to keep track of the sperm limitation (0=none, 1=limitation)
  if (lim.multi.2==1)	{
  AA[1,137] = AA[1,137] * pct.multi
  AA[1,149] = AA[1,149] * pct.multi
  AA[1,161] = AA[1,161] * pct.multi

  AA[108,137] = AA[103,137] * pct.multi
  AA[108,149] = AA[103,149] * pct.multi
  AA[108,161] = AA[103,161] * pct.multi
  #keep track of the limitation for next year
  lim.multi.1=1
  }
  }

  #primiparous
  n.primi= m[133] + m[145] + m[157]
  #only male able to mate (the soft-shell and moribund crabs are not included)
  n.small.m =sum(m[28:33]) + sum(m[40:45]) +  sum(m[52:59])
  n.big.m = sum(m[66:72]) + sum(m[80:89]) +  sum(m[96:105])
  ratio.big= n.big.m/n.small.m
  ratio.big[n.small.m == 0] = 0
  lim.primi=0			#to keep track of the sperm limitation (0=none, 1=limitation)
  if (ratio.big <= 0.3)	{

      #values of ratio.big can be between 0 and 0.3
      #depending on ratio.big, pct.primi will be between 0.5 and 1
  	pct.primi = 16.2*(ratio.big^2) - 4.8571*ratio.big + 1
      #we multiply the fecundity of the matrix by pct
      #The perc(lost of eggs) raise from ratio.big = 0.3 to 0.15 (max) and decline to zero at 0
      #(since not enough big males are present to "control" mating

  #keep track for next time steps of sperm limitation
  lim.primi=1}

  #if ratio > 0.3 no sperm limitation
  else if (ratio.big > 0.3){
  pct.primi=1
  }

  #males
  AA[1,133] = AA[1,133]* pct.primi
  AA[1,145] = AA[1,145]* pct.primi
  AA[1,157] = AA[1,157]* pct.primi

  #females
  AA[108,133] = AA[108,133]* pct.primi
  AA[108,145] = AA[108,145]* pct.primi
  AA[108,157] = AA[108,157]* pct.primi

  # out.lim is created here on the first time step. So the assignments
  # above in this routine are only done when i>1 . In the first time step
  # (i==1) the values on lim.primi, multi etc = 0.
  out.lim = list()
  out.lim[[1]] = AA
  out.lim[[2]] = lim.primi
  out.lim[[3]] = lim.multi.1
  out.lim[[4]] = lim.multi.2
  out.lim[[5]] = lim.multi.3
  out.lim[[6]] = pct.primi
  out.lim[[7]] = pct.multi

  out.lim
}


############
# Function to determine egg mortality
############
crab.model$egg.mort.f=function(m,AA,temp,out.lim,out.egg.mort, i,j){

  #By the time the larvae are released female mortality occur and the eggs are lost
  #we apply the female mortality to the eggs stages.
  #eggs mortality is a weigthed mean (by number of eggs coming from each stages)
  #of mortality of females carrying eggs
  #
  #Call by calc.t
  #Input: 	m : abundance vector
  #		AA : leslie matrix modified by molting choice (regular, terminal or skip)
  #		temp : temperature
  #		out.lim : percentage of lost eggs from sperm limitation
  #		i : year of simulation
  #		j : time of the year (1 or 2)
  #
  #Output: 	AA : 	matrix modified
  #		pct.primi2 : percentage of sperm limitation of primiparous from last time-step
  #		pct.multi2 : percentage of sperm limitation of multiparous from last time-step
  #		pct.primi3 : percentage of sperm limitation of primiparous from second last time-step
  #		pct.multi3 : percentage of sperm limitation of multiparous from second last time-step
  #Red Méthot - December 2005

  #extract the percentage of sperm limitation (from sperm.lim.f)
  pct.primi=out.lim[[6]]
  pct.multi=out.lim[[7]]


  #if first time step no pct.primi2, pct.multi2, pct.primi2 and pct.multi2
  # are set to 1 by default
  if (i+j==2)	{
  pct.primi2=1
  pct.multi2=1
  pct.primi3=1
  pct.multi3=1
  }


  else if (i+j >= 3)	{
  pct.primi2=out.egg.mort$pct.primi2
  pct.multi2=out.egg.mort$pct.multi2
  pct.primi3=out.egg.mort$pct.primi3
  pct.multi3=out.egg.mort$pct.multi3
  }

  #the matrix use to keep track of number off eggs producede by each stages
  n.egg=matrix(0,nrow=12,ncol=2)

  #############################################################################################
  ####################		one-year egg production		#################################
  #############################################################################################
  #when temperature >= 0.8

  #mortality from egg1 to larvae
  #males
  #number of eggs from each stages


  if (temp >= 0.8)	{
  #block the way for two years development of eggs
  AA[2,1]= 0
  AA[102,103]= 0

  #the abundance of female at actual time-step * fecundity * survival of female stages
  #male (n.egg[,2])

  #Instar IX
  n.egg[1,2]=m[134]*AA[1,133]*AA[135,134]*pct.primi
  n.egg[2,2]=m[136]*AA[1,135]*AA[137,136]*pct.multi
  n.egg[3,2]=m[138]*AA[1,137]*AA[139,138]*pct.multi
  n.egg[4,2]=m[140]*AA[1,139]*AA[141,140]*pct.multi

  #Instar X
  n.egg[5,2]=m[146]*AA[1,145]*AA[147,146]*pct.primi
  n.egg[6,2]=m[148]*AA[1,147]*AA[149,148]*pct.multi
  n.egg[7,2]=m[150]*AA[1,149]*AA[151,150]*pct.multi
  n.egg[8,2]=m[152]*AA[1,151]*AA[153,152]*pct.multi

  #Instar XI
  n.egg[9,2]=m[158]*AA[1,157]*AA[159,158] *pct.primi
  n.egg[10,2]=m[160]*AA[1,159]*AA[161,160] *pct.multi
  n.egg[11,2]=m[162]*AA[1,161]*AA[163,162] *pct.multi
  n.egg[12,2]=m[164]*AA[1,163]*AA[165,164] *pct.multi

  #total number of male eggs after female mortality
  n.egg.total=sum(n.egg[,2])

  #the mortality of egg1 to larvae
  AA[4,1]=n.egg.total/m[1]
  if (m[1]==0){AA[4,1]=0}

  #female (n.egg[,1])

  #Instar IX
  n.egg[1,1]=m[134]*AA[108,133]*AA[135,134]
  n.egg[2,1]=m[136]*AA[108,135]*AA[137,136]
  n.egg[3,1]=m[138]*AA[108,137]*AA[139,138]
  n.egg[4,1]=m[140]*AA[108,139]*AA[141,140]

  #Instar X
  n.egg[5,1]=m[146]*AA[108,145]*AA[147,146]
  n.egg[6,1]=m[148]*AA[108,147]*AA[149,148]
  n.egg[7,1]=m[150]*AA[108,149]*AA[151,150]
  n.egg[8,1]=m[152]*AA[108,151]*AA[153,152]

  #Instar XI
  n.egg[9,1]=m[158]*AA[108,147]*AA[149,148]
  n.egg[10,1]=m[160]*AA[108,149]*AA[151,150]
  n.egg[11,1]=m[162]*AA[108,151]*AA[153,152]
  n.egg[12,1]=m[164]*AA[108,153]*AA[155,154]

  #total number of female eggs after female mortality
  n.egg.total=sum(n.egg[,1])

  #the mortality of egg1 to larvae
  AA[111,108]=n.egg.total/m[1]
  if (m[108]==0){AA[111,108]=0}


  }

  ##############################################################################################
  ####			2-years eggs production 				############################
  ##############################################################################################

  #If water temperature < 0.8 the eggs need 2 years of development

  if (temp < 0.8){
  #In the matrix, the way to go directly from stage egg1 to larvae (if eggs production is one year)is blocked
  AA[4,1]= 0
  AA[111,108]= 0

  # less stages can produce eggs while temperature is cold

  AA[1,135] = 0
  AA[108,135] = 0
  AA[1,139] = 0
  AA[108,139] = 0

  AA[1,147] = 0
  AA[108,147] = 0
  AA[1,151] = 0
  AA[108,151] = 0

  AA[1,159] = 0
  AA[108,159]= 0
  AA[1,163] = 0
  AA[108,163]= 0

  ###############################
  #mortality from egg3 to larvae#
  ###############################

  #the abundance of female at actual time-step * fecundity * survival of female stages

  #males
  n.egg[1,2]= m[136]*AA[1,133]*AA[137,136]*pct.primi3
  n.egg[2,2]= m[140]*AA[1,137]*AA[141,140]*pct.multi3

  n.egg[3,2]= m[148]*AA[1,145] *AA[149,148]*pct.primi3
  n.egg[4,2]= m[152]*AA[1,149]*AA[153,152]*pct.multi3

  n.egg[5,2]= m[160]*AA[1,157] *AA[161,160]*pct.primi3
  n.egg[6,2]= m[164]*AA[1,161] *AA[165,164]*pct.multi3


  #total number of male eggs after female mortality
  n.egg.total=sum(n.egg[,2])

  #the mortality of egg3 to larvae
  AA[4,3]=n.egg.total/m[3]
  if (m[3]==0){AA[4,3]=0}

  #females
  n.egg[1,1]= m[136]*AA[108,133]*AA[137,136]*pct.primi3
  n.egg[2,1]= m[140]*AA[108,137]*AA[141,140]*pct.multi3

  n.egg[3,1]= m[148]*AA[108,145]*AA[149,148]*pct.primi3
  n.egg[4,1]= m[152]*AA[108,149]*AA[151,152]*pct.multi3

  n.egg[5,1]= m[160]*AA[108,157]*AA[161,160]*pct.primi3
  n.egg[6,1]= m[164]*AA[108,161]*AA[165,164]*pct.multi3


  #total number of female eggs after female mortality
  n.egg.total=sum(n.egg[,1])

  #the mortality of egg3 to larvae
  AA[111,110]=n.egg.total/m[105]
  if (m[110]==0){AA[111,110]=0}

  ##############################
  #mortality from egg2 to egg3 #
  ##############################

  #the abundance of female at actual time-step * fecundity * survival of female stages

  #n.egg=the number of eggs from each females stages still alive
  #(after mortality of females from egg1 to egg2 was apply)

  #males
  n.egg[1,2]=m[135]*AA[1,133]*AA[136,135]*pct.primi2
  n.egg[2,2]= m[139]*AA[1,137]*AA[140,139]*pct.multi2

  n.egg[3,2]= m[147]*AA[1,145]*AA[148,147]*pct.primi2
  n.egg[4,2]= m[151]*AA[1,149]*AA[152,151]*pct.multi2

  n.egg[5,2]= m[159]*AA[1,157]*AA[160,159]*pct.primi2
  n.egg[6,2]= m[163]*AA[1,161]*AA[164,163]*pct.multi2


  #total number of male eggs after female mortality
  n.egg.total=sum(n.egg[,2])

  #the mortality of egg2 to egg3
  AA[3,2]=n.egg.total/m[2]
  if (m[2]==0){AA[3,2]=0}



  #females
  n.egg[1,1]= m[135]*AA[108,133]*AA[136,135]*pct.primi2
  n.egg[2,1]= m[139]*AA[108,137]*AA[140,139]*pct.multi2

  n.egg[3,1]= m[147]*AA[108,145]*AA[148,147]*pct.primi2
  n.egg[4,1]= m[151]*AA[108,149]*AA[152,151]*pct.multi2

  n.egg[5,1]= m[159]*AA[108,157]*AA[160,159]*pct.primi2
  n.egg[6,1]= m[163]*AA[108,161]*AA[164,163]*pct.multi2

  #total number of female eggs after female mortality
  n.egg.total=sum(n.egg[,1])

  #the mortality of egg2 to egg3
  AA[110,109]=n.egg.total/m[109]
  if (m[109]==0){AA[110,109]=0}

  #keep percentage of eggs lost by sperm limitation for next time-step
  pct.primi3=pct.primi2
  pct.multi3=pct.multi2


  ######################## mortality from egg1 to egg2 ################################

  #abundance of stages * fecundity * stages mortality

  #the abundance of female at actual time-step * fecundity * survival of female stages * percentage of sperm limitation

  #male (n.egg[,2])

  #Instar IX
  n.egg[1,2]=m[134]*AA[1,133]*AA[135,134]*pct.primi
  n.egg[2,2]=m[138]*AA[1,137]*AA[139,138]*pct.multi

  #Instar X
  n.egg[3,2]=m[146]*AA[1,145]*AA[147,146]*pct.primi
  n.egg[4,2]=m[150]*AA[1,149]*AA[151,150]*pct.multi

  #Instar XI
  n.egg[5,2]=m[158]*AA[1,157]*AA[159,158]*pct.primi
  n.egg[6,2]=m[162]*AA[1,161]*AA[163,162]*pct.multi


  #total number of male eggs after female mortality
  n.egg.total=sum(n.egg[,2])

  #the mortality of egg1 to egg2
  AA[2,1]=n.egg.total/m[2]
  if (m[2]==0){AA[2,1]=0}

  #female (n.egg[,1])

  #Instar IX
  n.egg[1,1]=m[134]*AA[108,133]*AA[135,134]*pct.primi
  n.egg[2,1]=m[138]*AA[108,137]*AA[139,138]*pct.multi

  #Instar X
  n.egg[3,1]=m[146]*AA[108,145]*AA[147,146]*pct.primi
  n.egg[4,1]=m[150]*AA[108,149]*AA[151,150]*pct.multi

  #Instar XI
  n.egg[5,1]=m[158]*AA[108,157]*AA[159,158]*pct.primi
  n.egg[6,1]=m[162]*AA[108,161]*AA[163,162]*pct.multi

  #total number of female eggs after female mortality
  n.egg.total=sum(n.egg[,1])

  #the mortality of egg1 to egg2
  AA[109,108]=n.egg.total/m[104]
  if (m[108]==0){AA[109,108]=0}

  #keep percentage of eggs lost by sperm limitation for next time-step
  pct.primi2=pct.primi
  pct.multi2=pct.multi

  }
  out.egg.mort=list()
  out.egg.mort[[1]]=AA
  out.egg.mort[[2]]=pct.primi2
  out.egg.mort[[3]]=pct.multi2
  out.egg.mort[[4]]=pct.primi3
  out.egg.mort[[5]]=pct.multi3
  names(out.egg.mort)= c("AA", "pct.primi2", "pct.multi2", "pct.primi3", "pct.multi3")
  out.egg.mort
}









############
# Cannibalism induced mortality calculation
############
crab.model$cannibalism.f=function(m,AA,j,stage.m,temp,simple.cannibalism){
  #this function run calc.val to determine the intensity of cannibalism
  #Call by calc.t
  #Input: 	m : abundance vector
  #		AA : base leslie matrix
  #		j : time of year (1 or 2)
  #		stage.m : abundance vector of each Instar
  #		temp: temperature (affect the degree of cannibalism)
  #Output: AA: matrix modified by cannibalism
  #
  #Red Méthot - January 2005



  #we keep a copy of AA because fecundity must not be impacted by cannibalism
          AA.tempo=AA

  #we do a loop to treat each column of AA
  #males
  #k start from the Instar I (col.5)
  #! this is the instars as prey. i.e. the calc.val function calculates how much
  #! of stage k is eaten by all other stages. DD 7 June 2005
  for (k in 5:(ncol(AA)/2)){
      #we multiply the values of each column of AA
      #by a factor of cannibalism, calculated by calc.val
  		#^(1/2) because there is 2 time-step (year mortality)
      AA[,k] = crab.model$calc.val(m,j,k,stage.m,temp,simple.cannibalism)^(1/2) * AA[,k]
      }

  #females
  #from Instar 1 to Instar XI
  for (k in 112:168){
      #we multiply the values of each column of AA
      #by a factor of cannibalism, calculated by calc.val
      AA[,k] = crab.model$calc.val(m,j,k,stage.m,temp,simple.cannibalism)^(1/2) * AA[,k]
      }

  #for the fecundity, because we don't want to affect cannibalism to fecundity
  #males
  AA[1,123:156] = AA.tempo[1,123:156]
  #females
  AA[(nrow(AA)/2 + 1),123:156]=AA.tempo[(nrow(AA)/2 + 1),123:156]

  AA
}




                    

############
# Fishing induced mortality calculation
############
crab.model$fishing.f = function(AA, m, exp.rate, out.skip,i,catch)		{
  #! rewritten Daniel Duplisea May 2007
  #this function determine de proportion of crab taken by fishery for each stage
  #Input: 	AA : matrix affected by skip.f, TM.f, fecundity and cannibalism
  #		m : abundance vector
  #		exp.rate : exploitation rate
  #		list out.skip : skip.m = number of juvenile that skipped molting last time-step(come from skip.f)
  #		params (create from create.params)
  #
  #
  #Output:AA matrix affected by fishing
  #
  #
  #Authors: Daniel Duplisea and Red Méthot
  #Maurice Lamontagne Institute, Dept. of Fisheries and Oceans Canada
  #January 2005; Last revision: January 2005
  
  
  ## FISHERIES PREFERENCES
  # Fisheries target males in stages 12, 13 and 14. They have a preference for the
  # largest with decreasing preference as they decrease in size. Additionally there
  # are within stage differences in preference for fishermen such that they most
  # prefer the 3 year olds in a stage because they have a hard shell that is in
  # good condition. They younder and older cohorts in a stage have a lower
  # preference. Though they will take 2 over 4 or 5.
  
  ## CRAB COMPETITION AND CATCHABILITY
  # When there are a lot of hard shelled crabs (in the prime cohort -3) in the
  # large fishable stages, then the soft shelled individuals of those stages
  # i.e. cohort 1 are timid because the hard crabs will attack them and they
  # always loose in those encounters owing to their softshells. Therefore these
  # softer shell stages remain hidden and are not overly available to the fishing
  # gear. However, when the older cohorts are not too abundant then the soft
  # cohorts will be more active and available to the gear. The age 1 cohort is
  # then caught by the gear though they are not kept and therefore experience a
  # relatively high incidental fishing mortality that does not result in yield
  
  #retrieve de number of juveniles that skipped molt last year
  skip.m=out.skip$skip.m
  
  ########################################################
  #		conditions
  #	1=TM1			soft
  #	2=TM2 and TM3	hard and clean
  #	3=TM4 to TM8	intermediate
  #	4=TM9			hard and dirty
  #	5=TM10 to TM14	soft and dirty
  
  #We make a list to make changes of preference and fishable stages automatic
  condition = list()
  
  #posiiton in m (abundance of each stages) for each condition
  #cond.INSTAR.CONDITION
  
  condition[[1]]=c(26:27) #cond.9.1
  condition[[2]]=c(28:29)	 #cond.9.2
  condition[[3]]=c(30:31)  #cond.9.3
  condition[[4]]=c(32:33)  #cond.9.4
  condition[[5]]=c(34:35)  #cond.9.5
  condition[[6]]=c(36:37) #juvenile, cond.j.9
  condition[[7]]=c(38:39)  #cond.10.1
  condition[[8]]=c(40:41)  #cond.10.2
  condition[[9]]=c(42:43)  #cond.10.3
  condition[[10]]=c(44:45)  #cond.10.4
  condition[[11]]=c(46:47)  #cond.10.5
  condition[[12]]=c(48:49)  #cond.j.10
  
  condition[[13]]=c(50:51)  #cond.11.1
  condition[[14]]=c(52:53)  #cond.11.2
  condition[[15]]=c(54:57)  #cond.11.3
  condition[[16]]=c(58:59)  #cond.11.4
  condition[[17]]=c(60:61)  #cond.11.5
  condition[[18]]=c(62:63)  #cond.j.11
  
  condition[[19]]=c(64:65)  #cond.12.1
  condition[[20]]=c(66:67)  #cond.12.2
  condition[[21]]=c(68:71)  #cond.12.3
  condition[[22]]=c(72:73)  #cond.12.4
  condition[[23]]=c(74:75)  #cond.12.5
  condition[[24]]=c(76:77)  #cond.j.12
  
  condition[[25]]=c(78:79)  #cond.13.1
  condition[[26]]=c(80:81)  #cond.13.2
  condition[[27]]=c(82:85)  #cond.13.3
  condition[[28]]=c(86:87)  #cond.13.4
  condition[[29]]=c(88:91)  #cond.13.5
  condition[[30]]=c(92:93)  #cond.j.13
  
  condition[[31]]=c(94:95)  #cond.14.1
  condition[[32]]=c(96:97)  #cond.14.2
  condition[[33]]=c(98:101)  #cond.14.3
  condition[[34]]=c(102:103)  #cond.14.4
  condition[[35]]=c(104:107)  #cond.14.5
  
  
  #females
  
  condition[[36]]=c(133:134)  #condF.9.1
  condition[[37]]=c(135:136)  #condF.9.2
  condition[[38]]=c(137:138)  #condF.9.3
  condition[[39]]=c(139:140)  #condF.9.4
  condition[[40]]=c(141:142)  #condF.9.5
  condition[[41]]=c(143:144)  #condF.j.9
  
  condition[[42]]=c(145:146)  #condF.10.1
  condition[[43]]=c(147:148)  #condF.10.2
  condition[[44]]=c(149:150)  #condF.10.3
  condition[[45]]=c(151:152)  #condF.10.4
  condition[[46]]=c(153:154)  #condF.10.5
  condition[[47]]=c(155:156)  #condF.j.10
  
  condition[[48]]=c(157:158)  #condF.11.1
  condition[[49]]=c(159:160)  #condF.11.2
  condition[[50]]=c(161:164)  #condF.11.3
  condition[[51]]=c(165:166)  #condF.11.4
  condition[[52]]=c(167:168)  #condF.11.5
  
  
  #number of crabs of each condition
  #! this would be more efficient to just subscript the m vector directly rather
  #! the recreating the list of subscripts on each time step as above. DD 21 June 2005
  
  n.cond.9.1 = sum(m[condition[[1]]])
  n.cond.9.2 = sum(m[condition[[2]]])
  n.cond.9.3 = sum(m[condition[[3]]])
  n.cond.9.4 = sum(m[condition[[4]]])
  n.cond.9.5 = sum(m[condition[[5]]])
  #only the hard shell juvenile crabs are taken by trap, which mean only the fraction that skipped the last molt
  n.cond.j.9 = skip.m[2]
  
  n.cond.10.1 = sum(m[condition[[7]]])
  n.cond.10.2 = sum(m[condition[[8]]])
  n.cond.10.3 = sum(m[condition[[9]]])
  n.cond.10.4 = sum(m[condition[[10]]])
  n.cond.10.5 = sum(m[condition[[11]]])
  n.cond.j.10 = skip.m[3]
  
  n.cond.11.1 = sum(m[condition[[13]]])
  n.cond.11.2 = sum(m[condition[[14]]])
  n.cond.11.3 = sum(m[condition[[15]]])
  n.cond.11.4 = sum(m[condition[[16]]])
  n.cond.11.5 = sum(m[condition[[17]]])
  n.cond.j.11 = skip.m[4]
  
  n.cond.12.1 = sum(m[condition[[19]]])
  n.cond.12.2 = sum(m[condition[[20]]])
  n.cond.12.3 = sum(m[condition[[21]]])
  n.cond.12.4 = sum(m[condition[[22]]])
  n.cond.12.5 = sum(m[condition[[23]]])
  n.cond.j.12 = skip.m[5]
  
  n.cond.13.1 = sum(m[condition[[25]]])
  n.cond.13.2 = sum(m[condition[[26]]])
  n.cond.13.3 = sum(m[condition[[27]]])
  n.cond.13.4 = sum(m[condition[[28]]])
  n.cond.13.5 = sum(m[condition[[29]]])
  n.cond.j.13 = skip.m[6]
  
  n.cond.14.1 = sum(m[condition[[31]]])
  n.cond.14.2 = sum(m[condition[[32]]])
  n.cond.14.3 = sum(m[condition[[33]]])
  n.cond.14.4 = sum(m[condition[[34]]])
  n.cond.14.5 = sum(m[condition[[35]]])
  
  #females
  
  n.condF.9.1 = sum(m[condition[[36]]])
  n.condF.9.2 = sum(m[condition[[37]]])
  n.condF.9.3 = sum(m[condition[[38]]])
  n.condF.9.4 = sum(m[condition[[39]]])
  n.condF.9.5 = sum(m[condition[[40]]])
  n.condF.j.9 = skip.m[7]
  
  n.condF.10.1 = sum(m[condition[[42]]])
  n.condF.10.2 = sum(m[condition[[43]]])
  n.condF.10.3 = sum(m[condition[[44]]])
  n.condF.10.4 = sum(m[condition[[45]]])
  n.condF.10.5 = sum(m[condition[[46]]])
  n.condF.j.10 = skip.m[8]
  
  n.condF.11.1 = sum(m[condition[[48]]]) # corrected was 47... until F11.5. DD 23 June 2005
  n.condF.11.2 = sum(m[condition[[49]]])
  n.condF.11.3 = sum(m[condition[[50]]])
  n.condF.11.4 = sum(m[condition[[51]]])
  n.condF.11.5 = sum(m[condition[[52]]])
  
  
  ##############################soft shell TM 1 mortality#############################
  # this is a mortality caused by fishing activities on the softshell crabs in
  # instars 12, 13 and 14. (i.e the first cohort of each). The level of mortality
  # on these instars is determined by the availability to the gear. This in turn
  # is modelled as a function of the abundance of prime fishable males (see top of
  # file for further description. min 5% max 30%. DD 21 June 2005.
  
  #Instar XII, softshell mortality
    ratio = (n.cond.12.3 + n.cond.13.3 + n.cond.14.3)/n.cond.12.1
  ratio[n.cond.12.1==0] = 0
  if (ratio < 0.5)	{
  fish.mort= -5.8*ratio + 0.59
  fish.mort[fish.mort > 0.3] = 0.3
  fish.mort[fish.mort < 0.05] = 0.05
  fish.mort[n.cond.12.1 == 0 ] = 0}
  
  else {fish.mort=0.05}
    AA[34,33]=AA[34,33]*(1-fish.mort)
  
  #Instar XIII, softshell mortality
    ratio = (n.cond.13.3 + n.cond.14.3)/n.cond.13.1
  ratio[n.cond.13.1==0] = 0
  if (ratio < 0.5){
    fish.mort= -5.8*ratio + 0.59
    fish.mort[fish.mort > 0.3] = 0.3
    fish.mort[fish.mort < 0.05] = 0.05
    fish.mort[n.cond.13.1 == 0 ] = 0
    }
  else {
    fish.mort=0.05
    }
  
  AA[74,73]=AA[74,73]*(1-fish.mort)
  
  #Instar XIV, softshell mortality
  ratio = n.cond.14.3/n.cond.14.1
  ratio[n.cond.14.1==0] = 0
  if (ratio < 0.5)	{
    fish.mort= -5.8*ratio + 0.59
    fish.mort[fish.mort > 0.3] = 0.3
    fish.mort[fish.mort < 0.05] = 0.05
    fish.mort[n.cond.14.1 == 0 ] = 0
    }
  else {
    fish.mort=0.05
    }
  
  AA[85,84]=AA[85,84]*(1-fish.mort)
  
  
  ########################################################################################
  ########################################################################################
  # fisheries yield
  
    # take quota in order of preference by stage and cohort
    # take no more than 50% of biomass from a cohort before moving to
    # next preferred stage and cohort
  
  # maximum % of biomass that can be taken from any 1 cohort, must be greater then exp.rate
  #! moved to a parameter option in the parameters file. DD 22 June 2005.
  #! this is now called a fishing biomass refuge (i.e not fishable). Exploitation
  #! rate now applies only to the non-refuge biomass. This was a problem before because
  #! sometimes high exploitation rates could lead to TACs greater than total biomass available
  nonrefuge.biomass= 1-inputs$params$fishing.biomass.refuge
  
  
    # the cohorts that are fished in the population, stage and age, eg. 13.3 are
    # the 3 year olds who have terminally molted in stage 13.
  #fished.cohorts= c(12.2,12.3,12.4,12.5,12.juv,13.2,13.3,13.4,13.5,13.juv.,14.2,14.3,14.4,14.5)
  #replace by number on the list "condition" (from 1 to 53) ex. Instar XXIII of condition 4 = 28)
  fished.cohorts=c(20, 21 , 22, 23, 24, 26, 27, 28, 29, 30, 32, 33, 34, 35)
  
  # this is the number of fishable not the the number fished. changed DD 21 June 2005
  #n.fished=c(n.cond.12.2, n.cond.12.3, n.cond.12.4, n.cond.12.5, n.cond.j.12,
  n.fishable=c(n.cond.12.2, n.cond.12.3, n.cond.12.4, n.cond.12.5, n.cond.j.12,
   		       n.cond.13.2, n.cond.13.3, n.cond.13.4, n.cond.13.5, n.cond.j.13,
  		       n.cond.14.2, n.cond.14.3, n.cond.14.4, n.cond.14.5)
  
  # the order of preference in the fishery, i.e. the fished cohort 14.3 will be
  # taken first by the fishery
  preference.order= c(6, 3, 9, 12,14,
                      5, 2, 8, 11,13,
                      4, 1, 7, 10)
  #trouver une facon d'automatiser la creation du vecteur CW
  CW = c(96.7, 96.7, 96.7, 96.7, 96.7, 115, 115, 115, 115, 115, 136, 136, 136, 136)
  weigth.at.cw = exp(-6.943 + 2.745*log(CW))
  
  # this is the total biomass of the fishable instars and cohorts
  #! this is a converstion to Kg not tonnes re: weight at CW relationship. DD 22 June 2005
  cohort.biomass= n.fishable * (weigth.at.cw/1000) # convert to tonnes
  
  # this order the cohort biomass in terms of decreasing preference by the fishery
  # for each particular cohort
  cohort.biomass= cohort.biomass[order(preference.order)]
  
  # crab biomass in fishable instars and conditions
  fish.biomass = sum(cohort.biomass)
  
  #the TAC is derived from an effort control i.e. exploitation rate
  # TAC is now a function of exploitation rate of non-refuge biomass
  TAC= exp.rate*fish.biomass*nonrefuge.biomass
  
  # it is possible to fish only a percentage of the total biomass. This is like a
  # refuge. i.e. the fishery cannot take 100% of the biomass of any single or combined
  # cohort(s)
  fishable.cohort.biomass= cohort.biomass * nonrefuge.biomass
  
  #cumulate that fishable biomass in order of preference
  cumulative.fishable.biomass= cumsum(fishable.cohort.biomass)
  
  # fish the cohorts in order of preference until the TAC is reached
  totally.fished.cohorts= fished.cohorts[order(preference.order)][cumulative.fishable.biomass < TAC]
  totally.fished.cohorts.pref.order= 1:length(totally.fished.cohorts)
  
  # there will still be a proportion of a cohort that will be fished in order to reach
  # the TAC but it will not be the total amount possible to fish from that cohort
  fished.cohorts = c(totally.fished.cohorts,fished.cohorts[match(length(totally.fished.cohorts)+1,preference.order)])
  fished.cohorts.pref.order= match(1:(length(totally.fished.cohorts)+1),preference.order)
  
  # determine the amount removed from each cohort by fishing
  fishery.losses.maximally.fished.cohorts= fishable.cohort.biomass[totally.fished.cohorts.pref.order]
  fishery.losses.partially.fished.cohort= TAC - sum(fishery.losses.maximally.fished.cohorts)
  fishery.losses.by.cohort= c(fishery.losses.maximally.fished.cohorts,fishery.losses.partially.fished.cohort)
  

  #! this lookslike and error. i.e. the realised total exploitation rate and therefore the
  #! multiplier that should affect the whole population surivual rate in the Leslie matrix
  #! should be divided by the refuge proportion not multiplied.
  #Fmort = fishery.losses.by.cohort/fishable.cohort.biomass[1:length(fishery.losses.by.cohort)]*max.fished.prop
  #Fmort[fishable.cohort.biomass[1:length(fishery.losses.by.cohort)]*max.fished.prop == 0]= 0
  Fmort = fishery.losses.by.cohort/fishable.cohort.biomass[1:length(fishery.losses.by.cohort)]/nonrefuge.biomass
  Fmort[fishable.cohort.biomass[1:length(fishery.losses.by.cohort)]/nonrefuge.biomass == 0]= 0
  surv.f = 1-Fmort
  
  for (k in 1:length(surv.f)){
    AA[condition[[fished.cohorts[k]]],(condition[[fished.cohorts[k]]]-1) ]=AA[condition[[fished.cohorts[k]]],(condition[[fished.cohorts[k]]]-1) ]*surv.f[k]
    }
  #output the biomass of fished crab by condition and Instar
  for (k in 1:length(fished.cohorts))	{
    catch[i,fished.cohorts[k]]=fishery.losses.by.cohort[k]
    }
  
  out.fish=list()
  out.fish[[1]]=AA
  out.fish[[2]]=catch
  out.fish[[3]]=c((sum(fishery.losses.by.cohort)/sum(fishable.cohort.biomass[1:length(fishery.losses.by.cohort)]))*
  			nonrefuge.biomass, TAC, sum(n.fishable))
  out.fish
}








############
# Survival of larvae in the pelagic phase as function of temperature
############
crab.model$larv.surv.f=function(AA, surface.temperature){
  # larval mortality is a function of temperature. It looks like a quadratic
  # where survival is maximised at some intermediate temperature

  # temperature is modelled as a gaussian and taken from subarea 6
  # in the NW Gulf. The mean of the Gaussian is take as the mean of
  # june and july mean. This encompasses all the months where the
  # larva are potentially in the water.

  # the quadratic survival function was fitted from experiments
  # on larval survival for snow crabs from the Japan Sea. The survival
  # calculated is the multiplicative survival rate of all instar phases:
  # Z1, Z2, M, to C1

  larvae.survival= -0.0012*surface.temperature^2 + 0.0309*surface.temperature - 0.1664

  # This survival can sometime be negative with this function so the following
  # just sets a minumum value
  larvae.survival[larvae.survival<.0001]=.0001

  larvae.survival
}








############
# called by cannibalism
############
crab.model$calc.val=function(m, j, k,stage.m,temp,simple.cannibalism){
  
  #this function determine de factor of cannibalism for each stage 
  #Call by cannibalism.f
  #Input: 	m : abundance vector
  #		j : six months time-step (1 or 2)
  #		k : column of the matrix to modify
  #		stage.m : matrix of abundance merge by Instar (male and female separated)
  #		temp : temperature (affect cannibalism intensity)
  #		params (create from create.params)
  #		eaters.summer.exp
  #		eaters.summer.reg
  #		eaters.winter.exp
  #		eaters.winter.reg
  #
  #Output: val : factor of cannibalism
  #			
  #
  #Author: Red Méthot, adapted from earlier work of Jean-Francois Roy, c/o Denis Gilbert in Matlab
  #Maurice Lamontagne Institute, Dept. of Fisheries and Oceans Canada
  #December 2004; Last revision: January 2005
  
  #cannibalism=inputs$params$cannibalism
  #! this switch for cannibalism is not used, i.e. impossible to turn off cannibalism
  #! at this stage in the original red model, therefore commented out here. DD 14 June 2005
  fct.can.exp=inputs$params$fct.can.exp
  fct.can.reg=inputs$params$fct.can.reg
  stage=inputs$params$stages.lst[k] #stage is the stage eaten
  fem.cannibalism=inputs$params$fem.cannibalism
  can.cold=inputs$params$can.cold
  
  #mid is the number of stages
  mid = length(stage.m)/2
  #this function is used to get male and female at the same stage
  
  #! simple cannibalism switch added DD 14 June 2005.
  if (inputs$params$simple.cannibalism.on==0){
    #following the 6 months time-step, there's different predator stages
    if (j==1){
      eaters.exp=eaters.summer.exp[[stage]]
      eaters.reg=eaters.summer.reg[[stage]]
      }
    else if (j==2){
      eaters.exp=eaters.winter.exp[[stage]]
      eaters.reg=eaters.winter.reg[[stage]]
      }
    #stage will contain the index of the stage in the structures of cannibalism
  
    #! x is a single value representing the predation infliced up a stage. in the loop
    #! that stage is i. DD 7 June 2005
    x = 0
  
    #S.eaters(stage).exp is a list of the stages who eat the current stage
    #these stages are in the same region
    #this loop calculate a rate for each eater stage
    #! x is carried over between loops. It is important therefore to make sure that
    #! the initial cannibalism matrices (reg, exp) for a sinlge season do not have a
    #! a repeated predator-prey combination.
    for (i in 1:(length(eaters.exp))){
      #we add the number of individuals in the eater stage
      #for males and females, we multiply this sum by the factor of cannibalism
      #and we multiply by i, because the higher the stage is, the more he eats
      #! Note that by adding mid, then it means it affects females. DD 7 June 2005
      x = x + fct.can.exp * i * sum(stage.m[eaters.exp[i]], (stage.m[eaters.exp[i] + mid])*fem.cannibalism)
    }
    #this loop is the same, but it's for the eaters stages
    #who are not in the same region than the eaten stage
    #for these stages, even if they are not sizely equals, they equally eat
    for (i in 1:(length(eaters.reg))){
      x = x + fct.can.reg * sum(stage.m[eaters.reg[i]], (stage.m[eaters.reg[i] + mid])*fem.cannibalism)
      }
    }
  
  # this switch allows a single matrix to be used all the time. note that predation
  #! is of the form exp or reg. DD added 14 June 2005
  else if (inputs$params$simple.cannibalism.on==1){
      eaters.exp=simple.cannibalism[[stage]]
      x = 0
      for (i in 1:(length(eaters.exp))){
         x = x + fct.can.exp * i * sum(stage.m[eaters.exp[i]], (stage.m[eaters.exp[i] + mid])*fem.cannibalism)
         }
     }
  
  #we elevate e at the power -x to obtain a number between 0 and 1
  #this number will be the fraction of indidivuals who survive to the cannibalism
  val = exp(-x*0.001)
  if (temp < 0.8){
    val = exp(-x*can.cold*0.001)
    }
    #we can set a maximum of cannibalism
    #! this appears to be is a min but is a max because it is a survival rate. DD 8 june 2005
  val[val < 0.1]=0.1
  val
}






############
# Female fecundity
############
crab.model$fecundity.f= function(CW,reproductive.strategy="primiparous",model="sainte.marie"){
  
  # CW = median carapace width in reproductive stages (mm)
  
  if (reproductive.strategy=="primiparous"){
  if(model=="sainte.marie"){
  # sainte-marie 1993 relationship
  fecundity = round(exp(log(10)*(-0.050 + 2.616*log10(CW))))
  }
  else if (model=="conan"){
  # conan et al. 1989 relationship
  fecundity = round(exp(0.0350 + 2.5532*log(CW)))
  }
  }
     else if (reproductive.strategy=="multiparous"){
  if(model=="sainte.marie"){
  # sainte-marie 1993 relationship
  fecundity = round(exp(log(10)*(0.062 + 2.616*log10(CW))))
  }
  else if (model=="conan"){
  # conan et al. 1989 relationship
  fecundity = round(exp(0.3044 + 2.4894*log(CW)))
  }
  }
  
  fecundity
}
