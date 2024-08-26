params.f= function(){

#CREATE.PARAMS - create a list with all the parameters we need
#                  it's in that file that we set all the parameters

params=list()

########### STARTING ABUNDANCE VECTOR
  params$starting.abundance=start.abund.f(2001)

########### RECRUITMENT AND EXTERNAL ENVIRONMENT
  #limit of settlement number for Instar 1
  #calculate from year 1993 abundance (see Premier_stade_BSM.xls)
  params$lim.settle.instar1=30000000000

  # benthic temperature, affects egg development rate and growth
  params$temp=2

  # larvs.pct: % of eggs that reach the seafloor and become cohort 1 individuals
  # Proposed range of values : from 0.1 % to 1 %.
  #! the value here is a proportion, i.e. 1% = 0.01 DD 7 June 2005
  params$larvs.pct = 0.01

  # Stochastic recruitment function. Drawn from a log normal.
  # This is a multiplier of the deterministic recruitment
  # For purely deterministic recruitment set SD=0
  params$stochastic.recruitment.multiplier= call("rlnorm",1,0,.3)

  # Random pelagic temperture. if so then pelagic temperature is drawn from a
  # lognormal distribution with mean 9.7 C. This producdes a temperature range of
  # between about 7.6 and 12.4 C. The survival rate of pelagic phase larvae is then 
  # calculated from this.
  # For a constant termperature set SD=0
  params$random.pelagic.temperature= call("rlnorm",1,2.272,.05)

  
########### FISHING
  # 1 = fishing, 0 = no fishing
  params$fishing = 1

  # exploitation rate between 0 and 1
  params$exploitation.rate=0.4

  # the proportion of fishable cohort biomass which is not available for fishing. i.e.
  # a refuge. This could result from inefficiency in fishery, protected areas, costs
  # exceeding benefits in fishery or other reasons.
  params$fishing.biomass.refuge=0.2

  # fixed TAC strategy or not. if so, set the TAC here (kg).
  params$fixed.TAC=F
  params$TAC= 5000  
             
########### CANNIBALISM & MORTALITY
  # set to 0 to remove cannibalism
  params$cannibalism = 1

  # if simple cannibalism then one matrix is used for all seasons and the exp
  # multiplier is used. on=1, 0=0ff. added by DD 14 June 2005
  params$simple.cannibalism.on=1

  #Factor of multiplication for female cannialism (must be >= 1, 1 being no factor)
  #cannibalism by female could be more important (Squires and Dawe)
  params$fem.cannibalism = 1

  #when temperature is cold the geographic overlap between Instar is lesser
  #this constant will reduce cannibalism when temperature < 0.8
  #must be between 0 and 1 (1 is the null factor)
  params$can.cold=1

  # factors of multiplication for the cannibalism
  # fct.can.exp: factor that grows exponentially through the eaters stages
  # this factor is applied to the eaters stages who are in the same region as eaten stages
  # the lower the factor is, the less the eaters stages eat
  params$fct.can.exp = 0.0001
  #! the exp value is also used for simple cannibalism

  # fct.can.reg: Regional predation factor
  # This factor keeps the same value for each eaters stages
  # this factor is applied to the eaters stages who are not in the same region as the eaten stages
  # these eaters stages are less likely to see and eat eaten stages
  # the lower the factor is, the less the eaters stages eat
  params$fct.can.reg = 0.0001

  params$ext.death = matrix(0,nrow=14,ncol=2)
  #the first column is the # for the females
  #the second column is the # for the males
  #Following the curve (0.25*stage^(-0.347))

  params$ext.death[,1]=0.25*(1:14)^(-0.347)
  params$ext.death[,2]=0.25*(1:14)^(-0.347)

  # Set ext.death to NA for females of stage 12 to 14
  params$ext.death[12:14,1]=NA
  #if fishing mortality is added
  if (params$fishing != 0)	{
  params$ext.death[12:14,2] = c(0.70)
  }

  # the cannibalism function describing the cannibalism matrix. Breadth is the
  # number of predator instars on each prey instar. offset is the number of instars
  # between a prey instar and its first predator instar. refuge is the instar where
  # there is a cannibalism size refuge for it and all larger instars.
  params$breadth=2
  params$offset=4
  params$refuge=9  


########### SKIP MOLTING AND SPERM LIMITATION
  # whether or not skip molting is modelled dynamically, i.e. decisions made each
  # step based upon population or environmental conditions. (1=yes, 0=no)
  params$skip=1

  # sperm limitation (0 not included, 1 included in the model)
  params$sperm.lim=0

  #weighting value of males at each Instar (14 having more weight then 8)
  #e.g. One male 14 is equivalent to 2 "average" males
  params$weigth.male=matrix(0,nrow=14,ncol=1)
  params$weigth.male[8] = 0.5
  params$weigth.male[9] = 0.5
  params$weigth.male[10] = 1
  params$weigth.male[11] = 1
  params$weigth.male[12] = 1
  params$weigth.male[13] = 1.5
  params$weigth.male[14] = 2

########### OTHER FACTORS
  # carapace width (CW) of the crabs for the 3 females stages of fecundity
  # the CW is a mean
  params$CW.9 = (44.4 + 48.5) / 2   # Minimum and maximum sizes for this stage
  params$CW.10 = (55.7 + 57.1) / 2  # Minimum and maximum sizes for this stage
  params$CW.11 = 68

  # ext.death: % of mortality caused by something else than cannibalism (mainly molting and predation)
  # one % for males and one % for females, for each stage
  # Note: ANNUAL mortality rate
  #0.1 is attributed to molting thus for adults (which do no more molting)will have to substracted 0.1

  #stage membership for each position in the population vector
  params$stages.lst=c(0,0,0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,7,7,8,8,9,9,9,9,9,9,9,9,9,9,9,9,
  10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,
  12,12,12,12,12,12,12,12,12,12,12,12,12,12,
  13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
  14,14,14,14,14,14,14,14,14,14,14,14,14,14,
  0,0,0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,7,7,8,8,9,9,9,9,9,9,9,9,9,9,9,9,
  10,10,10,10,10,10,10,10,10,10,10,10,11,11,11,11,11,11,11,11,11,11,11,11,11,11,
  12,12,12,12,12,12,12,12,12,12,12,12,12,12,
  13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,13,
  14,14,14,14,14,14,14,14,14,14,14,14,14,14)

  #unique stage membership for each position in the population vector separated by sex
  params$stages= c(params$stages.lst[1:107],params$stages.lst[108:214]+15)

  #a vector defining the stage condition and sex of crabs in the abundance vector for TM crabs only
      #	1=TM1			soft
      #	2=TM2 and TM3	hard and clean
      #	3=TM4 to TM8	intermediate
      #	4=TM9			hard and dirty
      #	5=TM10 to TM14	soft and dirty 

  #We make a list to make changes of preference and fishable stages automatic
condition = list()

#posiiton in m (abundance of each stages) for each condition
#cond.INSTAR.CONDITION
params$stage.cond= vector(length=214)
params$stage.cond[1:25]=0
params$stage.cond[26:27]=9.1
params$stage.cond[28:29]=9.2
params$stage.cond[30:31]=9.3
params$stage.cond[32:33]=9.4
params$stage.cond[34:35]=9.5
params$stage.cond[36:37]=9.0 #Juvenile stage 9, i.e. not TM
params$stage.cond[38:39]= 10.1
params$stage.cond[40:41]= 10.2
params$stage.cond[42:43]= 10.3
params$stage.cond[44:45]= 10.4
params$stage.cond[46:47]= 10.5
params$stage.cond[48:49]= 10.0
params$stage.cond[50:51]= 11.1
params$stage.cond[52:53]= 11.2
params$stage.cond[54:57]= 11.3
params$stage.cond[58:59]= 11.4
params$stage.cond[60:61]= 11.5
params$stage.cond[62:63]= 11.0
params$stage.cond[64:65]= 12.1
params$stage.cond[66:67]= 12.2
params$stage.cond[68:71]= 12.3
params$stage.cond[72:73]= 12.4
params$stage.cond[74:75]= 12.5
params$stage.cond[76:77]= 12.0
params$stage.cond[78:79]= 13.1
params$stage.cond[80:81]= 13.2
params$stage.cond[82:85]= 13.3
params$stage.cond[86:87]= 13.4
params$stage.cond[88:91]= 13.5
params$stage.cond[92:93]= 13.0
params$stage.cond[94:95]= 14.1
params$stage.cond[96:97]= 14.2
params$stage.cond[98:101]= 14.3
params$stage.cond[102:103]= 14.4
params$stage.cond[104:107]= 14.5
#females
params$stage.cond[133:134]= 99.1
params$stage.cond[135:136]= 99.2
params$stage.cond[137:138]= 99.3
params$stage.cond[139:140]= 99.4
params$stage.cond[141:142]= 99.5
params$stage.cond[143:144]= 99.0
params$stage.cond[145:146]= 910.1
params$stage.cond[147:148]= 910.2
params$stage.cond[149:150]= 910.3
params$stage.cond[151:152]= 910.4
params$stage.cond[153:154]= 910.5
params$stage.cond[155:156]= 910.0
params$stage.cond[157:158]= 911.1
params$stage.cond[159:160]= 911.2
params$stage.cond[161:164]= 911.3
params$stage.cond[165:166]= 911.4
params$stage.cond[167:168]= 911.5

params
}
