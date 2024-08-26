###################################################
#  a list of lists, functions, data structures that are need to run the model but
#  are not generally calculation functions
###################################################

inputs= list()





############
# A list with all input parameters
############
start.abund.f= function(year=1993){

  if (year==1993){
    #vector of abundance at time zero
    #some stages MUST be zero (clarifiez)
    #6 put some egg stages ??
  
    #abundance from 1993 BSM survey
    m=matrix(0,nrow=214,ncol=1)
    m[5]= 142598432		#sum of instar 1 and 2 because a part of instar 1 have already did molting at time survey
    m[11]= 65918652		#sum of instar 3 and 4 because a part of instar 3 have already did molting at time survey
    m[17]= 696905
    m[21]= 14526524
    m[23]= 16509148
    m[25]= 11004420
    m[27]= 330583
    m[29]= 198350
    m[31]= 66117
    m[33]= 33058
    m[35]= 33058
    m[37]= 696905
    m[39]= 91783
    m[41]= 55070
    m[43]= 18357
    m[45]= 9178
    m[47]= 9178
    m[49]= 167093
    m[51]= 72254
    m[53]= 43352
    m[55]= 14451
    m[57]= 7225
    m[59]= 3613
    m[61]= 3613
    m[63]= 442998
    m[65]= 232738
    m[67]= 139643
    m[69]= 46548
    m[71]= 23274
    m[73]= 11637
    m[75]= 11637
    m[77]= 260790
    m[79]= 374687
    m[81]= 281015
    m[83]= 93672
    m[85]= 46836
    m[87]= 46836
    m[89]= 46836
    m[91]= 46836
    m[93]= 878236
    m[95]= 361331
    m[97]= 270998
    m[99]= 90333
    m[101]= 45166
    m[103]= 45166
    m[105]= 45166
    m[107]= 45166
  
    #females
    m[112]= 142360981
    m[118]= 30219602
    m[124]= 7037116
    m[128]= 8369062
    m[130]= 7741902
    m[132]= 6455975
    m[134]= 104400
    m[136]= 62640
    m[138]= 20880
    m[140]= 10440
    m[142]= 10440
    m[144]= 702751
    m[146]= 187884
    m[148]= 112730
    m[150]= 37577
    m[152]= 18788
    m[154]= 18788
    m[156]= 0
    m[158]= 276914
    m[160]= 166148
    m[162]= 55383
    m[164]= 27691
    m[166]= 13846
    m[168]= 13846
    }
  
  
  
  if (year==2001){
    #vector of abundance at time zero
    #some stages MUST be zero (clarifiez)
    #abundance from 2001 BSM survey
    m=matrix(0,nrow=214,ncol=1)
    m[5]= 34203353		#sum of instar 1 and 2 because a part of instar 1 have already did molting at time survey
    m[11]= 12384949		#sum of instar 3 and 4 because a part of instar 3 have already did molting at time survey
    m[17]= 2998791
    m[21]= 7284346
    m[23]= 4836803
    m[25]= 5245580
    m[27]= 42544734
    m[29]= 2552684
    m[31]= 8508945
    m[33]= 425447
    m[35]= 425447
    m[37]= 19187226
    m[39]= 1419759
    m[41]= 851855
    m[43]= 283952
    m[45]= 141976
    m[47]= 141976
    m[49]= 4820300
    m[51]= 1049507
    m[53]= 629704
    m[55]= 209901
    m[57]= 104951
    m[59]= 52475.325
    m[61]= 52475
    m[63]= 3905672
    m[65]= 1438035
    m[67]= 862821
    m[69]= 287607
    m[71]= 143803
    m[73]= 71902
    m[75]= 71902
    m[77]= 3498572
    m[79]= 1904354
    m[81]= 1428266
    m[83]= 476089
    m[85]= 238044
    m[87]= 238044
    m[89]= 238044
    m[91]= 238044
    m[93]= 1072400
    m[95]= 460258
    m[97]= 345194
    m[99]= 115065
    m[101]= 57532
    m[103]= 57532
    m[105]= 57532
    m[107]= 57532
  
    #females
    m[112]= 34203353
    m[118]= 7367466
    m[124]= 3495158
    m[128]= 7772890
    m[130]= 4540818
    m[132]= 5555717
    m[134]= 5107150
    m[136]= 3064290
    m[138]= 1021430
    m[140]= 510715
    m[142]= 510715
    m[144]= 1280060
    m[146]= 11846942
    m[148]= 7108165
    m[150]= 2369388
    m[152]= 1184694
    m[154]= 1184694
    m[156]= 0
    m[158]= 0
    m[160]= 0
    m[162]= 0
    m[164]= 0
    m[166]= 0
    m[168]= 0
    }
  m= m[,1]
  m
}






############
# A list with all input parameters
############
#CREATE.PARAMS - create a list with all the parameters we need
#                  it's in that file that we set all the parameters

inputs$params=list()

########### STARTING ABUNDANCE VECTOR
  inputs$params$starting.abundance=start.abund.f(2001)

########### RECRUITMENT AND EXTERNAL ENVIRONMENT
  #limit of settlement number for Instar 1
  #calculate from year 1993 abundance (see Premier_stade_BSM.xls)
  inputs$params$lim.settle.instar1=30000000000

  # benthic temperature, affects egg development rate and growth
  inputs$params$temp=2

  # larvs.pct: % of eggs that reach the seafloor and become cohort 1 individuals
  # Proposed range of values : from 0.1 % to 1 %.
  #! the value here is a proportion, i.e. 1% = 0.01 DD 7 June 2005
  inputs$params$larvs.pct = 0.01

  # Stochastic recruitment function. Drawn from a log normal.
  # This is a multiplier of the deterministic recruitment
  # For purely deterministic recruitment set SD=0
  inputs$params$stochastic.recruitment.multiplier= call("rlnorm",1,0,0.3)#0.3

  # Random pelagic temperture. if so then pelagic temperature is drawn from a
  # lognormal distribution with mean 9.7 C. This producdes a temperature range of
  # between about 7.6 and 12.4 C. The survival rate of pelagic phase larvae is then 
  # calculated from this.
  # For a constant termperature set SD=0
  inputs$params$random.pelagic.temperature= call("rlnorm",1,2.272,.05)

  
########### FISHING
  # 1 = fishing, 0 = no fishing
  inputs$params$fishing = 1

  # exploitation rate between 0 and 1
  inputs$params$exploitation.rate=0.4

  # the proportion of fishable cohort biomass which is not available for fishing. i.e.
  # a refuge. This could result from inefficiency in fishery, protected areas, costs
  # exceeding benefits in fishery or other reasons.
  inputs$params$fishing.biomass.refuge=0.2

  # fixed TAC strategy or not. if so, set the TAC here (kg).
  inputs$params$fixed.TAC=F
  inputs$params$TAC= 5000  
             
########### CANNIBALISM & MORTALITY
  # set to 0 to remove cannibalism
  inputs$params$cannibalism = 1

  # if simple cannibalism then one matrix is used for all seasons and the exp
  # multiplier is used. on=1, 0=0ff. added by DD 14 June 2005
  inputs$params$simple.cannibalism.on=1

  #Factor of multiplication for female cannialism (must be >= 1, 1 being no factor)
  #cannibalism by female could be more important (Squires and Dawe)
  inputs$params$fem.cannibalism = 1

  #when temperature is cold the geographic overlap between Instar is lesser
  #this constant will reduce cannibalism when temperature < 0.8
  #must be between 0 and 1 (1 is the null factor)
  inputs$params$can.cold=1

  # factors of multiplication for the cannibalism
  # fct.can.exp: factor that grows exponentially through the eaters stages
  # this factor is applied to the eaters stages who are in the same region as eaten stages
  # the lower the factor is, the less the eaters stages eat
  inputs$params$fct.can.exp = 0.0001
  #! the exp value is also used for simple cannibalism

  # fct.can.reg: Regional predation factor
  # This factor keeps the same value for each eaters stages
  # this factor is applied to the eaters stages who are not in the same region as the eaten stages
  # these eaters stages are less likely to see and eat eaten stages
  # the lower the factor is, the less the eaters stages eat
  inputs$params$fct.can.reg = 0.0001

  inputs$params$ext.death = matrix(0,nrow=14,ncol=2)
  #the first column is the # for the females
  #the second column is the # for the males
  #Following the curve (0.25*stage^(-0.347))
  inputs$params$ext.death[1:14,1]=0.25*(1:14)^(-0.347)
  inputs$params$ext.death[1:14,2]=0.25*(1:14)^(-0.347)

  # Set ext.death to NA for females of stage 12 to 14
  inputs$params$ext.death[12:14,1]=NA
  #if fishing mortality is added
  if (params$fishing != 0)	{
  params$ext.death[12:14,2] = c(0.70)
  }

  # the cannibalism function describing the cannibalism matrix. Breadth is the
  # number of predator instars on each prey instar. offset.instar is the number of instars
  # between a prey instar and its first predator instar. refuge is the instar where
  # there is a cannibalism size refuge for it and all larger instars.
  inputs$params$breadth=2
  inputs$params$offset.instar=4
  inputs$params$refuge=9  


########### SKIP MOLTING AND SPERM LIMITATION
  # whether or not skip molting is modelled dynamically, i.e. decisions made each
  # step based upon population or environmental conditions. (1=yes, 0=no)
  inputs$params$skip=1

  # sperm limitation (0 not included, 1 included in the model)
  inputs$params$sperm.lim=1

  #weighting value of males at each Instar (14 having more weight then 8)
  #e.g. One male 14 is equivalent to 2 "average" males
  inputs$params$weigth.male=matrix(0,nrow=14,ncol=1)
  inputs$params$weigth.male[8] = 0.5
  inputs$params$weigth.male[9] = 0.5
  inputs$params$weigth.male[10] = 1
  inputs$params$weigth.male[11] = 1
  inputs$params$weigth.male[12] = 1
  inputs$params$weigth.male[13] = 1.5
  inputs$params$weigth.male[14] = 2

########### OTHER FACTORS
  # carapace width (CW) of the crabs for the 3 females stages of fecundity
  # the CW is a mean
  inputs$params$CW.9 = (44.4 + 48.5) / 2   # Minimum and maximum sizes for this stage
  inputs$params$CW.10 = (55.7 + 57.1) / 2  # Minimum and maximum sizes for this stage
  inputs$params$CW.11 = 68

  # ext.death: % of mortality caused by something else than cannibalism (mainly molting and predation)
  # one % for males and one % for females, for each stage
  # Note: ANNUAL mortality rate
  #0.1 is attributed to molting thus for adults (which do no more molting)will have to substracted 0.1

  #stage membership for each position in the population vector
  inputs$params$stages.lst=c(0,0,0,0,1,1,1,2,2,2,3,3,3,4,4,4,5,5,5,6,6,7,7,8,8,9,9,9,9,9,9,9,9,9,9,9,9,
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
  inputs$params$stages= c(params$stages.lst[1:107],params$stages.lst[108:214]+15)

  #a vector defining the stage condition and sex of crabs in the abundance vector for TM crabs only
      #	1=TM1			soft
      #	2=TM2 and TM3	hard and clean
      #	3=TM4 to TM8	intermediate
      #	4=TM9			hard and dirty
      #	5=TM10 to TM14	soft and dirty 

  #We make a list to make changes of preference and fishable stages automatic
inputs$params$condition = list()

#posiiton in m (abundance of each stages) for each condition
#cond.INSTAR.CONDITION
inputs$params$stage.cond= vector(length=214)
inputs$params$stage.cond[1:25]=0
inputs$params$stage.cond[26:27]=9.1
inputs$params$stage.cond[28:29]=9.2
inputs$params$stage.cond[30:31]=9.3
inputs$params$stage.cond[32:33]=9.4
inputs$params$stage.cond[34:35]=9.5
inputs$params$stage.cond[36:37]=9.0 #Juvenile stage 9, i.e. not TM
inputs$params$stage.cond[38:39]= 10.1
inputs$params$stage.cond[40:41]= 10.2
inputs$params$stage.cond[42:43]= 10.3
inputs$params$stage.cond[44:45]= 10.4
inputs$params$stage.cond[46:47]= 10.5
inputs$params$stage.cond[48:49]= 10.0
inputs$params$stage.cond[50:51]= 11.1
inputs$params$stage.cond[52:53]= 11.2
inputs$params$stage.cond[54:57]= 11.3
inputs$params$stage.cond[58:59]= 11.4
inputs$params$stage.cond[60:61]= 11.5
inputs$params$stage.cond[62:63]= 11.0
inputs$params$stage.cond[64:65]= 12.1
inputs$params$stage.cond[66:67]= 12.2
inputs$params$stage.cond[68:71]= 12.3
inputs$params$stage.cond[72:73]= 12.4
inputs$params$stage.cond[74:75]= 12.5
inputs$params$stage.cond[76:77]= 12.0
inputs$params$stage.cond[78:79]= 13.1
inputs$params$stage.cond[80:81]= 13.2
inputs$params$stage.cond[82:85]= 13.3
inputs$params$stage.cond[86:87]= 13.4
inputs$params$stage.cond[88:91]= 13.5
inputs$params$stage.cond[92:93]= 13.0
inputs$params$stage.cond[94:95]= 14.1
inputs$params$stage.cond[96:97]= 14.2
inputs$params$stage.cond[98:101]= 14.3
inputs$params$stage.cond[102:103]= 14.4
inputs$params$stage.cond[104:107]= 14.5
#females
inputs$params$stage.cond[133:134]= 99.1
inputs$params$stage.cond[135:136]= 99.2
inputs$params$stage.cond[137:138]= 99.3
inputs$params$stage.cond[139:140]= 99.4
inputs$params$stage.cond[141:142]= 99.5
inputs$params$stage.cond[143:144]= 99.0
inputs$params$stage.cond[145:146]= 910.1
inputs$params$stage.cond[147:148]= 910.2
inputs$params$stage.cond[149:150]= 910.3
inputs$params$stage.cond[151:152]= 910.4
inputs$params$stage.cond[153:154]= 910.5
inputs$params$stage.cond[155:156]= 910.0
inputs$params$stage.cond[157:158]= 911.1
inputs$params$stage.cond[159:160]= 911.2
inputs$params$stage.cond[161:164]= 911.3
inputs$params$stage.cond[165:166]= 911.4
inputs$params$stage.cond[167:168]= 911.5











############
# binary vectors to select stages for matrix multiplication instead of using loops
# usually used for plotting results
############
inputs$stage.binaries.f= function(stages){
  # this function creates binary vectors to filter stages from the abundance
  # matrix so that plots can be constructed summing over rows by matrix
  # multiplication rather than apply loops

  if (stages=="TM.males"){
    binary= c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,0,0,
      1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,
      1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="TM.females"){
      binary= c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,1,1,1,1,0,0,1,1,1,1,1,1,
        1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="fishable.males"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="prime.fishable.males"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="softshell.males.9.10.11"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,1,1,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0)
    }
  if (stages=="all"){
      binary= rep(1,214)
    }
  if (stages=="primiparous.females"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,1,0,0,0,0,0,0,0,0,0,0,0,
        1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="multiparous.females"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,1,1,1,1,1,1,1,1,1,0,0,0,1,1,
        1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar1"){
      binary=c(0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar2"){
      binary=c(0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar3"){
      binary=c(0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar4"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar5"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar6"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar7"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar8"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar9"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar10"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar11"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,
        1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,
        1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar12"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar13"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }
  if (stages=="instar14"){
      binary=c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
        0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
    }
  if (stages=="CW"){
    binary= c(0,0,0,0,3.1,3.1,3.1,5.1,5.1,5.1,7.6,7.6,7.6,10.9,10.9,10.9,15.3,15.3,
  	     15.3,21,21,28.4,28.4,38.2,28.2,50.7,50.7,50.7,50.7,50.7,50.7,50.7,
  	     50.7,50.7,50.7,50.7,50.7,64.5,64.5,64.5,64.5,64.5,64.5,64.5,64.5,
  	     64.5,64.5,64.5,64.5,79.7,79.7,79.7,79.7,79.7,79.7,79.7,79.7,79.7,
  	     79.7,79.7,79.7,79.7,79.7,96.7,96.7,96.7,96.7,96.7,96.7,96.7,96.7,
  	     96.7,96.7,96.7,96.7,96.7,96.7,115,115,115,115,115,115,115,115,115,
  	     115,115,115,115,115,115,115,136,136,136,136,136,136,136,136,136,
  	     136,136,136,136,136,0,0,0,0,3.1,3.1,3.1,5.1,5.1,5.1,7.6,7.6,7.6,
  	     10.9,10.9,10.9,15.3,15.3,15.3,21,21,27.4,27.4,35.6,35.6,47.2,47.2,
  	     47.2,47.2,47.2,47.2,47.2,47.2,47.2,47.2,45.3,45.3,56.4,56.4,56.4,
  	     56.4,56.4,56.4,56.4,56.4,56.4,56.4,56.4,56.4,68,68,68,68,68,68,68,
  	     68,68,68,68,68,68,68,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
  	     0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }

  if (stages=="wgt"){
    binary= c(0,0,0,0,0.013,0.013,0.013,0.056,0.056,0.056,0.182,0.182,0.182,0.525,0.525,0.525,1.421,1.421,1.421,3.602,3.602,8.741,
      8.741,20.88,20.88,51.987,51.987,51.987,51.987,51.987,51.987,51.987,51.987,51.987,51.987,47.959,47.959,108.829,108.829,
      108.829,108.829,108.829,108.829,108.829,108.829,108.829,108.829,97.268,97.268,208.352,208.352,208.352,208.352,208.352,
      208.352,208.352,208.352,208.352,208.352,208.352,208.352,181.094,181.094,377.149,377.149,377.149,377.149,377.149,
      377.149,377.149,377.149,377.149,377.149,377.149,377.149,319.556,319.556,642,642,642,642,642,642,642,642,642,642,642,
      642,642,642,531.671,531.671,1074.232,1074.232,1074.232,1074.232,1074.232,1074.232,1074.232,1074.232,1074.232,1074.232,
      1074.232,1074.232,1074.232,1074.232,0,0,0,0,0.016,0.016,0.016,0.064,0.064,0.064,0.196,0.196,0.196,0.541,0.541,0.541,
      1.408,1.408,1.408,3.436,3.436,7.271,7.271,15.206,15.206,35.886,35.886,35.886,35.886,35.886,35.886,35.886,35.886,35.886,
      35.886,29.985,29.985,58.036,58.036,58.036,58.036,58.036,58.036,58.036,58.036,58.036,58.036,55.606,55.606,96.033,96.033,
      96.033,96.033,96.033,96.033,96.033,96.033,96.033,96.033,96.033,96.033,96.033,96.033,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }

  if (stages=="TM.mal.2to4"){
    binary= c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,
      0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,
      0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)
    }

  if (stages=="TM.mal.2to4.legal"){
    binary= c(0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,
      0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,
      0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0)

    }

  binary
}





