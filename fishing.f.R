fishing.f = function(AA, m, exp.rate, out.skip,i,catch)		{

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
skip.m=out.skip[[2]]

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

n.cond.9.1 = sum(m[condition[[1]] ])   
n.cond.9.2 = sum(m[condition[[2]] ]) 
n.cond.9.3 = sum(m[condition[[3]] ]) 
n.cond.9.4 = sum(m[condition[[4]] ]) 
n.cond.9.5 = sum(m[condition[[5]] ]) 
#only the hard shell juvenile crabs are taken by trap, which mean only the fraction that skipped the last molt
n.cond.j.9 = skip.m[2] 

n.cond.10.1 = sum(m[condition[[7]] ])   
n.cond.10.2 = sum(m[condition[[8]] ]) 
n.cond.10.3 = sum(m[condition[[9]] ]) 
n.cond.10.4 = sum(m[condition[[10]] ]) 
n.cond.10.5 = sum(m[condition[[11]] ]) 
n.cond.j.10 = skip.m[3]

n.cond.11.1 = sum(m[condition[[13]] ])   
n.cond.11.2 = sum(m[condition[[14]] ]) 
n.cond.11.3 = sum(m[condition[[15]] ]) 
n.cond.11.4 = sum(m[condition[[16]] ]) 
n.cond.11.5 = sum(m[condition[[17]] ]) 
n.cond.j.11 = skip.m[4] 

n.cond.12.1 = sum(m[condition[[19]] ])
n.cond.12.2 = sum(m[condition[[20]] ]) 
n.cond.12.3 = sum(m[condition[[21]] ]) 
n.cond.12.4 = sum(m[condition[[22]] ]) 
n.cond.12.5 = sum(m[condition[[23]] ]) 
n.cond.j.12 = skip.m[5]

n.cond.13.1 = sum(m[condition[[25]] ]) 
n.cond.13.2 = sum(m[condition[[26]] ]) 
n.cond.13.3 = sum(m[condition[[27]] ]) 
n.cond.13.4 = sum(m[condition[[28]] ]) 
n.cond.13.5 = sum(m[condition[[29]] ]) 
n.cond.j.13 = skip.m[6] 

n.cond.14.1 = sum(m[condition[[31]] ]) 
n.cond.14.2 = sum(m[condition[[32]] ]) 
n.cond.14.3 = sum(m[condition[[33]] ]) 
n.cond.14.4 = sum(m[condition[[34]] ]) 
n.cond.14.5 = sum(m[condition[[35]] ]) 

#females

n.condF.9.1 = sum(m[condition[[36]] ])   
n.condF.9.2 = sum(m[condition[[37]] ]) 
n.condF.9.3 = sum(m[condition[[38]] ]) 
n.condF.9.4 = sum(m[condition[[39]] ]) 
n.condF.9.5 = sum(m[condition[[40]] ]) 
n.condF.j.9 = skip.m[7] 

n.condF.10.1 = sum(m[condition[[42]] ])   
n.condF.10.2 = sum(m[condition[[43]] ]) 
n.condF.10.3 = sum(m[condition[[44]] ]) 
n.condF.10.4 = sum(m[condition[[45]] ]) 
n.condF.10.5 = sum(m[condition[[46]] ]) 
n.condF.j.10 = skip.m[8] 

n.condF.11.1 = sum(m[condition[[48]] ]) # corrected was 47... until F11.5. DD 23 June 2005
n.condF.11.2 = sum(m[condition[[49]] ])
n.condF.11.3 = sum(m[condition[[50]] ])
n.condF.11.4 = sum(m[condition[[51]] ])
n.condF.11.5 = sum(m[condition[[52]] ])


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
nonrefuge.biomass= 1-params$fishing.biomass.refuge


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
out.fish
}
