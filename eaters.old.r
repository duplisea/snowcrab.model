#! This creates the foodweb matrix for cannibalism, i.e who eats whom.
#! This is a very slow part of the simualation and part of the slowness
#! is probably because the data are stored in lists. It would probably
#! be faster to organise these into binary matrices to speed-up
#! simulations. DD 7 June 2005
# create a list with the cannibalism parameters

# eaters: Structure array for each stage

#########################
#	season=="summer"	#
#########################

# eaters(i).exp: enumeration of eaters stages that eat stage i
#                and are in the same region as stage i, and
#                whose predation grows EXPONENTIALLY through the stages
#

#! this creates vectors and then puts them in a list. This could be more
#! efficiently done by just putting them straight into the list. DD 7 June 2005
eaters.summer.exp1=c(4,5,6,7,8)
eaters.summer.exp2=c(5,6,7,8)
eaters.summer.exp3=c(6,7,8)
eaters.summer.exp4=c(7,8)
eaters.summer.exp5=c(8)
eaters.summer.exp6=c()
eaters.summer.exp7=c()
eaters.summer.exp8=c()
eaters.summer.exp9=c()
eaters.summer.exp10=c()
eaters.summer.exp11=c()
eaters.summer.exp12=c()
eaters.summer.exp13=c()
eaters.summer.exp14=c()
# the last vector is created to solve the technical
# problem of null vector but is not use in the model
eaters.summer.exp15=c(1)

eaters.summer.exp=list()
eaters.summer.exp[[1]]=eaters.summer.exp1
eaters.summer.exp[[2]]=eaters.summer.exp2
eaters.summer.exp[[3]]=eaters.summer.exp3
eaters.summer.exp[[4]]=eaters.summer.exp4
eaters.summer.exp[[5]]=eaters.summer.exp5
eaters.summer.exp[[6]]=eaters.summer.exp6
eaters.summer.exp[[7]]=eaters.summer.exp7
eaters.summer.exp[[8]]=eaters.summer.exp8
eaters.summer.exp[[9]]=eaters.summer.exp9
eaters.summer.exp[[10]]=eaters.summer.exp10
eaters.summer.exp[[11]]=eaters.summer.exp11
eaters.summer.exp[[12]]=eaters.summer.exp12
eaters.summer.exp[[13]]=eaters.summer.exp13
eaters.summer.exp[[14]]=eaters.summer.exp14
eaters.summer.exp[[15]]=eaters.summer.exp15
names(eaters.summer.exp)= c( "eaters.summer.exp1", "eaters.summer.exp2", "eaters.summer.exp3", "eaters.summer.exp4", "eaters.summer.exp5", "eaters.summer.exp6", "eaters.summer.exp7", "eaters.summer.exp8", "eaters.summer.exp9", "eaters.summer.exp10", "eaters.summer.exp11", "eaters.summer.exp12", "eaters.summer.exp13", "eaters.summer.exp14","crap")
#We don't need those vectors anymore
rm(eaters.summer.exp1,eaters.summer.exp2,eaters.summer.exp3,eaters.summer.exp4,eaters.summer.exp5,eaters.summer.exp6,eaters.summer.exp7,eaters.summer.exp8,eaters.summer.exp9,eaters.summer.exp10,eaters.summer.exp11,eaters.summer.exp12,eaters.summer.exp13,eaters.summer.exp14,eaters.summer.exp15)



##################################################################
# eaters(i).reg: enumeration of stages that eat stage i
#                but are NOT in the same REGION as stage i
################################################


eaters.summer.reg1=c(9)
eaters.summer.reg2=c(9,10)
eaters.summer.reg3=c(9,10,11)
eaters.summer.reg4=c(9,10,11,12)
eaters.summer.reg5=c(9,10,11,12,13)
eaters.summer.reg6=c(9,10,11,12,13,14)
eaters.summer.reg7=c(10,11,12,13,14)
eaters.summer.reg8=c(11,12,13,14)
eaters.summer.reg9=c(12,13,14)
eaters.summer.reg10=c()
eaters.summer.reg11=c()
eaters.summer.reg12=c()
eaters.summer.reg13=c()
eaters.summer.reg14=c()
# the last vector is created to solve the technical
# problem of null vector but is not use in the model
eaters.summer.reg15=c(1)

eaters.summer.reg=list()
eaters.summer.reg[[1]]=eaters.summer.reg1
eaters.summer.reg[[2]]=eaters.summer.reg2
eaters.summer.reg[[3]]=eaters.summer.reg3
eaters.summer.reg[[4]]=eaters.summer.reg4
eaters.summer.reg[[5]]=eaters.summer.reg5
eaters.summer.reg[[6]]=eaters.summer.reg6
eaters.summer.reg[[7]]=eaters.summer.reg7
eaters.summer.reg[[8]]=eaters.summer.reg8
eaters.summer.reg[[9]]=eaters.summer.reg9
eaters.summer.reg[[10]]=eaters.summer.reg10
eaters.summer.reg[[11]]=eaters.summer.reg11
eaters.summer.reg[[12]]=eaters.summer.reg12
eaters.summer.reg[[13]]=eaters.summer.reg13
eaters.summer.reg[[14]]=eaters.summer.reg14
eaters.summer.reg[[15]]=eaters.summer.reg15
names(eaters.summer.reg)= c( "eaters.summer.reg1", "eaters.summer.reg2", "eaters.summer.reg3", "eaters.summer.reg4", "eaters.summer.reg5", "eaters.summer.reg6", "eaters.summer.reg7", "eaters.summer.reg8", "eaters.summer.reg9", "eaters.summer.reg10", "eaters.summer.reg11", "eaters.summer.reg12", "eaters.summer.reg13", "eaters.summer.reg14","crap")
#We don't need those vectors anymore
rm(eaters.summer.reg1,eaters.summer.reg2,eaters.summer.reg3,eaters.summer.reg4,eaters.summer.reg5,eaters.summer.reg6,eaters.summer.reg7,eaters.summer.reg8,eaters.summer.reg9,eaters.summer.reg10,eaters.summer.reg11,eaters.summer.reg12,eaters.summer.reg13,eaters.summer.reg14,eaters.summer.reg15)



#########################
#	season=="winter"	#
#########################


# eaters(i).exp: enumeration of eaters stages that eat stage i
#                and are in the same region as stage i, and
#                whose predation grows EXPONENTIALLY through the stages
#

eaters.winter.exp1=c(4)
eaters.winter.exp2=c()
eaters.winter.exp3=c()
eaters.winter.exp4=c()
eaters.winter.exp5=c(8,9,10,11,12,13)
eaters.winter.exp6=c(9,10,11,12,13,14)
eaters.winter.exp7=c(10,11,12,13,14)
eaters.winter.exp8=c(11,12,13,14)
eaters.winter.exp9=c(12,13,14)
eaters.winter.exp10=c()
eaters.winter.exp11=c()
eaters.winter.exp12=c()
eaters.winter.exp13=c()
eaters.winter.exp14=c()
# the last vector is created to solve the technical
# problem of null vector but is not use in the model
eaters.winter.exp15=c(1)

eaters.winter.exp=list()
eaters.winter.exp[[1]]=eaters.winter.exp1
eaters.winter.exp[[2]]=eaters.winter.exp2
eaters.winter.exp[[3]]=eaters.winter.exp3
eaters.winter.exp[[4]]=eaters.winter.exp4
eaters.winter.exp[[5]]=eaters.winter.exp5
eaters.winter.exp[[6]]=eaters.winter.exp6
eaters.winter.exp[[7]]=eaters.winter.exp7
eaters.winter.exp[[8]]=eaters.winter.exp8
eaters.winter.exp[[9]]=eaters.winter.exp9
eaters.winter.exp[[10]]=eaters.winter.exp10
eaters.winter.exp[[11]]=eaters.winter.exp11
eaters.winter.exp[[12]]=eaters.winter.exp12
eaters.winter.exp[[13]]=eaters.winter.exp13
eaters.winter.exp[[14]]=eaters.winter.exp14
eaters.winter.exp[[15]]=eaters.winter.exp15
names(eaters.winter.exp)= c( "eaters.winter.exp1", "eaters.winter.exp2", "eaters.winter.exp3", "eaters.winter.exp4", "eaters.winter.exp5", "eaters.winter.exp6", "eaters.winter.exp7", "eaters.winter.exp8", "eaters.winter.exp9", "eaters.winter.exp10", "eaters.winter.exp11", "eaters.winter.exp12", "eaters.winter.exp13", "eaters.winter.exp14","crap")
#We don't need those vectors anymore
rm(eaters.winter.exp1,eaters.winter.exp2,eaters.winter.exp3,eaters.winter.exp4,eaters.winter.exp5,eaters.winter.exp6,eaters.winter.exp7,eaters.winter.exp8,eaters.winter.exp9,eaters.winter.exp10,eaters.winter.exp11,eaters.winter.exp12,eaters.winter.exp13,eaters.winter.exp14,eaters.winter.exp15)


##################################################################
# eaters(i).reg: enumeration of stages that eat stage i
#                but are NOT in the same REGION as stage i
################################################


eaters.winter.reg1=c(9)
eaters.winter.reg2=c(9,10)
eaters.winter.reg3=c(9,10,11)
eaters.winter.reg4=c(9,10,11,12)
eaters.winter.reg5=c()
eaters.winter.reg6=c()
eaters.winter.reg7=c()
eaters.winter.reg8=c()
eaters.winter.reg9=c()
eaters.winter.reg10=c()
eaters.winter.reg11=c()
eaters.winter.reg12=c()
eaters.winter.reg13=c()
eaters.winter.reg14=c()
# the last vector is created to solve the technical
# problem of null vector but is not use in the model
eaters.winter.reg15=c(1)

eaters.winter.reg=list()
eaters.winter.reg[[1]]=eaters.winter.reg1
eaters.winter.reg[[2]]=eaters.winter.reg2
eaters.winter.reg[[3]]=eaters.winter.reg3
eaters.winter.reg[[4]]=eaters.winter.reg4
eaters.winter.reg[[5]]=eaters.winter.reg5
eaters.winter.reg[[6]]=eaters.winter.reg6
eaters.winter.reg[[7]]=eaters.winter.reg7
eaters.winter.reg[[8]]=eaters.winter.reg8
eaters.winter.reg[[9]]=eaters.winter.reg9
eaters.winter.reg[[10]]=eaters.winter.reg10
eaters.winter.reg[[11]]=eaters.winter.reg11
eaters.winter.reg[[12]]=eaters.winter.reg12
eaters.winter.reg[[13]]=eaters.winter.reg13
eaters.winter.reg[[14]]=eaters.winter.reg14
eaters.winter.reg[[15]]=eaters.winter.reg15
names(eaters.winter.reg)= c( "eaters.winter.reg1", "eaters.winter.reg2", "eaters.winter.reg3", "eaters.winter.reg4", "eaters.winter.reg5", "eaters.winter.reg6", "eaters.winter.reg7", "eaters.winter.reg8", "eaters.winter.reg9", "eaters.winter.reg10", "eaters.winter.reg11", "eaters.winter.reg12", "eaters.winter.reg13", "eaters.winter.reg14","crap")
#We don't need those vectors anymore
rm(eaters.winter.reg1,eaters.winter.reg2,eaters.winter.reg3,eaters.winter.reg4,eaters.winter.reg5,eaters.winter.reg6,eaters.winter.reg7,eaters.winter.reg8,eaters.winter.reg9,eaters.winter.reg10,eaters.winter.reg11,eaters.winter.reg12,eaters.winter.reg13,eaters.winter.reg14,eaters.winter.reg15)

# simple cannibalism matrix. To be used for all seasons and situations.

simple.cannibalism1=c(8) #c(4,5,6,7,8,9)
simple.cannibalism2=c(9) #c(5,6,7,8,9,10)
simple.cannibalism3=c(10) #c(6,7,8,9,10,11)
simple.cannibalism4=c(11) #c(7,8,9,10,11,12)
simple.cannibalism5=c(10:13)#(8:12) #c(8,9,10,11,12,13)
simple.cannibalism6=c(11:14)#(9:13) #c(9,10,11,12,13,14)
simple.cannibalism7=c(12:14)#(10:14) #c(10,11,12,13,14)
simple.cannibalism8=c() #c(11,12,13,14)
simple.cannibalism9= c(10:11) #c(12,13,14)
simple.cannibalism10=c(11:12)
simple.cannibalism11=c(12:13)
simple.cannibalism12=c(13:14)
simple.cannibalism13=c()
simple.cannibalism14=c()
# the last vector is created to solve the technical
# problem of null vector but is not use in the model
simple.cannibalism15=c(1)

simple.cannibalism=list()
simple.cannibalism[[1]]=simple.cannibalism1
simple.cannibalism[[2]]=simple.cannibalism2
simple.cannibalism[[3]]=simple.cannibalism3
simple.cannibalism[[4]]=simple.cannibalism4
simple.cannibalism[[5]]=simple.cannibalism5
simple.cannibalism[[6]]=simple.cannibalism6
simple.cannibalism[[7]]=simple.cannibalism7
simple.cannibalism[[8]]=simple.cannibalism8
simple.cannibalism[[9]]=simple.cannibalism9
simple.cannibalism[[10]]=simple.cannibalism10
simple.cannibalism[[11]]=simple.cannibalism11
simple.cannibalism[[12]]=simple.cannibalism12
simple.cannibalism[[13]]=simple.cannibalism13
simple.cannibalism[[14]]=simple.cannibalism14
simple.cannibalism[[15]]=simple.cannibalism15
names(simple.cannibalism)= c( "simple.cannibalism1", "simple.cannibalism2",
  "simple.cannibalism3", "simple.cannibalism4", "simple.cannibalism5",
  "simple.cannibalism6", "simple.cannibalism7", "simple.cannibalism8",
  "simple.cannibalism9", "simple.cannibalism10", "simple.cannibalism11",
  "simple.cannibalism12", "simple.cannibalism13", "simple.cannibalism14","crap")
#We don't need those vectors anymore
rm(simple.cannibalism1,simple.cannibalism2,simple.cannibalism3,simple.cannibalism4,
  simple.cannibalism5,simple.cannibalism6,simple.cannibalism7,simple.cannibalism8,
  simple.cannibalism9,simple.cannibalism10,simple.cannibalism11,simple.cannibalism12,
  simple.cannibalism13,simple.cannibalism14,simple.cannibalism15)

