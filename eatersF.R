eaters.f= function(breadth=params$breadth,offset=params$offset,refuge.instar=params$refuge){

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
