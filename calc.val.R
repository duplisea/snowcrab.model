calc.val=function(m, j, k,stage.m,temp,simple.cannibalism){
  
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
  
  #cannibalism=params$cannibalism
  #! this switch for cannibalism is not used, i.e. impossible to turn off cannibalism
  #! at this stage in the original red model, therefore commented out here. DD 14 June 2005
  fct.can.exp=params$fct.can.exp
  fct.can.reg=params$fct.can.reg
  stage=params$stages.lst[k] #stage is the stage eaten
  fem.cannibalism=params$fem.cannibalism
  can.cold=params$can.cold
  
  #mid is the number of stages
  mid = length(stage.m)/2
  #this function is used to get male and female at the same stage
  
  #! simple cannibalism switch added DD 14 June 2005.
  if (params$simple.cannibalism.on==0){
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
  else if (params$simple.cannibalism.on==1){
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
