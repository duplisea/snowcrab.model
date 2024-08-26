cannibalism.f=function(m,AA,j,stage.m,temp,simple.cannibalism){
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
      AA[,k] = calc.val(m,j,k,stage.m,temp,simple.cannibalism)^(1/2) * AA[,k]
      }
  
  #females        
  #from Instar 1 to Instar XI
  for (k in 112:168){
      #we multiply the values of each column of AA
      #by a factor of cannibalism, calculated by calc.val
      AA[,k] = calc.val(m,j,k,stage.m,temp,simple.cannibalism)^(1/2) * AA[,k]
      }
  
  #for the fecundity, because we don't want to affect cannibalism to fecundity
  #males
  AA[1,123:156] = AA.tempo[1,123:156]
  #females
  AA[(nrow(AA)/2 + 1),123:156]=AA.tempo[(nrow(AA)/2 + 1),123:156]
  
  AA
}
