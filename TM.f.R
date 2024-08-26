TM.f=function(m,AA,temp){
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
  val=params$weigth.male
  
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
