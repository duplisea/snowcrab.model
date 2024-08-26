skip.f=function(m,AA,stage.m,j,simple.cannibalism)		{
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
  if (params$simple.cannibalism.on==0){
    if (j==1) {
    eaters.exp=eaters.summer.exp[[k]]
      }
  
    else if (j==2) {
    eaters.exp=eaters.winter.exp[[k]]
      }
    }
  
  else if (params$simple.cannibalism.on==1){
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
