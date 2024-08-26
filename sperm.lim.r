sperm.lim.f=function(m,AA,out.lim,i,temp){
  
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
