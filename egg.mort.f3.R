egg.mort.f=function(m,AA,temp,out.lim,out.egg.mort, i,j){
  
  #By the time the larvae are released female mortality occur and the eggs are lost
  #we apply the female mortality to the eggs stages.
  #eggs mortality is a weigthed mean (by number of eggs coming from each stages)
  #of mortality of females carrying eggs
  #
  #Call by calc.t
  #Input: 	m : abundance vector
  #		AA : leslie matrix modified by molting choice (regular, terminal or skip)
  #		temp : temperature
  #		out.lim : percentage of lost eggs from sperm limitation  
  #		i : year of simulation
  #		j : time of the year (1 or 2)
  #
  #Output: 	AA : 	matrix modified
  #		pct.primi2 : percentage of sperm limitation of primiparous from last time-step		 
  #		pct.multi2 : percentage of sperm limitation of multiparous from last time-step
  #		pct.primi3 : percentage of sperm limitation of primiparous from second last time-step
  #		pct.multi3 : percentage of sperm limitation of multiparous from second last time-step
  #Red Méthot - December 2005
  
  #extract the percentage of sperm limitation (from sperm.lim.f)
  pct.primi=out.lim[[6]]
  pct.multi=out.lim[[7]]
  
  
  #if first time step no pct.primi2, pct.multi2, pct.primi2 and pct.multi2
  # are set to 1 by default
  if (i+j==2)	{
  pct.primi2=1
  pct.multi2=1
  pct.primi3=1
  pct.multi3=1
  }
  
  
  else if (i+j >= 3)	{
  pct.primi2=out.egg.mort$pct.primi2
  pct.multi2=out.egg.mort$pct.multi2
  pct.primi3=out.egg.mort$pct.primi3
  pct.multi3=out.egg.mort$pct.multi3
  }
  
  #the matrix use to keep track of number off eggs producede by each stages
  n.egg=matrix(0,nrow=12,ncol=2)
  
  
  
  #############################################################################################
  ####################		one-year egg production		#################################
  #############################################################################################
  #when temperature >= 0.8
  
  
  #mortality from egg1 to larvae
  #males
  #number of eggs from each stages
  
  
  if (temp >= 0.8)	{
  #block the way for two years development of eggs
  AA[2,1]= 0
  AA[102,103]= 0
  
  
  
  #the abundance of female at actual time-step * fecundity * survival of female stages
  #male (n.egg[,2])
  
  #Instar IX
  n.egg[1,2]=m[134]*AA[1,133]*AA[135,134]*pct.primi
  n.egg[2,2]=m[136]*AA[1,135]*AA[137,136]*pct.multi
  n.egg[3,2]=m[138]*AA[1,137]*AA[139,138]*pct.multi
  n.egg[4,2]=m[140]*AA[1,139]*AA[141,140]*pct.multi
  
  #Instar X
  n.egg[5,2]=m[146]*AA[1,145]*AA[147,146]*pct.primi 
  n.egg[6,2]=m[148]*AA[1,147]*AA[149,148]*pct.multi
  n.egg[7,2]=m[150]*AA[1,149]*AA[151,150]*pct.multi
  n.egg[8,2]=m[152]*AA[1,151]*AA[153,152]*pct.multi
  
  #Instar XI
  n.egg[9,2]=m[158]*AA[1,157]*AA[159,158] *pct.primi
  n.egg[10,2]=m[160]*AA[1,159]*AA[161,160] *pct.multi
  n.egg[11,2]=m[162]*AA[1,161]*AA[163,162] *pct.multi
  n.egg[12,2]=m[164]*AA[1,163]*AA[165,164] *pct.multi
  
  #total number of male eggs after female mortality
  n.egg.total=sum(n.egg[,2])
  
  #the mortality of egg1 to larvae
  AA[4,1]=n.egg.total/m[1]
  if (m[1]==0){AA[4,1]=0}
  
  
  #female (n.egg[,1])
  
  #Instar IX
  n.egg[1,1]=m[134]*AA[108,133]*AA[135,134]
  n.egg[2,1]=m[136]*AA[108,135]*AA[137,136]
  n.egg[3,1]=m[138]*AA[108,137]*AA[139,138]
  n.egg[4,1]=m[140]*AA[108,139]*AA[141,140]
  
  #Instar X
  n.egg[5,1]=m[146]*AA[108,145]*AA[147,146] 
  n.egg[6,1]=m[148]*AA[108,147]*AA[149,148] 
  n.egg[7,1]=m[150]*AA[108,149]*AA[151,150] 
  n.egg[8,1]=m[152]*AA[108,151]*AA[153,152] 
  
  #Instar XI
  n.egg[9,1]=m[158]*AA[108,147]*AA[149,148] 
  n.egg[10,1]=m[160]*AA[108,149]*AA[151,150] 
  n.egg[11,1]=m[162]*AA[108,151]*AA[153,152] 
  n.egg[12,1]=m[164]*AA[108,153]*AA[155,154] 
  
  #total number of female eggs after female mortality
  n.egg.total=sum(n.egg[,1])
  
  #the mortality of egg1 to larvae
  AA[111,108]=n.egg.total/m[1]
  if (m[108]==0){AA[111,108]=0}
  
  
  }
  
  
  
  ##############################################################################################
  ####			2-years eggs production 				############################
  ##############################################################################################
  
  #If water temperature < 0.8 the eggs need 2 years of development
  
  if (temp < 0.8){
  #In the matrix, the way to go directly from stage egg1 to larvae (if eggs production is one year)is blocked
  AA[4,1]= 0
  AA[111,108]= 0
       
  # less stages can produce eggs while temperature is cold
  
  AA[1,135] = 0
  AA[108,135] = 0
  AA[1,139] = 0
  AA[108,139] = 0
  
  AA[1,147] = 0
  AA[108,147] = 0
  AA[1,151] = 0
  AA[108,151] = 0
  
  AA[1,159] = 0
  AA[108,159]= 0
  AA[1,163] = 0
  AA[108,163]= 0
  
  ###############################
  #mortality from egg3 to larvae#
  ###############################
  
  
  #the abundance of female at actual time-step * fecundity * survival of female stages
  
  
  #males
  n.egg[1,2]= m[136]*AA[1,133]*AA[137,136]*pct.primi3
  n.egg[2,2]= m[140]*AA[1,137]*AA[141,140]*pct.multi3
  
  n.egg[3,2]= m[148]*AA[1,145] *AA[149,148]*pct.primi3
  n.egg[4,2]= m[152]*AA[1,149]*AA[153,152]*pct.multi3
  
  n.egg[5,2]= m[160]*AA[1,157] *AA[161,160]*pct.primi3
  n.egg[6,2]= m[164]*AA[1,161] *AA[165,164]*pct.multi3
  
  
  #total number of male eggs after female mortality
  n.egg.total=sum(n.egg[,2])
  
  #the mortality of egg3 to larvae
  AA[4,3]=n.egg.total/m[3]
  if (m[3]==0){AA[4,3]=0}
  
  #females
  n.egg[1,1]= m[136]*AA[108,133]*AA[137,136]*pct.primi3
  n.egg[2,1]= m[140]*AA[108,137]*AA[141,140]*pct.multi3
  
  n.egg[3,1]= m[148]*AA[108,145]*AA[149,148]*pct.primi3 
  n.egg[4,1]= m[152]*AA[108,149]*AA[151,152]*pct.multi3
  
  n.egg[5,1]= m[160]*AA[108,157]*AA[161,160]*pct.primi3 
  n.egg[6,1]= m[164]*AA[108,161]*AA[165,164]*pct.multi3
  
  
  #total number of female eggs after female mortality
  n.egg.total=sum(n.egg[,1])
  
  #the mortality of egg3 to larvae
  AA[111,110]=n.egg.total/m[105]
  if (m[110]==0){AA[111,110]=0}
  
  
  
  
  ##############################
  #mortality from egg2 to egg3 #
  ##############################
  
  
  
  #the abundance of female at actual time-step * fecundity * survival of female stages
  
  #n.egg=the number of eggs from each females stages still alive 
  #(after mortality of females from egg1 to egg2 was apply)
  
  
  
  
  #males
  n.egg[1,2]=m[135]*AA[1,133]*AA[136,135]*pct.primi2
  n.egg[2,2]= m[139]*AA[1,137]*AA[140,139]*pct.multi2
  
  n.egg[3,2]= m[147]*AA[1,145]*AA[148,147]*pct.primi2
  n.egg[4,2]= m[151]*AA[1,149]*AA[152,151]*pct.multi2
  
  n.egg[5,2]= m[159]*AA[1,157]*AA[160,159]*pct.primi2 
  n.egg[6,2]= m[163]*AA[1,161]*AA[164,163]*pct.multi2
  
  
  #total number of male eggs after female mortality
  n.egg.total=sum(n.egg[,2])
  
  #the mortality of egg2 to egg3
  AA[3,2]=n.egg.total/m[2]
  if (m[2]==0){AA[3,2]=0}
  
  
  
  #females
  n.egg[1,1]= m[135]*AA[108,133]*AA[136,135]*pct.primi2
  n.egg[2,1]= m[139]*AA[108,137]*AA[140,139]*pct.multi2
  
  n.egg[3,1]= m[147]*AA[108,145]*AA[148,147]*pct.primi2 
  n.egg[4,1]= m[151]*AA[108,149]*AA[152,151]*pct.multi2 
  
  n.egg[5,1]= m[159]*AA[108,157]*AA[160,159]*pct.primi2
  n.egg[6,1]= m[163]*AA[108,161]*AA[164,163]*pct.multi2
  
  #total number of female eggs after female mortality
  n.egg.total=sum(n.egg[,1])
  
  #the mortality of egg2 to egg3
  AA[110,109]=n.egg.total/m[109]
  if (m[109]==0){AA[110,109]=0}
  
  #keep percentage of eggs lost by sperm limitation for next time-step
  pct.primi3=pct.primi2
  pct.multi3=pct.multi2
  
  
  ######################## mortality from egg1 to egg2 ################################
  
  #abundance of stages * fecundity * stages mortality
  
  
  
  #the abundance of female at actual time-step * fecundity * survival of female stages * percentage of sperm limitation
  
  #male (n.egg[,2])
  
  #Instar IX
  n.egg[1,2]=m[134]*AA[1,133]*AA[135,134]*pct.primi
  n.egg[2,2]=m[138]*AA[1,137]*AA[139,138]*pct.multi
  
  #Instar X
  n.egg[3,2]=m[146]*AA[1,145]*AA[147,146]*pct.primi
  n.egg[4,2]=m[150]*AA[1,149]*AA[151,150]*pct.multi
  
  #Instar XI
  n.egg[5,2]=m[158]*AA[1,157]*AA[159,158]*pct.primi
  n.egg[6,2]=m[162]*AA[1,161]*AA[163,162]*pct.multi
  
  
  #total number of male eggs after female mortality
  n.egg.total=sum(n.egg[,2])
  
  #the mortality of egg1 to egg2
  AA[2,1]=n.egg.total/m[2]
  if (m[2]==0){AA[2,1]=0}
  
  
  #female (n.egg[,1])
  
  #Instar IX
  n.egg[1,1]=m[134]*AA[108,133]*AA[135,134]*pct.primi
  n.egg[2,1]=m[138]*AA[108,137]*AA[139,138]*pct.multi
  
  #Instar X
  n.egg[3,1]=m[146]*AA[108,145]*AA[147,146]*pct.primi 
  n.egg[4,1]=m[150]*AA[108,149]*AA[151,150]*pct.multi 
  
  #Instar XI
  n.egg[5,1]=m[158]*AA[108,157]*AA[159,158]*pct.primi
  n.egg[6,1]=m[162]*AA[108,161]*AA[163,162]*pct.multi 
  
  #total number of female eggs after female mortality
  n.egg.total=sum(n.egg[,1])
  
  #the mortality of egg1 to egg2
  AA[109,108]=n.egg.total/m[104]
  if (m[108]==0){AA[109,108]=0}
  
  #keep percentage of eggs lost by sperm limitation for next time-step
  pct.primi2=pct.primi
  pct.multi2=pct.multi
  
  
  
  
  
  }
  out.egg.mort=list()
  out.egg.mort[[1]]=AA
  out.egg.mort[[2]]=pct.primi2
  out.egg.mort[[3]]=pct.multi2
  out.egg.mort[[4]]=pct.primi3
  out.egg.mort[[5]]=pct.multi3
  names(out.egg.mort)= c("AA", "pct.primi2", "pct.multi2", "pct.primi3", "pct.multi3") 
  out.egg.mort
}