Leslie.matrix.f= function(){



#extract the parameters from the list params
larvs.pct = params$larvs.pct
ext.death = params$ext.death
CW.9 = params$CW.9
CW.10 = params$CW.10
CW.11 = params$CW.11
stages.lst = params$stages.lst

#A is the basic Leslie survivorship matrix
#To keep track of each cohort, at each time step every cohorts move to another stage. For example,
#a crab of Instar X at terminal molt will be X-TM1 , X-TM2 the next time step...
#males 1:107, females 108:214

A = matrix(0,nrow=length(stages.lst),ncol=length(stages.lst))

#########################################################
######		survival rates			#########
#########################################################
#P is the proportion of the survivors from non-cannibalistic M, for each stage
#see also ext_death.xls

#P is the proportion of the survivors from non-cannibalistic M, for each stage
#P is elevated at the power 1/2 because there are 2 treatments a year
P = (1 - (ext.death)) ^ (1/2)

#0.1 is attributed to molting, thus TM do not suffer this mortality
#P2 is apply to adult (TM)
P2 = (1 - (ext.death-0.1)) ^ (1/2)


#P for males
# part to change to modify mortality rates

A[2,1] = 1						#egg(1) to egg(2)
A[3,2] = 1						#egg(2) to egg(3)
A[4,3] = 1						#egg(3) to larvae
A[4,1] = 1						#egg(1) to larvae
A[5,4] = larvs.pct				#larvae to Instar I (will be modified by the function larv.perc) 
A[6,5] = 0						#Instar I to I-skip(1)
A[7,6] = P2[1,2]					#Instar I-skip(1) to I-skip(2)
A[8,7] = P[1,2]					#Instar I-skip() to II
A[8,5] = P[1,2]					#Instar I to II
A[9,8] = 0						#Instar II to II-skip(1)
A[10,9] = P2[2,2]					#Instar II-skip(1) to II-skip(2)
A[11,10] = P[2,2]					#Instar II-skip(2) to III
A[11,8] = P[2,2]					#Instar II to III	
A[12,11] = 0					#Instar III to III-skip(1)
A[13,12] = P2[3,2]				#Instar III-skip(1) to III-skip(2)
A[14,13] = 	P[3,2]				#Instar III-skip(2) to IV
A[14,11] = P[3,2]					#Instar III to IV
A[15,14] = 0					#Instar IV to IV-skip(1)
A[16,15] = P2[4,2]				#Instar IV-skip(1) to IV-skip(2)
A[17,16] = P[4,2]					#Instar IV-skip(2) to V
A[17,14] = P[4,2]					#Instar IV to V
A[18,17] = 0					#Instar V to V-skip(1)
A[19,18] = P2[4,2]				#Instar V-skip(1) to V-skip(2)
A[20,19] = P[4,2]					#Instar V-skip(2) to VI (1)
A[20,17] = P[4,2]					#Instar V to VI(1)

A[21,20] = P[6,2]					#Instar VI(1) to VI(2)
A[22,21] = P[6,2]					#Instar VI(2) to VII(1)
A[23,22] = P[7,2]					#Instar VII(1) to VII(2)
A[24,23] = P[7,2]					#Instar VII(2) to VIII(1)
A[25,24] = P[8,2]					#Instar VIII(1) to VIII(2)
A[26,25] = P[8,2]					#Instar VIII(2) to IX-TM1
A[27,26] = P2[9,2]				#Instar IX-TM1 to IX-TM2 
A[28,27] = P2[9,2]				#Instar IX-TM2 to IX-TM3
A[29,28] = (1-0.60)^0.5				#Instar IX-TM3 to IX-TM4 (0.60 is a annual mortality rate)
A[30,29] = (1-0.80)^0.5				#Instar IX-TM4 to IX-TM5 (P is manually assigned)
A[31,30] = (1-0.80)^0.5				#Instar IX-TM5 to IX-TM6 (P is manually assigned)	
A[32,31] = P2[9,2]				#Instar IX-TM6 to IX-TM7
A[33,32] = (1-0.60)^0.5				#Instar IX-TM7 to IX-TM8
A[34,33] = (1-0.80)^0.5				#Instar IX-TM8 to IX-TM9 (P is manually assigned)
A[35,34] = (1-0.80)^0.5				#Instar IX-TM9 to IX-TM10 (P is manually assigned)	
A[36,25] = P[8,2]					#Instar VIII(2) to IX(1)
A[37,36] = P[9,2]					#Instar IX(1) to IX(2)
A[38,37] = P[9,2]					#Instar IX(2) to X-TM1
A[39,38] = P2[10,2]				#Instar X-TM1 to X-TM2
A[40,39] = P2[10,2]				#Instar X-TM2 to X-TM3
A[41,40] = (1-0.60)^0.5				#Instar X-TM3 to X-TM4
A[42,41] = (1-0.80)^0.5				#Instar X-TM4 to X-TM5 (P is manually assigned)
A[43,42] = (1-0.80)^0.5				#Instar X-TM5 to X-TM6 (P is manually assigned)
A[44,43] = P2[9,2]				#Instar X-TM6 to X-TM7
A[45,44] = (1-0.60)^0.5				#Instar X-TM7 to X-TM8
A[46,45] = (1-0.80)^0.5				#Instar X-TM8 to X-TM9 (P is manually assigned)
A[47,46] = (1-0.80)^0.5				#Instar X-TM9 to X-TM10 (P is manually assigned)	

A[48,37] = P[9,2]					#Instar IX(2) to X(1)
A[49,48] = P[10,2]				#Instar X(1) to X(2)
A[50,49] = P[10,2]				#Instar X(2) to XI-TM1
A[51,50] = P2[11,2]				#Instar XI-TM1 to XI-TM2
A[52,51] = P2[11,2]				#Instar XI-TM2 to XI-TM3
A[53,52] = P2[11,2]				#Instar XI-TM3 to XI-TM4
A[54,53] = (1-0.60)^0.5				#Instar XI-TM4 to XI-TM5
A[55,54] = (1-0.60)^0.5				#Instar XI-TM5 to XI-TM6
A[56,55] = (1-0.80)^0.5				#Instar XI-TM6 to XI-TM7 (P is manually assigned)
A[57,56] = (1-0.80)^0.5				#Instar XI-TM7 to XI-TM8 (P is manually assigned)
A[58,57] = P2[9,2]				#Instar XI-TM8 to XI-TM9
A[59,58] = (1-0.60)^0.5				#Instar XI-TM9 to XI-TM10
A[60,59] = (1-0.80)^0.5				#Instar XI-TM10 to XI-TM11 (P is manually assigned)
A[61,60] = (1-0.80)^0.5				#Instar XI-TM11 to XI-TM12 (P is manually assigned)	
A[62,49] = P[10,2]				#Instar X(2) to XI(1)
A[63,62] = P[11,2]				#Instar XI(1) to XI(2)
A[64,63] = P[11,2]				#Instar XI(2) to XII-TM1
A[65,64] = P2[12,2]				#Instar XII-TM1 to XII-TM2
A[66,65] = P2[12,2]				#Instar XII-TM2 to XII-TM3
A[67,66] = P2[12,2]				#Instar XII-TM3 to XII-TM4
A[68,67] = (1-0.60)^0.5				#Instar XII-TM4 to XII-TM5
A[69,68] = (1-0.60)^0.5				#Instar XII-TM5 to XII-TM6
A[70,69] = (1-0.80)^0.5				#Instar XII-TM6 to XII-TM7 (P is manually assigned)
A[71,70] = (1-0.80)^0.5				#Instar XII-TM7 to XII-TM8 (P is manually assigned)
A[72,71] = P2[9,2]				#Instar XII-TM8 to XII-TM9
A[73,72] = (1-0.60)^0.5				#Instar XII-TM9 to XII-TM10
A[74,73] = (1-0.80)^0.5				#Instar XII-TM10 to XII-TM11 (P is manually assigned)
A[75,74] = (1-0.80)^0.5				#Instar XII-TM11 to XII-TM12 (P is manually assigned)	
A[76,63] = P[11,2]				#Instar XI(2) to XII(1)
A[77,76] = P[12,2]				#Instar XII(1) to XII(2)
A[78,77] = P[12,2]				#Instar XII(2) to XIII-TM1
A[79,78] = P2[13,2]				#Instar XIII-TM1 to XIII-TM2
A[80,79] = P2[13,2]				#Instar XIII-TM2 to XIII-TM3
A[81,80] = P2[13,2]				#Instar XIII-TM3 to XIII-TM4
A[82,81] = P2[13,2]				#Instar XIII-TM4 to XIII-TM5
A[83,82] = (1-0.50)^0.5				#Instar XIII-TM5 to XIII-TM6
A[84,83] = (1-0.60)^0.5				#Instar XIII-TM6 to XIII-TM7
A[85,84] = (1-0.60)^0.5				#Instar XIII-TM7 to XIII-TM8
A[86,85] = (1-0.80)^0.5				#Instar XIII-TM8 to XIII-TM9 (P is manually assigned)
A[87,86] = (1-0.80)^0.5				#Instar XIII-TM9 to XIII-TM10 (P is manually assigned)
A[88,87] = P2[9,2]				#Instar XIII-TM10 to XIII-TM11
A[89,88] = (1-0.60)^0.5				#Instar XIII-TM11 to XIII-TM12
A[90,89] = (1-0.80)^0.5				#Instar XIII-TM12 to XIII-TM13 (P is manually assigned)
A[91,90] = (1-0.80)^0.5				#Instar XIII-TM13 to XIII-TM14 (P is manually assigned)	
A[92,77] = P[12,2]				#Instar XII(2) to XIII(1)
A[93,92] = P[13,2]				#Instar XIII(1) to XIII(2)
A[94,93] = P[13,2]				#Instar XIII(2) to XIV-TM1
A[95,94] = P2[14,2]				#Instar XIV-TM1 to XIV-TM2
A[96,95] = P2[14,2]				#Instar XIV-TM2 to XIV-TM3
A[97,96] = P2[14,2]				#Instar XIV-TM3 to XIV-TM4
A[98,97] = (1-0.40)^0.5				#Instar XIV-TM4 to XIV-TM5
A[99,98] = (1-0.50)^0.5				#Instar XIV-TM5 to XIV-TM6
A[100,99] = (1-0.60)^0.5				#Instar XIV-TM6 to XIV-TM7
A[101,100] = (1-0.70)^0.5				#Instar XIV-TM7 to XIV-TM8
A[102,101] = (1-0.80)^0.5				#Instar XIV-TM8 to XIV-TM9 (P is manually assigned)
A[103,102] = (1-0.80)^0.5				#Instar XIV-TM9 to XIV-TM10 (P is manually assigned)
A[104,103] = P2[9,2]				#Instar XIV-TM10 to XIV-TM11
A[105,104] = (1-0.60)^0.5				#Instar XIV-TM11 to XIV-TM12
A[106,105] = (1-0.80)^0.5				#Instar XIV-TM12 to XIV-TM13 (P is manually assigned)
A[107,106] = (1-0.80)^0.5				#Instar XIV-TM13 to XIV-TM14 (P is manually assigned)	



#P for females

A[109,108] = 1					#egg(1) to egg(2)
A[110,109] = 1					#egg(2) to egg(3)
A[111,110] = 1					#egg(3) to larvae
A[111,108] = 1					#egg(1) to larvae
A[112,111] = larvs.pct				#larvae to Instar I		
A[113,112] = 0					#Instar I to I-skip(1)
A[114,113] = P2[1,1]				#Instar I-skip(1) to I-skip(2)
A[115,114] = P[1,1]				#Instar I-skip(2) to II
A[115,112] = P[1,1]				#Instar I to II
A[116,115] = 0					#Instar II to II-skip(1)
A[117,116] = P2[2,1]				#Instar II-skip(1) to II-skip(2)
A[118,117] = P[2,1]				#Instar II-skip(2) to III
A[118,115] = P[2,1]				#Instar II to III	
A[119,118] = 0					#Instar III to III-skip(1)
A[120,119] = P2[3,1]				#Instar III-skip(1) to III-skip(2)
A[121,120] = P[3,1] 				#Instar III-skip(2) to IV
A[121,118] = P[3,1]				#Instar III to IV
A[122,121] = 0					#Instar IV to IV-skip(1)
A[123,122] = P2[4,1]				#Instar IV-skip(1) to IV-skip(2)
A[124,123] = P[4,1]				#Instar IV-skip(2) to V
A[124,121] = P[4,1]				#Instar IV to V
A[125,124] = 0					#Instar V to V-skip(1)
A[126,125] = P2[5,1]				#Instar V-skip(1) to V-skip(2)
A[127,126] = P[5,1]				#Instar V-skip(2) to VI(1)
A[127,124] = P[5,1]					#Instar V to VI(1)
A[128,127] = P[6,1]					#Instar VI(1) to VI(2)
A[129,128] = P[6,1]					#Instar VI(2) to VII(1)
A[130,129] = P[7,1]					#Instar VII(1) to VII(2)
A[131,130] = P[7,1]					#Instar VII(2) to VIII(1)
A[132,131] = P[8,1]					#Instar VIII(1) to VIII(2)
A[133,132] = P[8,1]					#Instar VIII(2) to IX-TM1
A[134,133] = P2[9,1]				#Instar IX-TM1 to IX-TM2 
A[135,134] = P2[9,1]				#Instar IX-TM2 to IX-TM3
A[136,135] = P2[9,1]				#Instar IX-TM3 to IX-TM4
A[137,136] = (1-0.80)^0.5				#Instar IX-TM4 to IX-TM5 (P is manually assigned)
A[138,137] = (1-0.80)^0.5				#Instar IX-TM5 to IX-TM6 (P is manually assigned)
A[139,138] = P2[9,2]				#Instar IX-TM6 to IX-TM7
A[140,139] = (1-0.60)^0.5				#Instar IX-TM7 to IX-TM8
A[141,140] = (1-0.80)^0.5				#Instar IX-TM8 to IX-TM9 (P is manually assigned)
A[142,141] = (1-0.80)^0.5				#Instar IX-TM9 to IX-TM10 (P is manually assigned)	
A[143,132] = P[8,1]					#Instar VIII(2) to IX(1)
A[144,143] = P[9,1]					#Instar IX(1) to IX(2)
A[145,144] = P[9,1]					#Instar IX(2) to X-TM1
A[146,145] = P2[10,1]				#Instar X-TM1 to X-TM2
A[147,146] = P2[10,1]				#Instar X-TM2 to X-TM3
A[148,147] = P2[10,1]				#Instar X-TM3 to X-TM4
A[149,148] = (1-0.80)^0.5			#Instar X-TM4 to X-TM5 (P is manually assigned)
A[150,149] = (1-0.80)^0.5			#Instar X-TM5 to X-TM6 (P is manually assigned)
A[151,150] = P2[9,2]				#Instar X-TM6 to X-TM7
A[152,151] = (1-0.60)^0.5				#Instar X-TM7 to X-TM8
A[153,152] = (1-0.80)^0.5				#Instar X-TM8 to X-TM9 (P is manually assigned)
A[154,153] = (1-0.80)^0.5				#Instar X-TM9 to X-TM10 (P is manually assigned)	
A[155,144] = P[9,1]				#Instar IX(2) to X(1)
A[156,155] = P[10,1]				#Instar X(1) to X(2)
A[157,156] = P[10,1]				#Instar X(2) to XI-TM1
A[158,157] = P2[11,1]				#Instar XI-TM1 to XI-TM2
A[159,158] = P2[11,1]				#Instar XI-TM2 to XI-TM3
A[160,159] = P2[11,1]				#Instar XI-TM3 to XI-TM4
A[161,160] = P2[11,1]				#Instar XI-TM4 to XI-TM5
A[162,161] = P2[11,1]				#Instar XI-TM5 to XI-TM6
A[163,162] = (1-0.80)^0.5			#Instar XI-TM6 to XI-TM7 (P is manually assigned)
A[164,163] = (1-0.80)^0.5			#Instar XI-TM7 to XI-TM8 (P is manually assigned)
A[165,164] = P2[9,2]				#Instar XI-TM8 to IX-TM9
A[166,165] = (1-0.60)^0.5				#Instar XI-TM9 to IX-TM10
A[167,166] = (1-0.80)^0.5				#Instar XI-TM10 to IX-TM11 (P is manually assigned)
A[168,167] = (1-0.80)^0.5				#Instar XI-TM11 to IX-TM12 (P is manually assigned)	
	
#for the fecundity of the Instar 9,10 and 11 
#we take the mean carapace width of each stage
#we calculate the number of eggs produced by a female at this CW
#we calculate a number for primiparous females and one for multiparous females
#then, we calculate a mean number of eggs, with the proportion
#of primiparous and multiparous females

sexe.ratio=0.5		#females/total
#the mortality of female is included in the calcul of fecundity
#! THIS IS WHERE A TAKES ON VALUES > 1
primi = fecundity.f(CW.9,"primiparous",)
A[1,133] = round(primi*(1-sexe.ratio)*A[134,133])
A[108,133]= round(primi*(sexe.ratio)*A[134,133])

multi = fecundity.f(CW.9,"multiparous",)
A[1,135] = round(multi*(1-sexe.ratio)*A[136,135])
A[108,135] = round(multi*sexe.ratio*A[136,135])
A[1,137] = round(multi*(1-sexe.ratio)*A[138,137])
A[108,137] = round(multi*sexe.ratio*A[138,137])
A[1,139] = round(multi*(1-sexe.ratio)*A[140,139]*0.6)		#the 0.6 is added because old female have less fecundity
A[108,139] = round(multi*sexe.ratio*A[140,139]*0.6)		#the 0.6 is added because old female have less fecundity

primi = fecundity.f(CW.10,"primiparous",)
A[1,145] = round(primi*(1-sexe.ratio)*A[146,145])
A[108,145] = round(primi*sexe.ratio*A[146,145])

multi = fecundity.f(CW.10,"multiparous",)
A[1,147] = round(multi*(1-sexe.ratio)*A[148,147])
A[108,147] = round(multi*sexe.ratio*A[148,147])
A[1,149] = round(multi*(1-sexe.ratio)*A[150,149])
A[108,149] = round(multi*sexe.ratio*A[150,149])
A[1,151] = round(multi*(1-sexe.ratio)*A[152,151]*0.6)		#the 0.6 is added because old female have less fecundity
A[108,151] = round(multi*sexe.ratio*A[152,151]*0.6)		#the 0.6 is added because old female have less fecundity

primi = fecundity.f(CW.11,"primiparous",)
A[1,157] = round(primi*(1-sexe.ratio)*A[158,157])
A[108,157]= round(primi*sexe.ratio*A[158,157])

multi = fecundity.f(CW.11,"multiparous",)
A[1,159] = round(multi*(1-sexe.ratio)*A[160,159])
A[108,159]=round(multi*sexe.ratio*A[160,159])
A[1,161] = round(multi*(1-sexe.ratio)*A[162,161])
A[108,161]=round(multi*sexe.ratio*A[162,161])
A[1,163] = round(multi*(1-sexe.ratio)*A[164,163]*0.6)	#the 0.6 is added because old female have less fecundity
A[108,163]=round(multi*sexe.ratio*A[164,163]*0.6)	#the 0.6 is added because old female have less fecundity
A[1,165] = round(multi*(1-sexe.ratio)*A[166,165]*0.4)	#the 0.4 is added because old female have less fecundity
A[108,165]=round(multi*sexe.ratio*A[166,165]*0.4)	#the 0.4 is added because old female have less fecundity

#A_base is now the complete basic survivorship leslie matrix
A.base = A
A.base
}


#shite=Leslie.matrix.f()
#write.table(shite,"d:/tmp/shite.csv",sep=";")