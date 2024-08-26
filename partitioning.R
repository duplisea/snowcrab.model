calc.adult = function(AA, temp)		{

#when temperature is cold (< 0.8) crab molt earlier

if (temp < 0.8)	{
pct.adults=params$pct.ad.cold
}

else if (temp <= 0.8)	{
pct.adults=params$pct.ad.warm
}

#male
#instar IX
AA[21,20]=A[31,20]*pct.adults
AA[31,20]=A[31,20]*(1-pct.adults[9,2])

#instar X
AA[33,32]=A[43,32]*pct.adults
AA[43,32]=A[43,32]*(1-pct.adults[10,2])

#instar XI
AA[45,44]=A[57,44]*pct.adults
AA[57,44]=A[57,44]*(1-pct.adults[11,2])

#instar XII
AA[59,58]=A[71,58]*pct.adults
AA[71,58]=A[71,58]*(1-pct.adults[12,2])

#instar XIII
AA[73,72]=A[87,72]*pct.adults
AA[87,72]=A[87,72]*(1-pct.adults[13,2])


#female
#instar IX
AA[123,122]=A[133,122]*pct.adults
AA[133,122]=A[133,122]*(1-pct.adults[9,1])

#instar X
AA[135,134]=A[145,134]*pct.adults
AA[145,134]=A[145,134]*(1-pct.adults[10,1])

}
