mat.dim=16
LME.cont= matrix(sample(c(0,1),mat.dim^2,replace=T),ncol=mat.dim) # LME contiguity matrix
first.sample= sample(1:mat.dim,mat.dim) # vector of starting rows/columns
first.LME= LME.cont[first.sample[1],1:first.sample-1] #select the lower triangle of row from first randomly selected starting row
first.LME==0