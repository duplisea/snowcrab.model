fecundity.f= function(CW,reproductive.strategy="primiparous",model="sainte.marie"){
  
  # CW = median carapace width in reproductive stages (mm)
  
  if (reproductive.strategy=="primiparous"){
  if(model=="sainte.marie"){
  # sainte-marie 1993 relationship
  fecundity = round(exp(log(10)*(-0.050 + 2.616*log10(CW))))
  }
  else if (model=="conan"){
  # conan et al. 1989 relationship
  fecundity = round(exp(0.0350 + 2.5532*log(CW)))
  }
  }
     else if (reproductive.strategy=="multiparous"){
  if(model=="sainte.marie"){
  # sainte-marie 1993 relationship
  fecundity = round(exp(log(10)*(0.062 + 2.616*log10(CW))))
  }
  else if (model=="conan"){
  # conan et al. 1989 relationship
  fecundity = round(exp(0.3044 + 2.4894*log(CW)))
  }
  }
  
  fecundity
}
