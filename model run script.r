## A SCRIPT TO RUN THE CRAB MODEL AND ANALYSE IT

  # run the function to make the initial parameters list. This list can be directly
  # modified after it is created and a copy is appended to the output of any model
  # run.
params= inputs$params

  # run the model (25 years and with cannibalism parameters in parameters file)
tmp2.res= crab.model$main.f(100)

  # plot model run results
analysis$plots.f(tmp2.res)
analysis$percent.plots.f(tmp3.res)



params$fishing=0