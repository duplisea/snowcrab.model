## Background

This model was started as matrix population (e.g. Caswell) in 2002. A
relatively simple working version was available in 2003. Red Methot and
a Co-op student from L’Université de Sherbrooke (Jean-François Gosselin)
assisted in this. The project was put on hold until 2007 when a post-doc
from France, Hilaire Drouineau, took over model development. Hilaire
hugely improved the model and processes. Hilaire also produced a version
where some of the time consuming loops were done in C++. That C++
version is not here, this is the 2005 fitted version.

The model was never a completely fitted model, although some
sub-processes were fitted to data. The model was intended to be used in
an MSE context where it would be the operating model. Therefore, several
of the processes could be turned on and off to create alternative
operating models.

We never got to the point on taking the work fully through to an MSE.

## Running the model

you need to run the script “crab.model.r”. It creates a list called
*crab.model* with all the functions necessary to run the model. The
model parameters are intialised by running the parameters function which
creates a list with all the parameters.

    # set up all initial parameters by running the params.f function
      params= inputs$params

    # run the model (25 years and with cannibalism parameters in parameters file)
      tmp2.res= crab.model$main.f(100)
      # this produces an out of bounds error.

    # plot model run results
      analysis$plots.f(tmp2.res)
      analysis$percent.plots.f(tmp3.res)

This script produces an out of bounds error. I would need to trouble
shoot this to take it further.

The coding seems primitive with several things hard-coded.

## Description and manuscript

There is a description of several of the processes and thinking on how a
manuscript on this could look in the manuscript folder.

## Use

Feel free to use the code and description of the model. Please
acknowledge us and cite the github repository for publication,
presentation and reporting. We are open to collaboration if you wish to
reach out but you can use this if you want to collaborate or not. We
make no guarantees on the quality of the code, error or results.
