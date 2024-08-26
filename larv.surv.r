larv.surv.f=function(AA, surface.temperature){
  # larval mortality is a function of temperature. It looks like a - quadratic
  # where survival is maximised at some intermediate temperature

  # temperature is modelled as a gaussian and taken from subarea 6
  # in the NW Gulf. The mean of the Gaussian is take as the mean of
  # june and july mean. This encompasses all the months where the
  # larva are potentially in the water.

  # the quadratic survival function was fitted from experiments
  # on larval survival for snow crabs from the Japan Sea. The survival
  # calculated is the multiplicative survival rate of all instar phases:
  # Z1, Z2, M, to C1

  larvae.survival= -0.0012*surface.temperature^2 + 0.0309*surface.temperature - 0.1664

  # This survival can sometime be negative with this function so the following
  # just sets a minumum value
  larvae.survival[larvae.survival<.0001]=.0001
  
  larvae.survival
}
