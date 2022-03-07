remove_burn_in = function(posterior, burn_in_pct=0.1){

  nSamples = nrow(posterior)
  n_burn_in_samples = nSamples * burn_in_pct

  return(posterior[n_burn_in_samples:nSamples, ])
}
# is true parameter recovered in HPD (highest posterior density) interval?
get_coverage = function(hpd_lower, hpd_upper, truth){
  if(truth >= hpd_lower & truth <= hpd_upper){
    return(TRUE)
  }else{
    return(FALSE)
  }
}

# Gets the HPD width relative to the true parameter
get_hpd_width_rel = function(hpd_lower, hpd_upper, truth){

  if (truth == 0){

    width = hpd_upper - hpd_lower

  }else{
    width = (hpd_upper - hpd_lower)/truth
  }

  return(width)
}

get_bias_rel = function(posterior_median, truth){

  if (truth == 0){
    bias = posterior_median - truth
  }else{
    bias = (posterior_median - truth) / truth
  }

  return(bias)
}

# Gets the root mean square error relative to the true parameter
get_RMSE_rel = function(posterior_median, truth){

  if(truth == 0){
    RMSE = sqrt(mean((posterior_median - truth)^2))

  }else{
    RMSE = sqrt(mean((posterior_median - truth)^2))/truth

  }

  return(RMSE)
}

library(HDInterval)
get_hpd_from_posterior = function(posterior){

  hpd = hdi(as.numeric(posterior), credMass = 0.95)

  return(hpd)
}
