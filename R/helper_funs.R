# Convert a brood table to a return table
brood_to_return <- function(bt){
  bt %>% group_by(Stock) |>
    pivot_longer(cols=contains("Age"), names_to="AgeNames", values_to="Return") %>%
    arrange(AgeNames) |>
    mutate(Age=parse_number(AgeNames),
           ReturnYear=BroodYear+Age) |>
    filter(!is.na(Return)) |>
    select(Stock, ReturnYear, AgeNames, Return) %>%
    pivot_wider(names_from=AgeNames, values_from=Return)
}

# Get number of estimated parameters from a built dlm model object
get_npar <- function(build){

  # process variances (timestep to timestep)
  v_par <- length(which(diag(build$W)>0))

  # regression coeffs plus intercept
  betas <- ncol(build$X)+1

  # number of parameters
  v_par + betas

}

# Calculate AIC for a dlm model object given the data and npar
get_AIC <- function(y, mod, npar){

  # y <- y[which(!is.na(y))] #
  # print(y)
  # n data points

  n <- length(which(!is.na(y)))

  # Negative log-likelihoods
  nLL <- dlmLL(y, mod)

  # Calculate Information Criteria (IC)
  AIC <- 2*npar +  2*nLL

  AICc <- AIC + (2*npar^2 + 2*npar) / (n -npar-1)

  AICc

}


