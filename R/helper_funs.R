#' Convert a Return Table to a Brood Table
#'
#' @param rt return table with column names: Stock, ReturnYear, and for each age (e.g., Age3). Must be in that format.
#' @param by_stock boolean for whether the data contians a Stock column.
#'
#' @return
#'
#' @examples
return_to_brood <- function(rt,by_stock=TRUE){

  # Find the min age column in the return table (rt), then make "sym" for tidy eval below
  min_age <- colnames(rt)[str_detect(colnames(rt),"Age")] |>  min() |>  rlang::sym()

  if(by_stock){
    # Reshape the data, filter rows with complete min_age data
    rt |>
      group_by(Stock) |>
      gather(key="AgeName", value="Return", contains("Age")) |>
      mutate(Age=parse_number(AgeName)) |>
      mutate(BroodYear=ReturnYear - Age) |>
      select(-ReturnYear, -Age) |>
      spread(AgeName, Return) |>
      # Unquo the min_age thing
      filter(!is.na(!!min_age)) |>
      ungroup() |>
      as.data.frame() |>
      arrange(Stock,BroodYear)}else{
        rt |>
          gather(key="AgeName", value="Return", contains("Age")) |>
          mutate(Age=parse_number(AgeName)) |>
          mutate(BroodYear=ReturnYear - Age) |>
          select(-ReturnYear, -Age) |>
          spread(AgeName, Return) |>
          # Unquo the min_age thing
          filter(!is.na(!!min_age)) |>
          ungroup() |>
          as.data.frame() |>
          arrange(BroodYear)

      }

}



#' Convert a brood table to a return table
#'
#' @param bt brood table with column names: Stock, BroodYear, and for each age (e.g., Age3). Must be in that format.
#'
#' @return
#'
#' @examples
brood_to_return <- function(bt){
  bt |>  group_by(Stock) |>
    pivot_longer(cols=contains("Age"), names_to="AgeNames", values_to="Return") |>
    arrange(AgeNames) |>
    mutate(Age=parse_number(AgeNames),
           ReturnYear=BroodYear+Age) |>
    filter(!is.na(Return)) |>
    select(Stock, ReturnYear, AgeNames, Return) |>
    pivot_wider(names_from=AgeNames, values_from=Return)
}

#
#' Get number of estimated parameters from a built dlm model object
#'
#' @param build built dlm model object
#'
#' @return integer. number of parameters
#'
#' @examples
get_npar <- function(build){

  # process variances (timestep to timestep)
  v_par <- length(which(diag(build$W)>0))

  # regression coeffs plus intercept
  betas <- ncol(build$X)+1

  # number of parameters
  v_par + betas

}

#
#' Calculate AICc for a dlm model object given the data and npar
#'
#' @param y response data
#' @param mod dlm model model object
#' @param npar number of parameters
#'
#' @return real, AICc
#'
#' @examples
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


#' Stretching mean
#'
#' @param x a vector of values
#' @param window_size the maximum number of values to include in mean
#'
#' @return
#'
#' @examples
#' vec <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)
#' stretching_mean(vec, window_size = 5)
stretching_mean <- function(x, window_size = Inf) {
  n <- length(x)

  # If window_size is greater than or equal to the length of x, use cumulative mean
  if (window_size >= n) {
    return(cumsum(x) / seq_along(x))
  }

  # Calculate the stretching mean up to the window size
  stretching_part <- cumsum(x[1:window_size]) / seq_along(x[1:window_size])

  # Apply rolling mean for the rest of the values
  rolling_part <- zoo::rollmean(x, window_size, align = "right", fill = NA)

  # Combine the stretching and rolling parts
  c(stretching_part, rolling_part[(window_size + 1):n])
}
