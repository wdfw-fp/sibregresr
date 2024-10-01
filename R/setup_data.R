#' Setup data
#'
#'  puts data into a list column with different subsets (different numbers of years excluded) in each row, for fitting
#'
#' @param df Input data. Either a return table or a brood table. Column names: Stock, ReturnYear (or BroodYear), and for each age (e.g., Age3). Must be in that format.
#' @param type whether df is a "return" or "brood" table. Should be one of those values.
#' @param n_eval number of one year-ahead predictions to make (i.e., maximum number of years to exclude)
#'
#' @return tibble with list columns
#'
#' @examples
setup_data<-function(df,
                     type="return",
                     n_forecasts=20){

  df  |>
    #if df is in brood table format, transform it into return table format.
    purrr::when(
      type =="brood" ~ brood_to_return(.),
      ~ .
    ) |>
    group_by(Stock) |>
    bind_rows(filter(.,ReturnYear==max(ReturnYear)) |>
                mutate(ReturnYear=ReturnYear+1) |>
                mutate(across(contains("Age"),\(x)x=NA))) |>
    nest() %>% mutate(n=dim(data[[1]])[1]) %>%
    nest() |>
    mutate(n=dim(data[[1]])[1]) |>
    ## Slice the data to re-fit subsets of data with years trimmed off the end
    crossing(n_years=c(-(1:(n_forecasts)),n)) |>
    mutate(Actual=purrr::map2(data,n_years, ~slice(.x, nrow(.x)+.y+1)),
           data=purrr::map2(data, n_years, ~return_to_brood(head(.x, n=.y),FALSE))) |>
    ## the list of PC1 models sourced in `pc1_mods.r`
    crossing(tibble(model_name=names(no_cov_mods),
                    model=no_cov_mods),
             # the ages for which you want forecasts
             Age=4:6) |>
    mutate(Actual=purrr::map2_dbl(Actual,Age,~.x |> pull(paste0("Age",.y)))) |>
    arrange(Stock,Age,n_years) #if you have n_years.

}
