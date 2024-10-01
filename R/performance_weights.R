


#' Calculated weighted average preds
#'
#' @param fits data frame returned by `fit_mods` function
#'
#' @return a data frame with predicitons from component models and weighted average models based on AICc, MAPE, and RMSE weights.
#'
#' @details
#' AICc weights are calculated as exp(-.5*deltaAICc) / sum(exp(-.5*deltaAICc))
#' MAPE and RMSE weights are calculated as, for example, Mape^-1 / sum(mape^-1)
#'
#'
#'
performance_weights<-function(fits){


  # Get the predictions, calculate error metrics
  preds_perf <- fits %>%
    filter(map_lgl(error,is.null)) %>%
    mutate(build=pmap(lst(estimates, model, xy_dat), ~with(list(...), model(parm=estimates$par, x.mat=cbind(xy_dat$x)))),
           filter=map2(xy_dat, build, ~dlmFilter((.x$y), .y)),
           npar=map_dbl(build, get_npar),
           AICc=pmap_dbl(lst(dat=xy_dat,model=build, npar), ~with(list(...), get_AIC(dat$y, model, npar))),
           pred=map_dbl(filter, ~exp(.x$f[length(.x$f)])),
           ReturnYear = map2_int(xy_og,Age,~(tail(.x$BroodYear,1)+.y))) %>%
    mutate(
      Er=pred-Actual,
      AEr=abs(Er),
      PE=100*((Er)/Actual),
      APE=abs(PE),
      SQE=(pred-Actual)^2,
      logQ=log(pred/Actual),
      AlogQ=abs(logQ)
    ) |>
    group_by(Stock, Age, model_name) %>%
    arrange(Stock,Age,ReturnYear) %>%

    mutate(
      RMSE=dplyr::lag(sqrt(stretching_mean(SQE,window_size=10)),1,default=NA),
      MAPE=dplyr::lag(100*stretching_mean(APE,window_size=10),1,default=NA),
      MeanSA=dplyr::lag(100*(exp(stretching_mean(AlogQ,window_size=10))-1),1,default=NA)
    ) %>%
    group_by(Stock,Age,ReturnYear) %>%
    mutate(across(RMSE:MeanSA,~(1/.x) / sum(1/.x), .names="{.col}_weight"),
           deltaAICc= AICc - min(AICc),
           AICc_weight=exp(-.5*deltaAICc) / sum(exp(-.5*deltaAICc))) |>
    (\(df)
     bind_rows(df,
               group_by(df,Stock,Age,Actual,ReturnYear) %>% mutate(across(contains("_weight"),~.x*pred,.names="{.col}_pred")) |>
                 summarize_at(vars(contains("_pred")),sum) |>
                 pivot_longer(cols=RMSE_weight_pred:AICc_weight_pred,names_to = "model_name",values_to = "pred")
     )
    )() |>
    select(Stock,Age,model_name,ReturnYear,Obs=Actual,Pred=pred) |>
    arrange(Stock,Age,model_name,ReturnYear)


}
