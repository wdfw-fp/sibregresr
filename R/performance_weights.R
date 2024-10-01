


#' Calculated weighted average forecasts
#'
#' @param fits data frame returned by `fit_mods` function
#' @param n_years maximum number of years of predictions to include in performance metrics. Set to infinity to use a stretching window.
#'
#' @return a data frame with predictions from component models and weighted average models based on AICc, MAPE, and RMSE weights.
#'
#' @details
#' AICc weights are calculated as:
#'  $exp(-.5 deltaAICc) / sum(exp(-.5 deltaAICc))$
#' MAPE and RMSE weights are calculated as, for example:
#'  MAPE^-1 / sum(MAPE^-1)
#'
#' - *n_wts* = the number of years of forecasts and observations that were used to generate MAPE and RMSE weights for ensemble forecasts.
#' - Individual year metrics:
#'     - *Er* = forecast error
#'     - *AEr* = absolute forecast error
#'     - *PE*= percent errr,
#'     - *APE* = absolute percent error
#'     - *SQE* = square error
#'     - *logQ* = log (forecast/observed)- AlogQ = absolute logQ
#'  - Multi-year bias metrics
#'      - *MEr* = mean error
#'      - *MPE* = mean percent error
#'      - *MeanlogQ* = mean logQ
#'  - Multi-year precision metrics
#'      - *RMSE* = root mean square error
#'      - *MAer* = mean absolute error
#'      - *MAPE* = mean absolute percent error
#'      - *MeanSA* = mean absolute logQ
#'  - *log_sd* = standard deviation from observed in log space for a the last *n_years* of forecasts (i.e., (sqrt(sum(logQ^2)/n)))
#'  - *n_sd* = the number of years that were used to calculate sample standard deviation from observed
performance_weights<-function(fits,
                              n_years=15){


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
      RMSE=dplyr::lag(sqrt(stretching_mean(SQE,window_size=n_years)),1,default=NA),
      MAPE=dplyr::lag(100*stretching_mean(APE,window_size=n_years),1,default=NA),
      MeanSA=dplyr::lag(100*(exp(stretching_mean(AlogQ,window_size=n_years))-1),1,default=NA),
      n_wts= pmin(dplyr::lag(seq_along(APE),1),n_years)
    ) %>%
    group_by(Stock,Age,ReturnYear) %>%
    mutate(across(RMSE:MeanSA,~(1/.x) / sum(1/.x), .names="{.col}_weight"),
           deltaAICc= AICc - min(AICc),
           AICc_weight=exp(-.5*deltaAICc) / sum(exp(-.5*deltaAICc))) |>
    # add ensembles
    (\(df)
     bind_rows(df,
               group_by(df,Stock,Age,Actual,ReturnYear,n_wts) %>% mutate(across(contains("_weight"),~.x*pred,.names="{.col}_pred")) |>
                 summarize_at(vars(contains("_pred")),sum) |>
                 pivot_longer(cols=RMSE_weight_pred:AICc_weight_pred,names_to = "model_name",values_to = "pred")
     )
    )() |>
    # add totals across age
    select(Stock,Age,model_name,ReturnYear,Obs=Actual,Pred=pred,n_wts) |>
    arrange(Stock,Age,model_name,ReturnYear)|>
    filter(!is.na(Pred)) |>
    (\(df)
     bind_rows(
       df,
       (  group_by(df,Stock,ReturnYear,model_name) |>
            summarize(across(c(Obs,Pred,Age,n_wts),sum)))
     ))() |>
    #calculate error metrics
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
      MEr = dplyr::lag(stretching_mean(Er,window_size=n_years),1,default=NA),
      MPE = dplyr::lag(100*stretching_mean(PE,window_size=n_years),1,default=NA),
      MeanlogQ = dplyr::lag(100*(exp(stretching_mean(logQ,window_size=n_years))-1),1,default=NA),
      MAPE = dplyr::lag(100*stretching_mean(APE,window_size=n_years),1,default=NA),
      MeanSA = dplyr::lag(100*(exp(stretching_mean(AlogQ,window_size=n_years))-1),1,default=NA),
      RMSE = dplyr::lag(sqrt(stretching_mean(SQE,window_size=n_years)),1,default=NA),
      MAEr = dplyr::lag(stretching_mean(AEr,window_size=n_years),1,default=NA),
      n_sd = pmin(dplyr::lag(seq_along(logQ),1),n_years),
      log_sd = dplyr::lag(stretching_samp_sd(logQ,window_size=n_years),1) # sample standard deviation in log space
    )|>
    arrange(model_name,Stock,Age,ReturnYear) |>
    mutate(n_wts=ifelse(model_names%in%c("MAPE_weight_pred" ,"MeanSA_weight_pred", "RMSE_weight_pred" ),n_wts,NA))
}
