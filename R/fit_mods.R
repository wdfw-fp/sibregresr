#' Prep data for each Stock/Age/n_years and  fit the models
#'
#' @param dat tible returned from `setup_data` function
#'
#'@details
#' Can take several seconds as it is conducting maximum likeihood optimization for many models (n mdel * n stocks * n forecasts)
#'
#'
#' @return tible
#'
#' @examples
fit_mods<-function(dat){

fits<-  dat %>%
  mutate(xy_og=purrr::map2(data,Age, ~.x %>% select(BroodYear, y=paste0("Age",.y), x=paste0("Age", .y-1)) %>% filter(!is.na(x))),
         ## For the retrospective fits, replace the last y with NA to get forecasts
         xy_dat=purrr::map2(xy_og,model_name,~.x %>% mutate(y=if_else(BroodYear==max(BroodYear),NA_real_,log(y)),
                                                     mod_name=.y,
                                                     x=if_else(str_detect(mod_name,"IntOnly"),1,log(x)))),
         # Debug=FALSE runs the optimization in C++ so its faster.
         MLE=purrr::map2(xy_dat,model, safely(~dlmMLE(y=.x$y, x.mat=cbind(.x$x),parm=rep(0,3),build=.y,hessian=FALSE,debug=FALSE))),
         estimates=map(MLE,"result"),
         error=map(MLE,"error"))



# Models that failed, none fail with full data sets but some retrospective fits were failing
failed_fits<-fits %>%
  filter(map_lgl(error, ~!is.null(.x))) %>%
  mutate(error=map_chr(error,as.character))
  #select(Stock, Age, n_years, model_name, error)

if(nrow(failed_fits)>0){
warning("There are ",nrow(failed_fits), " models that failed to fit. You can extract the failed models and the error messages from the returned object (e.g., fit) with \n
        fits %>%
  filter(map_lgl(error, ~!is.null(.x))) %>%
  mutate(error=map_chr(error,as.character))
        ")
print(head(failed_fits))
}

fits
}
