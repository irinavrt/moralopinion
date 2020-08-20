#' transforms variable to binary with 1 indicating agreement to the default position and 0 - disagreement
#' if relevant, the neutral middle category is omited
#' @var varible to transform

dichotomize <- function(var){
  # for already binary items recode "yes" (1st level) to 1 and "no" to 0
  if(nlevels(var) == 2){
    return(2 - as.numeric(var))
  }
  # if item has even number of levels recode first half of levels as 1, and second as 0
  if(nlevels(var)%%2 == 0){
    return(ifelse(as.numeric(var) > nlevels(var)/2, 0, 1))
  } else {
    # if item has odd number of levels recode the middle level to NA, first half of levels as 1, and second as 0
    middle <- ceiling(nlevels(var)/2)
    return(case_when(
      as.numeric(var) == middle ~ NA_real_,
      as.numeric(var) < middle ~ 1,
      as.numeric(var) > middle ~ 0,
      TRUE ~ NA_real_)
    )
  }
}

#' describe test results
#' @test_object output of either proportional, chi-squared, or correlation test

format_test <- function(test_object){
  if(class(test_object) != "htest") stop("not a test object") 
  
  if(test_object$method == "1-sample proportions test with continuity correction"){
    pr <- tidy(test_object)
     return(sprintf("%.2f, 95%% CI [%.2f, %.2f]", pr$estimate, pr$conf.low, pr$conf.high))
  }
  if(test_object$method == "Pearson's product-moment correlation"){
    pr <- tidy(test_object)
    p_value <- ifelse(pr$p.value < 0.001, "< 0.001", sprintf("= %.2f", pr$p.value))
    return(sprintf("*r* (%.0f) = %.2f, 95%% CI [%.2f, %.2f], p %s", 
            pr$parameter, pr$estimate, pr$conf.low, pr$conf.high,  p_value))
  }
  if(test_object$method == "Pearson's Chi-squared test with Yates' continuity correction"){
    pr <- tidy(test_object)
    p_value <- ifelse(pr$p.value < 0.001, "< 0.001", sprintf("= %.2f", pr$p.value))
    return(sprintf("$\\chi^2$(%.0f) = %.1f, p %s", pr$parameter, pr$statistic, p_value))
  }
}

#' describe model results
#' @model output of lm or lmer function
#' @coef_name variable name which effect to report

format_coef <- function(model, coef_name){
  if(class(model) != "lm") stop("not a model object") 
  ef <- tidy(model, conf.int = TRUE) %>% filter(term == coef_name)
  p_value <- ifelse(ef$p.value < 0.001, "< 0.001", sprintf("= %.2f", ef$p.value))
  return(sprintf("$\\beta$ = %.2f, 95%% CI [%.2f, %.2f], t(%.0f) = %.1f, p %s", 
                 ef$estimate, ef$conf.low, ef$conf.high, model$df.residual, ef$statistic, p_value))
}


