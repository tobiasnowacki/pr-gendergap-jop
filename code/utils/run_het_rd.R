# Write function to get optimal bandwidth
run_het_rd = function(data, outcome, running, x_formula, poly = 1, bw_fac = c(1, 2, 0.5), bw_override = NULL, w = FALSE){

  # subset to available outcomes
  data = data[!is.na(data[[outcome]]), ]

  # Wrapper fn for estimation
  estimate_het_rd = function(data, formula, bw, running, w){
    data = data %>% filter(abs(winmargin_loc) < bw)

    if (w == TRUE){
      data = weighting(data, 1, bw)
    } else {
      data$wght = 1 / nrow(data)
    }

    # print(data %>% head)

    out_list = list()
    out_list$out = felm(formula, data, exactDOF = TRUE, weights = data$wght)
    out_list$n_left = nrow(data %>% filter(winmargin_loc < 0))
    out_list$n_right = nrow(data %>% filter(winmargin_loc > 0))
    out_list$out_mean = mean(data[[outcome]], na.rm = TRUE)
    out_list$bw = bw
    return(out_list)
  }

  # Grab formula
  f = paste0(outcome, " ~ ", x_formula) %>% as.formula

  # Identify optimal bandwidth on whole data
  if(is.null(bw_override)){
      target_bw = rdbwselect(y = data[[outcome]],
             x = data[[running]],
             p = poly)$bws[1]
  bw_vec = bw_fac * target_bw

  cat(bw_vec)
  }
  if(!is.null(bw_override)){
    bw_vec = bw_override
  }

  # Run polynomial regression w/ FE
  mod_list = map(bw_vec, ~ estimate_het_rd(data, f, .x, running, w = w))

  return(mod_list)
}
