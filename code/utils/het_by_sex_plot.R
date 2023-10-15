# Function to make het plot
het_by_sex_plot = function(data, out_var){
    dat_pruned_f = data %>% filter(female == 1)
    dat_pruned_m = data %>% filter(female == 0)

    pf = rdplot(x = dat_pruned_f$winmargin_loc, y = dat_pruned_f[[out_var]], p = 1)
    pm = rdplot(x = dat_pruned_m$winmargin_loc, y = dat_pruned_m[[out_var]], p = 1)

    bin_fem = pf$vars_bins %>% mutate(sex = "female")
    bin_men = pm$vars_bins %>% mutate(sex = "male")
    pol_fem = pf$vars_poly %>% mutate(sex = "female")
    pol_men = pm$vars_poly %>% mutate(sex = "male")

    bin_dat = rbind(bin_fem, bin_men)
    pol_dat = rbind(pol_fem, pol_men) %>%
        mutate(below = rdplot_x < 0) %>%
        filter(rdplot_x != 0)

    return(list(bin_dat, pol_dat))
}
