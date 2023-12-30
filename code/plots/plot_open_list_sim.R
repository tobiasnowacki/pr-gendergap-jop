library(tidyverse)

# Generate quality for 2 incumbents and 6 non-incumbents
f <- 8 # no. candidates
k <- 4 # no. elected

b_i <- -0.05 # party bias vs. women
v_i <- 0.1 # party bias pro incumbents

p <- 0.6 # noisy quality voters
r <- 0.25 # heuristic unbiased voters
q <- 1 - p - r # heuristic biased voters

# Write this up and send to Gary + Sole.
# Think about the right visualisation.

# Function to generate election results
return_election_results <- function(theta, inc, female,
                                    b_i = -0.1, v_i = 0.1,
                                    p = 0.5, r = 0.3) {
  # set up remaining fraction
  q <- 1 - p - r

  # set up party list rank
  theta_hat <- theta + female * b_i + inc * v_i + rnorm(8, 0, 0.05)
  list_rank <- rank(-theta_hat)

  # Who are p voters voting for?
  theta_tilde <- theta + rnorm(8, 0, 0.1)
  p_rank <- rank(-theta_tilde)
  p_vote <- as.numeric(p_rank <= 4)

  # Who are r voters voting for?
  r_rank <- inc
  list_rank_ninc <- rank(-theta_hat[!as.logical(inc)])
  r_ninc_vote <- as.numeric(list_rank_ninc <= 2)
  r_vote <- r_rank
  r_vote[!as.logical(inc)][as.logical(r_ninc_vote)] <- 1

  # Who are q voters voting for?
  q_rank <- inc & !female
  q_list_rank_ninc <- rank(-theta_hat[!as.logical(inc) & !female])
  q_left <- 4 - sum(q_rank)
  q_ninc_vote <- as.numeric(q_list_rank_ninc <= q_left)
  q_vote <- q_rank
  q_vote[!as.logical(inc) & !female][as.logical(q_ninc_vote)] <- 1

  # Calculate vote shares for all candidates
  pref_vote_shares <- p * p_vote + r * r_vote + q * q_vote
  share_rank <- rank(-pref_vote_shares)
  elected_t0 <- share_rank <= 4

  # Determine who is running again
  running_t1 <- rbinom(8, 1, 0.8 * elected_t0 + 0.4 * (1 - elected_t0))
  running_t1_n <- sum(running_t1)
  running_t1_f <- sum(running_t1[female == 1], na.rm = TRUE)
  running_t1_m <- sum(running_t1[female == 0], na.rm = TRUE)

  # Return list object
  out <- list(
    theta = theta,
    incumbent = inc,
    female = female,
    shares = pref_vote_shares,
    rank = share_rank,
    elected = elected_t0,
    running_again = running_t1,
    n_running_again = running_t1_n,
    running_again_f = running_t1_f,
    running_again_m = running_t1_m,
    inc_ratio = c(
      mean(elected_t0[inc == 1 & female == 0]),
      mean(elected_t0[inc == 1 & female == 1]),
      mean(elected_t0[inc == 0 & female == 0]),
      mean(elected_t0[inc == 0 & female == 1])
    )
  )
  return(out)
}


run_both_rounds <- function(b = -0.1, v = 0.1, p = 0.3, r = 0.4) {
  # Draw initial folks
  theta <- c(runif(2, 0.5, 1), runif(6, 0, 1))
  inc <- c(1, 1, rep(0, 6))
  female <- c(0, 1, rbinom(6, 1, 0.5))

  # Run first round
  first_round <- return_election_results(theta, inc, female, b, v, p, r)
  theta_0 <- first_round$theta
  female_0 <- first_round$female
  running_t1 <- as.logical(first_round$running_again)
  t1_incs <- first_round$n_running_again

  theta_0_update <- theta_0[running_t1]
  theta_0_update[as.logical(first_round$elected)[running_t1]] <- theta_0_update[as.logical(first_round$elected)[running_t1]] + 0.05

  # Set up new candidates
  theta_1 <- c(theta_0_update, runif(f - t1_incs, 0, 1))
  inc_1 <- c(first_round$elected[running_t1], rep(0, f - t1_incs))
  female_1 <- c(female_0[running_t1], rbinom(f - t1_incs, 1, 0.5))

  female_win_not_run <- sum(first_round$elected[!running_t1 & as.logical(female)], na.rm = TRUE)
  # print(female_win_not_run)
  male_win_not_run <- sum(first_round$elected[!running_t1 & !female], na.rm = TRUE)

  second_round <- return_election_results(theta_1, inc_1, female_1, b, v, p, r)
  # Vector of first-election losers who run again
  female_run <- female_0[running_t1] & !first_round$elected[running_t1]
  male_run <- !female_0[running_t1] & !first_round$elected[running_t1]
  second_round$share_win_m <- mean(
    c(
      second_round$elected[1:t1_incs][male_run],
      rep(0, sum(!first_round$female) - first_round$running_again_m)
    ),
    na.rm = TRUE
  )
  second_round$share_win_f <- mean(
    c(
      second_round$elected[1:t1_incs][female_run],
      rep(0, sum(first_round$female) - first_round$running_again_f)
      ),
    na.rm = TRUE
  )
  # second_round$test <- second_round$elected[1:t1_incs][as.logical(second_round$incumbent)[1:t1_incs] & as.logical(!second_round$female)[1:t1_incs]]
  second_round$share_win_inc_m <- mean(
    c(
      second_round$elected[1:t1_incs][as.logical(second_round$incumbent)[1:t1_incs] & as.logical(!second_round$female)[1:t1_incs]],
      rep(0, male_win_not_run)
    ),
    na.rm = TRUE
  )
  second_round$share_win_inc_f <- mean(
    c(
      second_round$elected[1:t1_incs][as.logical(second_round$incumbent[1:t1_incs]) & as.logical(second_round$female[1:t1_incs])],
      rep(0, female_win_not_run)
    ),
    na.rm = TRUE
  )
  return(second_round)
}

run_simulation <- function(
  n = 200, m = 200, b = -0.1, v = 0.1, p = 0.3, r = 0.4,
  simname = "Specify a name"
  ) {

  # Run simulation
  sim_out <- replicate(n, {
    replicate(m,
      {
        obj <- run_both_rounds(b, v, p, r)
        c(
          obj$share_win_inc_m,
          obj$share_win_inc_f,
          obj$inc_ratio[3:4],
          obj$share_win_m,
          obj$share_win_f
        )
      },
      simplify = "matrix"
    ) %>%
      t() %>%
      colMeans(na.rm = TRUE)
  })

  # Tidy dataframe
  sim_out <- sim_out %>%
    t() %>%
    as.data.frame() %>%
    pivot_longer(V1:V6, names_to = "stat") %>%
    mutate(
      gender = case_when(
        stat %in% c("V1", "V3", "V5") ~ "male",
        TRUE ~ "female"
      ),
      type = case_when(
        stat %in% c("V1", "V2") ~ "Pr(Win in t1 | Won in t0)",
        stat %in% c("V3", "V4") ~ "Pr(Win in t1 | Lost or didn't run in t0)",
        stat %in% c("V5", "V6") ~ "Pr(Win in t1 | Lost in t0)"
      ) %>% as.factor(),
      scenario = simname
    )

  # Relevel factor for later plotting
  sim_out$type <- fct_relevel(
    sim_out$type,
    c(
      "Pr(Win in t1 | Won in t0)",
      "Pr(Win in t1 | Lost in t0)",
      "Pr(Win in t1 | Lost or didn't run in t0)"
    )
  )

  return(sim_out)
}

# Plot simulation distribution
plot_distributions <- function(plot_obj, title){

  ggplot(plot_obj, aes(x = value)) +
    geom_histogram(aes(fill = gender), alpha = 0.2, position = "identity") +
    facet_wrap(~type, scales = "free") +
    labs(x = "Proportion", y = "Outcome", fill = "Gender", title = title) +
    theme_bw()
}
# Scenarios
# . 1. only quality voters, no bias
#   2. only quality voters, bias
#   3. only heuristic voters, no bias
#   4. only heuristic voters, bias
#   5. quality + heuristic voters, no bias
#   6. quality + heuristic voters, no bias
#   7. all voters, no bias
#   8. all voters, bias

scenario1 <- run_simulation(
  n = 100, m = 200, b = 0.0, p = 1, r = 0,
  simname = "Quality-observing voters, no party bias"
)

scenario2 <- run_simulation(n = 100, m = 200, b = -0.1, p = 1, r = 0, simname = "Quality-observing voters, party bias")

scenario3 <- run_simulation(n = 100, m = 200, b = -0.0, p = 0, r = 1, simname = "Heuristic voters, no party bias")

scenario4 <- run_simulation(n = 100, m = 200, b = -0.1, p = 0, r = 1, simname = "Heuristic voters, party bias")

scenario5 <- run_simulation(n = 100, m = 200, b = -0.0, p = 0.3, r = 0.7, simname = "Quality and Heuristic voters, no party bias")

scenario6 <- run_simulation(n = 100, m = 200, b = -0.1, p = 0.3, r = 0.7, simname = "Quality and Heuristic voters, party bias")

scenario7 <- run_simulation(n = 100, m = 200, b = -0.0, p = 0.45, r = 0.25)
# plot_distributions(scenario7, "Scenario 7: All, Without Party Bias")

# If r > p (and a sufficiently large q), then we start seeing the result supported by empirics!
scenario7a <- run_simulation(n = 100, m = 200, b = -0.0, p = 0.3, r = 0.4, simname = "All voters, no party bias")

scenario8 <- run_simulation(n = 100, m = 200, b = -0.1, p = 0.5, r = 0.25)
# plot_distributions(scenario7, "Scenario 8: All, With Party Bias")
scenario8a <- run_simulation(n = 100, m = 200, b = -0.1, p = 0.3, r = 0.4, simname = "All voters, party bias")

statdf <- rbind(
  scenario1, scenario2, scenario3,
  scenario4, scenario5, scenario6,
  scenario7a, scenario8a
) |>
  group_by(gender, type, scenario) |>
  summarise(
    mean = mean(value),
    q10 = quantile(value, 0.1),
    q90 = quantile(value, 0.9)
  ) |>
  mutate(
    bias = grepl(", party", scenario) |> as.factor(),
    bias = ifelse(
      bias == TRUE,
      "Party bias = -0.1", "No party bias"
    ),
    upper_scenario = str_match(scenario, "^([^,])+")[, 1],
    type = case_when(
      type == "Pr(Win in t1 | Won in t0)" ~ "Incumbent",
      type == "Pr(Win in t1 | Lost in t0)" ~ "Prev. Loser",
      type == "Pr(Win in t1 | Lost or didn't run in t0)" ~ "Non-incumb."
    )
  )

statdf$upper_scenario <- fct_relevel(statdf$upper_scenario, c("Quality-observing voters", "Heuristic voters", "Quality and Heuristic voters", "All voters"))
statdf$type <- fct_relevel(statdf$type, c("Incumbent", "Prev. Loser", "Non-incumb."))

ggplot(statdf, aes(x = type)) +
  geom_point(aes(y = mean, colour = gender, alpha = type, shape = type),
    position = position_dodge2(width = 0.5)
  ) +
  geom_linerange(aes(ymin = q10, ymax = q90, colour = gender, alpha = type), position = position_dodge2(width = 0.5)) +
  facet_grid(bias ~ upper_scenario) +
  scale_colour_viridis_d(end = 0.7) +
  scale_alpha_manual(values = c(1, 1, 0.4)) +
  theme_bw() +
  labs(y = "Pr(Win in t = 1)", x = "Scenario") +
  theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1))
ggsave("output/figures/theory_sim.pdf", width = 7, height = 4)
