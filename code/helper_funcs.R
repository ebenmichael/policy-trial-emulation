##########################################################################
## Estimated stacked DiD
##########################################################################

fit_event_manual <- function(outcome_var, date_var,
                       policy_var, data, max_time_to = NULL) {


  # rescaling by minimum time
  policy_time_to <- paste0(policy_var, "_time_to")
  policy_times <- data[[policy_time_to]]
  if(is.null(max_time_to)) {
    max_time_to <- max(policy_times[is.finite(policy_times)])
  }

  # keep track of policy variables
  tmpdat <- data %>%
    mutate(policy_var = data[[policy_var]],
           date_var = data[[date_var]],
           outcome = data[[outcome_var]],
           policy_var_time_to = date_var - policy_var)

  # get cohort counts
  cohort_weights <- tmpdat %>% 
    group_by(policy_var, policy_var_time_to) %>%
    count() %>%
    ungroup() %>%
    group_by(policy_var_time_to) %>%
    mutate(pct = n / sum(n)) %>%
    ungroup() %>%
    rename(cohort = policy_var,
           event_time = policy_var_time_to) %>%
    mutate(cohort = str_replace_all(cohort, "-", "_"))

  # get distinct policy times
  policy_times <- tmpdat %>% 
    distinct(policy_var) %>% 
    filter(!is.na(policy_var)) %>% 
    pull()

  # estimate 2x2 DiDs
  map_df(policy_times,
         function(x) {
            x_dat <- tmpdat %>% 
              filter(policy_var == x |
                        policy_var > x + max_time_to |
                        is.na(policy_var),
                    date_var <= x + max_time_to) %>%
              mutate(trt = coalesce(1 * (policy_var == x), 0))

            x_dat %>%
              group_by(date_var) %>%
              summarise(estimate = mean(outcome[trt == 1]) - 
                                   mean(outcome[trt == 0]),
                      event_time = max(policy_var_time_to, na.rm = T),
                      nt = sum(trt),
                      nc = sum(1 - trt)) %>%
              mutate(estimate = estimate - estimate[event_time == -1],
                     cohort = str_replace_all(x, "-", "_")) %>%
              filter(!is.na(estimate)) %>%
              ungroup() %>%
              select(-date_var)
         }) -> cohort_dids

  cohort_dids %>% 
    left_join(cohort_weights, by = c("event_time", "cohort")) %>%
    group_by(event_time) %>%
    summarise(estimate = sum(estimate * pct),
              nt = sum(nt),
              nc = sum(nc)) %>%
    ungroup() %>%
    mutate(cohort = "average") %>%
    bind_rows(cohort_dids)
}

fit_event_jack <- function(outcome_var, date_var, unit_var,
                           policy_var, data, max_time_to = NULL) {
  # get jackknife estimates
  units <- unique(data[[unit_var]])
  map_df(units,
        function(i) {
          cat(paste("Dropping", i, "\n"))
          fit_event_manual(outcome_var, date_var,
                           policy_var, data[data[[unit_var]] != i,],
                           max_time_to) %>%
            mutate(dropped = i)
        }) -> jack_ests
  # compute standard errors and join in with results
  full_res <- fit_event_manual(outcome_var, date_var, policy_var,
                               data, max_time_to)
  jack_ests %>%
    group_by(cohort, event_time) %>%
    summarise(se =  sqrt((n() - 1) / n() * 
                         sum((estimate - mean(estimate)) ^ 2 ))) %>%
    right_join(full_res)
}