# function censor creates censored observations and discrete response times (drt), given a user-defined censoring time (timeout) and bin width in ms
censor <- function(df, timeout, bin_width){
  if(!(timeout %% bin_width == 0)){
    return("The censoring time must be a multiple of the bin width!")
  }
  if(timeout < 0 | bin_width < 0){
    return("Both timeout and bin_width must be larger than 0!")
  }
  df %>% mutate(right_censored = 0,
                rtc = ifelse(rt > timeout, timeout, rt) %>% round(digits=2), # censored response times
                right_censored = ifelse(rtc == timeout,1,right_censored), # 1 = right censored observation, 0 = observed rt
                drt = ceiling(rtc/bin_width), # drt = discrete RT or time bin rank
                cens_time = timeout, bin_width = bin_width) # save both user-defined parameters for plotting
}

# function ptb creates a person-trial-bin oriented data set
ptb <- function(df){
  df %>% uncount(weights = drt) %>% # create drt rows per trial
    group_by(trial) %>% 
    mutate(period = 1:n()) %>% # create time bin (or time period) ranks within each trial
    mutate(event = if_else(period == max(period) & right_censored == 0, 1, 0)) %>% # event = 1 indicates response occurrence
    ungroup()
}

# function setup_lt sets up a life table for each level of condition (1 independent variable)
setup_lt <- function(ptb){
  ptb %>% mutate(event = str_c("event", event)) %>%
    group_by(condition,period) %>% 
    count(event) %>% 
    ungroup() %>% 
    pivot_wider(names_from = event,
                values_from = n) %>% 
    mutate(event0 = ifelse(is.na(event0),0,event0), # replace NA with 0
           event1 = ifelse(is.na(event1),0,event1),
           risk_set = event0 + event1) %>% # define the risk set
    mutate(hazard = (event1 / risk_set) %>% round(digits = 3)) %>% # calculate hazard estimate
    mutate(se_haz = sqrt((hazard * (1 - hazard)) / risk_set) %>% round(digits = 4), # standard error for hazard
           pmass = event1/max(risk_set),
           se_pmass = sqrt((pmass * (1 - pmass)) / max(risk_set))) %>% 
    group_by(condition) %>%
    mutate(survival = (cumprod(1-hazard)) %>% round(digits = 4), # calculate survival estimate
           term     = (cumsum(hazard / (risk_set * (1 - hazard)))) %>% round(digits = 7), # intermediate calculation
           se_surv  = (survival * sqrt(term)) %>% round(digits = 5)  ) %>% # Greenwood's (1926) approximation
    ungroup() 
}

# function calc_ca calculates the conditional accuracies 
calc_ca <- function(df){
  df %>% filter(right_censored==0) %>%
    group_by(condition,drt,cens_time,bin_width) %>%
    summarize(ca = mean(acc) %>% round(digits = 2),
              n = n(),
              .groups = 'drop') %>%
    ungroup() %>%
    mutate(period = drt,
           se_ca = sqrt((ca * (1-ca)) / n) %>% round(digits = 3)) %>%
    select(-drt)
}

# function join_lt_ca joins the conditional accuracies to the life tables
join_lt_ca <- function(df1,df2){df1 %>% left_join(df2, join_by(condition,period))}

# function extract_median is used to extract the S(t).50 quantiles (i.e., the estimated median RTs) when plotting the data
extract_median <- function(df){
  above_pct50 <- df %>% 
    group_by(condition) %>%
    filter(survival >= .5) %>% 
    slice(n()) # take last row
  below_pct50 <- df %>% 
    group_by(condition) %>%
    filter(survival < .5) %>% 
    slice(1) # take first row
  # pull period above
  period_above <- pull(above_pct50, period)
  # pull survivor function values
  survival_above <- pull(above_pct50, survival)
  survival_below <- pull(below_pct50, survival)
  # estimate median by interpolation
  median_period <- period_above+((survival_above-.5)/(survival_above-survival_below))*((period_above+1)-period_above)
}

# function plot_eha plots the hazard, survivor, and ca(t) functions, when there is one independent variable
plot_eha <- function(df,subj,haz_yaxis){ # set the upper limit of the y-axis for the hazard functions to haz_yaxis == 1 when plotting the data of individual subjects (see line 244)
  library(patchwork)
  cutoff <- df %>% pull(cens_time) %>% max(na.rm=T)
  binsize <- df %>% pull(bin_width) %>% max(na.rm=T)
  median_period <- extract_median(df)
  n_conditions <- nlevels(df$condition)
  data_median <- c()
  for(i in 1:n_conditions){
    data_median <- append(data_median, c(median_period[i], median_period[i]))
  }
  
  data_medians <- tibble(period= data_median,
                         survival = rep(c(.5, 0),n_conditions),
                         condition = rep(1:n_conditions, each=2))
  # plot the hazard functions
  p1 <- df %>% ggplot(aes(x=period, color=condition, group=condition)) +
    geom_line(aes(y=hazard)) +
    geom_point(aes(y=hazard), size=1) + labs(color="Condition") +
    geom_linerange(aes(ymin=hazard-se_haz, ymax=hazard+se_haz), show.legend = F) +
    scale_x_continuous(breaks = c(0,1:(cutoff/binsize)), labels=c(0,1:(cutoff/binsize)*binsize),
                       limits = c(0,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,haz_yaxis)) +
    labs(x="Time bin t's endpoint (ms)", y="h(t)", title = paste("Subject ", subj)) +
    theme(legend.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle=90))
  # plot the survivor functions
  p2 <-df %>%
    ggplot(aes(x=period, color=condition, group=condition)) +
    geom_line(aes(y=survival), show.legend = F) +
    geom_point(aes(y=survival), size=1, show.legend = F) +
    geom_linerange(aes(ymin=survival-se_surv, ymax=survival+se_surv), show.legend = F) +
    # add vertical lines at the median RTs in the plot of the survivor functions using geom_path(). 
    # Make sure you apply the same levels and labels for the factor condition as in Tutorial 1a. 
    geom_path(aes(x=period, y=survival, color=factor(condition, levels =c(1,2,3),labels=c("blank","congruent","incongruent"))),
              data = data_medians, 
              linetype = 3, show.legend = F) +
    
    scale_x_continuous(breaks = c(0,1:(cutoff/binsize)), labels=c(0,1:(cutoff/binsize)*binsize),
                       limits=c(0,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x="Time bin t's endpoint (ms)", y="S(t)",
         colour="Condition") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle=90))
  # plot the conditional accuracy functions
  p3 <-df %>%
    ggplot(aes(x=period, color=condition, group=condition)) +
    geom_line(aes(y=ca), show.legend = F) +
    geom_point(aes(y=ca), size=1, show.legend = F) +
    geom_linerange(aes(ymin=ca-se_ca, ymax=ca+se_ca), show.legend = F) +
    scale_x_continuous(breaks = c(0,1:(cutoff/binsize)), labels=c(0,1:(cutoff/binsize)*binsize),
                       limits=c(0,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x="Time bin t's endpoint (ms)", y="ca(t)",
         colour="Condition") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle=90))
  # plot the probability mass functions
  p4 <-df %>%
    ggplot(aes(x=period, color=condition, group=condition)) +
    geom_line(aes(y=pmass), show.legend = F) +
    geom_point(aes(y=pmass), size=1, show.legend = F) +
    geom_linerange(aes(ymin=pmass-se_pmass, ymax=pmass+se_pmass), show.legend = F) +
    scale_x_continuous(breaks = c(0,1:(cutoff/binsize)), labels=c(0,1:(cutoff/binsize)*binsize),
                       limits=c(0,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,.5)) +
    labs(x="Time bin t's endpoint (ms)", y="P(t)",
         colour="Condition") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle=90))
  
  
  (p1|p3) / (p2|p4)
}

# function plot_eha_agg plots the hazard, survivor, and ca(t) functions with a label for 
# aggregated data.
plot_eha_agg <- function(df,subj,haz_yaxis){ 
  library(patchwork)
  cutoff <- df %>% pull(cens_time) %>% max(na.rm=T)
  binsize <- df %>% pull(bin_width) %>% max(na.rm=T)
  median_period <- extract_median(df)
  n_conditions <- nlevels(df$condition)
  data_median <- c()
  for(i in 1:n_conditions){
    data_median <- append(data_median, c(median_period[i], median_period[i]))
  }
  
  data_medians <- tibble(period= data_median,
                         survival = rep(c(.5, 0),n_conditions),
                         condition = rep(1:n_conditions, each=2))
  # plot the hazard functions
  p1 <- df %>% ggplot(aes(x=period, color=condition, group=condition)) +
    geom_line(aes(y=hazard)) +
    geom_point(aes(y=hazard), size=1) + labs(color="Condition") +
    geom_linerange(aes(ymin=hazard-se_haz, ymax=hazard+se_haz), show.legend = F) +
    scale_x_continuous(breaks = c(0,1:(cutoff/binsize)), labels=c(0,1:(cutoff/binsize)*binsize),
                       limits = c(0,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,haz_yaxis)) +
    labs(x="Time bin t's endpoint (ms)", y="h(t)", title = "Aggregated data (N=6)") +           # New info in title
    theme(legend.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle=90))
  # plot the survivor functions
  p2 <-df %>%
    ggplot(aes(x=period, color=condition, group=condition)) +
    geom_line(aes(y=survival), show.legend = F) +
    geom_point(aes(y=survival), size=1, show.legend = F) +
    geom_linerange(aes(ymin=survival-se_surv, ymax=survival+se_surv), show.legend = F) +
    geom_path(aes(x=period, y=survival, color=factor(condition, levels =c(1,2,3),labels=c("blank","congruent","incongruent"))),
              data = data_medians, 
              linetype = 3, show.legend = F) +
    
    scale_x_continuous(breaks = c(0,1:(cutoff/binsize)), labels=c(0,1:(cutoff/binsize)*binsize),
                       limits=c(0,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x="Time bin t's endpoint (ms)", y="S(t)",
         colour="Condition") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle=90))
  # plot the conditional accuracy functions
  p3 <-df %>%
    ggplot(aes(x=period, color=condition, group=condition)) +
    geom_line(aes(y=ca), show.legend = F) +
    geom_point(aes(y=ca), size=1, show.legend = F) +
    geom_linerange(aes(ymin=ca-se_ca, ymax=ca+se_ca), show.legend = F) +
    scale_x_continuous(breaks = c(0,1:(cutoff/binsize)), labels=c(0,1:(cutoff/binsize)*binsize),
                       limits=c(0,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x="Time bin t's endpoint (ms)", y="ca(t)",
         colour="Condition") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle=90))
  # plot the probability mass functions
  p4 <-df %>%
    ggplot(aes(x=period, color=condition, group=condition)) +
    geom_line(aes(y=pmass), show.legend = F) +
    geom_point(aes(y=pmass), size=1, show.legend = F) +
    geom_linerange(aes(ymin=pmass-se_pmass, ymax=pmass+se_pmass), show.legend = F) +
    scale_x_continuous(breaks = c(0,1:(cutoff/binsize)), labels=c(0,1:(cutoff/binsize)*binsize),
                       limits=c(0,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,.5)) +
    labs(x="Time bin t's endpoint (ms)", y="P(t)",
         colour="Condition") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle=90))
  
  (p1|p3) / (p2|p4)
}


# function setup_lt_2IV sets up life tables for 2 independent variables
setup_lt_2IV <- function(ptb){
  ptb %>% mutate(event = str_c("event", event)) %>%
    group_by(condition1,condition2,period) %>% 
    count(event) %>% 
    ungroup() %>% 
    pivot_wider(names_from = event,
                values_from = n) %>% 
    mutate(event0 = ifelse(is.na(event0),0,event0), # replace NA with 0
           event1 = ifelse(is.na(event1),0,event1),
           risk_set = event0 + event1) %>% # define the risk set
    mutate(hazard = (event1 / risk_set) %>% round(digits = 3)) %>% # calculate hazard estimate
    mutate(se_haz = sqrt((hazard * (1 - hazard)) / risk_set) %>% round(digits = 4),
           pmass = event1/max(risk_set),
           se_pmass = sqrt((pmass * (1 - pmass)) / max(risk_set))) %>% 
    group_by(condition1,condition2) %>%
    mutate(survival = (cumprod(1-hazard)) %>% round(digits = 4), # calculate survival estimate
           term     = (cumsum(hazard / (risk_set * (1 - hazard)))) %>% round(digits = 7), # intermediate calculation
           se_surv  = (survival * sqrt(term)) %>% round(digits = 5)  ) %>% # Greenwood's (1926) approximation
    ungroup() 
}

# function calc_ca_2IV calculates the conditional accuracies for 2 IVs
calc_ca_2IV <- function(df){
  df %>% filter(right_censored==0) %>%
    group_by(condition1,condition2,drt,cens_time,bin_width) %>%
    summarize(ca = mean(acc) %>% round(digits = 2),
              n = n(),
              .groups = 'drop') %>%
    ungroup() %>%
    mutate(period = drt,
           se_ca = sqrt((ca * (1-ca)) / n) %>% round(digits = 3)) %>%
    select(-drt)
}

# function join_lt_ca_2IV joins the conditional accuracies to the life tables when there are two independent variables
join_lt_ca_2IV <- function(df1,df2){df1 %>% left_join(df2, join_by(condition1,condition2,period))}

# function extract_median_2IV is used to extract the S(t).50 quantiles when there are two independent variables
extract_median_2IV <- function(df){
  above_pct50 <- df %>% 
    group_by(condition1,condition2) %>%
    filter(survival >= .5) %>% 
    slice(n()) # take last row
  below_pct50 <- df %>% 
    group_by(condition1,condition2) %>%
    filter(survival < .5) %>% 
    slice(1) # take first row
  # pull period above
  period_above <- pull(above_pct50, period)
  # pull survivor function values
  survival_above <- pull(above_pct50, survival)
  survival_below <- pull(below_pct50, survival)
  # estimate median by interpolation
  median_period <- period_above+((survival_above-.5)/(survival_above-survival_below))*((period_above+1)-period_above)
}

# function plot_eha_2IV plots the hazard, survivor, and ca(t) functions, when there are two independent variables
plot_eha_2IV <- function(df,subj,haz_yaxis){ 
  library(patchwork)
  cutoff <- df %>% pull(cens_time) %>% max(na.rm=T)
  binsize <- df %>% pull(bin_width) %>% max(na.rm=T)
  median_period <- extract_median_2IV(df)
  n_iv1 <- nlevels(df$condition1) 
  n_iv2 <- nlevels(df$condition2) 
  n_conditions <- n_iv1*n_iv2
  data_median <- c()
  for(i in 1:n_conditions){
    data_median <- append(data_median, c(median_period[i], median_period[i]))
  }
  # Create correctly named factors (here: prime and mask) and specify the same levels and labels as above
  data_medians <- tibble(period= data_median,
                         survival = rep(c(.5, 0),n_conditions),
                         prime =  factor(rep(rep(1:n_iv2, each=2),n_iv1),levels=c(1:3),labels=c("blank","congruent","incongruent")),
                         mask = factor(rep(1:n_iv1, each=2*n_iv2),levels=c(1:4),labels=c("nomask", "relevant", "irrelevant","lines")))
  
  # plot the hazard functions
  p1 <-df %>% 
    mutate(mask = condition1,       # Create correctly named factor
           prime = condition2) %>%  # Create correctly named factor
    ggplot(aes(x=period, color=prime, group=prime)) + # Apply correctly named factor
    geom_line(aes(y=hazard)) +
    geom_point(aes(y=hazard), size=1) + labs(color="Prime") +
    geom_linerange(aes(ymin=hazard-se_haz, ymax=hazard+se_haz), show.legend = F) +
    scale_x_continuous(breaks = c(1:(cutoff/binsize)), labels=c(1:(cutoff/binsize)*binsize),
                       limits = c(1,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x="Time bin t's endpoint (ms)", y="h(t)", title = paste("Subject ", subj)) +
    theme(legend.background = element_rect(fill = "transparent"),
          panel.grid = element_blank(),
          legend.position = "top",
          axis.text.x = element_text(angle=90)) +
    facet_wrap(~mask,nrow=1,ncol=4) # Apply correctly named factor  
  
  # plot the survivor functions
  p2 <-df %>%
    mutate(mask = condition1,      # Create correctly named factor
           prime = condition2) %>% # Create correctly named factor
    ggplot(aes(x=period, color=prime, group=prime)) + # Apply correctly named factor
    geom_line(aes(y=survival), show.legend = F) +
    geom_point(aes(y=survival), size=1, show.legend = F) +
    geom_linerange(aes(ymin=survival-se_surv, ymax=survival+se_surv), show.legend = F) +
    # add vertical lines to indicate the estimated median RTs with geom_path()
    geom_path(aes(x=period, y=survival),
              data = data_medians, 
              linetype = 3, show.legend = F) +
    scale_x_continuous(breaks = c(1:(cutoff/binsize)), labels=c(1:(cutoff/binsize)*binsize),
                       limits=c(1,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x="Time bin t's endpoint (ms)", y="S(t)") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle=90)) +
    facet_wrap(~mask,nrow=1,ncol=4) # Apply correctly named factor
  
  
  # plot the conditional accuracy functions
  p3 <-df %>%
    mutate(mask = condition1,       # Create correctly named factor
           prime = condition2) %>%  # Create correctly named factor
    ggplot(aes(x=period, color=prime, group=prime)) + # Apply correctly named factor
    geom_line(aes(y=ca), show.legend = F) +
    geom_point(aes(y=ca), size=1, show.legend = F) +
    geom_linerange(aes(ymin=ca-se_ca, ymax=ca+se_ca), show.legend = F) +
    scale_x_continuous(breaks = c(1:(cutoff/binsize)), labels=c(1:(cutoff/binsize)*binsize),
                       limits=c(1,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x="Time bin t's endpoint (ms)", y="ca(t)") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle=90)) +
    facet_wrap(~mask,nrow=1,ncol=4) # Apply correctly named factor
  # plot the probability mass functions
  p4 <-df %>%
    mutate(mask = condition1,       # Create correctly named factor
           prime = condition2) %>%  # Create correctly named factor
    ggplot(aes(x=period, color=prime, group=prime)) + # Apply correctly named factor
    geom_line(aes(y=pmass), show.legend = F) +
    geom_point(aes(y=pmass), size=1, show.legend = F) +
    geom_linerange(aes(ymin=pmass-se_pmass, ymax=pmass+se_pmass), show.legend = F) +
    scale_x_continuous(breaks = c(1:(cutoff/binsize)), labels=c(1:(cutoff/binsize)*binsize),
                       limits=c(1,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,.5)) +
    labs(x="Time bin t's endpoint (ms)", y="P(t)") +
    theme(panel.grid = element_blank(),
          axis.text.x = element_text(angle=90)) +
    facet_wrap(~mask,nrow=1,ncol=4) # Apply correctly named factor
  
  p1/p2/p3/p4
}
