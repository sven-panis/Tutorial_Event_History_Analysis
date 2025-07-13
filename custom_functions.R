# function censor creates censored observations and discrete response times (drt), 
# given a user-defined censoring time (timeout) and bin width in ms
censor <- function(df, timeout, bin_width){

  if(!(timeout %% bin_width == 0)){
    return("The censoring time must be a multiple of the bin width!")
  }
  if(timeout < 0 | bin_width < 0){
    return("Both timeout and bin_width must be larger than 0!")
  }
  df %>% mutate(right_censored = 0,
                # censored response times
                rtc = ifelse(rt > timeout, timeout, rt) %>% round(digits=2),
                # 1 = right censored observation, 0 = observed rt
                right_censored = ifelse(rtc == timeout,1,right_censored), 
                # drt = discrete RT or time bin rank
                drt = ceiling(rtc/bin_width), 
                # save both user-defined parameters for plotting
                cens_time = timeout, 
                bin_width = bin_width) 
}

# function ptb creates a person-trial-bin oriented data set
ptb <- function(df){
  df %>% uncount(weights = drt) %>% # create drt rows per trial
    group_by(trial) %>% 
    mutate(period = 1:n()) %>% # create time bin (or time period) ranks within each trial
    mutate(event = if_else(period == max(period) & right_censored == 0, 1, 0)) %>% 
    # event = 1 indicates response occurrence
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
    # replace NA with 0
    mutate(event0 = ifelse(is.na(event0),0,event0), 
           event1 = ifelse(is.na(event1),0,event1),
           # define the risk set
           risk_set = event0 + event1) %>% 
    # calculate hazard estimate
    mutate(hazard = (event1 / risk_set) %>% round(digits = 3)) %>% 
    # standard error for hazard, probability mass, and se for P(t)
    mutate(se_haz = sqrt((hazard * (1 - hazard)) / risk_set) %>% round(digits = 4), 
           pmass = event1/max(risk_set),
           se_pmass = sqrt((pmass * (1 - pmass)) / max(risk_set))) %>% 
    group_by(condition) %>%
    # calculate survival estimate (Greenwood's (1926) approximation) and se of S(t)
    mutate(survival = (cumprod(1-hazard)) %>% round(digits = 4), 
           term     = (cumsum(hazard / (risk_set * (1 - hazard)))) %>% round(digits = 7), 
           se_surv  = (survival * sqrt(term)) %>% round(digits = 5)) %>% 
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
join_lt_ca <- function(df1,df2){df1 %>% 
    left_join(df2, join_by(condition,period)) %>%
    select(condition,period,event1,risk_set,hazard,se_haz,survival,se_surv, 
           ca,se_ca,pmass,se_pmass,bin_width,cens_time,event0,term,n)  
  }

# function extract_median is used to extract the S(t).50 quantiles 
# (i.e., the estimated median RTs) when plotting the data
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

# function plot_eha plots the hazard, survivor, and ca(t) functions, 
# when there is one independent variable
plot_eha <- function(df,subj,haz_yaxis=1,first_bin_shown=1,aggregated_data=F,Nsubj=6){ 
  
  # haz_yaxis: controls the upper limit of the y-axis of the hazard plot.
  #            Recommended to set this to 1 when plotting the data of individual subjects.
  # first_bin_shown: default = 1. In case the first few bins do not contain data for any person, 
  #.           you can set first_bin_shown to another value, such as 5 (i.e., do not show the 
  #            first 4 bins in the plots).
  # aggregated_data: For a dataset with data aggregated across individuals, set to TRUE. 
  #            This only controls the title of the plots.
  # Nsubj: The number of individuals before aggregation. Only used to set the title of the plot.
  
  library(patchwork) # to combine figures
  library(RColorBrewer) # color palette 
  cutoff <- df %>% pull(cens_time) %>% max(na.rm=T)
  binsize <- df %>% pull(bin_width) %>% max(na.rm=T)
  median_period <- extract_median(df)
  n_conditions <- nlevels(df$condition)
  
  # create a data file with medians
  data_median <- c()
  for(i in 1:n_conditions){
    data_median <- append(data_median, c(median_period[i], median_period[i]))
  }
  # set up new data file to plot vertical lines indicating quantile S(t).50
  data_medians <- tibble(period = data_median,
                         survival = rep(c(.5, 0),n_conditions),
                         condition = rep(1:n_conditions, each=2))
  
  # Remove the first "first_bin_shown - 1" bins from the plots.
  df <- df %>% filter(period >= first_bin_shown)
  
  # plot the hazard functions
  p1 <- df %>% ggplot(aes(x=period, color=condition, group=condition)) +
    geom_line(aes(y=hazard), linewidth=1) +
    geom_point(aes(y=hazard,shape=condition), 
               size=2.5) + 
    geom_linerange(aes(ymin=hazard-se_haz, ymax=hazard+se_haz), 
                   show.legend = F, linewidth = 0.6) +
    scale_color_brewer(palette = "Dark2", name="Condition") +
    scale_shape_discrete(name ="Condition", solid=T) + 
    scale_x_continuous(breaks = c(first_bin_shown:(cutoff/binsize)), 
                       labels = c(first_bin_shown:(cutoff/binsize)*binsize),
                       limits = c(first_bin_shown,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,haz_yaxis)) +
    labs(x = "time bin (upper bound in ms)", 
         y = "h(t)") +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle=90))
  
  # plot the survivor functions
  p2 <-df %>%
    ggplot(aes(x=period, color=condition, group=condition)) +
    geom_line(aes(y=survival), linewidth=1) +
    geom_point(aes(y=survival,shape=condition), 
               size=2.5) +
    geom_linerange(aes(ymin=survival-se_surv, ymax=survival+se_surv), 
                   show.legend = F, linewidth = 0.6) +
    
    # add vertical lines at the median RTs in the plot of the survivor functions using geom_path(). 
    # Make sure you apply the same levels and labels for the factor condition as in Tutorial 1a. 
    geom_path(aes(x=period, 
                  y=survival, 
                  color=factor(condition, 
                               levels = c(1,2,3),
                               labels = c("blank","congruent","incongruent"))),
              data = data_medians, 
              linetype = 3, 
              show.legend = F,
              linewidth = 0.8) +
    scale_color_brewer(palette = "Dark2", name="Condition") +
    scale_shape_discrete(name ="Condition", solid=T) + 
    scale_x_continuous(breaks = c(first_bin_shown:(cutoff/binsize)), 
                       labels = c(first_bin_shown:(cutoff/binsize)*binsize),
                       limits = c(first_bin_shown,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = "time bin (upper bound in ms)", 
         y = "S(t)") +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle=90))
  
  # plot the conditional accuracy functions
  p3 <-df %>%
    ggplot(aes(x=period, color=condition, group=condition)) +
    geom_line(aes(y=ca), linewidth=1) +
    geom_point(aes(y=ca, shape=condition), 
               size=2.5) +
    geom_linerange(aes(ymin=ca-se_ca, ymax=ca+se_ca), 
                   show.legend = F, linewidth = 0.6) +
    scale_color_brewer(palette = "Dark2", name="Condition") +
    scale_shape_discrete(name ="Condition", solid=T) + 
    scale_x_continuous(breaks = c(first_bin_shown:(cutoff/binsize)), 
                       labels = c(first_bin_shown:(cutoff/binsize)*binsize),
                       limits = c(first_bin_shown,cutoff/binsize)) +

    scale_y_continuous(limits = c(0,1)) +
    labs(x = "time bin (upper bound in ms)", 
         y = "ca(t)") +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle=90))
  
  # plot the probability mass functions
  p4 <-df %>%
    ggplot(aes(x=period, color=condition, group=condition)) +
    geom_line(aes(y=pmass), linewidth=1) +
    geom_point(aes(y=pmass, shape=condition), 
               size=2.5) +
    geom_linerange(aes(ymin=pmass-se_pmass, ymax=pmass+se_pmass), 
                   show.legend = F, linewidth = 0.6) +
    scale_color_brewer(palette = "Dark2", name="Condition") +
    scale_shape_discrete(name ="Condition", solid=T) + 

    scale_x_continuous(breaks = c(first_bin_shown:(cutoff/binsize)), 
                       labels = c(first_bin_shown:(cutoff/binsize)*binsize),
                       limits = c(first_bin_shown,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,.5)) +
    labs(x = "time bin (upper bound in ms)", 
         y = "P(t)") +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle=90))
  
  # create title
  if(aggregated_data){
    title = str_c("Descriptive stats for aggregated data (N = ", Nsubj,")")
  } else {
    title = str_c("Descriptive stats for subject ", subj)
  }
  
  # patchwork the plots
  p1_theme <- p1 + theme(legend.position = "none")
  p2_theme <- p2 + theme(legend.position = "none")
  p3_theme <- p3 + theme(legend.position = "none")
  
  p1_theme + p2_theme + p3_theme + p4 + 
    plot_annotation(title = title) +
    plot_layout(guides = "collect", 
                axes = "collect_x") & 
    theme(legend.position = "top")
 
}

# function setup_lt_2IV sets up life tables for 2 independent variables
setup_lt_2IV <- function(ptb){
  
  ptb %>% 
    mutate(event = str_c("event", event)) %>%
    group_by(condition1,condition2,period) %>% 
    count(event) %>% 
    ungroup() %>% 
    pivot_wider(names_from = event,
                values_from = n) %>% 
    mutate(event0 = ifelse(is.na(event0),0,event0),
           event1 = ifelse(is.na(event1),0,event1),
           risk_set = event0 + event1) %>% 
    mutate(hazard = (event1 / risk_set) %>% round(digits = 3)) %>% 
    mutate(se_haz = sqrt((hazard * (1 - hazard)) / risk_set) %>% round(digits = 4),
           pmass = event1/max(risk_set),
           se_pmass = sqrt((pmass * (1 - pmass)) / max(risk_set))) %>% 
    group_by(condition1,condition2) %>%
    mutate(survival = (cumprod(1-hazard)) %>% round(digits = 4), 
           term     = (cumsum(hazard / (risk_set * (1 - hazard)))) %>% round(digits = 7), 
           se_surv  = (survival * sqrt(term)) %>% round(digits = 5)  ) %>%
    ungroup() 
}

# function calc_ca_2IV calculates the conditional accuracies for 2 IVs
calc_ca_2IV <- function(df){
  df %>% 
    filter(right_censored==0) %>%
    group_by(condition1,condition2,drt,cens_time,bin_width) %>%
    summarize(ca = mean(acc) %>% round(digits = 2),
              n = n(),
              .groups = 'drop') %>%
    ungroup() %>%
    mutate(period = drt,
           se_ca = sqrt((ca * (1-ca)) / n) %>% round(digits = 3)) %>%
    select(-drt)
}

# function join_lt_ca_2IV joins the conditional accuracies to the life tables 
# when there are two independent variables
join_lt_ca_2IV <- function(df1,df2){df1 %>% 
    left_join(df2, join_by(condition1,condition2,period))}

# function extract_median_2IV is used to extract the S(t).50 quantiles 
# when there are two independent variables
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

# function plot_eha_2IV plots the hazard, survivor, and ca(t) functions, 
# when there are two independent variables. 
plot_eha_2IV <- function(df,subj,haz_yaxis=1,first_bin_shown=1,aggregated_data=F,Nsubj=6){
  
  # see plot_eha() for more information on the parameters.
  library(patchwork)
  library(RColorBrewer)
  
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
  # Create correctly named factors (here: prime and mask) and 
  # specify the same levels and labels as above
  data_medians <- tibble(period= data_median,
                         survival = rep(c(.5, 0),n_conditions),
                         prime =  factor(rep(rep(1:n_iv2, each=2), n_iv1),
                                         levels=c(1:3),
                                         labels=c("blank","congruent","incongruent")),
                         mask = factor(rep(1:n_iv1, each=2*n_iv2),
                                       levels=c(1:4),
                                       labels=c("nomask", "relevant", "irrelevant","lines")))
   
  # Remove the first "first_bin_shown - 1" bins from the plots.
  df <- df %>% filter(period >= first_bin_shown)
  
  # plot the hazard functions
  p1 <-df %>% 
    mutate(mask = condition1,       # Create correctly named factor (here: mask)
           prime = condition2) %>%  # Create correctly named factor (here: prime)
    ggplot(aes(x=period, color=prime, group=prime)) + # Apply correctly named factor
    geom_line(aes(y=hazard)) +
    geom_point(aes(y=hazard), 
               size=1) + 
    geom_linerange(aes(ymin=hazard-se_haz, ymax=hazard+se_haz),
                  show.legend = F) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = c(first_bin_shown:(cutoff/binsize)),
                      labels = c(first_bin_shown:(cutoff/binsize)*binsize),
                      limits = c(first_bin_shown,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,haz_yaxis)) +
    labs(x = "bin endpoints (ms)",
        y = "h(t)",
        color = "Prime") +
    theme(panel.grid.minor = element_blank(),
         axis.text.x = element_text(angle=90)) +
    facet_wrap(~mask, nrow=1, ncol=4) # Apply correctly named factor

  # plot the survivor functions
  p2 <-df %>%
    mutate(mask = condition1,      # Create correctly named factor
           prime = condition2) %>% # Create correctly named factor
    ggplot(aes(x=period, color=prime, group=prime)) + # Apply correctly named factor
    geom_line(aes(y=survival)) +
    geom_point(aes(y=survival),
               size=1) +
    geom_linerange(aes(ymin=survival-se_surv, ymax=survival+se_surv),
                   show.legend = F) +
    # add vertical lines to indicate the estimated median RTs with geom_path()
    geom_path(aes(x=period, y=survival),
              data = data_medians,
              linetype = 3,
              show.legend = F) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = c(first_bin_shown:(cutoff/binsize)),
                       labels = c(first_bin_shown:(cutoff/binsize)*binsize),
                       limits = c(first_bin_shown,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = "bin endpoints (ms)",
         y = "S(t)",
         color = "Prime") +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle=90)) +
    facet_wrap(~mask,nrow=1,ncol=4) # Apply correctly named factor

  # plot the conditional accuracy functions
  p3 <-df %>%
    mutate(mask = condition1,       # Create correctly named factor
           prime = condition2) %>%  # Create correctly named factor
    ggplot(aes(x=period, color=prime, group=prime)) + # Apply correctly named factor
    geom_line(aes(y=ca)) +
    geom_point(aes(y=ca),
               size=1) +
    geom_linerange(aes(ymin=ca-se_ca, ymax=ca+se_ca),
                   show.legend = F) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = c(first_bin_shown:(cutoff/binsize)),
                       labels = c(first_bin_shown:(cutoff/binsize)*binsize),
                       limits = c(first_bin_shown,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,1)) +
    labs(x = "bin endpoints (ms)",
         y = "ca(t)",
         color = "Prime") +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle=90)) +
    facet_wrap(~mask,nrow=1,ncol=4) # Apply correctly named factor

  # plot the probability mass functions
  p4 <-df %>%
    mutate(mask = condition1,       # Create correctly named factor
           prime = condition2) %>%  # Create correctly named factor
    ggplot(aes(x=period, color=prime, group=prime)) + # Apply correctly named factor
    geom_line(aes(y=pmass)) +
    geom_point(aes(y=pmass),
               size=1) +
    geom_linerange(aes(ymin=pmass-se_pmass, ymax=pmass+se_pmass),
                   show.legend = F) +
    scale_color_brewer(palette = "Dark2") +
    scale_x_continuous(breaks = c(first_bin_shown:(cutoff/binsize)),
                       labels = c(first_bin_shown:(cutoff/binsize)*binsize),
                       limits = c(first_bin_shown,cutoff/binsize)) +
    scale_y_continuous(limits = c(0,.5)) +
    labs(x = "bin endpoints (ms)",
         y = "P(t)",
         color = "Prime") +
    theme(panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle=90)) +
    facet_wrap(~mask,nrow=1,ncol=4) # Apply correctly named factor

  # create title
  if(aggregated_data){
    title = str_c("Descriptive stats for aggregated data (N = ", Nsubj,")")
  } else {
    title = str_c("Descriptive stats for subject ", subj)
  }

  # patchwork the plots
  p1_theme <- p1 + theme(legend.position = "none")
  p2_theme <- p2 + theme(legend.position = "none")
  p3_theme <- p3 + theme(legend.position = "none")

  (p1_theme / p2_theme / p3_theme / p4) +
    plot_annotation(title = title) +
    plot_layout(guides = "collect",
                axes = "collect_x") & 
    theme(legend.position = "bottom")
}
