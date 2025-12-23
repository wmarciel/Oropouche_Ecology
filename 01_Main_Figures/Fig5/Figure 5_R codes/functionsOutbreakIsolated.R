#functions ------
listIncidenceAndPeaks <- function(case_linelist, group_for_incidence  = "abbrev_state", earliest_start  = as.Date("2022-07-01"), end_date = as.Date("2025-02-28"), int = "1 week", rolling_stats = c(3,5,7), column_for_peaks = "cases"){
  list_incidences <- list()
  for (s in unique(case_linelist[,group_for_incidence])){
    st <- case_linelist[case_linelist[,group_for_incidence] == s,]
    
    if(dim(st)[1] == 0){
      print(paste0("No cases in ",s))
      next
    }
    
    
    first_case <- min(st$dt_collection)
    
    start_at = earliest_start
    
    if(int == "1 week") start_at = first_case - 7*(max(rolling_stats)+1)
    if(int == "day") start_at = first_case - (max(rolling_stats)+7)
    if(int == "month") start_at = first_case - 31*(max(rolling_stats)+1)
    
    
    in.s <- incidence::incidence(st$dt_collection, 
                                 interval = int, 
                                 first_date = start_at, 
                                 last_date = end_date,
                                 standard = TRUE)
    
    inc.df <- data.frame(date = in.s$dates, cases = in.s$counts)
    
    for(n in rolling_stats){
      inc.df[,paste0("mean", n)] <- rollmean(inc.df$cases, n, fill=0 ,align= "center")
      inc.df[,paste0("sum", n)] <- rollsum(inc.df$cases, n, fill=0 ,align= "center")
      
    }
    
    peaklist <- peakwindow(inc.df$date, inc.df[,column_for_peaks]);
    peaks_peakwindow <- peaklist$peaks
    peaks_peakwindow$x <- as.Date(peaks_peakwindow$x)
    peaks_peakwindow$st_date <- inc.df$date[peaks_peakwindow$xleft]
    peaks_peakwindow$end_date <- inc.df$date[peaks_peakwindow$xright]
    
    peaks_peakwindow$peak_max <- inc.df$cases[peaks_peakwindow$index]
    peaks_peakwindow$peakcasetotal <- NA
    for (i in 1:length(peaks_peakwindow$index)){
      peaks_peakwindow$peakcasetotal[i] <- sum(inc.df$cases[peaks_peakwindow$xleft[i]:peaks_peakwindow$xright[i]])
      
    }
    
    peaks_findpeaks <- data.frame(findpeaks(inc.df[,column_for_peaks], minpeakdistance = 30))
    if(dim(peaks_findpeaks)[1]> 0){
      names(peaks_findpeaks) <- c("peak_max","max_idx","st_idx","end_idx")
      
      peaks_findpeaks$peak_date <- inc.df$date[peaks_findpeaks$max_idx]
      peaks_findpeaks$st_date <- inc.df$date[peaks_findpeaks$st_idx]
      peaks_findpeaks$end_date <- inc.df$date[peaks_findpeaks$end_idx]
      peaks_findpeaks$peakcasetotal <- NA
      for (i in 1:length(peaks_findpeaks$st_idx)){
        peaks_findpeaks$peakcasetotal[i] <- sum(inc.df$cases[peaks_findpeaks$st_idx[i]:peaks_findpeaks$end_idx[i]])
        
      }
    }
    
    
    # list_incidences[[s]] <- inc.df[(which(inc.df$mean5 > 0)[1]):length(inc.df$date),]
    list_incidences[[s]]$incidence <- inc.df
    list_incidences[[s]]$peaks_peakwindow <- peaks_peakwindow
    list_incidences[[s]]$peaks_findpeaks <- peaks_findpeaks
    
    rm(inc.df,peaks_peakwindow)
  }
  
  return(list_incidences)
}

longFormatIncidence <- function(case_linelist, population_dataframe = FALSE,group_for_incidence  = "abbrev_state", int = "1 week"){
  in.s <- incidence::incidence(case_linelist$dt_collection, 
                               interval = int,
                               groups = case_linelist[,group_for_incidence],
                               standard = TRUE)
  
  in.wide <- as.data.frame(in.s)
  
  in.wide %>%  pivot_longer(
    cols = unique(case_linelist[,group_for_incidence]),
    names_to = group_for_incidence,
    values_to = "cases"
  ) -> inc_state
  
  as.data.frame(cumulate(in.s)) %>% pivot_longer(
    cols = unique(case_linelist[,group_for_incidence]),
    names_to = group_for_incidence,
    values_to = "cumulative_cases"
  ) -> cum_state
  
  inc_df <- merge(inc_state,cum_state)
  
  if(any(class(population_dataframe) == "data.frame")){
    if(group_for_incidence == "abbrev_state") inc_df <- left_join(inc_df, population_dataframe, by = join_by(abbrev_state == abbrev_state))
    if(group_for_incidence == "st_type") inc_df <- left_join(inc_df, population_dataframe, by = join_by(st_type == st_type))
    if(group_for_incidence == "muni_code") inc_df <- left_join(inc_df, population_dataframe, by = join_by(muni_code == six_digit_id))
    
    
    inc_df$per100k = 100000*inc_df$cases/inc_df$total_population
    inc_df$cumper100k = 100000*inc_df$cumulative_cases/inc_df$total_population
  }
  
  return(inc_df)
}

# thinking about how to filter long absences -----
isolateTransmissionPeriods <- function(incidence_df, case_ll, units_for_gap, minimum_total_cases_for_period, mean_cases_for_period_override = 500){
  
  ungrouped_incidence = incidence_df
  ungrouped_incidence$gap_to_next_case <- NA
  
  case_idxs <- which(ungrouped_incidence$cases > 0)
  next_case_gap <- c(case_idxs[-1],length(ungrouped_incidence$dates)+1+units_for_gap) - case_idxs
  ungrouped_incidence$gap_to_next_case[case_idxs] <- next_case_gap
  
  periods <- as.data.frame(matrix(ncol = 10, nrow = 0)); names(periods) = c("st_idx","end_idx","st_date", "end_date","q1_date","median_date", "sum_cases", "max_cases","mean_cases", "period")
  gaps <- which(ungrouped_incidence$gap_to_next_case >= units_for_gap); gaps <- c(0,gaps)
  p.count = 0
  for(g in 2:length(gaps)){
    
    end_idx = gaps[g]
    st_idx = case_idxs[which(case_idxs > (gaps[g-1]))][1]
    
    st_date = ungrouped_incidence$dates[st_idx]
    end_date = ungrouped_incidence$dates[end_idx]
    
    sum_cases = sum(ungrouped_incidence$cases[st_idx:end_idx])
    max_cases = max(ungrouped_incidence$cases[st_idx:end_idx])
    mean_cases = mean(ungrouped_incidence$cases[st_idx:end_idx])
    
    gap_ll <- subset(case_ll, dt_collection >= st_date & dt_collection <= end_date)
    
    if(sum_cases < minimum_total_cases_for_period){period = "ISOLATED"; q1_date = NA; median_date = NA}
    if(sum_cases > 1 & mean_cases >= mean_cases_for_period_override){p.count = p.count +1; period = as.character(p.count);q1_date = as.Date((summary(gap_ll$dt_collection)[[2]])); median_date = as.Date((summary(gap_ll$dt_collection)[[3]]))}
    if(sum_cases >= minimum_total_cases_for_period){ p.count = p.count +1; period = as.character(p.count);q1_date = as.Date((summary(gap_ll$dt_collection)[[2]])); median_date = as.Date((summary(gap_ll$dt_collection)[[3]]))}
    
    periods <- rbind(periods,
                     cbind(st_idx,end_idx,st_date, end_date,q1_date,median_date, sum_cases, max_cases, mean_cases,period))  
  }
  
  gaps# periods$st_date = as.Date(periods$st_date);periods$end_date = as.Date(periods$end_date)
  periods$st_date = as.Date(as.numeric(periods$st_date));periods$end_date = as.Date(as.numeric(periods$end_date))
  
  incidence_df$transmission_period = NA
  
  for(i in 1:length(periods$period)){
    incidence_df$transmission_period[periods$st_idx[i]:periods$end_idx[i]] <- periods$period[i]
  }
  # incidence_df$transmission_period <- factor(incidence_df$transmission_period)
  # periods$period <- factor(periods$period)
  return(list(incidence = incidence_df, transmission_periods = periods))
}

transmissionPeriodsManyUnits <- function(incidence,cases,testing = "don't worry about it my guy", geog_column = "code_muni",units_for_gap = 21, gap_unit = "day",to_return = "dfs",minimum_total_cases_for_period = 3, mean_cases_for_period_override = 500){
  period_list <- list()
  incidence_list <- list()
  
  
  for(location in unique(incidence[,geog_column])){
    local_inc <- incidence[which(incidence[, geog_column] == location),]
    case_ll <- cases[which(cases[, geog_column] == location),]
    output <- isolateTransmissionPeriods(local_inc,case_ll, units_for_gap,minimum_total_cases_for_period)
    incidence_list[[location]] <- output$incidence
    output$transmission_periods$location <- location
    
    
    if("data.frame" %in% class(testing) ){
      if(gap_unit != "day") warning("If your gaps aren't measured in days, you need to come back and edit the function for recent/following tests")
      output$transmission_periods$recent_test_count <- NA
      output$transmission_periods$following_test_count <- NA
      output$transmission_periods$following_test_positives <- NA
      loc_tests <- testing[testing[,geog_column] == location,] %>%  arrange(dt_collection)
      for(i in 1:nrow(output$transmission_periods)){
        output$transmission_periods$recent_test_count[i] = length(which(loc_tests$dt_collection< output$transmission_periods$st_date[i] &
                                                                          loc_tests$dt_collection>= output$transmission_periods$st_date[i] - units_for_gap))
        
        output$transmission_periods$following_test_count[i] = length(which(loc_tests$dt_collection>= output$transmission_periods$st_date[i] &
                                                                             loc_tests$dt_collection<= output$transmission_periods$st_date[i] + units_for_gap)) - 1
        
        output$transmission_periods$following_test_positives[i] = sum(loc_tests$oro_result_tf[which(loc_tests$dt_collection>= output$transmission_periods$st_date[i] &
                                                                                                      loc_tests$dt_collection<= output$transmission_periods$st_date[i] + units_for_gap)])  - 1
        
      }
      output$transmission_periods$following_test_prop <- output$transmission_periods$following_test_positives/output$transmission_periods$following_test_count 
    }
    
    
    
    
    
    period_list[[location]] <- output$transmission_periods
  }
  
  if(to_return == "dfs") {
    periods = bind_rows(period_list)
  
  periods$case_outcome <- NA
  periods$case_outcome[which(periods$period == "ISOLATED")] <- "ISOLATED"
  periods$case_outcome[which(periods$period != "ISOLATED")] <- "OUTBREAK"
  
  periods$st_date <- as.Date(floor(as.numeric(periods$st_date)))
  periods$q1_date <- as.Date(floor(as.numeric(periods$q1_date)))
  periods$median_date <- as.Date(floor(as.numeric(periods$median_date)))
  }
  
  if(to_return == "list") return(list(incidences = incidence_list, periods = period_list))
  if(to_return == "dfs") return(list(incidence = bind_rows(incidence_list), periods = periods))
  
}
