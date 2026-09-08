##################################Survival

cages80<-read_excel("C:/Users/martinezmartinez/Desktop/DATA_ANALYSIS/IGP_2025/Data.xlsx",sheet = 1, col_names = TRUE)

make_survival_df <- function(data, species, time, units, extTime = 2, expDuration = NULL) {
  # Coerce species column to numeric
  data[[species]] <- as.numeric(data[[species]])
  
  find_extinction <- function(counts, times, extTime, expDuration) {
    # Immediate return for NA in first observation
    if(is.na(counts[1])) {
      return(list(extinct = NA_real_, extinction_time = NA_real_))
    }
    # Replace subsequent NAs with 0
    counts[is.na(counts)] <- 0
    
    # Calculate presence/absence
    presence <- ifelse(counts == 0, 0, 1)
    cz <- rle(presence == 0)
    extinct <- FALSE
    extinct_week <- NA_real_
    run_start <- 1
    
    # Find first extinction run
    for(i in seq_along(cz$lengths)) {
      if(cz$values[i] && cz$lengths[i] >= extTime) {
        extinct <- TRUE
        extinct_week <- times[run_start]  # First zero of the run
        break
      }
      run_start <- run_start + cz$lengths[i]
    }
    
    # Handle survival cases
    if(!extinct) {
      extinct_week <- if(any(presence == 1)) max(times[presence == 1]) 
      else if(!is.null(expDuration)) expDuration else max(times)
    }
    
    list(extinct = as.numeric(extinct), extinction_time = extinct_week)
  }
  
  # Sort data by time within units
  data <- data[order(data[[units]], data[[time]]), ]
  expDuration <- if(is.null(expDuration)) max(data[[time]], na.rm = TRUE) else expDuration
  
  do.call(rbind, lapply(split(data, data[[units]]), function(df) {
    # Ensure time ordering
    df <- df[order(df[[time]]), ]
    res <- find_extinction(df[[species]], df[[time]], extTime, expDuration)
    data.frame(
      unit = unique(df[[units]]),
      extinct = res$extinct,
      extinction_time = res$extinction_time
    )
  }))
}

multi_species_survival <- function(data, species_cols, time_col, unit_col, extTime = 2) {
  # Coerce all species columns to numeric
  for (sp in species_cols) {
    data[[sp]] <- as.numeric(data[[sp]])
  }
  
  # Sort data by unit and time
  data <- data[order(data[[unit_col]], data[[time_col]]), ]
  
  all_species <- lapply(species_cols, function(sp) {
    surv_df <- make_survival_df(data, sp, time_col, unit_col, extTime)
    names(surv_df)[2:3] <- paste(sp, names(surv_df)[2:3], sep = "_")
    surv_df
  })
  
  final_df <- Reduce(
    function(x, y) merge(x, y, by = "unit", all = TRUE), 
    all_species
  )
  
  final_df
}

# enter your info here!
survival_data <- multi_species_survival(
  cages80,
  species_cols = c("ac","am","ma","cc","my","ol","aa"),
  time_col = "week",
  unit_col = "cage",
  extTime = 2
)

survival_data$enem=cages80$enem[match(survival_data$unit, cages80$cage)]

survival_data_long<- as.data.frame(survival_data %>%
                                     pivot_longer(
                                       cols = -c(unit, enem),
                                       names_to = c("species", "variable"),
                                       names_pattern = "^(..)_?(.*)$"
                                     ) %>%
                                     pivot_wider(
                                       names_from = variable,
                                       values_from = value
                                     ))

survival_data_long


#Remove useless combinations
survival_data_long=survival_data_long%>%filter(!((survival_data_long$sp=="ac" & survival_data_long$enem %in% c("ma+ol","my+aa","cc+ma","cc+my","ec+sr","ec+am")) | 
                                                   (survival_data_long$sp=="am" & survival_data_long$enem %in% c("ma+ol","ac+ol","my+aa","cc+ma","cc+my","ec+sr")) | 
                                                   (survival_data_long$sp=="ma" & survival_data_long$enem %in% c("ac+am","ac+ol","my+aa","cc+my","ec+sr","ec+am")) | 
                                                   (survival_data_long$sp=="cc" & survival_data_long$enem %in% c("ac+am","ma+ol","ac+ol","my+aa","ec+sr","ec+am")) |
                                                   (survival_data_long$sp=="my" & survival_data_long$enem %in% c("ac+am","ma+ol","ac+ol","cc+ma","ec+sr","ec+am"))| 
                                                   (survival_data_long$sp=="ol" & survival_data_long$enem %in% c("ac+am","my+aa","cc+ma","cc+my","ec+sr","ec+am"))| 
                                                   (survival_data_long$sp=="aa" & survival_data_long$enem %in% c("ac+am","ma+ol","ac+ol","cc+ma","cc+my","ec+sr","ec+am"))| 
                                                   (survival_data_long$sp=="sr" & survival_data_long$enem %in% c("ac+am","ma+ol","ac+ol","my+aa","cc+ma","cc+my","ec+am"))| 
                                                   (survival_data_long$sp=="ec" & survival_data_long$enem %in% c("ac+am","ma+ol","ac+ol","my+aa","cc+ma","cc+my"))))


survival_data_long
