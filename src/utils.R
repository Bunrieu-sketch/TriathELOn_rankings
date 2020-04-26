#' Cleans a triathlon race dataframe into usable format,
#' removes non-finishers and converts times into number of seconds 
#'
#' @param df A triathlon race dataframe
#' @return The dataframe in usable format 
clean_df <- function(df){
  df <- df %>%
    clean_names() %>%
    filter(position != 'DNF', position != 'DSQ', position != 'DNS', position != 'LAP') %>%
    # convert all times to seconds
    mutate(athlete_name = paste(athlete_first, athlete_last),
           swim = period_to_seconds(hms(swim)),
           t1 = period_to_seconds(hms(t1)),
           bike = period_to_seconds(hms(bike)),
           after_b = swim + t1 + bike,
           t2 = period_to_seconds(hms(t2)),
           run = period_to_seconds(hms(run)),
           position = as.numeric(position),
           total_time = period_to_seconds(hms(total_time)))
  # rearrange column names
  df <- df %>% select(1:2, athlete_name, 5:9, after_b, 10:11, total_time, everything())
  df  
}


#' Takes an n-person race and creates template dataframe of (n choose 2) one on one races
#' @param race_df A triathlon race dataframe
#' @param period  The time period of the race (integer)
#' @return The template dataframe with results not filled out
create_ovo_df <- function(race_df, period){ 
  num_players <- nrow(race_df)
  df <- data.frame("time" = rep(period,choose(num_players,2)), 
                   # default of 0 for now, fill in later with the results
                   "player1" = rep(0,choose(num_players,2)), 
                   "player2" = rep(0,choose(num_players,2)),
                   # only store wins (ie 1 beats 2)
                   "result" = rep(1,choose(num_players,2)))
  df 
}


#' Fills in the results of a one on one dataframe
#' @param ovo_df A one on one template dataframe
#' @param race_df  A triathlon race dataframe
#' @return The (n choose 2) dataframe wth results fiilled in
results_df <- function(ovo_df, race_df){
  counter = 1
  # Iterate through the race dataframe and fill in the results into the ovo_df
  for (row1 in 1:(nrow(race_df)-1)){
    for (row2 in (row1+1):(nrow(race_df))){
      # THIS IS PLAYER 1 ID
      ovo_df[counter, 2] <- (race_df[row1, 2] %>% pull())
      # THUS IS PLAYER 2 ID
      ovo_df[counter, 3] <- (race_df[row2, 2] %>% pull())
      counter = counter + 1    
    }
  }
  ovo_df
}


#' Wrapper function to create filled out "one vs one" dataframe in one function call
#' @param race_df Triathlon race df
#' @param period Time period of the race
#' @return Filled out one-vs-one dataframe
results_df_wrapper <- function(race_df, period){
  ovo_df <- create_ovo_df(race_df, period)
  results <- results_df(ovo_df, race_df)
  results
}


#' Determines glicko ratings from a one-vs-one dataframe
#' @param ovo_df A "one-vs-one" triathlon dataframe
#' @param ... Previous state of glicko ratings, if applicable
#' @return Updated glicko ratings after processing the one vs one matchups
glicko_ratings <- function(ovo_df, ...){
  gl <- glicko(ovo_df, init = c(1500, 350), status = ...)
  gl$ratings
}


#' Takes current ratings and ratings deviation from a glicko ratings object
#' @param A glicko ratings object
#' @return The ratings and ratings deviation columns for each player
updated_rankings <- function(glicko_output){
  glicko_output[1:2]
}


#' Appends pre-race glicko rankings to a triathlon race df
#' Can use these pre-race rankings to determine kendall's tau with post race results
#' @param race_df Triathlon race df
#' @param rankings_df 
#' @return race_df with glicko rating and glicko rank appended
append_glicko_to_race <- function(race_df, rankings_df){
  # Create a new column for the ratings
  pre_rating <- rep(0, nrow(race_df))  
  glicko_score <- race_df %>% select(athlete_id) %>% pull()
  
  # Check if racers have a rating in the rankings_df
  # If not, give the racer a default newcomer 1500 rating
  for (row in (1:length(glicko_score))){
    if (glicko_score[row] %in% rankings_df$Player){
      glicko_score[row] <- rankings_df %>% filter(Player == glicko_score[row]) %>% 
        select(Rating) %>% pull()
    }
    else glicko_score[row] <- 1500
  }
  
  # Once have appended raw ratings score, add a 'rank' column which orders
  # racers by their glicko rating
  race_df <- cbind(race_df, glicko_score) %>%
    mutate(glicko_rank = min_rank(desc(glicko_score)))
  race_df
}


#' Calculates kendall's tau of pre race glicko rank and finish position, and
#' then updates the glicko rantings based on the results
#' @param glicko_obj Current glicko ratings of the system
#' @param rankings_df The subset of glicko ratings with rating and deviation
#' @race_df Triathlon race df
#' @ovo_df Triathlon "one-vs-one" df 
#' @c parameter c for glicko function
#' @return updated glicko ratings object
#' @return updated ranking object
#' @return kendall's tau between glicko ranking and finishing position

race_update <- function(glicko_obj, rankings_df, race_df, ovo_df, c){
  # first add glicko rankings to the df
  race_gl <- append_glicko_to_race(race_df, rankings_df)
  # compare kendall's tau of the two rankings systems and append to the rankings_df
  correlation <- cor(race_gl$glicko_rank, race_gl$position, method = "kendall")
  # compare R2 of the two ranking systems and append to the rankings df
  r_squared <- summary(lm(race_gl$position~race_gl$glicko_rank))$adj.r.squared
    
  # finally, update the rankings after the next race
  glicko <- glicko_ratings(ovo_df, status = glicko_obj, c = c)
  rankings_df <- updated_rankings(glicko)
  
  list(glicko, rankings_df, correlation, r_squared)
}

read_races <- function(calendar, gender){
  # define years and make empty list to hold races
  years <- names(calendar)
  races <- list()
  # iterate through calendar, download race and ovo race 
  for (i in seq_along(calendar)){
    year <- as.numeric(years[i])
    series <- calendar[[i]]
    for (race in series){
      race_df <- read_csv(paste0("../data/",year,"_races/races_clean/",race,"_",gender,".csv"), 
                          col_types = cols())
      ovo_df <- read_csv(paste0("../data/",year,"_races/ovo_races/",race,"_",gender,".csv"),
                         col_types = cols())
      # format all races as list of lists for use in functions
      race_tuple <- list(race_df, ovo_df)
      # add to list of races
      races[[length(races)+1]] <-  race_tuple
    }
  }
  races
}


find_best_c <- function(c_vector, races_list){
  # DF to store results at different levels of c
  average_results <- data.frame(matrix(NA, nrow = length(c_vec), ncol = 1))
  # Period incrementer for races
  iterator <- 1
  # iterate through each c
  for (c in c_vec){
    # reset period
    period <- 1
    # reset glicko and correlations to the 1st race
    first_race <- races_list[[1]][2]
    glicko <- glicko_ratings(first_race)
    rankings_df <- updated_rankings(glicko)
    # keep track of R^2 values
    r2_tracker <- rep(NA, length(races_list))
    
    # iterate through each race in the series
    for (race in races_list){
      race_df <- race[[1]]
      race_ovo <- race[[2]]
      # adjust period for the glicko race objects
      period <- period + 1
      # pass in rotating value of c
      update <- race_update(glicko, rankings_df, race_df, race_ovo, c)
      # save results from the update for use in next iteration
      glicko <- update[[1]]
      rankings_df <- update[[2]]
      r2_tracker[period-1] <- update[[4]]
    }
    # Find the average R2 at that level of C
    average_results[iterator,1] <- r2_tracker %>% mean()
    iterator <- iterator + 1
  }
  # find argmax for C for average R^2
  results_summary <- tibble(c = c_vec, average_R_squared = average_results[[1]])
  best_c <- results_summary  %>% arrange(desc(average_R_squared)) %>%
    select(c) %>% head(1) %>% pull()
  best_c
}

compare_rankings <- function(races_list, best_c){
    # Track the kendall's tau between both approaches
    tau_glicko <- rep(NA, length(races_list))
    tau_itu <- rep(NA, length(races_list))
    # Track R^2 btwn both approaches
    r2_glicko <- rep(NA, length(races_list))
    r2_itu <- rep(NA, length(races_list))

    # Reset glicko and correlations
    first_race <- races_list[[1]][2]
    glicko <- glicko_ratings(first_race)
    rankings_df <- updated_rankings(glicko)
    # iterate through each race
    count <- 1
    for (race in races_list){
            race_df <- race[[1]]
            race_ovo <- race[[2]]
            # pass in optimal value of C
            update <- race_update(glicko, rankings_df, race_df, race_ovo, c = best_c)
            # pass in results from the update for use in next iteration
            glicko <- update[[1]]
            rankings_df <- update[[2]]
            # just a list
            tau_glicko[count] <- update[[3]]
            r2_glicko[count] <- update[[4]]
            tau_itu[count] <- cor(race_df$start_number, race_df$position, method = "kendall")
            if (race_df %>% select(start_number) %>% unique() %>% pull() %>% length() > 1) {
                r2_itu[count] <- summary(lm(race_df$position~race_df$start_number))$adj.r.squared
            }
            count <- count + 1
    }
    list(tau_glicko, tau_itu, r2_glicko, r2_itu)
}

summary_stats <- function(rankings_list, year_vec){
  results <- tibble("tau_glicko" = rankings_list[[1]], 
                    "tau_itu" = rankings_list[[2]], 
                    "r2_glicko" = rankings_list[[3]], 
                    "r2_itu" = rankings_list[[4]], 
                    "year" = year_vec)
  tidy_df <- results %>% gather("metric", "score", -year) %>%
    filter(year > 2012) %>%
    group_by(metric) %>%
    summarise(mean_metric = mean(score))
  tidy_df
}



rankings_over_time <- function(rankings_list, gender, year_vec){
  results <- tibble("tau_glicko" = rankings_list[[1]], 
                    "tau_itu" = rankings_list[[2]], 
                    "r2_glicko" = rankings_list[[3]], 
                    "r2_itu" = rankings_list[[4]], 
                    "year" = year_vec)
  tidy_df <- results %>% gather("metric", "score", -year)
  
  # take data after year 2012 for plotting and summarizing
  year_summaries <- tidy_df %>%
    filter(year  > 2012)  %>%
    group_by(year, metric) %>%
    summarize(average_score = mean(score))
  
  # create and display plots
  r2<- year_summaries %>% filter(metric == "r2_glicko" | metric == "r2_itu")
  kendall <- year_summaries %>% filter(metric == "tau_glicko" | metric == "tau_itu")
  
  r2_plot<- ggplot(r2) + aes(x = year, y = average_score, colour = metric) +
    geom_line() + coord_cartesian(ylim = c(0, 1)) +
    labs(y = "Yearly average R2", 
         title = paste("R2 comparisons between ITU rankings and glicko scores-",
                       gender))
  
  kendall_plot <- ggplot(kendall) + aes(x = year, y = average_score, colour = metric) +
    geom_line() + coord_cartesian(ylim = c(0, 1)) +
    labs(y = "Yearly average Kendall's Tau", 
         title = paste("Kendall's Tau comparisons between ITU rankings and glicko scores-",
                       gender))
  
  grid.arrange(r2_plot, kendall_plot, nrow = 2)
  
}



