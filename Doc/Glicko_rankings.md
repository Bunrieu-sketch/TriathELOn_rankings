TriathELOn Rankings
================

### Introduction

The aim of this analysis is to develop a new ranking system for elite
triathletes competing on the [ITU triathlon](triathlon.org) World
Triathlon Series, which is the top level of international triathlon
competition. The ranking system will be an application of the famous
[Glicko ranking system](http://glicko.net/glicko.html), most commonly
used for international chess rankings. After the new ranking system is
established, it will be tested against the ITU’s internal ranking system
to determine which is superior in terms of predictive power.

### What is the glicko ranking system?

The Glicko rating system was created by Professor Mark Glickman to
expand and improve upon the famous (Elo rating system)\[\], which
attempted to probabilistically rate chess players based on the results
of their matchups. In both the `Elo` and `Glicko` rating systems, each
player is associated with a Rating, and the differences in Ratings
between the two players is what drives the prediction for the outcome of
a match. The larger the difference between two players, the higher the
system will predict the stronger player’s chances to win. After each
match, in a Bayesian-esque process rating points are transferred from
one player to the other in a zero sum fashion and both players ratings
are updated. The pre match Rating differences between the two players is
what determines the amount of rating points transferred between the
players. If one player was heavily favoured and won the match, then not
a lot of rating points are transferred (this match has not provided much
additional information to the system), however for two closely ranked
players, larger ratings point changes will occur. <br> The Glicko system
expands upon the Elo system by adding a reliability measure to a players
rating, or RD (ratings deviation). In this way, players who have played
more games and played more recently will have lower ratings deviations,
and players who have not played in a long time will intuitively have
higher rating’s deviations. Additional information and examples about
the `Glicko` formula can be found
[here](http://www.glicko.net/glicko/glicko.pdf)

### Adapting Glicko rating system to ITU triathlon

Both the Glicko and Elo rating systems were designed and derived with
2-player zero sum games like chess in mind. To use the system in a
triathlon context I will turn an n-person triathlon race into (n)(n-1)/2
individual “one-on-one” races. There are a couple problems with this
approach will be discussed at the end. Additionally, a “ratings period”
must be determined for each race. In the glicko system, the longer since
a player has played their last game, the higher is their ratings
deviation. Somewhat arbitrarily, during a WTS season I have set the
difference between races to be “1 period” and the difference from the
last race of a season and the first race of the next season to be “6
periods”, so that periods roughly align with months passed.

### Project scope

To begin, I will read in the data from men’s race results for 2009 WTS
and 2010 WTS. At this point, I am only looking at men’s races and this
short time period to make analysis simpler, but will scale up to longer
time frames and both genders soon once the process is refined. <br> For
each event, I will read in both a dataframe of normal race results
including all finishers, their places, times and their swim-bike-run
splits. Additionally, in order to apply the `glicko` formula to an \(n\)
person triathlon race, I also read in a `one-vs-one` dataframe, with
\(n(n-1)/2\) rows, each row giving the result of the one vs one race
between two competitors.<br> Explanation of data sources, extraction and
cleaning is included in the `data` directory. <br> All functions used in
this process are included in the `src` directory.

``` r
# Read in all rae results and one-vs-one race results
# 2009
# Tongyeong
ty_df_09 <- read_csv("../data/2009_races/races/Tongyeong_men.csv", col_types = cols())
ty_ovo_09 <- read_csv("../data/2009_races/ovo_races/Tongyeong_men.csv", col_types = cols())
# Madrid
md_df_09 <- read_csv("../data/2009_races/races/Madrid_men.csv", col_types = cols())
md_ovo_09 <- read_csv("../data/2009_races/ovo_races/Madrid_men.csv", col_types = cols())
# Washington
ws_df_09 <- read_csv("../data/2009_races/races/Washington_men.csv", col_types = cols())
ws_ovo_09 <- read_csv("../data/2009_races/ovo_races/Washington_men.csv", col_types = cols())
# Kitz
kz_df_09 <- read_csv("../data/2009_races/races/Kitz_men.csv", col_types = cols())
kz_ovo_09 <- read_csv("../data/2009_races/ovo_races/Kitz_men.csv", col_types = cols())
# Hamburg
hb_df_09 <- read_csv("../data/2009_races/races/Hamburg_men.csv", col_types = cols())
hb_ovo_09 <- read_csv("../data/2009_races/ovo_races/Hamburg_men.csv", col_types = cols())
# London
ld_df_09 <- read_csv("../data/2009_races/races/London_men.csv", col_types = cols())
ld_ovo_09 <- read_csv("../data/2009_races/ovo_races/London_men.csv", col_types = cols())
# Yokohama
yk_df_09 <- read_csv("../data/2009_races/races/Yokohama_men.csv", col_types = cols())
yk_ovo_09 <- read_csv("../data/2009_races/ovo_races/Yokohama_men.csv", col_types = cols())
# Gold Coast
gc_df_09 <- read_csv("../data/2009_races/races/Gold-coast_men.csv", col_types = cols())
gc_ovo_09 <- read_csv("../data/2009_races/ovo_races/Gold-coast_men.csv", col_types = cols())

# 2010
# Sydney
sd_df_10 <- read_csv("../data/2010_races/races/Sydney_men.csv", col_types = cols())
sd_ovo_10 <- read_csv("../data/2010_races/ovo_races/Sydney_men.csv", col_types = cols())
# Seoul
sl_df_10 <- read_csv("../data/2010_races/races/Seoul_men.csv", col_types = cols())
sl_ovo_10 <- read_csv("../data/2010_races/ovo_races/Seoul_men.csv", col_types = cols())
# Madrid
md_df_10 <- read_csv("../data/2010_races/races/Madrid_men.csv", col_types = cols())
md_ovo_10 <- read_csv("../data/2010_races/ovo_races/Madrid_men.csv", col_types = cols())
# Hamburg
hb_df_10 <- read_csv("../data/2010_races/races/Hamburg_men.csv", col_types = cols())
hb_ovo_10 <- read_csv("../data/2010_races/ovo_races/Hamburg_men.csv", col_types = cols())
# London
ld_df_10 <- read_csv("../data/2010_races/races/London_men.csv", col_types = cols())
ld_ovo_10 <- read_csv("../data/2010_races/ovo_races/London_men.csv", col_types = cols())
# Kitz
kz_df_10 <- read_csv("../data/2010_races/races/Kitz_men.csv", col_types = cols())
kz_ovo_10 <- read_csv("../data/2010_races/ovo_races/Kitz_men.csv", col_types = cols())
# Budapest
bd_df_10 <- read_csv("../data/2010_races/races/Budapest_men.csv", col_types = cols())
bd_ovo_10 <- read_csv("../data/2010_races/ovo_races/Budapest_men.csv", col_types = cols())
```

### Ranking metric

Before going forward with selecting the optimal `glicko` model, it is
important to finalize how different ranking systems will be compared
against each other. The `Kendall's tau` coefficient is an appropriate
metric to use here, as it measures the ordinal relationship between two
columns. To compare two “pre-race rankings”, one can calculate a
`Kendall's tau` coefficient between each ranking and the final race
results, and see which ranking system has a higher coefficient. A
`Kendall's tau` of 0 indicates no relationship, and a `Kendall's tau`
value of 1 indicates a perfect relationship. The formula for `Kendall's
tau` is as
follows:

\[\text{kendall's tau} = \frac{\text{number of concordant pairs - number of discordant pairs}}{\text{total number of pairs}}\]

For example, the vectors \[1,3,2\] and \[1,2,3\] would have a `Kendall's
tau` coefficient of 1/3, calculated as:<br>

$  = 0.333 $

### Initial value of players

For new players entering the system, they are given the default values
from the `PlayerRatings` package of a Rating of 1500, and a Rating
Deviation of 350. In the first race, everyone is new and all start with
this rating, and as the seasons progress, the number of new players in
each race decreases. Hence the first race is excluded from the analysis
of comparing ranking systenms.

### Determining value of parameter c

In the `glicko` formula, the ratings deviation of a player will decrease
with every performance gathered from them, and the ratings deviation of
a player will increase the greater period of time since their last
performance. The parameter `c` is governs the increase in uncertainty
between ratings periods. <br> To determine the optimal level of `c`, for
each of the last 14 races in our dataset I can calculate the value of
Kendall’s tau for a given level of `c`. Then, I can take the average of
these measurements, and compare the average kendall’s tau values for
each level of `c`.

**Process to determine the optimal value of c**<br> - for each level of
C<br> —\> for each race in the dataset<br> ——\> update race results
dataframe with previous glicko rankings, and rank athletes according to
their glicko rankings<br> ——\> determine kendall’s tau correlation
between glicko rankings and their finish results<br> ——\> use the
results of the race to appropriately update the glicko rankings<br>

``` r
# List of races
races <- list(list(md_df_09, md_ovo_09), list(ws_df_09, ws_ovo_09), list(kz_df_09, kz_ovo_09),
              list(hb_df_09, hb_ovo_09), list(ld_df_09, ld_ovo_09), list(yk_df_09, yk_ovo_09), 
              list(gc_df_09, gc_ovo_09), list(sd_df_10, sd_ovo_10), list(sl_df_10, sl_ovo_10), 
              list(md_df_10, md_ovo_10), list(hb_df_10, hb_ovo_10), list(ld_df_10, ld_ovo_10), 
              list(kz_df_10, kz_ovo_10), list(bd_df_10, bd_ovo_10))

### Find optimal level of C
c_vec <- seq(1, 5, by=1)

# Period incrementer for races
average_results <- rep(NA, length(c_vec))
iterator <- 1

# DF to store results at different levels of c
# iterate through each c
for (c in c_vec){
    # reset period
    period <- 1
    # reset glicko and correlations to the 1st race
    glicko <- glicko_ratings(ty_ovo_09)
    rankings_df <- updated_rankings(glicko)
    correlation_tracker <- rep(NA, length(races))
    # iterate through each race
    for (race in races){
        race_df <- race[[1]]
        race_ovo <- race[[2]]
        # adjust period for the glicko race objects
        period <- period + 1
        # pass in rotating value of c
        update <- race_update(glicko, rankings_df, race_df, race_ovo, c)
        # save results from the update for use in next iteration
        glicko <- update[[1]]
        rankings_df <- update[[2]]
        correlation_tracker[period-1] <- update[[3]]
    }
    # Find the average kendall's tau at that level of C
    average_results[iterator] <- correlation_tracker %>% mean()
    iterator <- iterator + 1
}
results_summary <- tibble(c = c_vec, average_kendall_tau = average_results)
results_summary
```

    ## # A tibble: 5 x 2
    ##       c average_kendall_tau
    ##   <dbl>               <dbl>
    ## 1     1               0.416
    ## 2     2               0.416
    ## 3     3               0.416
    ## 4     4               0.416
    ## 5     5               0.418

## Comparing glicko rankings to ITU rankings

Now that the optimal c level has been found to be 5, we can compare the
two ranking systems (ITU rankings and Glicko rankings). To do this, the
process will be similar to above, except now we are working with a fixed
value of c, and are also calculating the `kendall's tau coefficient` for
the itu ranking system. The process followed is: <br><br> - for each
race in the dataset<br> —\> update race results dataframe with previous
glicko rankings, and rank athletes according to their glicko
rankings<br> —\> determine kendall’s tau correlation between glicko
rankings and their finish results<br> —\> determine kendall’s tau
correlation between itu rankings and their finish results<br> —\> use
the results of the race to appropriately update the glicko
rankings<br>

``` r
best_c <- results_summary  %>% arrange(desc(average_kendall_tau)) %>% select(c) %>% head(1) %>% pull()
# reset glicko and correlations
glicko <- glicko_ratings(ty_ovo_09)
rankings_df <- updated_rankings(glicko)
# Track the kendall's tau between both approaches
tau_glicko <- rep(NA, length(races))
tau_itu <- rep(NA, length(races))
# iterate through each race
count <- 1
for (race in races){
        race_df <- race[[1]]
        race_ovo <- race[[2]]
        # pass in rotating value of c
        update <- race_update(glicko, rankings_df, race_df, race_ovo, c = best_c)
        # pass in results from the update for use in next iteration
        glicko <- update[[1]]
        rankings_df <- update[[2]]
        # just a list
        tau_glicko[count] <- update[[3]]
        tau_itu[count] <- cor(race_df$start_number, race_df$position, method = "kendall")
        count <- count + 1
    }
```

    ## Warning in cor(race_df$start_number, race_df$position, method = "kendall"):
    ## the standard deviation is zero

``` r
comparison_df <- tibble(race = c("md-09", "ws-09", "kz-09", 
                                 "hb-09", "ld-09", "yk-09", 
                                 "gc-09", "sd-10", "sl-10", 
                                 "md-10", "hb-10", "ld-10",
                                 "kz-10", "bd-10"),
                       glicko = tau_glicko, itu = tau_itu) %>%
                       gather(key = method, value = kendalls, -race)
```

To compare the results of the two methods, I will make a bar plot
showing the `Kendall's tau` coefficient for both methods at each of the
last 14 races in the dataset. <br>

``` r
comparison_df %>% ggplot() + 
                    aes(x = factor(race, 
                                   level = c("md-09", "ws-09", "kz-09", 
                                 "hb-09", "ld-09", "yk-09", 
                                 "gc-09", "sd-10", "sl-10", 
                                 "md-10", "hb-10", "ld-10",
                                 "kz-10", "bd-10")), 
                        y = kendalls, fill = method) +
                    geom_bar(stat = "identity", position = "dodge") +
                    labs(x = "race", y = "kendalls tau", title = "Comparison of rank orderings - 2009/2010")
```

    ## Warning: Removed 3 rows containing missing values (geom_bar).

![](Glicko_rankings_files/figure-gfm/plotting-1.png)<!-- -->

### Discussion of results

Unfortunately the ITU rankings were not available for three of the races
in the dataset (`Gold Coast 2009`, `Seoul 2010`, `Budapest 2010`), so we
are unable to do a full comparison between the two ranking systems.
Taking the mean of the two ranking sytems between the 11 races where we
have `kendall's tau` for both methods, we can see the average score is
slightly higher using the `ITU` rankings than using the `glicko`
rankings.

``` r
comparison_df %>%
      filter(race != 'gc-09' & race != 'sl-10' & race!= 'bd-10') %>%
      group_by(method) %>%
      summarise(mean_kendalls = mean(kendalls))
```

    ## # A tibble: 2 x 2
    ##   method mean_kendalls
    ##   <chr>          <dbl>
    ## 1 glicko         0.397
    ## 2 itu            0.415

### Issues with Glicko formula and further improvements

  - one `performance` has become ~50 performances, and therefore
    incorrectly low Ratings Deviations are achieved quickly. A quick and
    dirty resolution to this is to set the minimum Ratings Deviation to
    30, which is a measure suggested by Prof. Glickman, in order to
    ensure that meaningful changes in performance level can be always be
    made.
  - DNF’s. For simplicity, these were removed. It is difficult to
    determine the best one size fits all approach to DNFs for the simple
    reason that they are often not random. Many DNFs are random (bike
    crashes, etc), however there are certainly many cases where an
    athlete may pull the plug when they are having a poor performance.
    In these cases, their glicko rankings really should drop on account
    of this, but they are being removed from the results so their
    ranking will not change (ratings deviation will increase however.)
  - Including the racing tier one below the WTS (World Cup races) would
    be helfpful as then athletes who are making their WTS debut, instead
    of starting with the default 1500 rating, would likely already have
    a ranking via performances at the World Cup level. Additionally, for
    athetes who race on both circuits, it would allow for more
    information to be gathered.