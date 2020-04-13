#### Under construction

## TriathELOn rankings
Applying the [Glicko](http://www.glicko.net/glicko.html) ranking system to historical [ITU](triathlon.org) World Series triathlon results.

#### Analysis
Main analysis/report is at [Doc/Glicko_rankings.md](https://github.com/zanderhinton/TriathELOn_rankings/blob/master/Doc/Glicko_rankings.md)

#### Data 
Races included in the modelling are the 2009-2019 World Triathlon Series men's and women's results. All race results and extraction and cleaning processes are included in the [Data Directory](https://github.com/zanderhinton/TriathELOn_rankings/blob/master/data/README_data.md) directory. 

#### Source code
The analysis relies heavily on the [Player Ratings](https://cran.r-project.org/web/packages/PlayerRatings/PlayerRatings.pdf) package, and some additional functions used in the [src directory](https://github.com/zanderhinton/TriathELOn_rankings/tree/master/src)
