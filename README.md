# Under construction!

## TriathELOn
Applying the [Glicko](http://www.glicko.net/glicko.html) ranking system to historical ITU World Series triathlon results.

#### Analysis
Main analysis/report is at [Doc/TriathELOn_rankings.md](nba.com)

#### Data 
Races included in the modelling are the 2009-2010 World Triathlon Series men's results. All race results and extraction and cleaning processes are included in the [Data Directory](nba.com) directory. 



#### Data
 The 2009 results have been downloaded directly from [triathlon.org](https://triathlon.org/results#q=&hPP=15&idx=events_reverse_sort&p=0&dFR%5Bevent_categories.cat_name%5D%5B0%5D=World%20Triathlon%20Series&dFR%5Byear%5D%5B0%5D=2009&fR%5Bfederation_event%5D%5B0%5D=false&is_v=1), while the 2010 results were queried from the [Triathlon Developers API](https://www.google.com/search?q=triathlon.org+developers&oq=&sourceid=chrome&ie=UTF-8). The code to query and clean these race results can be seen in a notebook in the data directory.