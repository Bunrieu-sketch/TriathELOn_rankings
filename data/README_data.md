## Data Sources in TriathELOn project
Races included in the analysis are the 2009 and 2010 World Triathlon Series men's results.

 The 2009 results have been downloaded directly from [triathlon.org](https://triathlon.org/results#q=&hPP=15&idx=events_reverse_sort&p=0&dFR%5Bevent_categories.cat_name%5D%5B0%5D=World%20Triathlon%20Series&dFR%5Byear%5D%5B0%5D=2009&fR%5Bfederation_event%5D%5B0%5D=false&is_v=1). <br>
 The cleaning process for this data is in the [2009 cleaning notebook](https://github.com/zanderhinton/TriathELOn_rankings/blob/master/data/data_cleaning_2009.ipynb)<br><br>
 The 2010 results were queried from the [Triathlon Developers API](https://developers.triathlon.org/). <br>
 The code to query  for these results can be seen in the [2010 extraction notebook](https://github.com/zanderhinton/TriathELOn_rankings/blob/master/data/data_extraction_2010.ipynb), while the cleaning code for this data is in the [2010 cleaning notebook](https://github.com/zanderhinton/TriathELOn_rankings/blob/master/data/data_cleaning_2010.ipynb).