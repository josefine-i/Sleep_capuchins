# Sleep_capuchins

This repository includes all code needed to reproduce the results from my Bachelor's thesis: "Sleep ecology in capuchin monkeys (Cebus capucinus)"

We calculated vedba (overall movement) from raw accelerometry data that is available for download from the project called “Food for thought” on [Movebank](https://www.movebank.org/). 
As is the GPS data.
This research only includes data of the species "Cebus capucinus". 
The weather data is provided by the Smithsonian Research Insitute on Barro Colorado Island in Panama and can be downloaded on their website: [Rain](https://smithsonian.figshare.com/articles/dataset/Barro_Colorado_Island_Clearing_Precipitation/10042463) and [Temperature](https://smithsonian.figshare.com/articles/dataset/Barro_Colorado_Island_Clearing_Air_Temperature/10042451).

To reproduce the results, run the code in the numbered order, as following codes take in produced dataframes from previous codes.
  
`01_installing_packages.R`: installs all needed packages for the analysis

`02_Download_acc_from_movebank_script.R`: is downloading the data from movebank into R directly

`03_calculating_vedba_from_raw_acc.R`: Calculating vedba from raw acc: taking the raw acc data and calculating the overall movement 

`04_sleep_analysis.R`: calculates sleep parameters by setting a movement threshold under which the animal is considered asleep. Onset and waking times are determines, from which sleep times and effciency is calculated.

`05_Looking_at_weather_data.R`: is preparing the weather data, removes outiners etc.

`06_Rain-Temp-Correlation.R`: checks if there is a correlation between rain and temperature that needs to be considered in further analysis 

`07_Weather_and_sleep.R`: Tests for effects of rain and temperature on sleep efficency, TST and SPT. 

`08_Capuchin_sleep_site_home_range_overlap.R`: reads in home range gps files, calculates coordinate sof sleep sites and compares them to overlap zones of the home ranges of other individuals in the same year.

`09_overlap_zone_hypothesis.R`: Tests effect of sleep inside or outside the overlapping home range zones on the sleep parameters.

`10-Onset-vedba-Plot.R`: creates a figure of the average vedba overall during the day and sunset/sunrise times. And the same for onset and waking times. It therefore takes in "sun_dat" from Code 04.  

`11_Nap_Hypothesis.R`: tests for an effect of bad sleep in the previous night on more naptime during the next day

`12_Exhaustion_sleep_hypothesis.R`: creates a dataframe of travelled distance during the day and tests if that predicts sleep time.
