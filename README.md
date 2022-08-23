# Sleep_capuchins

This repository includes all code needed to reproduce the results from my Bachelor's thesis: "Sleep ecology in capuchin monkeys (Cebus capucinus)"

We calculated vedba (overall movement) from raw accelerometry data that is available for download from the project called “Food for thought” on [Movebank](https://www.movebank.org/). 
As is the GPS data.
This research only includes data of the species "Cebus capucinus". 
The weather data is provided by the Smithsonian Research Insitute on Barro Colorado Island in Panama and can be downloaded on their website: [Rain](https://smithsonian.figshare.com/articles/dataset/Barro_Colorado_Island_Clearing_Precipitation/10042463) and [Temperature](https://smithsonian.figshare.com/articles/dataset/Barro_Colorado_Island_Clearing_Air_Temperature/10042451).

To reproduce the results, run the code in the numbered order, as following codes take in produced dataframes from previous codes.
  
`01_installing_packages.R`: installs all needed packages for the analysis

Code 02: Download acc from movebank script: is downloading the data from movebank into R directly

Code 03: Calculating vedba from raw acc: taking the raw acc data and calculating the overall movement 

Code 04: Sleep analysis: is setting a movement threshold under which the animal is considered asleep. From that it calulates sleep times and other sleep paramters

Code 05: Looking at weather data: is preparing the weather data, removes outiners etc.

Code 06: Rain-Temp-Correlation: checks if there is a correlation between rain and temperature that needs to be considered in further analysis 

Code 07: Weather and sleep: Tests for effects of rain and temperature on sleep efficency, TST and SPT. 

Code 08: Capuchin sleep site home range overlap: reads in home range gps files, calculates coordinate sof sleep sites and compares them to overlap zones of the home ranges of other individuals in the same year.

Code 09: Overlap zone hypothesis: Tests effect of sleep inside or outside the overlapping home range zones on the sleep parameters.

Code 10: Onset vedba plot: creates a figure of the average vedba overall during the day and sunset/sunrise times. And the same for onset and waking times

Code 11: Nap Hypothesis: tests for an effect of bad sleep in the previous night on more naptime during the next day

Code 12: Exhaustion sleep hypotheis: creates a dataframe of travelled distance during the day and tests if that predicts sleep time.
