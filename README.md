# Fall 2020 ISYE 7405 Final Project

Repository for Fall 2020 ISYE 7405 Final Project

## Directory Structure

```
.
├── src
├── _data
│   └── train.csv
```

Note that the large csv files are ignored, so the data folder should be created
and should include the train.csv file

## Workflow

1. Preprocess.R -> ./data/NewTaxiDataFiltered.csv, ./data/NewTaxiData.csv
2. ExtractGPS.R + ./data/NewTaxiDataFiltered.csv -> ./data/chunks/NewDataXY_part1, ..., ./data/chunks/NewDataXY_part6,  ./data/chunks/XYticks_part1, ./data/chunks/XYticks_part2, ./data/chunks/XYticks_part3
3. ExtractStartingLocation.R + ./data/chunks/NewDataXY_part1, ..., ./data/chunks/NewDataXY_part6 -> ./data/chunks/STAND_LOCATIONS.csv
4. EDAFiguresTables.R + ./data/NewTaxiData.csv -> ./data/output/FigALL.png, ./data/output/Fig4.png, ./data/output/TableIILATEX.txt, ./data/output/TableILATEX.txt

## Exploratory Questions

1.	If specific locations are related to longer journeys
2.	If specific location are related to larger waiting times
3.	If people from specific areas call instead of going to Taxistand
4.	If same people tend to call
5.	If people seem to prefer specific Taxistands (is this related tot location for example the center of the city?)
6.	If the way people move during the Weekends changes (e.g. do they use more taxis or less during the weekend? + If the Taxistands that the people use during the weekend changes, i.e. if the waiting times of the taxistands change according to location)
7.	If the proportion of the number of people that call for taxis changes during the weekends
8.	If the average time of the trips during the weekend changes significantly.
9.	Is there a way to improve the waiting times (maybe use the first Data to form a strategy and then use the other data to check whether this strategy performs better
10.	Remove the Data with missing values
11.	Remove the Drivers that have performed a few trips
12.	Check whether there are Taxi Drivers that work occasionally
13.	Check wether the Driver with the most services work more or just have a better strategy (choice of Taxistands and Days, e.g. weekends or Workdays)
14.	What should they do with their fleet, would it be better to reduce or increase their fleet?
15.	Would it be a good idea to increase or reduce the Taxistands?










