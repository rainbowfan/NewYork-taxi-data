# NewYork-taxi-data
##About this project
New York City taxi data exploration and analysis is one of my projects in 2015 Spring STA 242 Statistical Programming course at UC Davis.

##About data
Data used in the analysis is publicly available and can be downloaded from this website. http://www.andresmh.com/nyctaxitrips/

##Questions being answered 
1. Compute the deciles of the total amount less the tolls.
2. Fit a linear regression predicting total amount less the tolls using trip time as the predictor.
3. Use multiple regression and add the surchage as a regressor, estimating the resulting coefficients.

##About the analysis
Two distinct ways parallel computation and random sampling with Bag of Little Bootstraps are implemented to solve the first two tasks mentioned above. Corresponding R code can be found in Approach1.R and Approach2.R respectively.
Before extracting the data,"fare"          




