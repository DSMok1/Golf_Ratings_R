##Golf Ratings
#####By Daniel Myers

This repository contains my preliminary efforts to create a superior golf rating system to the Official World Golf Rankings, located at http://www.owgr.com/ranking. I use RVest to compile data, and then use BigLM() to calculate a huge linear regression over the prior 4 years of data.  The weighting and priors used to generate the final predictive ratings were developed to maximize forward predictive power.  There is a (weeks ago)^0.98 time weighting, and two lightly-weighted priors based on number of events played recently and average tournament quality played.

Note: the Tournament Monte Carlo simulations have not been cross validated to maximize accuracy at this time--they depend on the specifics of the estimated forward standard deviation for each player's rating, something that hasn't been fully validated.

Current predictve ratings are located at:
CSV: https://github.com/DSMok1/Golf_Ratings_R/blob/master/Output/Golf_Ratings_Current.csv
Formatted Excel of the top 1000: https://github.com/DSMok1/Golf_Ratings_R/blob/master/Output/Golf_Ratings_Current.xlsx

Here's the top 50 currently:
![Top 50 Image](/Output/Golf_Ratings_Current_top_50.png)