##Golf Ratings
#####By Daniel Myers

This repository contains my preliminary efforts to create a superior golf rating system to the Official World Golf Rankings, located at http://www.owgr.com/ranking. I use RVest to compile data, and then use BigLM() to calculate a huge linear regression over the prior 4 years of data.  The weighting and priors used to generate the final predictive ratings were developed to maximize forward predictive power.

Note: the Tournament Monte Carlo simulations have not been cross validated to maximize accuracy at this time--they depend on the specifics of the estimated forward standard deviation for each player's rating, something that hasn't been fully validated.
