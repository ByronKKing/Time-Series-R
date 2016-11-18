# Time Series in R

This a collection of R scripts that I saved from my Time Series Analysis course that I took as an undergrad. I managed to save and comment fully four of my projects.

## Project 1

This folder contains 3 R scripts and an R Markdown that summarizes many of the results from the previous scripts. 

The first script is generic pseudo-code for taking a time series and creating ARIMA models with different parameters in a for loop, and finding the model with the lowest AIC. 

The second script calls an R function (in the tv.R script) that calls the imbd API and retrieves ratings for a user-entered show. I retrieve the ratings for The Sopranos, Boardwalk Empire, and 24, and apply ARIMA models to these series. I then find the lowest AIC.

In the third script, I model the time series for the show "24", and model this series using a linear regression model with seasonality as well as a generalized least squares regression model also with seasonality.

## Project 2

In this first script, I similarly build pseudo-code to take a time series, create a matrix to store the AIC values of different ARIMA models, and then in a for loop build ARIMA models of different parameters and store these AIC values in the matrix. 

In the second script, I run this script on the differenced "24" ratings time series, and plot the acf and pacf of the residuals. In the third script, I run a similar analysis, only I apply the function to a dataset pertaining to Canadian labor productivity and employment.

## Bit Coin

Here I read in a csv file of bitcoin trading activity. The csv contains daily trading information since 7/18/2010 on bitcoin stock, including open, close, high and low prices. I plot the acf and pacf of the series as well as the differenced series, and plot these series over time. 

I then use the nnetar package to model the series using neural network time series forecasts. I split the series, build a model on a training set, then create a for loop that builds nnetar models, forecasts the future price, and then stores the residuals. I then calculate the mean squared error and mean absolute error for these models.

## Flu: Google Trends

Finally, I add an R script that retrieves data from google concerning word mentions over time. I pick the word "flu" and model these series using Holt Winters exponential smoothing, and then predict mentions 2 years ahead for the word "flu". I then proceed to model this series using generalized least squares regression and ARIMA methods.

