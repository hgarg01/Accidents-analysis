# Detailed Analysis of Motor Accidents happening on the UK roads
## Motivation
There are several motor accidents that happen every day around the globe. The analysis of these motor accidents can help provide critical actionable insights to facilitate informed decisions and drive business outcomes (such as reducing accidents, saving lives etc.) and business applications to reduce costs (Motor Vehicle Repair Claims, Personal Injury Claims etc).
## Aim of the project 
In this project, we aim to answer the following questions :
1. What are the most statistically significant variables , that help us to predict the severity of accidents accurately?
2. Can we evaluate different statastical prediction models and choose the best one depending on the performance?
3. Can we warn the user of high risk accident areas on the route chosen by user in real time?
## Secondary Data
This project uses the following databases :-
1. Accidents Database : Obtained from govt of UK website https://data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data It includes all the details about the accident including the location of the accident, Road/Weather conditions at the time of accident and 2. Vehicle Database : This data is also obtained from the above mentioned source and includes details regarding the vehicles involved in the accidents. 
3. Traffic Flow Data : This data describes the annual average volume of traffic flow at the location of the accident and has been obtained from https://data.gov.uk/dataset/9562c512-4a0b-45ee-b6ad-afc0f99b841f/highways-england-network-journey-time-and-traffic-flow-data
## Project Files
This Project is being built in R and is divided into 3 files : AccidentAnalysis, Models and FrontEnd. AccidentAnalysis includes the detailes analysis of the 3 tables and all the preprocessing. Models file contains the algorithms used to build models and evaluate performance and FrontEnd contains the front end of the project being developed in RShiny. 
## Method and Methodology
After importing the tables thorough data cleaning, data is preprocessed. Some fields are changed so that they become consistent across all the tables. Redundant levels of some categorical columns are removed and datatypes of all the fields are fixed as they should be.
### Feature Elimination
Some feathures that don't seem to be contributing much for our final analysis model are removed after comparing the correlation values and Chi squared vales.
### Feature Engineering
Hour and Month fields are cyclic features. They are encoded using sine and cosine transformations. Road name is another important feature for the predictive model and has more that 5000 different categories, thus making it unsuitable for most of the predictive algorithms. This feature is encoded using mean encoding.
### Exploratory Data Analysis
Different plots for all the features in the 3 tables are plotted and detailed analysis is done in order to see which features contribute most in the predictive modeling, removing the ones that are not so useful.
### Class Imbalance
A plot between the different number of samples available for different accident severity values, reveals that the data is highly skewed with maximum number of samples(85%) belonging to the major class- Mild, 14.85% of samples belonging to the serious class and only a small(0.15%) of data belonging to the minority class. In order to deal with this class imbalance, stratified sampling of the training set is done.
### Predictive Modeling
Following algorithms are used for predictive modeling in this project:
1. Naive Bayes
2. Multinomial Regression
3. Weighted Random Forest
4. Ensemble of the above models
### Front End
Front end is being developed in Rshiny and aims to show warning on the route chosen by user if there is high probability of fatal/serious accident on some route.
## Status
The exploratory analysis part of the project is more or less done. Various algorithms are used and evaluated for best performance. Some algorithms are being run on the **AWS cloud** to overcome the memory and processing limitations presesnted by the laptop. Different ensembles are being tried to see what is the best accuracy that we can obtain. Front end of the project is in the initial stage and needs a bit of work before it can be presesnted here.
