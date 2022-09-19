# Gold-Prices-Forecasting

This project uses gold prices data obtained from:

https://www.kaggle.com/datasets/nisargchodavadiya/daily-gold-price-20152021-time-series?select=Gold+Price.csv

which is a time series data frame pre-processed from https://in.investing.com/commodities/gold-mini by Nisarg Chodavadiya.

This report discusses the various attempts at tackling a challenge of forecasting gold mini futures prices relatively accurately. The gold mini futures prices are measures in Indian Rupee (INR) and the data are obtained from: https://www.kaggle.com/datasets/nisargchodavadiya/daily-gold-price-20152021-time-series?select=Gold+Price.csv which is a time series data frame pre-processed from https://in.investing.com/commodities/gold-mini by Nisarg Chodavadiya.

In order to determine a successful predictive algorithm, this work first divides the data into training and validation set with 90-10% split. It then divides the training data again into training and test sets with 80-20% split. All the training is done on this second training set and tested on the test set. The most successful algorithm was then chosen to run on the validation set as a final determination of the predictive success. Success was defined as root mean square error (RMSE) and mean absolute error (MAE), which can effectively be interpreted as a typical error in predicting star rating. 

The report is split into 4 areas. Following this Introduction, the Analysis section begins with data, packages, and libraries we need for this project, followed by data exploration and preparation before discussing the definition of success for the analysis. The analysis section then examines 7 different models including ARIMA(p,d,q) in terms of their prediction accuracy. The third section then discusses the modeling results of the most successful model on the validation set and the model performance. Finally the report concludes with a brief summary, its limitations, and suggestions for future work.
