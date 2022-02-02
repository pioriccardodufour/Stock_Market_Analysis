# Stock_Market_Analysis

Yahoo_finance_historical_data_crawling takes the historical data from Yahoo Finance and saves an Excel file for each company.
"import" opens all the excel files together in a list, then creates a dataset with all the closing stock prices in the period of interest. 
"analysis" considers the previous dataset a creates a new one containing all the daily stock price growths or drops, in percentage.
"comparison" estimates the probability of a company stock value to increase more than x%; different companies are compared on the same chart.
"growth_after_drop_avg" defines the probability of a generic French stock price to growth or drop of a certain amount, after a DROP occurring.
