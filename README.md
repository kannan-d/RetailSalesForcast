<b>Retail Mart Sales Forcast</b>

<h4>About Retail Mart</h4>

“Retail Mart” is an online store super giant having worldwide operations. It takes orders and delivers across the globe and deals with all the major product categories - consumer, corporate & home office.

The Mart caters to 7 different market segments and in 3 major categories. 

<h4>The Project Goal</h4>
<ul>
 <li>To help the Sales Department to build a 6 Month plan by giving a forcase of the sales and the demand for the next 6 months, that would help them manage the revenue and inventory accordingly. </li>

<li> To forecast at 7 market segements in 3 categories at granular level</li>

<li> To find the top 5 most profitable and consistent segment from these 21 and forecast the sales and demand for these segments.</li></ul>

<h4>Project Execution</h4>
<h5>Data Understanding:</h5>
The data has the transaction level data, where each row represents a particular order made on the online store. There are 24 attributes related to each such transaction. The “Market” attribute has 7-factor levels representing the geographical market sector that the customer belongs to. The “Segment” attribute represents the 3 segment that the customer belongs to.
<h5>Data preparation:</h5>
 <ul>
 <li>Segmented the whole dataset into the 21 subsets based on the market and the customer segment level. </li>
 <li>Converted the transaction-level data into a time series. </li>
 <li> Create an aggregate the 3 attributes - Sales, Quantity & Profit, over the Order Date to arrive at monthly values for these attributes. </li>
  <li> To find the 5 most profitable and consistently profitable segments,using the coefficient of variation of the Profit for all 21 market segments.</li></ul>
  <h5>Model building:</h5>
With the 2 most profitable segments, the next challenge is to forecast the sales and quantity for the next 6 months. Data Smoothening is done before classical decomposition and auto ARIMA is performed for forecasting.
<h5>Model evaluation:</h5>
The final best fit model is used to forecast the sales/demand for next 6 months using this model. The model is tested to predict the last initially separate out data of the last 6 months values from the dataset, after aggregating the transaction level data into the monthly data. The results is then checked against the seperated 6 months forecast using the out-of-sample figures.


<h4>Thank you for time! Visit Again for More Exciting Machine Learning and Data Science Projects.<h4>
