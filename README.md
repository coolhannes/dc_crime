## Crime in DC

This is a short and unofficial analysis of DC crime in 2023 through June compared to data since 2008. 

### Data 
Crime Data: https://crimecards.dc.gov/
I downloaded the data from the website above and saved it as a csv file locally. It takes a while, but you can download city-wide crime since 2008.

Census Estimates are pulled from the API. Get a key at: https://api.census.gov/data/key_signup.html and add it by running `census_api_key("YOUR KEY HERE")`.

### Analysis
I used R to get the average rate (per 1000 people based on the census ACS estimates) of crime in DC by police district, year, and month. I averaged pre-2023 data and used the standard deviation to estalish a 95% confidence interval. I then compared the 2023 data to the average and plotted it.