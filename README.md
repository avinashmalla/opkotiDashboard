# opkotiDashboard
## This is a work under progress. I am learning to extract, clean and present data. 
Analysing data extracted from a Finnish company OP's real estate website.

## Author
- Avinash Malla


The dashboard can be viewed with *app.R*. Requires *forDash.csv*.
In order to get fresh data,
1. Run *op-koti-python.ipynb*. This will extract data from [OP Koti](https://op-koti.fi/myytavat/asunnot). Data will be cleaned and stored in a csv file.
2. Run *op-koti-r.ipynb*. This will take the cleaned data from the python script's csv file and carry out further modifications and again store it in another csv file for the dashboard.
3. Run *app.R*. This is an R script for the dashboard. I have played around with visualisation and dynamic data selection for the table.
