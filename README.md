
# prepmod-scrape

This scrapes COVID-19 vaccination clinics published on the state of Alaska's [Prepmod website](https://cw2-alaska-production.herokuapp.com/clinic/search). The prepmod.R script generates a CSV after parsing the html into an R dataframe. It can further push that cleaned data to Amazon S3. That output is further published to the web as [anchoragecovidvaccine.org](https://anchoragecovidvaccine.org/) as an interactive HTML table. 

## Data Source: 

The vaccine clinic information is sourced from what is published on the DHSS [PrepMod](https://cw2-alaska-production.herokuapp.com/clinic/search). The information refreshes every 10 minutes and the exact number of appointments may be different depending on how quickly people sign up for the appointments. Users need to refresh the page to bring in new data. This is only a subset of the providers that are administering vaccines in Alaska.

## How to use the html page: 

The current [anchoragecovidvaccine.org](https://anchoragecovidvaccine.org/) shows a snapshot of the available appointments for published clinics on the Alaska DHSS Prepmod portal. The table is sortable by each of the columns. Click on the appts_available column to sort by the number of appointments available. There is also a search bar on the right side of the page.

 This is a project by the Anchorage Innovation Team. This is an experimental website and may have incomplete or missing data.
