# DCMS presentation
An overview of the Department for Culture, Media and Sport's activities, performance and trends over time.

What is it about?
-------
It is a web app and a simple overview of the DCMS' spending, resources, and performance trends that enables 
direct visual analysis of different types of activities and expenditure in a “compact” and easy way. 

To help identify possible trends and provide insight into the datasets, I use descriptive statistics and 
interactive visual tools that allow to dynamically explore and summarise the main characteristics of the dataset.

You can view the running app [here](https://wildsquare.shinyapps.io/DCMS_presentation_ext). It allows to create 
and customise graphical data summaries, e.g., barplots and boxplots in just a few clicks without prior knowledge of 
R (statistical programming language used in this project).

I used R and RStudio’s shiny framework for R. The graphical abilities of R and its packages ggplot2, shiny 
and shinyapps make it a very good choice for any data analysis and visualisation, while shiny enables turning 
statistical analysis into an interactive web application without the knowledge of HTML or Javascript. 
On the top of that, it adjusts the layout of the app for best viewing experience on desktops, tablets and smartphones.

One more thing I'd like to mention is that it is a self-learning project that I am doing it in my spare time.


Context 
---------
The datasets are based on the DCMS' Annual Reports, statistics published at gov.uk, and number of other governmental 
sources that are listed below. 

Data Sources: 

- [Department for Culture, Media and Sport](https://www.gov.uk/government/organisations/department-for-culture-media-sport)
    + [Statistics: GOV.UK](https://www.gov.uk/government/statistics?departments[]=department-for-culture-media-sport);
    + [Taking Part Survey](https://www.gov.uk/government/statistics/taking-part-201415-quarter-4-statistical-release);
    + [Annual Accounts](https://www.gov.uk/government/publications/dcms-annual-report-and-accounts-2013-14);
- [Annual Business Survey- ONS Data Section](http://www.ons.gov.uk/ons/datasets-and-tables/index.html);
- [Economic Value of Sport- SportEngland](http://www.sportengland.org/research/economic-value-of-sport-local-model/);
- [Industry Statistics- Gambling Commission](http://www.gamblingcommission.gov.uk/Gambling-data-analysis/statistics/Industry-statistics.aspx);
- [BFI Statistical Booklet](http://www.bfi.org.uk/education-research/film-industry-statistics-research/statistical-yearbook);
- [OFCOM Data Tables](http://stakeholders.ofcom.org.uk/market-data-research/market-data/communications-market-reports/tables/q4-2014/).


Prerequisites
--------
- R version 3.2.1 (2015-06-18)
- RStudio Version 0.99.447 – © 2009-2015 RStudio, Inc.
- Platform: i386-w64-mingw32/i386 (32-bit)

Attached base packages: grid, stats, graphics, grDevices, utils, datasets, methods, base     
Other attached packages: RColorBrewer_1.1-2, scales_0.2.5, reshape2_1.4.1, ggplot2_1.0.1, rmarkdown_0.7, 
shinyapps_0.4.1.4, shiny_0.12.1      

To extract tables from hundreds of pages pdf files I used a very nice and easy programme: 
[Tabula](https://github.com/tabulapdf/tabula). 


And finally, where to find it?
---------
The online presentation is available [here](https://wildsquare.shinyapps.io/DCMS_presentation).
It works best with Firefox and Google Chrome web browsers. Please, don’t try to load it with an older version of 
Internet Explorer as it won't correctly render iframes. 

Alternatively, [see it as a website](https://wildsquare.shinyapps.io/DCMS_presentation_ext). 
It can be loaded with any browser. It is also optimal for responsive viewing using any (mobile) device. 

For more results, please refer to the Excel "companion" file available at [OneDrive](http://1drv.ms/1fAsxIY). 


Questions? 
----------
Email me at [anothercontext@hotmail.com](mailto:anothercontext@hotmail.com). Alternatively, find me 
[@iAnotherContext](http://twitter.com/ianothercontext).

<sup>[Email](mailto: anothercontext@hotmail.com) - 
[Twitter](https://twitter.com/ianothercontext) - 
[Facebook](https://www.facebook.com/anothercontext) - 
[GitHub](https://github.com/AnotherContext) - 
[OneDrive](http://1drv.ms/1fAsxIY)</sup>
