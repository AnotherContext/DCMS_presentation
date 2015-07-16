spend <- read.csv("data/SpendingData.csv", fileEncoding="latin1")
colnames(spend)[5:13] <- c("2007-08", "2008-09", "2009-10", "2010-11", "2011-12", "2012-13", "2013-14", "2014-15", "2015-16")
total <- melt(spend, id=c("Resource.Type", "Expense.Type", "Department", "Broad.Activity"), 
              variable.name = "Year", value.name = "Spending.Value")
total$Spending.Value <- ifelse(is.na(total$Spending.Value), 0, total$Spending.Value)

spend_totals <- read.csv("data/SpendingTotals.csv", fileEncoding="latin1")
colnames(spend_totals) <- c("Year", "SpendingTotals")
