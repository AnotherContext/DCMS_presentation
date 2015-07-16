#check for NA, "-", blanks etc, or will be convered to factors
mgIncome <- read.csv("data/MGIncome.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
colnames(mgIncome) <- c("Income.Type", "Institute","2007-08", "2008-09", "2009-10", "2010-11", "2011-12", "2012-13")
inc <- melt(mgIncome, id=c("Income.Type", "Institute"), variable.name = "Year", value.name = "Value")

mgVisit <- read.csv("data/MGVisits.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
colnames(mgVisit) <- c("Institute", "2007-08", "2008-09", "2009-10", "2010-11", "2011-12", "2012-13")
visit <- melt(mgVisit, id=c("Institute"), variable.name = "Year", value.name = "Value")

tourism <- read.csv("data/Tourism.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
tourism <- tourism[ tourism$Year != "2015", c(1,3)]
colnames(tourism) <- c("Year", "Visits")
tourism$Year <- as.factor(tourism$Year)

sport1 <- read.csv("data/SportImpactWider.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
sport2 <- read.csv("data/SportImpact.csv", stringsAsFactors=FALSE, fileEncoding="latin1")

movie <- read.csv("data/UKBoxOffice.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
colnames(movie) <- c("Year", "BoxOffice", "Change", "Studio", "Indep")

com <- read.csv("data/Telecom.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
gambling <- read.csv("data/Gambling.csv", stringsAsFactors=FALSE, fileEncoding="latin1")

net1 <- read.csv("data/InternetConnection.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
net2 <- read.csv("data/InternetAccess.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
license <- read.csv("data/EntLicenses.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
abs <- read.csv("data/ABS.csv", stringsAsFactors=FALSE, fileEncoding="latin1")
