emp <- read.csv("data/ABSEmp.csv", fileEncoding="latin1")
colnames(emp)<- c("Desc", "Year", "Sizeband", "No of Enterprises", "Turnover", "aGVA", "Empl. Costs")
emp <- melt(emp, id= c("Desc", "Year", "Sizeband"))
