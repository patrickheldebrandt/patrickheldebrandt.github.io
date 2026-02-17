install.packages("tidyverse")
library(haven)
TEDS_2016 <- 
haven::read_dta("https://github.com/datageneration/home/blob/master/DataProgramming/data/TEDS_2016.dta?raw=true")

# Question 4: “What problems do you encounter when working with the dataset?”
TEDS_2016$PartyID <- factor(TEDS_2016$PartyID, labels=c("KMT","DPP","NP","PFP", "TSU", "NPP","NA"))
attach(TEDS_2016)
head(PartyID)
tail(PartyID)
# Q4 Missing values, multiple versions of variables (Age, age, Edu, edu, etc.), binary variables
# Q5 Drop rows missing key variables
# Q6


# Q7

# Q8
TEDS_2016$Tondu<-as.numeric(TEDS_2016$Tondu,labels=c("Unification now”,
“Status quo, unif. in future”, “Status quo, decide later", "Status quo
forever", "Status quo, indep. in future", "Independence now”, “No response"))

# bar chart
library(ggplot2)
ggplot(TEDS_2016, aes(x=as_factor(tondu_labeled)))+ geom_bar() + 
  labs(
    x= "Tondu (Taiwan Independence vs Unification)",
    y = "count",
    title = "distribution of Tondu preferences (TEDS_2016)"
  ) + 
  theme(axis.text.x = element_text(angle = 35, hjust = 1))