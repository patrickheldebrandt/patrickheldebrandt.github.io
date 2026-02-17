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
install.packages("descr")
library(dplyr)
library(ggplot2)
library(haven)
library(descr)

teds6 <- TEDS_2016 %>%
  select(Tondu, female, DPP, age, income, edu, Econ_worse)

teds6 <- teds6 %>%
  mutate(
    Tondu = as_factor(Tondu),
    female = as_factor(female),
    DPP = as_factor(DPP),
    edu = as_factor(edu),
    Econ_worse = as_factor(Econ_worse)
  )
    
    teds6 %>%
      group_by(Tondu) %>%
      summarise(n = n())

    
    tondu_by_female <- teds6 %>%
      filter(!is.na(Tondu), !is.na(female)) %>%
      group_by(female, Tondu) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(female) %>%
      mutate(pct = n / sum(n))
    
    ggplot(tondu_by_female, aes(x = female, y = pct, fill = Tondu)) +
      geom_col(position = "fill") +
      labs(x = "Female", y = "Proportion", title = "Tondu distribution by gender")
    
    tondu_by_dpp <- teds6 %>%
      filter(!is.na(Tondu), !is.na(DPP)) %>%
      group_by(DPP, Tondu) %>%
      summarise(n = n(), .groups = "drop") %>%
      group_by(DPP) %>%
      mutate(pct = n / sum(n))
    
    ggplot(tondu_by_dpp, aes(x = DPP, y = pct, fill = Tondu)) +
      geom_col(position = "fill") +
      labs(x = "DPP support", y = "Proportion", title = "Tondu distribution by DPP support")
# Q7

# Q8
TEDS_2016$Tondu<-as.numeric(TEDS_2016$Tondu,labels=c("Unification now”,
“Status quo, unif. in future”, “Status quo, decide later", "Status quo
forever", "Status quo, indep. in future", "Independence now”, “No response"))

TEDS_2016$Tondu <- factor(
  TEDS_2016$Tondu,
  labels = c(
    "Unification now",
    "Status quo, unif. in future",
    "Status quo, decide later",
    "Status quo forever",
    "Status quo, indep. in future",
    "Independence now",
    "No response"
  )
)

TEDS_2016 %>%
  filter(!is.na(Tondu)) %>%
  group_by(Tondu) %>%
  summarise(count = n())

freq(TEDS_2016$Tondu)

TEDS_2016 %>%
  filter(!is.na(Tondu)) %>%
  group_by(Tondu) %>%
  summarise(
    count = n(),
    proportion = count / sum(count)
  )

ggplot(TEDS_2016 %>% filter(!is.na(Tondu)),
       aes(x = Tondu)) +
  geom_bar() +
  labs(
    title = "Distribution of Taiwanese Identity (Tondu)",
    x = "Taiwanese identity",
    y = "Count"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(TEDS_2016 %>% filter(!is.na(Tondu)),
       aes(x = Tondu)) +
  geom_bar(aes(y = after_stat(prop), group = 1)) +
  labs(
    title = "Proportion of Taiwanese Identity Categories",
    x = "Taiwanese identity",
    y = "Proportion"
  ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
# bar chart
