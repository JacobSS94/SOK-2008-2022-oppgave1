#OPPGAVE 5
install.packages("tidyverse")
install.packages("readxl")
install.packages("ineq")
install.packages("openxlsx")
library(tidyverse)  
library(readxl) 
library(ineq)
library(openxlsx)

setwd("~/SKOLE/SOK- 2008 - DEN NORDIKSE MODELLEN")

Sys.setlocale(locale="no_NO")

decile_data <- read_excel("GCIPrawdatatest.xlsx", skip = 2)  
#The data is now in a 'tibble' (like a spreadsheet for R). Let's use the head function to look at the first few rows:
head(decile_data) 
#Now we use loops to complete our task. We begin by creating a new variable in our dataset, gini, which we initially set to 0 for all country-year combinations.
decile_data$gini <- 0
#Now we use a loop to run through all the rows in our dataset (country-year combinations). For each row we will repeat the Gini coefficient calculation from R walk-through 5.4 and save the resulting value in the gini variable we created.
#The function that calculates Gini coefficients from a vector of numbers is called Gini,and we apply it to the income deciles
# Give us the number of rows in decile_data
noc <- nrow(decile_data)

for (i in seq(1, noc)){
  # Go to Row I to get the decile data
  decs_i <- unlist(decile_data[i, 3:12])
  decile_data$gini[i] <- Gini(decs_i)
}
#With this code, we calculated 4,799 Gini coefficients without having to manually run the same command 4,799 times. We now look at some summary measures for the gini variable.
#First we use the subset function to select Nordic countries and save their data as temp_data. As an example, we have chosen four anglophone countries: the UK, the US, Ireland, and Australia.
temp_data <- subset(
  decile_data, Country %in% c("United States","Sweden","Finland","Norway", 
                              "Denmark"))
#Now we plot the data using ggplot.

ggplot(temp_data, 
       aes(x = Year, y = gini, color = Country)) +
  geom_line(size = 1) +
  theme_bw() +
  ylab("Gini") +
  ggtitle("Gini coefficients for Nordic countries")
#This example is based on great webpages of CORE: https://www.core-econ.org/doing-economics/book/text/05-03.html#extension-r-walk-through-55-calculating-gini-coefficients-for-all-countries-and-all-years-using-a-loop. Take a look!



# OPPGAVE 6
library(PxWebApiData)
library(gglorenz)
library(tidyverse)
library(hrbrthemes)
library(janitor)
library(ggplot2)
library(cowplot)
#Hvilke variabler som finnes i tabellen
variables <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                     returnMetaFrames = TRUE)
names(variables)


values <- ApiData("https://data.ssb.no/api/v0/en/table/12558/", 
                  returnMetaData = TRUE)

values[[1]]$values

values[[2]]$values 

values[[3]]$values

values[[4]]$values

values[[5]]$values
data <- ApiData("https://data.ssb.no/api/v0/en/table/12558/",
                Tid =c("2005","2020"), 
                Desiler=c("01", "02", "03" ,"04", "05", "06" ,"07", "08" ,"09", "10"), #Vi velger alle desiler
                InntektSkatt="00", 
                ContentsCode="VerdiDesil", 
                Region=c("5401","1902")) 

data <- data %>% 
  as.tibble()

data <- data %>% 
  clean_names()

Tromsø2005 <- data %>% 
  filter(x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$year == "2005")

Tromsø2020 <- data %>% 
  filter(x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$year == "2020")

PLOTT1 <- ggplot(Tromsø2005, aes(Tromsø05$x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$value)) +
  stat_lorenz(desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  labs(x = "Befolkning i prosent",
       y = "Inntekt i prosent",
       title = "Lorenzkurve 2005") +
  annotate_ineq(Tromsø2005$x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$value)


PLOTT2 <- ggplot(Tromsø2020, aes(Tromsø2020$x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$value)) +
  stat_lorenz(desc = FALSE) +
  coord_fixed() +
  geom_abline(linetype = "dashed") +
  theme_minimal() +
  hrbrthemes::scale_x_percent() +
  hrbrthemes::scale_y_percent() +
  labs(x = "Befolkning i prosent",
       y = "Inntekt i prosent",
       title = "Lorenzkurve 2020") +
  annotate_ineq(Tromsø20$x12558_households_by_region_income_before_after_tax_decil_group_contents_and_year$value)

plot_grid(PLOTT1, PLOTT2)
