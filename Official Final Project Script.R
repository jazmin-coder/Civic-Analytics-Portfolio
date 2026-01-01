library(scales)
library(ggrepel)
library(patchwork)
library(tibble)
library(tidyverse)
library(haven)
library(skimr)
library(dplyr)
library(tidyr)
library(psych)
library(ggplot2)
library(naniar)
library(scales)
library(stringr)
library(purrr)
library(RColorBrewer)
library(treemapify)
library(wesanderson)
library(sf)
library(showtext)
library(officer)
library(flextable)

setwd("C:/Users/jazmi/OneDrive/Desktop/PA402 Lab/Research Project/Project Scripts")

#Adding the font Merriweather for the graphs
font_add_google("Merriweather", "merriweather")
showtext_auto()

#Opening files and selecting variables to analyze
visits2019 <- read.csv("Libraries_-_2019_Visitors_by_Location.csv")
visits2020 <- read.csv("Libraries_-_2020_Visitors_by_Location.csv")
visits2021 <- read.csv("Libraries_-_2021_Visitors_by_Location.csv")
visits2022 <- read.csv("Libraries_-_2022_Visitors_by_Location.csv")
visits2023 <- read.csv("Libraries_-_2023_Visitors_by_Location.csv")

visits2019 <- visits2019 %>% 
  select(LOCATION, ADDRESS, ZIP, YTD)

visits2020 <- visits2020 %>% 
  select(BRANCH, ADDRESS, ZIP, YTD)

visits2021 <- visits2021 %>% 
  select(BRANCH, ADDRESS, ZIP, YTD)

visits2022 <- visits2022 %>% 
  select(BRANCH, ADDRESS, ZIP, YTD)

visits2023 <- visits2023 %>% 
  select(BRANCH, ADDRESS, ZIP, YTD)


#data transformation: removing non-numerical characters in values and preparing
#datasets for full merge

visits2019 <- visits2019 %>%
  mutate(
    YTD = gsub(",", "", YTD), 
    YTD = gsub("[^0-9]", "", YTD),     
    YTD = as.numeric(YTD)              
  )

visits2019 <- visits2019 %>% 
  rename(YTD_2019 = YTD,
         BRANCH = LOCATION)

visits2020 <- visits2020 %>%
  mutate(
    YTD = gsub(",", "", YTD),     
    YTD = gsub("[^0-9]", "", YTD),     
    YTD = as.numeric(YTD)              
  )

visits2020 <- visits2020 %>% 
  rename(YTD_2020 = YTD)

visits2021 <- visits2021 %>%
  mutate(
    YTD = gsub(",", "", YTD),     
    YTD = gsub("[^0-9]", "", YTD),     
    YTD = as.numeric(YTD)              
  )

visits2021 <- visits2021 %>% 
  rename(YTD_2021 = YTD)

visits2022 <- visits2022 %>%
  mutate(
    YTD = gsub(",", "", YTD),     
    YTD = gsub("[^0-9]", "", YTD),     
    YTD = as.numeric(YTD)              
  )

visits2022 <- visits2022 %>% 
  rename(YTD_2022 = YTD)

visits2023 <- visits2023 %>%
  mutate(
    YTD = gsub(",", "", YTD),     
    YTD = gsub("[^0-9]", "", YTD),     
    YTD = as.numeric(YTD)              
  )

visits2023 <- visits2023 %>% 
  rename(YTD_2023 = YTD)

datasets <- list(visits2019, visits2020, visits2021, visits2022, visits2023)

combined_visits <- reduce(datasets, full_join, by = "BRANCH")

combined_visits <- combined_visits %>% 
  select(BRANCH, ADDRESS, ZIP, YTD_2019, YTD_2020, YTD_2021, YTD_2022, YTD_2023)

combined_visits <- combined_visits %>% 
  mutate(
    average = (YTD_2019 + YTD_2020 + YTD_2021 + YTD_2022 + YTD_2023)/5
  )

combined_visits <- combined_visits [-81, ]
combined_visits <- combined_visits %>% 
  rename(address = ADDRESS)

#Categorizing library addresses by Chicago community area      
chicago_addresses <- data.frame(
  address = c(
    "3401 W. Foster Ave.","955 E. 131st St.","5055 S. Archer Ave.","5615 W. Race Ave.",
    "6100 W. Irving Park Rd.","8148 S. Stony Island Ave.","2111 W. 47th St.","1962 W. 95th St.",
    "1226 W. Ainslie St.","4904 S. Lake Park Ave.","1350 W. 89th St.","4314 S. Archer Ave.",
    "1701 N. Milwaukee Ave.","5630 N. Lincoln Ave.","642 W. 43rd St.","3647 S. State St.",
    "6120 S. Kedzie Ave.","2100 S. Wentworth Ave.","6423 W. 63rd Pl.","731 E. 63rd St.",
    "3400 S. Halsted St.","733 N. Kedzie Ave.","3353 W. 13th St.","7455 W. Cornelia Ave.",
    "5331 W. Devon Ave.","6000 N. Broadway St.","2807 W. 55th St.","6871 W. Belden Ave.",
    "6348 S. Archer Ave.","1000 E. 73rd St.","4801 S. Michigan Ave.","400 S. State St.",
    "3048 E. 130th St.","1605 N. Troy St.","4024 N. Elston Ave.","5363 W. Lawrence Ave.",
    "2401 E. 100th St.","6151 S. Normal Blvd.","3436 S. King Dr.","115 S. Pulaski Rd.",
    "1659 W. Melrose St.","1150 W. Fullerton Ave.","1336 W. Taylor St.","2311 S. Kedzie Ave.",
    "3030 W. Fullerton Ave.","1805 S. Loomis St.","6 S. Hoyne Ave.","4400 W. Lawrence Ave.",
    "1915 W. 35th St.","644 W. Belmont Ave.","11010 S. Kedzie Ave.","310 W. Division St.",
    "5724 W. North Ave.","4300 W. North Ave.","6800 N. Western Ave.","7454 W. Balmoral Ave.",
    "5108 W. Belmont Ave.","11001 S. Indiana Ave.","6083 N. Northwest Hwy.","6907 N. Clark St.",
    "4101 W. 79th St.","5440 S. Racine Ave.","9055 S. Houston Ave.","2505 E. 73rd St.",
    "4455 N. Lincoln Ave.","7506 S. Racine Ave.","2708 S. Pulaski Rd.","929 W. Buena Ave.",
    "3710 E. 106th St.","11071 S. Hoyne Ave.","163 E. Pearson Street","3104 N. Narragansett Ave.",
    "4856 W. Chicago Ave.","1745 W. 63rd St.","4020 W. 63rd St.","122 N. Aberdeen St.",
    "830 W. 119th St.","1625 W. Chicago Ave.","415 East 79th St.","9525 S. Halsted St.",
    "8530 S. Kedzie Ave."
  ),
  community_area = c(
    "Albany Park","Riverdale","Archer Heights","Austin","Dunning","Avalon Park","New City","Beverly",
    "Uptown","Kenwood","Auburn Gresham","Brighton Park","Logan Square","Lincoln Square","Fuller Park","Douglas",
    "Chicago Lawn","Armour Square","Clearing","Woodlawn","Bridgeport","Humboldt Park","North Lawndale","Dunning",
    "Forest Glen","Edgewater","Gage Park","Montclare","Garfield Ridge","Greater Grand Crossing","Grand Boulevard","Loop",
    "Hegewisch","Humboldt Park","Irving Park","Jefferson Park","South Deering","Englewood","Douglas","West Garfield Park",
    "Lake View","Lincoln Park","Near West Side","South Lawndale","Logan Square","Lower West Side","Near West Side","Albany Park",
    "McKinley Park","Lake View","Mount Greenwood","Near North Side","Austin","Humboldt Park","West Ridge","Norwood Park",
    "Belmont Cragin","Roseland","Norwood Park","Rogers Park","Ashburn","Englewood","South Chicago","South Shore",
    "North Center","Englewood","South Lawndale","Uptown","East Side","Morgan Park","Near North Side","Belmont Cragin",
    "Austin","Englewood","West Lawn","Near West Side","West Pullman","West Town","Chatham","Washington Heights","Ashburn"
  ),
  area_number = c(
    14,54,57,25,17,45,61,72,3,39,69,58,22,4,34,35,66,34,64,42,
    60,23,29,17,12,77,63,18,56,69,38,32,55,23,16,11,51,68,35,26,
    6,7,28,30,22,31,28,14,59,6,73,8,25,23,2,10,19,49,10,1,70,68,
    46,43,5,68,30,3,52,75,8,19,25,68,65,28,53,24,44,73,70
  )
)

totalvisits_areas <- merge(combined_visits, chicago_addresses, by = "address", all.x = "TRUE")

totalvisits_areas <- totalvisits_areas %>% 
  relocate(community_area, area_number, .before = ZIP)

#Transforming Hardship Index dataset and merging
hardship_index <- read.csv("Chicago_Hardship Index_2019-23.csv")
hardship_index <- hardship_index %>% 
  rename(area_number = Area.Number,
         hardship_index = Hardship.Index.Value)

#Cleaning the totalvisits dataset
totalvisits_areas$community_area[73] <- "Auburn Gresham"
totalvisits_areas$area_number[73] <- 71
totalvisits_areas$community_area[52] <- "New City"
totalvisits_areas$area_number[52] <- 61
totalvisits_areas$community_area[15] <- "West Englewood"
totalvisits_areas$area_number[15] <- 67
totalvisits_areas$area_number[63] <- 37
totalvisits_areas$community_area[14] <- "West Town"
totalvisits_areas$area_number[14] <- 24                                
totalvisits_areas$community_area[49] <- "Portage Park"                              
totalvisits_areas$area_number[49] <- 15
totalvisits_areas$community_area[32] <- "North Park"
totalvisits_areas$area_number[32] <- 13
totalvisits_areas$community_area[10] <- "Washington Heights"
totalvisits_areas$area_number[10] <- 73
totalvisits_areas$area_number[3] <- 74

total_df <- merge(totalvisits_areas, hardship_index, by = "area_number", all.x = TRUE)

#Plotting the data
plot_data <- total_df %>% 
  select(area_number, BRANCH, Community, average, hardship_index)

#visualization prep
df_clean <- plot_data %>%
  mutate(Side = case_when(
  Community %in% c("Rogers Park", "West Ridge", "Uptown", "Lincoln Square", 
                   "North Center", "Lake View", "Lincoln Park", "Near North Side", 
                   "Edgewater", "Forest Glen", "North Park", "Albany Park", 
                   "Jefferson Park", "Norwood Park", "Dunning", "Irving Park", "Portage Park") ~ "North Side",
  
  Community %in% c("Austin", "Humboldt Park", "West Town", "West Garfield Park", 
                   "Near West Side", "North Lawndale", "South Lawndale", 
                   "Lower West Side", "Logan Square", "Belmont Cragin", "Montclare") ~ "West Side",
  TRUE ~ "South Side" 
))

df_clean$Side[23] <- "North Side"

df_clean <- df_clean %>% 
  filter(BRANCH != "Harold Washington Library Center")

outliers <- df_clean %>% 
  filter(BRANCH %in% c("Sulzer Regional Library", "Chinatown"))

df_clean <- df_clean %>%
  mutate(Side = factor(Side, levels = c("North Side", "West Side", "South Side")))

#boxplot
ggplot(df_clean, aes(x = Side, y = average)) +
  geom_boxplot(fill = "gray80", outlier.shape = NA, alpha = 0.5, color = "grey40", width = 0.5) +
  geom_jitter(aes(fill = hardship_index), 
              color = "white", # White outline makes dots pop against each other
              shape = 21,      # Shape 21 allows for both fill and outline
              size = 3.5, 
              width = 0.2, 
              alpha = 0.9) +
  geom_text_repel(
    data = outliers, aes(label = BRANCH), size = 5, family = "merriweather",
    color = "black", box.padding = 0.5, max.overlaps = Inf) +
  scale_fill_gradientn(
    colours = c("#7196A4","#D5AF8B","#C93C46"),
    name = "Hardship\nIndex"
  ) +
  scale_y_continuous(
    labels = label_number(scale = 1/1000), limits = c(0, 250000),
                    breaks = seq(0, 300000, by = 100000)) +
  theme_minimal() +
  labs(
    title = "Distribution of Library Visits by Area",
    subtitle = "Excludes the Harold Washington Library Center due to extreme visitor scale (average yearly visits in thousands)",
    x = NULL,
    y = "Average Yearly Visits",
    fill = "Hardship Index",
    caption = "Source: Chicago Data Portal and Great Cities Institute (2019–2023)") +
  theme(
    legend.position = "bottom",
    legend.title = element_text(size = 16, family = "merriweather"),
    legend.text = element_text(size = 16, family = "merriweather"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_text(size = 16, family = "merriweather"),
    axis.title.y = element_text(size = 16, family = "merriweather"),
    axis.text.x = element_text(color = "black", size = 16, family = "merriweather"),
    axis.text.y = element_text(color = "black", size = 16, family = "merriweather"),
    plot.title = element_text(size = 30, face = "bold", color = "black", family = "merriweather"),
    plot.caption = element_text(hjust = 0, size = 14, color = "black", margin = margin(t = 15), family = "merriweather"),
    plot.subtitle = element_text(size = 16, family = "merriweather"))

#scatterplot
ggplot(df_clean, aes(x = hardship_index, y = average, color = Side)) +
  geom_point(alpha = 0.8, size = 3) +
  geom_smooth(aes(x = hardship_index, y = average), 
              method = "lm", se = FALSE, color = "grey8", linewidth = 0.5) +
  geom_text_repel(
    data = outliers, aes(label = BRANCH), size = 5, family = "merriweather",
    color = "black", box.padding = 0.5, max.overlaps = Inf) +
  scale_y_continuous(labels = label_number(scale = 1/1000), limits = c(0, 300000),
    breaks = seq(0, 300000, by = 100000)) +
  scale_color_manual(values = c("North Side" = "#7196A4",
    "South Side" = "#C93C46", "West Side"  = "#D5AF8B")) +
  theme_minimal() +
  labs(
    title = "Does Hardship Affect Visits to the Chicago Public Library?",
    subtitle = "Excludes the Harold Washington Library Center due to extreme visitor scale (Average yearly visits in thousands)",
    x = "Hardship Index",
    y = "Average Yearly Visits",
    color = NULL,
    caption = "Source: Chicago Data Portal and Great Cities Institute (2019–2023)") +
  theme(
    legend.position = "top",
    legend.text = element_text(size = 16, family = "merriweather"),
    panel.grid = element_blank(),
    axis.line = element_line(color = "black"),
    axis.ticks = element_line(color = "black"),
    axis.title.x = element_text(size = 16, family = "merriweather"),
    axis.title.y = element_text(size = 16, family = "merriweather"),
    axis.text.x = element_text(color = "black", size = 16, family = "merriweather"),
    axis.text.y = element_text(color = "black", size = 16, family = "merriweather"),
    plot.title = element_text(size = 30, face = "bold", color = "black", family = "merriweather"),
    plot.caption = element_text(hjust = 0, size = 14, color = "black", margin = margin(t = 15), family = "merriweather"),
    plot.subtitle = element_text(size = 16, family = "merriweather"))

##final results table
cor_by_side <- df_clean %>%
  group_by(Side) %>%
  summarise(
    cor_value   = cor(hardship_index, average, use = "complete.obs"),
    p_value     = cor.test(hardship_index, average)$p.value,
    avg_visits  = mean(average, na.rm = TRUE),
    avg_hardship = mean(hardship_index, na.rm = TRUE)
  )

results_table <- bind_rows(
  data.frame(
    Region      = "Citywide",
    Correlation = round(cor_citywide$estimate, 2),
    p_value     = signif(cor_citywide$p.value, 3),
    avg_visits  = round(mean(df_clean$average, na.rm = TRUE), 0),
    avg_hardship = round(mean(df_clean$hardship_index, na.rm = TRUE), 1)
  ),
  cor_by_side %>%
    rename(Region = Side, Correlation = cor_value) %>%
    mutate(
      Correlation = round(Correlation, 2),
      p_value     = signif(p_value, 3),
      avg_visits  = round(avg_visits, 0),
      avg_hardship = round(avg_hardship, 1)
    )
)
