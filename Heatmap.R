library("here")
library("tidyverse")
library("openxlsx")
library("magrittr")

# Load data
Data_Original <- read.xlsx(paste0(here(), "/Data.xlsx"), detectDates = T)

# Wrangle data
Data <-
  Data_Original %>%
  mutate(Tier = substr(Advice_title, 1, 6),
         Exposure_Site_Number = row_number(),
         Delay = Added_date_dtm - Exposure_date_dtm) %>%
  select(Exposure_Site_Number, Suburb:Added_time, Delay, Tier, everything())

# Create range of dates
Dates <- seq(as.Date("2021-07-08"), as.Date("2021-07-27"), "days")

# Get maximum and minimum number of all exposure sites
Max_Min <- 
  Data %>%
  count(Added_date_dtm, Exposure_date_dtm) %>%
  drop_na() %>%
  summarise(Max = max(n), Min = min(n))

# Create heatmap of all exposure sites
Exposure_Heatmap_All_Sites <-
  expand_grid(Exposure_Date = Dates, Added_Date = Dates) %>%
  distinct() %>%
  left_join(Data %>%
              count(Added_date_dtm, Exposure_date_dtm) %>%
              set_colnames(c("Added_Date", "Exposure_Date", "Number of Exposure Sites")),
            by = c("Added_Date", "Exposure_Date")) %>%
  ggplot(aes(as.factor(Added_Date), as.factor(Exposure_Date))) +
  geom_tile(aes(fill = `Number of Exposure Sites`), colour = "darkgray", size = 0.25) +
  scale_fill_gradient2(low = "white", mid = "#FFC300", high = "#C70039", na.value = "#fcf8f6", midpoint = Max_Min$Max/2, limits = c(Max_Min$Min, Max_Min$Max)) +
  geom_text(aes(label = `Number of Exposure Sites`)) +
  labs(x = "Exposure site announcement date", y = "Exposure site date", title = "Exposure sites in Victoria's 5th lockdown") +
  theme(legend.position = "right",
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white")) +
  annotate("text", x = 1, y = 0.75, label = "1. Exposed & announced before lockdown", hjust = 0, angle = 90) +
  annotate("text", x = length(Dates), y = 0.75, label = "2. Exposed before lockdown, announced after lockdown", hjust = 0, angle = 90) +
  annotate("text", x = length(Dates), y = length(Dates), label = "3. Exposed & announced after lockdown", hjust = 1, angle = 90) +
  annotate("segment", x = 8.5, xend = 8.5, y = 0.5, yend = length(Dates) + 0.5) +
  annotate("segment", x = 0.5, xend = length(Dates) + 0.5, y = 8.5, yend = 8.5)

# Save heatmap of all exposure sites to file
ggsave("Exposure_Heatmap_All_Sites.jpg", Exposure_Heatmap_All_Sites, width = 13, height = 13, dpi = 300)

# Create heatmap of Tier 1 exposure sites
Exposure_Heatmap_Tier1_Sites <-
  expand_grid(Exposure_Date = Dates, Added_Date = Dates) %>%
  distinct() %>%
  left_join(Data %>%
              filter(Tier == "Tier 1") %>%
              count(Added_date_dtm, Exposure_date_dtm) %>%
              set_colnames(c("Added_Date", "Exposure_Date", "Number of Exposure Sites")),
            by = c("Added_Date", "Exposure_Date")) %>%
  ggplot(aes(as.factor(Added_Date), as.factor(Exposure_Date))) +
  geom_tile(aes(fill = `Number of Exposure Sites`), colour = "darkgray", size = 0.25) +
  scale_fill_gradient2(low = "white", mid = "#FFC300", high = "#C70039", na.value = "#fcf8f6", midpoint = Max_Min$Max/2, limits = c(Max_Min$Min, Max_Min$Max)) +
  geom_text(aes(label = `Number of Exposure Sites`)) +
  labs(x = "Exposure site announcement date", y = "Exposure site date", title = "Tier 1 exposure sites in Victoria's 5th lockdown") +
  theme(legend.position = "right",
        axis.ticks = element_blank(),
        plot.title = element_text(hjust = 0.5),
        axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white", colour = "white")) +
  annotate("text", x = 1, y = 0.75, label = "1. Exposed & announced before lockdown", hjust = 0, angle = 90) +
  annotate("text", x = length(Dates), y = 0.75, label = "2. Exposed before lockdown, announced after lockdown", hjust = 0, angle = 90) +
  annotate("text", x = length(Dates), y = length(Dates), label = "3. Exposed & announced after lockdown", hjust = 1, angle = 90) +
  annotate("segment", x = 8.5, xend = 8.5, y = 0.5, yend = length(Dates) + 0.5) +
  annotate("segment", x = 0.5, xend = length(Dates) + 0.5, y = 8.5, yend = 8.5)

# Save heatmap of Tier 1 exposure sites to file
ggsave("Exposure_Heatmap_Tier1_Sites.jpg", Exposure_Heatmap_Tier1_Sites, width = 13, height = 13, dpi = 300)







