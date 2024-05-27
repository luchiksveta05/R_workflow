#setwd("~/perso/tmp/project/penguins")

#data <- read.csv("https://raw.githubusercontent.com/holtzy/R-graph-gallery/master/DATA/data_2.csv")
data <- read.table(file = file.path(here::here(), "Input", "data.csv"), sep=",", header = TRUE)
data2 <- read_excel(file.path(here::here(), "Input", "data.xlsx"))
summary(data2)

data2 <- data2 %>%
  mutate (bill_length_mm = as.numeric(bill_length_mm))%>%
  mutate (bill_depth_mm = as.numeric(bill_length_mm))

print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Torgersen")$bill_length_mm), 2))
print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Biscoe")$bill_length_mm), 2))
print(round(mean(subset(na.omit(data), species == "Adelie" & island == "Dream")$bill_length_mm), 2))

# Plot
penguins_clean <- na.omit(data2)
plot(penguins_clean$bill_length_mm, penguins_clean$bill_depth_mm, type = "n", xlab = "Bill Length (mm)", ylab = "Bill Depth (mm)", main = "Penguin Bill Dimensions")
points(
  penguins_clean$bill_length_mm[penguins_clean$species == "Adelie"], penguins_clean$bill_depth_mm[penguins_clean$species == "Adelie"],
  col = "red", pch = 16
)
points(penguins_clean$bill_length_mm[penguins_clean$species == "Chinstrap"], penguins_clean$bill_depth_mm[penguins_clean$species == "Chinstrap"], col = "green", pch = 17)
points(penguins_clean$bill_length_mm[penguins_clean$species == "Gentoo"],
  penguins_clean$bill_depth_mm[penguins_clean$species == "Gentoo"],
  col = "blue", pch = 18
)
legend("topright",
  legend = unique(penguins_clean$species),
  col = c(
    "red",
    "green",
    "blue"
  ), pch = c(16, 17, 18)
)

# Data analysis with dplyr and ggplot2
library(dplyr)
library(ggplot2)

penguins_clean2 <- data2 %>%
  mutate (bill_length_mm = as.numeric(bill_length_mm))%>%
filter(if_all(everything(), ~ !is.na(.)))

penguins_clean_mean <- penguins_clean2 %>%
  group_by(island)%>%
  summarize(bill_length_mm = mean(bill_length_mm))

ggplot(penguins_clean2, aes(x = bill_length_mm, y = bill_depth_mm, color = species, shape = species)) +
  geom_point(size = 3) +
  labs(
    title = "Penguin Bill Dimensions",
    x = "Bill Length (mm)",
    y = "Bill Depth (mm)"
  ) +
  scale_color_manual(values = c("Adelie" = "red", "Chinstrap" = "green", "Gentoo" = "blue")) +
  scale_shape_manual(values = c("Adelie" = 16, "Chinstrap" = 17, "Gentoo" = 18)) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.title = element_blank(),
    legend.position = c(0.95, 0.95),
    legend.justification = c("right", "top")
  )

#Read excel files
## Load the package (not that it is not part of the core tidyverse!)
library(readxl)

## Read a file
data <- read_excel(file.path(here::here(), "Input", "data.xlsx"))
