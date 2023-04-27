
#' ---
#' title: " UNICEF Assignment Story"
#' subtitle: "UNICEF data set Indicator 2 merged with metadata"
#' ### date: "27 April 2023"
#' author: "Ranjitha Rajkumar"
#' output: "html_document"
#' 
#' ---
#' # Story
#'
#' The main emphasis of this visualisation is the worldwide life expectancy with population and countries.
#' This makes it evident that a country's life expectancy increases with its GDP, which suggests that better educated and competent workers are responsible for raising life expectancy rates.
#' According to graphics mentioned towards the end, Norway is the best-performing nation in terms of GDP, followed by Qatar, the United States, Australia, and Iceland. 
#' The population of a country and life expectancy are inversely correlated, as seen by the line graph that compares them. 
#' The third bar graph demonstrates the top 10 countries with GDP, demonstrating that life expectancy increases as GDP increases. 
#' The fourth graph, which shows the time series plot change in observation value and GDP over the period of time, is crucial for understanding whether life expectancy has increased or decreased over time.
#' 
#' **Note: The data is biased which is shown in the scatter plot.**
#'  
#'  *This report consists of four charts which are mentioned below.*
#'  
#'  * Scatter plot with regression line
#'  * World Map 
#'  * Bar chart
#'  * Time series 



# install.packages("stringr")
# install.packages("Hmisc")
# install.packages("forcats")
# install.packages("ggthemes")


library(ggplot2)
library(maps)
library(gridExtra)
library(plotly)
library("scales") 
library("stringr") 
library("Hmisc") 
library("forcats") 
library("ggthemes") 
library(RColorBrewer)
library(ggiraph)
library(tidyr)
library(dplyr)
library(ggrepel)
library(tidyverse)


#--Scatter plot

data_metadata <- read.csv("/cloud/project/unichef_data_indicator_2 metadata groupd.csv")
View(data_metadata)

data_metadata$gdp_in_10k <- data_metadata$GDP.per.capita..constant.2015.US.. / 10000
data_metadata$pop_mil <- data_metadata$Population..total / 1000000
data_metadata$obs_val_in_10k <- data_metadata$obs_true_val / 10000


fit <- lm(obs_true_val ~ gdp_in_10k, data = data_metadata)

data_metadata %>% 
  plot_ly(x = ~gdp_in_10k) %>% 
  add_markers(y = ~obs_true_val) %>% 
  add_lines(x = ~gdp_in_10k, y = fitted(fit))%>%
  layout(showlegend = F)%>%
  layout(title = 'Scatter plot of Population and Observation Values', plot_bgcolor = "#e5ecf6")

#-----
#world map

ggplot_worldData <- map_data('world') %>% fortify
View(ggplot_worldData)

world_dataframe <- select(data_metadata, region = Country, "obs_value" = obs_true_val)
View(world_dataframe)

world_dataframe_new <- left_join(world_dataframe, ggplot_worldData, by = "region")
View(world_dataframe_new)

p <- ggplot() +
  geom_map(data = ggplot_worldData, map = ggplot_worldData,
           aes(x = long, y = lat, group = group, map_id=region),
           fill = "white", colour = "#7f7f7f", size=0.5) + 
  geom_map(data = world_dataframe, map=ggplot_worldData, 
           aes(fill=obs_value,map_id=region),
           colour="#7f7f7f", size=0.5) + geom_text_repel() +
  coord_map("rectangular", lat0=0, xlim=c(-180,180), ylim=c(-60, 90)) +
  scale_fill_continuous(low="#cccce6", high="#1a1a8d", guide="colorbar") +
  scale_y_continuous(breaks=c()) +
  scale_x_continuous(breaks=c()) +
  labs(fill="Obs Value", title="World Map", x="", y="") + 
  theme_bw()
p

#---bar chart


data_metadata_yr_grp <- data_metadata %>% group_by(Country) %>% 
  summarise(avg_obs_true_val = mean(obs_true_val),
            avg_gdp = mean(GDP.per.capita..constant.2015.US..),
            .groups = 'drop')
View(data_metadata_yr_grp)

# Change the colors of individual bars (default fill colors)

data <- data_metadata_yr_grp[with(data_metadata_yr_grp,order(-avg_gdp)),]

data <- data[1:10,]
View(data)

fig <- ggplot(data, aes(x=Country, y=avg_gdp, fill=Country)) + 
  geom_bar(stat="identity") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5)) +
  ggtitle("Fig. B: Rotated x-axis")

fig + theme(legend.position = "none")

#----time series

#time series
ts <- select(data_metadata, time_period, pop_mil, obs_true_val, GDP.per.capita..constant.2015.US..)
View(ts)

ts_group <- ts %>% group_by(time_period) %>% 
  summarise(avg_pop_mil= mean(pop_mil),
            avg_obs_true_val = mean(obs_true_val),
            avg_gdp = mean(GDP.per.capita..constant.2015.US..),
            .groups = 'drop')
view(ts_group)

plot_obs<- ggplot(data = ts_group, aes(x = time_period, y = avg_obs_true_val))+ geom_line(color = "#1a1a8d", size = 2)+
  theme(panel.background = element_rect(fill='#e6e6f2'))

plot_gdp<- ggplot(data = ts_group, aes(x = time_period, y = avg_gdp))+ geom_line(color = "#1a1a8d", size = 2)+
  theme(panel.background = element_rect(fill='#e6e6f2'))

grid.arrange(plot_obs,plot_gdp, ncol = 1)
