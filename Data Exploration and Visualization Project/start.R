#Assume all the packages are already installed
library(collapsibleTree)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(plotly)
library(treemap)
library(dplyr)
library(gridBase)
library(RColorBrewer)

#Read data
data = read.csv("IndustryData.csv", stringsAsFactors = TRUE)

#Group by industry type, industry name and year
tree = aggregate(data,
                 by = list(Type = data$TypeName,
                           Industry = data$Industry,
                           Year = data$Year), 
                 FUN = mean)

#Pick up useful columns
tree = subset(tree, select = c(Type, Industry, Year, ROA, Asset))

#Calculate the growth rate
tree=cbind(tree,0)
names(tree)[6] = "Rate"

for(i in tree$Industry){
  for(j in 2009:2018){
    tree[tree$Industry == i & tree$Year == j, 6] = 
      tree[tree$Industry == i & tree$Year == j, 5]/
      tree[tree$Industry == i & tree$Year == 2009, 5]
  }
}

#Set different color for different industry type
tree$Color = tree$Type
levels(tree$Color) = colorspace::rainbow_hcl(18)

#Run shiny app
runApp()
