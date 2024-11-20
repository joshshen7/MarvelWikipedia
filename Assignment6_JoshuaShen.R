library(XML)
library(httr)


url <- "https://en.wikipedia.org/wiki/Marvel_Studios"

tabs <- GET(url)

wiki_table <- readHTMLTable(rawToChar(tabs$content), stringsAsFactors = F)
length(wiki_table)

highest_grossing <- wiki_table[[5]]
View(highest_grossing)
highest_grossing <- highest_grossing[, -1]
colnames(highest_grossing) <- c('Film', 'Year', 'BoxOfficeGross')
View(highest_grossing)
highest_grossing <- highest_grossing [-1, ]

#Fixing the misaligned data in the table
library(dplyr)
library(tidyr)
highest_grossing <- highest_grossing %>%
  mutate(
    BoxOfficeGross = ifelse(grepl('\\$', Year), Year, BoxOfficeGross),
    Year = ifelse(grepl('\\$', Year), NA, Year)
  )

highest_grossing[11, 'Year'] <- '2019'
highest_grossing[14, 'Year'] <- '2017'
highest_grossing[11, 'BoxOfficeGross'] <- '1,131,416,446'
highest_grossing[14, 'BoxOfficeGross'] <- '863,756,903'
View(highest_grossing)

#BarChart of Highest grossing films - Top 10 films
library(ggplot2)
library(scales)
highest_grossing <- highest_grossing %>%
  mutate(
    BoxOfficeGross = as.numeric(gsub('[\\$,]', '', BoxOfficeGross))
  )

top10 <- data.frame(highest_grossing[1:10, ])
View(top10)

ggplot(top10, aes(x = reorder(Film, BoxOfficeGross), y = BoxOfficeGross)) +
  geom_bar(stat = 'identity', fill = 'black') +
  labs(title = 'Top 10 Highest Marvel Grossing Films Worldwide',
       x = 'Film',
       y = 'National Box Office Gross') +
  theme_minimal() +
  coord_flip() + 
  scale_y_continuous(labels = label_comma()) +
  geom_text(aes(label = label_comma()(BoxOfficeGross)), vjust = -0.5, color = 'red', size = 3.5)

#Pie Chart of Highest Box Office $ by Year of the Top 10
top10_percentage <- top10 %>%
  group_by(Year) %>%
  summarise(TotalGross = sum(BoxOfficeGross, na.rm = TRUE))

top10_percentage$Percentage <- top10_percentage$TotalGross / sum(top10_percentage$TotalGross) * 100

ggplot(top10_percentage, aes(x = '', y = TotalGross, fill = factor(Year))) + 
  geom_bar(stat = 'identity', width = 1) +  # Make sure bars fill the pie chart
  coord_polar(theta = 'y') +  # Convert bar chart to pie chart
  labs(title = 'Percentage of Box Office Gross by Year - Top 10 Marvel Films') +
  geom_text(aes(label = paste(round(Percentage, 1), '%')), 
            position = position_stack(vjust = 0.5), 
            color = 'white') +  # Correctly place labels
  scale_y_continuous(labels = NULL) # Hide y-axis labels (TotalGross will not show)

write.csv(top10, "top10_marvelfilms.csv", row.names = FALSE)
write.csv(top10_percentage, "top10percentage_marvelfilms.csv", row.names = FALSE)
write.csv(highest_grossing, "highestgrossingmarvel.csv", row.names = FALSE)
