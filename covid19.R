library(data.table)
library(ggplot2)
library(ggthemes)
library(gridExtra)
library(scales)
library(readr)
library(dplyr)
library(plotly)

# Get data from John Hopkins public repo
covid <- fread("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", fill=TRUE)
# Map and province info not required
covid$`Province/State` <- NULL
covid$Lat <- NULL
covid$Long <- NULL

# Reshape
covLong <- melt(covid, id.vars = "Country/Region")
# Parse dates
covLong$variable <- sub("^([0-9])/","0\\1/", as.character(covLong$variable))
covLong$variable <- sub("/([0-9])/","/0\\1/", as.character(covLong$variable))
covLong$date <- parse_date(as.character(covLong$variable), format = "%m/%d/%y")

# Rename and delete for convenience
covLong$variable <- NULL
covLong$Country <- covLong$`Country/Region`
covLong$`Country/Region` <- NULL

# No. of cases by country
df <- covLong %>% group_by(Country,date) %>% tally(value)
# Countries to consider
df <- subset(df, df$Country == "Germany" | df$Country == "Italy" | df$Country == "Spain" | df$Country == "US")
# Plot only if cases > 0
df <- subset(df, df$n  > 0)

# Manual alignment of case growth curves
df$axis <- c(1:sum(df$Country == "Germany"), 1:sum(df$Country == "Italy"), 1:sum(df$Country == "Spain"), 1:sum(df$Country == "US"))
df$axis[df$Country == "Italy"] <- df$axis[df$Country == "Italy"] + 11
df$axis[df$Country == "Spain"] <- df$axis[df$Country == "Spain"] + 6
df$axis[df$Country == "US"] <- df$axis[df$Country == "US"] - 7

# Reorder for plot
df$Country <- reorder(df$Country, -df$n, sum)

# Aligned plot for n > 20
p1 <- ggplot(subset(df, axis > 20), aes(x=axis, y=n, group=Country, color=Country, shape= Country)) + 
  geom_point(size=4,alpha=1) + geom_line(alpha=1) + scale_colour_tableau(palette = "Classic 10 Medium") +  
    ggtitle("Covid-19 cases by country (aligned)") + scale_shape_manual(values = c(20, 20, 20, 20)) +
  scale_y_continuous(breaks=seq(0, 200000, 5000), labels = label_number(big.mark = ".", decimal.mark = ",")) + 
  xlab("Aligned days since first patients") + ylab("No. of cases") + theme_bw() + 
  theme(panel.grid.major.y  = element_line(color = "gray60", linetype = "dotted"), 
        panel.grid.major.x  = element_line(color = "gray80", linetype = "dotted"),
        panel.grid.minor = element_blank()) # theme_bw()
show(p1)

# Unaligned plot
p2 <- ggplot(df, aes(x=date, y=n, group=Country, color=Country, shape= Country)) + 
  geom_point(size=4,alpha=1) + geom_line(alpha=1) + scale_colour_tableau(palette = "Classic 10 Medium") +  
  ggtitle("Covid-19 cases by country") + scale_shape_manual(values = c(20, 20, 20, 20)) +
  scale_x_date(labels = date_format("%d.%m."), date_breaks="5 days") +
  scale_y_continuous(breaks=seq(0, 200000, 5000), labels = label_number(big.mark = ".", decimal.mark = ",")) + 
  xlab("Date") + ylab("No. of cases") + theme_bw() + 
  theme(panel.grid.major.y  = element_line(color = "gray60", linetype = "dotted"), 
        panel.grid.major.x  = element_line(color = "gray80", linetype = "dotted"),
        panel.grid.minor = element_blank()) # theme_bw()
show(p2)

# Arrange to single PDF/PNG
p3 <- grid.arrange(p2, p1, nrow=2)
# Save
ggsave("Covid-19-daily.pdf",plot=p3, device = cairo_pdf(), width = 14, height=10)
ggsave("Covid-19-daily.png",plot=p3, width = 14, height=10)

