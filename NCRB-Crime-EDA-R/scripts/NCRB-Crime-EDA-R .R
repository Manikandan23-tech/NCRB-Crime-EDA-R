library(readxl)
library(vcd)

data <- read_excel("NCRB dataset.xlsx")
data <- na.omit(data)

high_crime <- subset(data, crime_status == "High")
south_region <- subset(data, region == "South")
high_apprehended <- subset(data, apprehended > mean(data$apprehended))
backlog_states <- subset(data, pending > apprehended)
```

# Central Tendency Measures

```{r}
mean(data$apprehended)
median(data$apprehended)

mean(data$acquitted)
median(data$acquitted)

mean(data$pending)
median(data$pending)
```

# Histogram Analysis

## Distribution of Apprehended Cases Across States

hist(
  data$apprehended,
  breaks = 10,
  col = "steelblue",
  border = "black",
  main = "Distribution of Apprehended Cases Across States",
  xlab = "Number of Apprehended Cases",
  ylab = "Frequency"
)

## Distribution of Apprehended Cases in High Crime States


hist(
  high_crime$apprehended,
  breaks = 10,
  col = "red",
  border = "black",
  main = "Distribution of Apprehended Cases in High Crime States",
  xlab = "Number of Apprehended Cases",
  ylab = "Frequency"
)


## Distribution of Apprehended Cases in South Region

hist(
  south_region$apprehended,
  breaks = 10,
  col = "purple",
  border = "black",
  main = "Distribution of Apprehended Cases in South Region",
  xlab = "Number of Apprehended Cases",
  ylab = "Frequency"
)


# Scatter Plot Analysis

## Relationship Between Apprehended and Pending Cases

plot(
  data$apprehended,
  data$pending,
  col = "red",
  pch = 19,
  main = "Relationship Between Apprehended and Pending Cases",
  xlab = "Apprehended Cases",
  ylab = "Pending Cases"
)


## Relationship Between Apprehended and Acquitted Cases

plot(
  high_apprehended$apprehended,
  high_apprehended$acquitted,
  col = "blue",
  pch = 19,
  main = "Relationship Between Apprehended and Acquitted Cases",
  xlab = "Apprehended Cases",
  ylab = "Acquitted Cases"
)


# Density Plot

d <- density(data$acquitted)

plot(
  d,
  col = "purple",
  lwd = 2,
  main = "Density Distribution of Acquitted Cases",
  xlab = "Acquitted Cases",
  ylab = "Density"
)

polygon(d, col = rgb(0.5,0,0.5,0.3), border = "purple")


# Pending Cases Distribution

hist(
  backlog_states$pending,
  breaks = 10,
  col = "darkred",
  border = "black",
  main = "Distribution of Pending Cases in Backlog States",
  xlab = "Pending Cases",
  ylab = "Frequency"
)

# Boxplot Analysis

## Apprehended Cases Distribution Across Regions


boxplot(
  apprehended ~ region,
  data = data,
  col = "orange",
  main = "Apprehended Cases Distribution Across Regions",
  xlab = "Region",
  ylab = "Apprehended Cases",
  las = 2
)

## Apprehended Cases Distribution in High Crime States

boxplot(
  apprehended ~ region,
  data = high_crime,
  col = "pink",
  main = "Apprehended Cases Distribution Across Regions in High Crime States",
  xlab = "Region",
  ylab = "Apprehended Cases",
  las = 2
)