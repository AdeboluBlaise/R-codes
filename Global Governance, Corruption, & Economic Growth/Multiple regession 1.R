
library(dplyr)
library(Hmisc)
library(lmtest)
library(car)
library(ggplot2)
library(psych)
library(gridExtra)
library(corrplot)

data <- read.csv("C:/Users/Adebolu/Downloads/qog_std_ts_jan23.csv")
View(data)

#Attaching only the variables we need by Subsetting the data
data2 <- subset(data, select = c(cname, year,wbgi_gee, wdi_gdpgr, wjp_overall,ti_cpi))

# View the subsetted data
head(data2)

#Removing years less than 2012
data2 <- subset(data2, year >= 2012)
View(data2)

# Check for missing values
null_counts <- colSums(is.na(data2))
print(null_counts)
cleaned_data <- na.omit(data2)

# Check the dimensions of the cleaned dataset
dim(cleaned_data)
View(cleaned_data)
head(cleaned_data)

#Summary statistics
describe(cleaned_data)


#Identification of outliers
# Select numeric variables
numeric_vars <- cleaned_data[, sapply(cleaned_data, is.numeric)]

# Create boxplot
dev.new(width = 10, height = 8)  # Open a new larger plot window
boxplot(numeric_vars, main = "Boxplot of Variables", xlab = "Variables")

boxplot(numeric_vars, main = "Boxplot of  Variables", xlab = "Variables")


#Boxplot for growth rate

# Create the boxplot using ggplot
ggplot(numeric_vars, aes(y = wdi_gdpgr)) +
  geom_boxplot(fill = "#1F78B4", color = "#7570B3", outlier.color = "#E7298A") +
  labs(title = "Boxplot for Growth Rate", y = "Growth Rate") +
  theme_minimal()



attach(cleaned_data)

## EDA
colors <- c("#1F78B4", "#33A02C", "#FB9A99", "#E31A1C", "#FF7F00")
#CORRUPTION EDA
most_corrupt_country <- cleaned_data %>%
  filter(ti_cpi == min(ti_cpi)) %>%
  select(cname, year)

print(paste("The most corrupt country is", most_corrupt_country$cname, "in the year", most_corrupt_country$year, "with corruption perception rate of", min(ti_cpi)))

least_corrupt_country <-cleaned_data %>%
  filter(ti_cpi == max(ti_cpi)) %>%
  select(cname, year)

print(paste("The least corrupt country is", least_corrupt_country$cname, "in the year", most_corrupt_country$year))

average_corruption <- cleaned_data %>%
  group_by(cname) %>%
  summarise(average_corruption = mean(ti_cpi))%>%
  arrange(desc(average_corruption))
head(average_corruption)

# Top 5 countries
top5a <- head(average_corruption, 5)
last5a <- tail(average_corruption, 5)
library(ggplot2)

library(ggplot2)

# Create a bar plot for the top 5 values
plot1 <- ggplot(top5a, aes(x = reorder(cname, -average_corruption), y = average_corruption, fill = average_corruption)) +
  geom_bar(stat = "identity",width = 0.5) +
  scale_fill_gradient(low = "#E6F5E6", high = "#006600")+
  labs(title = "Least Corrupt", x = "Country", y = "Average Corruption") +
  theme_minimal()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, hjust = 1))


# Create a bar plot for the last 5 values
plot2 <- ggplot(last5a, aes(x = reorder(cname, -average_corruption), y = average_corruption, fill = average_corruption)) +
  geom_bar(stat = "identity",width = 0.5) +
  scale_fill_gradient(low = "#FFEEEE", high = "#AA0000")+
  labs(title = "Most Corrupt", x = "Country", y = "Average Corruption")+
  theme_minimal()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(plot1, plot2, ncol = 2)

## GOVERNMENT effectiveness EDA
least_effectiveed_country <- cleaned_data %>%
  filter(wbgi_gee == min(wbgi_gee)) %>%
  select(cname, year)

print(paste("The least effectiveed country is", least_effectiveed_country$cname, "in the year", least_effectiveed_country$year))

best_effectiveed_country <-cleaned_data %>%
  filter(wbgi_gee == max(wbgi_gee)) %>%
  select(cname, year)

print(paste("The least effectiveed country is", best_effectiveed_country$cname, "in the year", best_effectiveed_country$year))

average_effectiveness <- cleaned_data %>%
  group_by(cname) %>%
  summarise(average_effectiveness = mean(wbgi_gee))%>%
  arrange(desc(average_effectiveness))

top5b <- head(average_effectiveness, 5)
last5b <- tail(average_effectiveness, 5)



# Create a bar plot for the top 5 values
plot1 <- ggplot(top5b, aes(x = reorder(cname, -average_effectiveness), y = average_effectiveness, fill = average_effectiveness)) +
  geom_bar(stat = "identity",width = 0.5) +
  scale_fill_gradient(low = "#E6F5FF", high = "#0000CC")+
  labs(title = "Top 5 Effective govt", x = "Country", y = "Average Effectiveness") +
  theme_minimal()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, hjust = 1))


# Create a bar plot for the last 5 values
plot2 <- ggplot(last5b, aes(x = reorder(cname, average_effectiveness), y = average_effectiveness, fill = average_effectiveness)) +
  geom_bar(stat = "identity",width = 0.5) +
  scale_fill_gradient(low = "#FFEEEE", high = "#AA0000")+
  labs(title = "Least 5 Effective govt", x = "Country", y = "Average Effectiveness")+
  theme_minimal()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, hjust = 1))

grid.arrange(plot1, plot2, ncol = 2)

## RULE OF LAW EDA
least_ruled_country <- cleaned_data %>%
  filter(wjp_overall == min(wjp_overall)) %>%
  select(cname, year)

print(paste("The least ruled country is", least_ruled_country$cname, "in the year", least_ruled_country$year))

best_ruled_country <-cleaned_data %>%
  filter(wjp_overall == max(wjp_overall)) %>%
  select(cname, year)

print(paste("The best ruled country is", best_ruled_country$cname, "in the year", best_ruled_country$year))


average_ruled <- cleaned_data %>%
  group_by(cname) %>%
  summarise(average_ruled = mean(wjp_overall))%>%
  arrange(desc(average_ruled))


top5c <- head(average_ruled, 5)
last5c <- tail(average_ruled, 5)

# Create a bar plot for the top 5 values
plot1 <- ggplot(top5c, aes(x = reorder(cname, -average_ruled), y = average_ruled, fill = average_ruled)) +
  geom_bar(stat = "identity",width = 0.5) +
  scale_fill_gradient(low = "#FFFFE6", high = "#CCCC00")+
  labs(title = "Top 5 Rule Adhered countries", x = "Country", y = "Average Adherence") +
  theme_minimal()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, hjust = 1))


# Create a bar plot for the last 5 values
plot2 <- ggplot(last5c, aes(x = reorder(cname, -average_ruled), y = average_ruled, fill = average_ruled)) +
  geom_bar(stat = "identity",width = 0.5) +
  scale_fill_gradient(low = "#FFEEEE", high = "#AA0000")+
  labs(title = "Least 5 Rule Adhered countries", x = "Country", y = "Average Adherence")+
  theme_minimal()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(plot1, plot2, ncol = 2)


## GDP GROWTH EDA
least_gdp_country <- cleaned_data %>%
  filter(wdi_gdpgr == min(wdi_gdpgr)) %>%
  select(cname, year)

print(paste("The least gdp country is", least_gdp_country$cname, "in the year", least_gdp_country$year))

best_gdp_country <-cleaned_data %>%
  filter(wdi_gdpgr == max(wdi_gdpgr)) %>%
  select(cname, year)

print(paste("The best gdp country is", best_gdp_country$cname, "in the year", best_gdp_country$year))

average_gdp <- cleaned_data %>%
  group_by(cname) %>%
  summarise(average_gdp = mean(wdi_gdpgr))%>%
  arrange(desc(average_gdp))

top5d <- head(average_gdp, 5)
last5d <- tail(average_gdp, 5)

# Create a bar plot for the top 5 values
plot1 <- ggplot(top5d, aes(x = reorder(cname, -average_gdp), y = average_gdp, fill = average_gdp)) +
  geom_bar(stat = "identity",width = 0.5) +
  scale_fill_gradient(low = "#FFE6CC", high = "#CC6600")+
  labs(title = "Top GDP growth", x = "Country", y = "Average GDP growth") +
  theme_minimal()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, hjust = 1))


# Create a bar plot for the last 5 values
plot2 <- ggplot(last5d, aes(x = reorder(cname, -average_gdp), y = average_gdp, fill = average_gdp)) +
  geom_bar(stat = "identity",width = 0.5) +
  scale_fill_gradient(low = "#FFEEEE", high = "#AA0000")+
  labs(title = "Least GDP growth", x = "Country", y = "Average GDP growth")+
  theme_minimal()+
  theme(legend.position = "none", 
        axis.text.x = element_text(angle = 90, hjust = 1))
grid.arrange(plot1, plot2, ncol = 2)



# CORRELATION
cor_result <- rcorr(as.matrix(cleaned_data[, c( "wjp_overall", "ti_cpi", "wbgi_gee")]), type = "pearson")

cor_matrix <- cor_result$r
p_values <- cor_result$P

# Create a correlation matrix with p-values
correlation_matrix <- matrix(paste(round(cor_matrix, 2), ifelse(p_values < 0.05, "*", ""), sep = ""), nrow = ncol(cor_matrix), dimnames = dimnames(cor_matrix))

# Print the correlation matrix with p-values
print(correlation_matrix)
correlation_matrix1 <- cor(cleaned_data[, c("wjp_overall", "ti_cpi", "wbgi_gee")])

# Display the correlation matrix plot with values
corrplot(correlation_matrix1, method = "number")


# MULTIPLE REGRESSION for the initial model
multiple_regression_model <- lm(wdi_gdpgr ~ ti_cpi + wbgi_gee + wjp_overall, data = cleaned_data)
summary(multiple_regression_model)

#calculate the VIF for each predictor variable in the model
vif(multiple_regression_model)


# MULTIPLE REGRESSION for the second model
multiple_regression_model2 <- lm(wdi_gdpgr ~ wjp_overall + wbgi_gee, data = cleaned_data)
summary(multiple_regression_model2)

# Linearity of the data
plot(multiple_regression_model2, 1)

#Normality of residual
model_residuals=multiple_regression_model2$residuals
# Create the QQ plot
qqnorm(model_residuals, col = "#1F78B4", pch = 16)
qqline(model_residuals, col = "#E31A1C")



#Homoscedacity 
ncvTest(multiple_regression_model2)
#Using the  Breusch–Pagan Test to Check Heteroscedasticity, using the bptest function from the lmtest package,
bptest(multiple_regression_model)
#Normality of Residuals
shapiro.test(resid(multiple_regression_model2))
#Autocorrelation (Independence assumption)
dwtest(multiple_regression_model2)
#Multi-collinearity - calculate the VIF for each predictor variable in the model
vif(multiple_regression_model2)
