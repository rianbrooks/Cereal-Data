#------Cereal data from Kaggle------
# Rian Brooks
# Nov, 2023


# Install necessary packages & call
install.packages("corrplot")
library(corrplot)

install.packages("ggplot2")
library(ggplot2)

install.packages("GGally")
install.packages("psych")
install.packages("lattice")

library(GGally)
library(psych)
library(dplyr)
library(lattice)


# Upload csv file with file location
cereal <-read.csv(file = "cereal.csv", header=TRUE)  

# Returns structure of variables
str(cereal)

# Search for NA
#   Returns table with NA locations 
print("Row & column positions of NA values")
which(is.na(cereal), arr.ind = TRUE)  ## returns 0 ?

# Noticed there are negative values
#     Should we remove those?
print("Row & column position of -1 values")
which(cereal == -1, arr.ind = TRUE)


## Casting variables----------------

#  casting type, mfr & shelf as factors
cereal$type <- as.factor(cereal$type)
cereal$mfr <- as.factor(cereal$mfr)
cereal$shelf <- as.factor(cereal$shelf)

# How many of each mfr 
cereal %>%
  count(mfr)

##----Correlation------

# Creates a correlation function
cereal.cor <- cor(cereal[, c(4:12,14:16)]) 

# graphing correlation function
corrplot(cereal.cor, title = "Correlation Matrix")

##--------Graphing=--------


# histograms to look at distribution
cereal %>%
  ggplot( aes(x = vitamins)) +
  geom_histogram(binwidth = 5, fill="#69b3a2", color="#e9ecef", alpha=0.9) 


#Bar plot mfr vs rating, grouped by shelf
ggplot(cereal, aes(x = mfr, y = rating, fill = shelf)) +
  geom_bar(position = "stack", stat = "identity") +
  labs(title = "Manufacturer vs Rating", 
          y = "Consumer Rating",
          x = "Manufacturer")


#box plot rating v sugar, grouped by shelf
ggplot(cereal, aes(sugars, rating)) +
  geom_point(aes(color = shelf), size = 3)


# Exploring correlation between potassium and fiber 
# positive correlation with some outliers
#   fill gives us info on calorie content
plot(x = cereal$potass, y = cereal$fiber)
ggplot(cereal, aes(x = potass, 
                     y = fiber,
                     color = calories,
                     fill = calories)) +
  geom_point(size = 3)

# calories and sugar
ggplot(cereal, aes(x = carbo,
                   y = sugars)) +
  geom_point(size = 3) +
  labs(title = "Sugar vs Complex Carbs",
       x = "Complex Carbs Per Serving",
       y = "Sugars Per Serving")


# distribution of protein variable
boxplot(cereal$protein,
        main = "Protein Content",
        ylab = "Protein in grams",
        col = "orange",
        border = "brown")

# sugars
boxplot(cereal$sugars,
        main = "Sugar per Serving",
        ylab = "Sugar in grams",
        col = "purple",
        border = "navy")

# calorie vs fat
ggplot(cereal, aes(x = calories,
                   y = fat)) +
  geom_point(size = 3) +
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = " Calories vs Fat",
       x = "Calories per Serving",
       y = "Fat per Serving")


## Exploring Manufacturer
#   mfr count
ggplot(cereal, aes(x = mfr, y = after_stat(count))) +
  geom_bar(fill = "#5ab2f6", color="#e9ecef") +
  labs(title = "Distribution of Manufacturer", 
       y = "Frequecny", x= "Manufacturer")



## Exploring shelf location
#   grouped bar chart: shelf, calorie & mfr
ggplot(cereal, aes(fill = shelf,
                   y = sugars,
                   x = mfr)) +
  geom_bar(position = "dodge", stat = "identity")
  

# Exploring correlation between calories and weight
#   with lm 
ggplot(cereal, aes(x = weight, y = calories)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Calories vs Weight", x = "Weight per Serving",
       y = "Calories per Serving") 
  

# Exploring correlation between rating and sugar
#   with line of best fit ?

# scatter plot with line of best fit, linear model
ggplot(data = cereal, aes(x = sugars, y = rating)) +
  geom_point() + 
  geom_smooth(method = "lm", se = FALSE) +
  labs(title = "Sugar vs Rating", x = "Sugars Per Serving",
       y = "Consumer Rating")


# Scatter matrix
pairs(cereal[,c(4:16)], main = "Scatter Plot Matrix")

Scatter_Matrix <- ggpairs(cereal,columns = c(4:10), 
                          title = "Scatter Plot Matrix for Cereal Dataset", 
                          axisLabels = "show") 
ggsave("Scatter plot matrix.png", Scatter_Matrix, width = 5, 
       height = 5, units = "in") 

Scatter_Matrix #density plot is diagonal


##----- Feature Engineering - Creating variables-------

## Create % DV for sodium
rec_DV_sodium <- 2300 #recommended DV of sodium
cereal$sodium_per_DV <- (cereal$sodium/rec_DV_sodium)*100

## Create % DV for fat
rec_DV_fat <- 78 #recommended DV of fat
cereal$fat_per_DV <- (cereal$fat/rec_DV_fat)*100

## Create % DV for sugar
rec_DV_sugar <- 24 #recommended DV of sugar
cereal$sugar_per_DV <- (cereal$sugar/rec_DV_sugar)*100


#    Exploring popular brands
fivenum(cereal$rating)

# returns top 5 rated 
which(cereal$rating > 65, arr.ind = TRUE)
cereal %>%
  filter(rating > 65)

# returns lowest rated 5
which(cereal$rating < 23, arr.ind = TRUE)
cereal %>%
  filter(rating < 23)

## ------healthy variable, 0 or 1---------

#Using pipe function (%>%), we have to 
#  store the new data set in the current data set
#  so the healthy variable is both created &
#  stored. 
cereal <- cereal %>%
  mutate(healthy = case_when(sugar_per_DV < 5 &
                              sodium_per_DV > 0 &
                               fat_per_DV < 5 ~ "1",
                             TRUE ~ "0"))
cereal %>%
  count(healthy)

# locate healthy cereals
cereal %>%
  filter(healthy == "1")
