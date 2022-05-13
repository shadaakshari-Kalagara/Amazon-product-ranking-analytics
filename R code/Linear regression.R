data <- read.csv("modelfile.csv")

str(data)

data$product_link <- NULL
data$name <- NULL
data$rating_link <- NULL
data$link <- NULL

# For correlation 
data$category <- NULL
data$prime <- NULL
data$Discount <- NULL

# correlation map
cormat <- round(cor(data),2)
head(cormat)

library(reshape2)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Heatmap

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
# Create a ggheatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                       midpoint = 0, limit = c(-1,1), space = "Lab", 
                       name="Pearson\nCorrelation") +
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap + 
  geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.6, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))


# creating dummy variables
library(caret)
dummies <- dummyVars( rank ~ . , data = data)
ex <- data.frame(predict(dummies, newdata = data))
names(ex) <- gsub("\\.", "", names(ex))
data <- cbind(data$rank, ex)
names(data)[1] <- 'rank'


data$Discount_yes <- ifelse(data$Discount == 0, 0, 1)

hist(data$rank)
hist(data$avg_rating)
hist(data$rating_count)
hist(data$sentiment.score) 
hist(data$customer_review_count) # log transform
hist(log(data$customer_review_count+1))

hist(data$Answered_Questions) # log transform
hist(log(data$Answered_Questions+1))

hist(data$Price) # log transform
hist(log(data$Price+1))




# full model for electronics
data_electronics <- subset(data, data$categoryelectronics == 1)

model <- lm(log(rank) ~ log(Price+1) + primeprime  + Discount_yes +
                    sentimentscore  + rating_count + avg_rating  + customer_review_count + 
                    Images + log(Answered_Questions+1) 
             , data = data_electronics)
summary(model)

#2
model <- lm(log(rank) ~ log(Price+1) + primeprime  + Discount_yes +
              sentimentscore  + rating_count + avg_rating   + 
              Images + log(Answered_Questions+1) 
            , data = data_electronics)
summary(model)

#3
model <- lm(log(rank) ~ log(Price+1)  + Discount_yes +
              sentimentscore  + rating_count + avg_rating   + 
              Images + log(Answered_Questions+1) 
            , data = data_electronics)
summary(model)

#4
model <- lm(log(rank) ~ log(Price+1)  + Discount_yes +
              sentimentscore  + rating_count + avg_rating   + 
            log(Answered_Questions+1) 
            , data = data_electronics)
summary(model)

#5
model <- lm(log(rank) ~ log(Price+1)  + Discount_yes +
              rating_count    + 
              log(Answered_Questions+1) 
            , data = data_electronics)
summary(model)

#6
model <- lm(log(rank) ~ Discount_yes +
              rating_count   
            , data = data_electronics)
summary(model)

#7 

model <- lm(log(rank) ~ log(Price+1)  + Discount_yes + Discount_yes*log(Price+1) +
              rating_count   + Discount_yes*rating_count + Discount_yes*log(Answered_Questions+1)+
              log(Answered_Questions+1) 
            , data = data_electronics)
summary(model)


# full model for pc
data_pc <- subset(data, data$categorypc == 1)

model <- lm(log(rank) ~ log(Price+1) + primeprime  + Discount_yes +
              sentimentscore  + rating_count + avg_rating  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_pc)
summary(model)

# 2

model <- lm(log(rank) ~ log(Price+1) + primeprime  + Discount_yes +
              sentimentscore + avg_rating  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_pc)
summary(model)

# 3

model <- lm(log(rank) ~ log(Price+1) + primeprime  + Discount_yes +
              sentimentscore + avg_rating  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_pc)
summary(model)

# 4

model <- lm(log(rank) ~ log(Price+1) + primeprime  + Discount_yes +
              sentimentscore  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_pc)
summary(model)

# 5
model <- lm(log(rank) ~ log(Price+1) + primeprime  +
              sentimentscore  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_pc)
summary(model)

# 6
model <- lm(log(rank) ~ log(Price+1) + primeprime  +
              sentimentscore  + customer_review_count + 
              log(Answered_Questions+1) 
            , data = data_pc)
summary(model)

# 7
model <- lm(log(rank) ~ primeprime  +
              sentimentscore  + customer_review_count + 
              log(Answered_Questions+1) 
            , data = data_pc)
summary(model)

# 8
model <- lm(log(rank) ~ customer_review_count + 
              log(Answered_Questions+1) 
            , data = data_pc)
summary(model)


# full model for toys
data_toys <- subset(data, data$categorytoys == 1)

model <- lm(log(rank) ~ log(Price+1) + primeprime  + primeprime*log(Price+1) + Discount_yes +
              sentimentscore  + rating_count + avg_rating  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_toys)
summary(model)

# 2
model <- lm(log(rank) ~ log(Price+1) + primeprime  + primeprime*log(Price+1) + Discount_yes +
              sentimentscore  + rating_count + avg_rating  + customer_review_count + 
              log(Answered_Questions+1) 
            , data = data_toys)
summary(model)

# 3
model <- lm(log(rank) ~ log(Price+1) + primeprime  + Discount_yes +
               rating_count + avg_rating  + customer_review_count + 
              log(Answered_Questions+1) 
            , data = data_toys)
summary(model)

# 4

model <- lm(log(rank) ~ log(Price+1) + primeprime  + Discount_yes +
              rating_count  + customer_review_count + 
              log(Answered_Questions+1) 
            , data = data_toys)
summary(model)

# 5

model <- lm(log(rank) ~ log(Price+1) + primeprime +
              rating_count  + customer_review_count + 
              log(Answered_Questions+1) 
            , data = data_toys)
summary(model)

# 5

model <- lm(log(rank) ~ log(Price+1) + primeprime +
              rating_count  + customer_review_count + 
              log(Answered_Questions+1) 
            , data = data_toys)
summary(model)

# 6
model <- lm(log(rank) ~ primeprime +
              rating_count  + customer_review_count 
            , data = data_toys)
summary(model)

# 7
model <- lm(log(rank) ~
              rating_count  + customer_review_count 
            , data = data_toys)
summary(model)


# full model for grocery

data_grocery <- subset(data, data$categorygrocery == 1)

model <- lm(log(rank) ~ log(Price+1) + primeprime  + primeprime*log(Price+1) + Discount_yes +
              sentimentscore  + rating_count + avg_rating  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_grocery)
summary(model)

# full model for homeneeds
data_homeneeds <- subset(data, data$categoryhomeneeds == 1)
model <- lm(log(rank) ~ log(Price+1) + primeprime  + primeprime*log(Price+1) + Discount_yes +
              sentimentscore  + rating_count + avg_rating  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_homeneeds)
summary(model)


# full model for fashion 
#1
data_fashion <- subset(data, data$categoryfashion == 1)
model <- lm(log(rank) ~ log(Price+1) + primeprime  + Discount_yes +
              sentimentscore  + rating_count + avg_rating  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_fashion)
summary(model)

# 2
model <- lm(log(rank) ~ log(Price+1) + primeprime  +
              sentimentscore  + rating_count + avg_rating  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_fashion)
summary(model)

# 3

model <- lm(log(rank) ~ log(Price+1)  +
              sentimentscore  + rating_count + avg_rating  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_fashion)
summary(model)

# 4
model <- lm(log(rank) ~ log(Price+1)  +
              sentimentscore  + rating_count + avg_rating  + 
              Images + log(Answered_Questions+1) 
            , data = data_fashion)
summary(model)

# 5
model <- lm(log(rank) ~ log(Price+1)  +
              rating_count + avg_rating  + 
              Images + log(Answered_Questions+1) 
            , data = data_fashion)
summary(model)


# full model for pets

data_pets <- subset(data, data$categorypetsupplies == 1)
model <- lm(log(rank) ~ log(Price+1) + primeprime  + primeprime*log(Price+1) + Discount_yes +
              sentimentscore  + rating_count + avg_rating  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_pets)

summary(model)

# 2
model <- lm(log(rank) ~ log(Price+1)  + primeprime*log(Price+1) + Discount_yes +
              sentimentscore  + rating_count + avg_rating  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_pets)

summary(model)

# 3
model <- lm(log(rank) ~ log(Price+1)  + primeprime*log(Price+1) + Discount_yes +
              sentimentscore  + rating_count  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_pets)

summary(model)

# 4

model <- lm(log(rank) ~ log(Price+1) + Discount_yes +
              sentimentscore  + rating_count  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_pets)

summary(model)

# 5
model <- lm(log(rank) ~ log(Price+1) + Discount_yes +
              sentimentscore  + customer_review_count + 
              Images + log(Answered_Questions+1) 
            , data = data_pets)

summary(model)

# 6
model <- lm(log(rank) ~ log(Price+1) + Discount_yes +
              sentimentscore + 
              Images + log(Answered_Questions+1) 
            , data = data_pets)

summary(model)

# 7
model <- lm(log(rank) ~ log(Price+1) +
              sentimentscore + 
              Images + log(Answered_Questions+1) 
            , data = data_pets)

summary(model)

# prime vs non- prime 

model <- lm(log(rank) ~ log(Price+1) + primeprime  + primeprime*log(Price+1) 
            , data = data)
summary(model)






