### R workbook to examine my own reading habits. Data from my personal google sheets

setwd("C:/Users/tgord/MyPyScripts/EDA/2019 YE")
library(ggplot2)
library(dplyr)
library(cowplot)
library(tidyverse)
library(skimr)

file="TG Entertainment List - Book Page.csv"
### Loading in the data from my google sheets
books_df <- read.csv(file=file)

### Breakdown of summary
skim(books_df)
head(books_df)
colnames(books_df)

### Get the time and month
books_df$End.Date <- as.Date(books_df$End.Date,  "%m/%d/%Y")
books_df$Start.Date <- as.Date(books_df$Start.Date,  "%m/%d/%Y")
books_df$monthnum <- strftime(books_df$Start.Date, format="%m") 
summary(books_df$Year.Read)

### Select 2019 data only
books_df19 <- books_df[books_df$Year.Read %in% 2019,]
summary(books_df19)

summary(books_df19$Pages)
summary(books_df19$Estimated.Time..m.)

### Books read end date timeline
p <- ggplot(books_df19, aes(x=End.Date, y=Pages)) + 
  geom_line() 
p


books_df19$date_diff <- (books_df19$End.Date)-
  (books_df19$Start.Date)

### Author and Books frequency 
bdf <- table(books_df19$Author) %>% as.data.frame(colnames=c("Author", "Books")) %>%
  drop_na() %>% arrange(desc(Freq)) 
head(bdf)
upd_bdf <- subset(bdf, Freq!=0)

names(books_df19)
#upd_bdf$Count = as.character(upd_bdf$Count)
names(upd_bdf)[1] <- 'Author'
names(upd_bdf)[2] <- 'Count'
names(upd_bdf)

### Total pages by author 
test1 <- books_df19 %>% group_by(Author) %>% 
  summarise(Pages = sum(Pages))

### Barh graph illustrating my most popular authors for the year
upd_bdf %>% mutate(Author = reorder(Author, Count)) %>% 
  ggplot(aes(Author, Count, label=Count)) +
  geom_bar(stat='identity') +
  coord_flip() +
  scale_color_gradient() +
  theme_grey() +
  ggtitle('Authors I read the most') +
  theme(legend.position = "none")

upd_bdf1 <- merge(upd_bdf,test1,by="Author")


### Graph illustrating most pages read per author
upd_bdf1 %>% mutate(Author = reorder(Author, Pages)) %>% 
  ggplot(aes(Author, Pages, label=Count)) +
  geom_bar(stat='identity', aes(fill=Count)) +
  coord_flip() +
  scale_y_continuous() +
  ylab("") + 
  theme_grey() +
  ggtitle(toupper("Author by most pages & # of Books I read 2019")) +
  theme(legend.position = "right")

### Getting both pages and time to complete for comparison
pages <- books_df19 %>%
  ggplot(aes(Pages)) +
  geom_histogram(bins=10) +
  theme_light() +
  ylim(0,40)

est_time <- books_df19 %>%
  ggplot(aes(Estimated.Time..m.)) +
  geom_histogram(bins=10) +
  theme_light() +
  ylim(0, 40)

plot_grid(pages, est_time, align= "hv", nrow = 2) + ggtitle('Pages vs Time')

skim(books_df19$date_diff)
lower <- round(mean(books_df19$date_diff) - IQR(books_df19$date_diff))
upper <- round(mean(books_df19$date_diff) + IQR(books_df19$date_diff))

### Most books I finish between 6-18 days
cat("Lower range of days:", lower, ".Upper range of days:", upper)

books_df19 %>%
  ggplot(aes(date_diff)) +
  geom_histogram(bins=50) +
  ggtitle('Aging Days') +
  scale_x_continuous() +
  theme_light() 

### This graph illustrates how I started reading a lot less books when I started working
books_df19 %>%
  ggplot(aes(monthnum)) +
  geom_bar() +
  ggtitle('Month I started books') +
  theme_light() 

colnames(upd_bdf1)
min(books_df19$Pages)
max(books_df19$Pages)

library(data.table)

data_books <- fread(file)

### Testing data_table. Graph shows my favorite genres
a_test <- data_books[`Year Read`== 2019, sum( count = .N ), by = Genre]
colnames(a_test)
a_test %>% mutate(Genre = reorder(Genre, V1)) %>% 
  ggplot(aes(x=Genre, y=V1), label=V1) +
  geom_bar(stat='identity') +
  coord_flip() +
  geom_text(aes(label=V1), vjust= -.15, angle=270)

### There is a high correlation between number of pages and estimated time to read
cor(books_df$Pages, books_df$Estimated.Time..m.)
myvars <- c("Pages", "Estimated.Time..m.", "Genre")
test_df <- books_df[myvars]
sum(is.na(test_df)) 
### remove na values

test_df1 <- na.omit(test_df)

head(test_df)
set.seed(40)

### rename time for simplicity
test_df1 <- test_df1 %>% 
  rename(
    time = Estimated.Time..m.
  )


model1 <- lm(time ~ Pages, data=test_df1)

### Genre does not add much value move foreward with model1
model2 <- lm(time ~ Pages + c(Genre), data=test_df1)

print(summary(model1))

### Generate fake data to test
new <- as.numeric(runif(25, 100, 800))

predictions <- model1 %>% predict(data.frame(Pages = new))

pred_df <- cbind(data.frame(new), data.frame(predictions))

pred_df
### Smaller books should take a smaller amount of time, usually less than the page count
### Larger books usually take longer for me to read than the page count

## Model performance

###
AIC(model1)
BIC(model1)

### Less than .05 pages are signifcant 
anova(model1)

vcov(model1) 

### Confidence intervals for both items predictive values 
confint(model1, level=0.95)

### Residuals hist. Items clustered around 0 is a good indication of normality 
ggplot(data=test_df1, aes(model1$residuals)) +
  geom_histogram(bins = 30)

### Chart of pages vs time smoothed data_points to illustrate relationship of variables
#### How it maps for linear regression 

ggplot(data = test_df1, aes(x = Pages, y = time)) +
  geom_point() +
  stat_smooth(method = "lm", col = "dodgerblue3") +
  theme(panel.background = element_rect(fill = "white"),
        axis.line.x=element_line(),
        axis.line.y=element_line())

