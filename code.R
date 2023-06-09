df <- read.csv("data.csv")

#head and tail
head(df, 10)
tail(df, 10)

#extract the oldest 3
or <- order(df$dob)
ext <- df[or[1:3],c("gender", "avg_commute", "ancestry")]
ext

#extract the patients who has more than 2 children
more_2_children <- df[df$children > 2, c("gender", "daily_internet_use", "avg_commute", "ancestry", "disease")]
more_2_children

#rows that have na's and not have na's
table(rowSums(is.na(df)))

#summary
print("For numerical data:")
summary(df[,c("zipcode", "children", "avg_commute", "daily_internet_use")])

cat_df <- as.data.frame(df[,c("gender","employment_status", "education", "marital_status", "ancestry","disease", "dob")])

cat <- lapply(cat_df, table)
print("For categorical data:")
cat

#columns that have missing values       
which(is.na(df), arr.ind=TRUE)
df <- na.omit(df)
table(complete.cases(df))

#average daily use
avg <- tapply(df$daily_internet_use, df$education, mean)
avg

barplot(avg, main = "Average daily use", col = c(1, 2, 3, 4, 5, 6), legend = TRUE, args.legend= list(bty = "l", x = "right", ncol = 1), cex.names = 0.9, las = 2)

#distribution of child count
hist(df$children, main = "Children distribution", col = 4)

#Men and women 's avg commute distribution
males <- df[df$gender == 'male', "avg_commute"]
females <- df[df$gender == 'female', "avg_commute"]
plot(males, type = 'l', col = 3, main = "Men's avg commute distribution")
plot(females, type = 'l', col =4, main = "Women's avg commute distribution")

#distribution of gender
barplot(table(df$gender), main = "Gender distribution", col = c(4,2))

#distribution of gender for each disease
tbl <- with(df, table(df$gender,df$disease))
barplot(tbl, beside=TRUE, legend=TRUE, col = c(1:2), main = "Distribution of gender for each disease", args.legend= list(bty = "l", x = "top", ncol = 3), cex.names = 0.6, las = 2)

#Relationship between age and disease
library(eeptools)
var <- age_calc(as.Date(df$dob), Sys.Date(), units = "years")
df['age'] = floor(var)
barplot(table(df$age), col = 2, xlab = "age", main = "Relationship between age and disease", cex.names = 0.6, las = 2)

#Total number of children per disease
total_children <- tapply(df$children, df$disease, sum)
total_children
barplot(total_children, col = c(1:13), ylab = "children", main = "Total number of children per disease", cex.names = 0.6, las = 2)

#Ancestry distribution
barplot(table(df$ancestry), col = "red", ylab = "Distribution", main = "Ancestry distribution", cex.names = 0.6, las = 2)
