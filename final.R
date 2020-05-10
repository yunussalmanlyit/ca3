
# reading the csv value into the dataframe

dublin <- read.csv("dublin2017.csv",  header = TRUE)

str(dublin)

# removing the unwanted column with many empty cells
install.packages("mice")

library("mice")

md.pattern(dublin)

install.packages("VIM")

library(VIM)

totalmissing_values <- aggr(dublin, prop = TRUE, numbers = TRUE)

dublin$Property.Size.Description <- NULL

# we can see the post.code variables has some missing values we will try to remove it

dublin[dublin == ""] <- NA

sum(is.na(dublin))

# its better to remove the variables with majority missing values

colSums(is.na(dublin))

# remove the postal.code and property size

dublin <- na.omit(dublin)

dublin$Price.... <-gsub("€","",dublin$Price....)

names(dublin)[names(dublin) == "Date.of.Sale..dd.mm.yyyy."] <- "date"

names(dublin)[names(dublin) == "Postal.Code"] <- "pcode"

names(dublin)[names(dublin) == "Price...."] <- "cost"

names(dublin)[names(dublin) == "Not.Full.Market.Price"] <- "market"

names(dublin)[names(dublin) == "VAT.Exclusive"] <- "vat"

names(dublin)[names(dublin) == "Description.of.Property"] <- "des"


colSums(is.na(dublin))

# second dataset
# importing the second dataset

ireland <- read.csv("ppi.csv", header = TRUE)

colnames(ireland)

md.pattern(ireland)

install.packages("VIM")

library(VIM)

totalmissing_values <- aggr(ireland, prop = TRUE, numbers = TRUE)


ireland$Price..â... <- gsub("â‚¬","",ireland$Price..â...)

names(ireland)[names(ireland) == "Date.of.Sale..dd.mm.yyyy."] <- "date"

names(ireland)[names(ireland) == "Price..â..."] <- "cost"

names(ireland)[names(ireland) == "Not.Full.Market.Price"] <- "market"

names(ireland)[names(ireland) == "VAT.Exclusive"] <- "vat"

names(ireland)[names(ireland) == "Description.of.Property"] <- "des"

names(ireland)[names(ireland) == "Postal.Code"] <- "pcode"

ireland[ireland == ""] <- NA


colSums(is.na(ireland))

ireland <- na.omit(ireland)

colSums(is.na(ireland))


# we will omit those NAS



# as we have to choose only 2017 years data we will extract only those year

ireland$year <- substring(ireland$date, 7, 10)

install.packages("sqldf")

library(sqldf)

colnames(ireland)

alldublin <- sqldf("SELECT date, Address, pcode, County, cost, market,
               vat, des FROM ireland WHERE year = '2017'")


str(alldublin)

# combining the two datasets

fulldublin <- rbind(dublin, alldublin)

str(fulldublin)

write.csv(fulldublin, 'cleaned.csv')

str(fulldublin)

# changing the dataset structure

str(fulldublin)

# 2018 dublin datasets

# reading the csv value into the dataframe

dublin2 <- read.csv("dublin2018.csv",  header = TRUE)

str(dublin2)

# removing the unwanted column with many empty cells
install.packages("mice")

library("mice")

md.pattern(dublin2)

install.packages("VIM")

library(VIM)

totalmissing_values <- aggr(dublin2, prop = TRUE, numbers = TRUE)

dublin2$Property.Size.Description <- NULL

# we can see the post.code variables has some missing values we will try to remove it

dublin2[dublin2 == ""] <- NA

sum(is.na(dublin2))

# its better to remove the variables with majority missing values

colSums(is.na(dublin2))

# remove the postal.code and property size

dublin2 <- na.omit(dublin2)



dublin2$Price.... <-gsub("€","",dublin2$Price....)

names(dublin2)[names(dublin2) == "Date.of.Sale..dd.mm.yyyy."] <- "date"

names(dublin2)[names(dublin2) == "Postal.Code"] <- "pcode"

names(dublin2)[names(dublin2) == "Price...."] <- "cost"

names(dublin2)[names(dublin2) == "Not.Full.Market.Price"] <- "market"

names(dublin2)[names(dublin2) == "VAT.Exclusive"] <- "vat"

names(dublin2)[names(dublin2) == "Description.of.Property"] <- "des"


colSums(is.na(dublin2))

# second dataset
# importing the second dataset

ireland2 <- read.csv("ppi.csv", header = TRUE)

colnames(ireland2)

md.pattern(ireland2)

install.packages("VIM")

library(VIM)

totalmissing_values <- aggr(ireland2, prop = TRUE, numbers = TRUE)


ireland2$Price..â... <- gsub("â‚¬","",ireland$Price..â...)

names(ireland2)[names(ireland2) == "Date.of.Sale..dd.mm.yyyy."] <- "date"

names(ireland2)[names(ireland2) == "Price..â..."] <- "cost"

names(ireland2)[names(ireland2) == "Not.Full.Market.Price"] <- "market"

names(ireland2)[names(ireland2) == "VAT.Exclusive"] <- "vat"

names(ireland2)[names(ireland2) == "Description.of.Property"] <- "des"

names(ireland2)[names(ireland2) == "Postal.Code"] <- "pcode"

ireland2[ireland2 == ""] <- NA


colSums(is.na(ireland2))

ireland2 <- na.omit(ireland2)

colSums(is.na(ireland))


# we will omit those NAS


# as we have to choose only 2018 years data we will extract only those year

ireland2$year <- substring(ireland2$date, 7, 10)

install.packages("sqldf")

library(sqldf)

colnames(ireland2)

alldublin2 <- sqldf("SELECT date, Address, pcode, County, cost, market,
               vat, des FROM ireland2 WHERE year = '2018'")


str(alldublin2)

# combining the two datasets

fulldublin2 <- rbind(dublin2, alldublin2)

str(fulldublin2)

finaldata <- rbind(fulldublin, fulldublin2)

write.csv(finaldata, 'final.csv')

str(finaldata)

install.packages("mice")

library("mice")

md.pattern(finaldata)

totalmissing_values <- aggr(finaldata,
                            prop = TRUE, numbers = TRUE)

str(finaldata)


# changing the dataset structure

finaldata$cost <- gsub(",", "", as.character(finaldata$cost))

finaldata$Address <- as.integer(finaldata$Address)

finaldata$County <- as.integer(finaldata$County)

finaldata$vat <- as.integer(finaldata$vat)

finaldata$des <- as.integer(finaldata$des)

finaldata$market <- as.integer(finaldata$market)

finaldata$date <- as.integer(finaldata$date)

finaldata$cost <- as.factor(finaldata$cost)

finaldata$cost <- as.integer(finaldata$cost)

finaldata$pcode <- as.integer(finaldata$pcode)

str(finaldata)


# as we can observe all the data variables are in integers / numeric
# so now we can perform the correlation

opar <- par(no.readonly = TRUE)

library(corrplot)

corrplot(corr = cor(finaldata),tl.col = "Black",tl.cex = 0.90)


# plotting the variables

prop.table(table(finaldata$pcode))

barplot(height = prop.table(table(finaldata$pcode)),
        main = "postal codes in dublin",
        ylab = "frequency",
        xlab = "dublin",
        col = "grey")

prop.table(table(finaldata$cost))

barplot(height = prop.table(table(finaldata$cost)),
        main = "cost of property prices",
        ylab = "price",
        xlab = "ireland",
        col = "Green")

# creating the plot

install.packages("ggplot2")

install.packages("viridis")

library(ggplot2)

library(viridis)
str(finaldata)

colnames(finaldata)

hist(finaldata$Address, main = "county addressess", xlab = "", col = "grey")

hist(finaldata$cost, main = "cost based on county", xlab = "", col = "Orange")

hist(finaldata$pcode, main = "postal codes", xlab = "Dublin")

    
# principal component analysis

principal <- prcomp(finaldata, center = TRUE, scale. = TRUE)

principal

summary(principal)

principal$rotation[1:nrow(principal$rotation), 1:4]


# we will look at the eigenvalues and its variances

library("factoextra")

eig_values <- get_eigenvalue(principal)

eig_values

# now we will plot the principal component anlysis and variables

fviz_eig(principal, addabels = TRUE, ylim = c(0,50))

pca_for_variables <- get_pca_var(principal)

pca_for_variables

library("corrplot")

corrplot(pca_for_variables$cor, is.corr = FALSE)

fviz_pca_var(principal,col.var = "black")

head(pca_for_variables$cos2,10)

fviz_cos2(principal, choice = "var", axes = 1:2)

fviz_pca_var(principal,col.var = "cos2",
             gradient.cols = c("red", "Blue", "Green"),
             repel = TRUE)

head(pca_for_variables$contrib,20)

fviz_contrib(principal, choice = "var", axes =1, top =20)

fviz_contrib(principal, choice = "var", axes =2, top =20)

fviz_contrib(principal, choice = "var", axes =1:5, top =10)

# graph of price variable

fviz_pca_biplot(principal, 
                col.ind = finaldata$cost, palette = "jco", 
                addEllipses = TRUE, label = "var",
                col.var = "black", repel = TRUE,
                legend.title = "cost")


# power analysis on dataset

install.packages("pwr")

library(pwr)

effective_size <- cohen.ES(test = "r", size = "large")

effective_size

power_analysis <- pwr.r.test(n = NULL, r = 0.5, sig.level = 0.05,
                             power = 0.95,
                             alternative = "two.sided")

power_analysis
plot(power_analysis)

# hypothesis testing on data variables to find the p value.

test <- cor.test(finaldata$cost, finaldata$County,
                 method = 'pearson', exact = FALSE)

test
