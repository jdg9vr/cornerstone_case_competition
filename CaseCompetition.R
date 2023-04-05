library(tidyverse)
library(stringr)
library(ggplot2)

# Read data

WS_sales <- read_csv('03. WS Sales.csv')
WS_customers <- read_csv("04. WS Customer Key.csv")
HH_sales <- read_csv('05. HH Sales.csv')
HH_customers <- read_csv('06. HH Customer Key.csv')
competitors <- read_csv('02. Competitors in Market.csv')

# Clean data

colnames(competitors) <- c("Company", "Pacific/Western Market Share", 
                           'Midwest Market Share', 'Southeast Market Share', 'Northeast Market Share')
competitors <- competitors[-c(1:5),]
competitors <- competitors[-c(24:31),]

competitors$`Pacific/Western Market Share` <- 
  str_replace_all(competitors$`Pacific/Western Market Share`, '-', '0')
competitors$`Pacific/Western Market Share` <- 
  gsub('\\$', '', competitors$`Pacific/Western Market Share`)
competitors$`Pacific/Western Market Share` <- 
  as.numeric(competitors$`Pacific/Western Market Share`)

competitors$`Midwest Market Share` <- 
  str_replace_all(competitors$`Midwest Market Share`, '-', '0')
competitors$`Midwest Market Share` <- 
  gsub('\\$', '', competitors$`Midwest Market Share`)
competitors$`Midwest Market Share` <- 
  as.numeric(competitors$`Midwest Market Share`)

competitors$`Southeast Market Share` <- 
  str_replace_all(competitors$`Southeast Market Share`, '-', '0')
competitors$`Southeast Market Share` <- 
  gsub('\\$', '', competitors$`Southeast Market Share`)
competitors$`Southeast Market Share` <- 
  as.numeric(competitors$`Southeast Market Share`)

competitors$`Northeast Market Share` <- 
  str_replace_all(competitors$`Northeast Market Share`, '-', '0')
competitors$`Northeast Market Share` <- 
  gsub('\\$', '', competitors$`Northeast Market Share`)
competitors$`Northeast Market Share` <- 
  as.numeric(competitors$`Northeast Market Share`)


WS_sales$date_sold <- as.Date(WS_sales$date_sold, format = '%m/%d/%Y')
HH_sales$transaction_date <- as.Date(HH_sales$transaction_date, format = '%m/%d/%Y')
WS_sales <- WS_sales[,-6]

# Merging data
WS <- left_join(WS_sales, WS_customers, by = 'customer')
HH <- left_join(HH_sales, HH_customers, by = 'cust_ID')

# Data exploration
theme_set(theme_bw())

WS %>% group_by(region) %>% ggplot(aes(x=date_sold, y=units_sold)) + geom_point(aes(color=region))
WS %>% group_by(region) %>% ggplot(aes(x=date_sold, y=units_sold)) + geom_point() + facet_wrap(.~region)


HH %>% group_by(region) %>% ggplot(aes(x=transaction_date, y=qty)) + geom_point() + facet_wrap(.~region)
HH %>% group_by(region) %>% ggplot(aes(x=transaction_date, y=qty)) + geom_point(aes(color=region))

WS %>% ggplot(aes(x=units_sold, y=price_sold)) + geom_point(aes(color=cust_category, shape=region))

WS %>% group_by(cust_category) %>% ggplot(aes(x=units_sold, y=price_sold)) + geom_point(aes(color=cust_category))

WS %>% filter(region == 'Northeast') %>% 
  ggplot(aes(x=units_sold, y=price_sold)) + geom_point(aes(color=cust_category))


### HHI Calculations

competitors[24, 1:5] <- c("Total", sum(competitors$`Pacific/Western Market Share`), 
                          sum(competitors$`Midwest Market Share`),
                          sum(competitors$`Southeast Market Share`),
                          sum(competitors$`Northeast Market Share`))

competitors2 <- competitors
competitors2[,-1] <- sapply(2:5, function(y) 
  sapply(competitors[, y], function(x) (as.numeric(x)/as.numeric(competitors[24, y]))*100))

competitors3 <- competitors2[,-1]^2
competitors3 <- competitors3[-24,]

pre_HHI <- apply(competitors3[1:23,], 2, function(x) sum(x))

newcompetitors <- competitors
newcompetitors[4, ] <- c("Merged", 115.5+52.5, 49+63, 31.5+78.8, 140+87.5)
newcompetitors <- newcompetitors[-7,]

newcompetitors2 <- newcompetitors
newcompetitors2[,-1] <- sapply(2:5, function(y) 
  sapply(newcompetitors[, y], function(x) (as.numeric(x)/as.numeric(newcompetitors[23, y]))*100))

newcompetitors3 <- newcompetitors2[,-1]^2

post_HHI <- apply(newcompetitors3[1:22,], 2, function(x) sum(x))


post_HHI-pre_HHI

#### Other graphs

colors <- c('black', 'firebrick2', 'grey', 'orange')

WS %>% ggplot(aes(units_sold, price_sold)) + 
  geom_point(aes(color=cust_category)) + 
  labs(title='Wellness Solutions Price vs. Quantity',
       x='Quantity', 
       y='Price') + scale_color_manual(name = "Customer Category", values=c('black', 'orange')) + 
  theme(legend.position=c(.78, .13), axis.title = element_text(size=16),
        plot.title=element_text(face='bold', size=20),
        legend.title=element_text(size=15),
        legend.text=element_text(size=13))


WS %>% ggplot(aes(units_sold, price_sold)) + 
  geom_point(aes(color=region)) + 
  labs(title='Wellness Solutions Price vs. Quantity',
       x='Quantity', 
       y='Price') + scale_color_manual(name = "Region", values=colors) + 
  theme(legend.position=c(.85, .2), axis.title = element_text(size=16),
        plot.title=element_text(face='bold', size=20),
        legend.title=element_text(size=15),
        legend.text=element_text(size=13))

HH %>% ggplot(aes(qty, transaction_price)) + 
  geom_point(aes(color=cust_type)) + 
  labs(title='Holistic Health Price vs. Quantity',
       x='Quantity', 
       y='Price') + scale_color_manual(name = "Customer Category", values=c('black', 'orange')) + 
  theme(legend.position=c(.8, .13), axis.title = element_text(size=16),
        plot.title=element_text(face='bold', size=20),
        legend.title=element_text(size=15),
        legend.text=element_text(size=13))



HH %>% ggplot(aes(qty, transaction_price)) + 
  geom_point(aes(color=region)) + 
  labs(title='Holistic Health Price vs. Quantity',
  x='Quantity', 
  y='Price') + scale_color_manual(name = "Region", values=colors) + 
  theme(legend.position=c(.85, .2), axis.title = element_text(size=16),
        plot.title=element_text(face='bold', size=20),
        legend.title=element_text(size=15),
        legend.text=element_text(size=13))

HH %>% ggplot(aes(x = transaction_date, y = unit_cost)) + geom_point(aes(color=region))
WS %>% ggplot(aes(x = date_sold, y = COGS)) + geom_point(aes(color=region))


WS %>% ggplot(aes(x = units_sold, y = price_sold)) + geom_point(aes(color=region))

WS %>% ggplot(aes(x=units_sold, y=price_sold)) + geom_point(aes(color=cust_category))

### Markup Boxplot
margin <- WS[,3]-WS[,2]
markuppercent <- (margin/WS[,2])
lerner <- (margin/WS[,3])
WS<- cbind(WS, margin, markuppercent, lerner)
colnames(WS)[8:10] <- c('margin', 'markuppercent', 'lerner')
WS %>% ggplot(aes(y = lerner, x = region, color=region)) + geom_boxplot(color='dark orange 2') + 
  labs(title='Wellness Solutions Lerner Index by Region',
       x='Region', 
       y='Lerner Index') + 
  theme(axis.title = element_text(size=16),
        plot.title=element_text(face='bold', size=17),
        axis.text=element_text(size=12))


margin2 <- HH[,4]-HH[,2]
markuppercent2 <- 100*(margin2/HH[,2])
lerner2 <- (margin2/HH[,2])
HH <- cbind(HH, margin2,markuppercent2, lerner2)
colnames(HH)[8:10] <- c('margin', 'markuppercent', 'lerner')
HH %>% ggplot(aes(y = lerner, x = region)) + geom_boxplot(color='dark orange 2') + 
  labs(title='Holistic Health Index by Region',
       x='Region', 
       y='Lerner Index') + 
  theme(axis.title = element_text(size=16),
        plot.title=element_text(face='bold', size=17),
        axis.text=element_text(size=12))


lerners_data <- cbind(region=WS$region, lerner=WS$lerner)
extra <- cbind(region=HH$region, lerner=HH$lerner)
extra <- as.data.frame(extra)
extra$region <- 
  str_replace_all(extra$region, 'Midwest', 'Mid-West')
extra$region <- 
  str_replace_all(extra$region, 'Pacific', 'West Coast')
lerners_data <- rbind(lerners_data, extra)
lerners_data <- as.data.frame(lerners_data)
lerners_data$lerner <- as.character(lerners_data$lerner)
lerners_data$lerner <- as.numeric(lerners_data$lerner)

ggplot(lerners_data, aes(x=region, y=lerner)) + geom_boxplot(color='dark orange 1') + 
  stat_summary(fun.y='mean', geom='text', aes(label=..y..))




