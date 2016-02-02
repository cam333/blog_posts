
# Data Pull
product_raw <- read.csv("https://raw.githubusercontent.com/cam333/blog_posts/master/HW_data/product.csv")

product_class_raw <- read.csv("https://raw.githubusercontent.com/cam333/blog_posts/master/HW_data/product_class.csv")

promotion_raw <- read.csv("https://raw.githubusercontent.com/cam333/blog_posts/master/HW_data/promotion.csv")

transactions_raw <- read.csv("https://raw.githubusercontent.com/cam333/blog_posts/master/HW_data/transactions.csv")

# look at ths summary of the data sets
summary(product_raw)
summary(product_class_raw)
summary(promotion_raw)
summary(transactions_raw)

#didn't see any NAs

# Question 1
# lets merge some data
library(dplyr)
library(ggplot2)

data1 <- left_join(product_class_raw,product_raw, by = "product_class_id")
data2 <- left_join(transactions_raw,data1, by = "product_id")

department_data <- data2 %>%
  mutate(product_department = gsub("'","",product_department),
         quarter = gsub("'","",quarter),
         month_of_year = gsub("'","",month_of_year)) %>%
  group_by(product_department,quarter) %>%
  summarize(total_sales = sum(as.numeric(unit_sales))) %>%
  arrange(total_sales)

ggplot(department_data, aes(x = reorder(product_department,total_sales), y = total_sales)) +
  geom_bar(stat = 'identity',aes(fill = product_department)) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))

ggplot(department_data, aes(x = reorder(product_department,total_sales), y = total_sales)) +
  geom_bar(stat = 'identity',aes(fill = product_department)) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  facet_grid(.~quarter)

