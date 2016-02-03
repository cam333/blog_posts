library(dplyr)
library(ggplot2)
# Data Pull
#this is a git test!
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

all_data <- left_join(product_class_raw,product_raw, by = "product_class_id") %>%
  left_join(transactions_raw,data1, by = "product_id")%>%
  mutate(product_department = gsub("'","",product_department),
         product_category = gsub("'","",product_category),
         product_subcategory = gsub("'","",product_subcategory),
         product_family = gsub("'","",product_family),
         quarter = gsub("'","",quarter),
         month_of_year = gsub("'","",month_of_year))

# Have a look at unit sales by department
department_data <- all_data  %>%
  group_by(product_department) %>%
  summarize(total_sales = sum(as.numeric(unit_sales), na.rm = TRUE)) %>%
  na.omit()

ggplot(department_data, aes(x = reorder(product_department,total_sales), y = total_sales)) +
  geom_bar(stat = 'identity',aes(fill = product_department)) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  coord_flip()+
  xlab("Product Department") +
  ylab("Unit Sales") +
  ggtitle("Sum of Unit Sales by Product Department")

# by category

category_data <- all_data  %>%
  group_by(product_category) %>%
  summarize(total_sales = sum(as.numeric(unit_sales), na.rm = TRUE)) %>%
  na.omit()

ggplot(category_data, aes(x = reorder(product_category,total_sales), y = total_sales)) +
  geom_bar(stat = 'identity',aes(fill = product_category)) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  coord_flip()+
  xlab("Product Department") +
  ylab("Unit Sales") +
  ggtitle("Sum of Unit Sales by Product Department by product_category")


# by store


sub_category_data <- all_data  %>%
  group_by(product_subcategory) %>%
  summarize(total_sales = sum(as.numeric(unit_sales), na.rm = TRUE), product_category = last(product_category)) %>%
  na.omit() %>%
  arrange(product_category)



ggplot(sub_category_data , aes(x = product_subcategory, y = total_sales)) +
  geom_bar(stat = 'identity',aes(fill = product_category)) +
  theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5)) +
  coord_flip()+
  xlab("Product Department") +
  ylab("Unit Sales") +
  ggtitle("Sum of Unit Sales by Product Department by product_category")


