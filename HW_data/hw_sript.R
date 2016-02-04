library(dplyr)
library(ggplot2)
# Data Pull
#this is a git test! MORE STUFF
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
  left_join(transactions_raw,., by = "product_id")%>%
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


# by sub category


sub_category_data <- all_data  %>%
  group_by(product_subcategory) %>%
  summarize(total_sales = sum(as.numeric(unit_sales), na.rm = TRUE), product_department = last(product_department)) %>%
  na.omit() %>%
  arrange(product_department)

all_letters <- paste(paste(letters,collapse="|"),paste(LETTERS,collapse="|"),sep="|")

promotion_data <- data.frame(promotion_raw,stringsAsFactors = FALSE) %>%
  mutate(media_type = gsub("'","",media_type),
         promotion_id = gsub("'","",promotion_id),
         start_date = gsub("'","",start_date),
         end_date = gsub("'","",end_date),
         cost = gsub("'","",cost),
         promotion_name = gsub("'","",promotion_name),
         promotion_district_id = gsub("'","",promotion_district_id),
         all_types = ifelse(grepl(all_letters,as.character(cost)) == FALSE, media_type,paste(media_type,cost,sep=",")),
         new_date_1 = ifelse(lead(all_types) == "",lead(promotion_id),"false"),
         new_date_2 = ifelse(lead(promotion_district_id) != "" & lead(all_types) == "", lead(promotion_district_id), "false"),
         real_start_date = ifelse(new_date_1 == "false" & new_date_2 == 'false',start_date,new_date_1),
         real_end_date =   ifelse(new_date_1 != "false" & new_date_2 != 'false',new_date_2,end_date),
         real_cost =  ifelse(lead(promotion_id)!= "" & lead(promotion_district_id) != "" & lead(all_types) == "", end_date,
                      ifelse(lead(promotion_id)!= "" & lead(promotion_district_id) == "", start_date, cost))) %>%
  filter(real_start_date != "" & real_start_date != "NULL" ) %>%
  mutate(all_types = gsub("'","",all_types)) %>%
  select(-new_date_1,-new_date_2,-start_date,-end_date,-cost)



