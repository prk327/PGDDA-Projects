setwd("E:/UpGrad IIIT PGDDA Program/Course 1/Course 1 Group Case Study")


#Creating a data frame companies from companies.txt by mapping string variable to "NA" and character variable should be stores as character

companies <- read.delim("companies.txt",header = TRUE,sep = "\t",na.strings = "",stringsAsFactors = FALSE)


#Creating a data frame Rounds2 from rounds2.csv by mapping string variable to "NA" and character variable should be stores as character


rounds2 <- read.csv("rounds2.csv",header = TRUE,na.strings = "",stringsAsFactors = FALSE)


#checking the variable attribute

str(companies)

str(rounds2)

#Counting the number of unique companies in rounds2 file:-
#since R is a case sensative language we need to first convert the unique company column in lower case format
#also we need to remove any leading and trailing blanks

companies$permalink <- trimws(tolower(companies$permalink))


rounds2$company_permalink <- trimws(tolower(rounds2$company_permalink))


#after converting the format we can now use the unique function on permalink to get the unique companies name
#further we can use nrow to count the number of those companies

nrow(data.frame(unique(companies$permalink)))

nrow(data.frame(unique(rounds2$company_permalink)))

#now we need to find out if there is any companies which are not present in round2 file
#for this we would be matching the colunm from round2 with the column in companies DF
#and then sum the total match

sum(rounds2$company_permalink %in% companies$permalink)

#If the sum of total match is less than the sum of round2 observation then 
#we can know that there is some companies which are not present in companies
#so for camparison we have use if statement to match the number of rows with the 
#sum of match of companies name, the "No" will represent that all companies are
#present in round2 files

a = sum(rounds2$company_permalink %in% companies$permalink)
b = nrow(rounds2)

c <- if (a == b) {print("No")} else {print("Yes")}

paste ("Are there any companies in the rounds2 file which are not present in companies ? Answer",c )



#now before we can merge the two files we have to figure out the granularity of each files
#view the companies file

View(companies)

#we then check for any duplicate companies by creating a DF of Duplicate companies

x <- subset(companies,duplicated(companies$name) == TRUE)

View(x)

#we can then use any company name and find out why it is duplicate
#lets use company "Adtena"


x <- subset(companies,companies$name == "Adtena")

View(x)

#after viewing we can see that the company status if different
#so we can consider ignoring "closed" company status for our anysis


#we set subset base on status not equal to closed

companies$status <- trimws(tolower(companies$status))

companies_1 <- subset(companies,companies$status != "closed")

#now lets find out the rounf2 granulatiry

x = subset(rounds2,duplicated(rounds2$company_permalink) == TRUE)

View(x)

#we can use company permalink of any duplicate to analyse 


x = subset(rounds2,rounds2$company_permalink == "/organization/0xdata")

View(x)

x = subset(rounds2,rounds2$company_permalink == "/organization/-qounter")

View(x)

#after analysing both the two companies we came to the conclusion that
#the granularity level of round2 is at company_parmalink,funding_round_type and funded_at

#we will first create a new colunm based on above three colunm

rounds2$funding_round_type <- trimws(tolower(rounds2$funding_round_type))

rounds2$unique <- paste(rounds2$company_permalink,rounds2$funding_round_type,rounds2$funded_at)

#remove all the duplicate unique colunm

rounds1 <- subset(rounds2,duplicated(rounds2$unique) == FALSE)

#we would also like to first structure our data and for that
#we would be creating a new column primary sector by cleaning the data

companies_1$category_list <- trimws(tolower(companies_1$category_list))

#and then by taking first sector as primary sector 

companies_1$primary_sector <- sub("[|].*","",companies_1$category_list)

#now we will load the mapping fire in r to map the category to primary sector

mapping <- read.csv("mapping_file.csv",header = TRUE,stringsAsFactors = FALSE,na.strings = "")


#now we will clean the mapping file for merge

mapping$category_list <- trimws(tolower(mapping$category_list))

#now we will map the main sector to primary sector
#for that we will create map_companies

map_companies <- merge(companies_1,mapping,by.x = "primary_sector",by.y = "category_list",all = FALSE)



#after merge we see that there is difference in the no of observations
#so we will check which observation are not included in merge file

x <- which(companies_1$primary_sector %in% mapping$category_list == FALSE)

y = companies_1[x,]

View(y)

#we see that there are 55 observations

z <- factor(y$primary_sector)

levels(z)

#we see that there are 36 primary sector which are not categorised under mapping files
#so for analysis perpose we will put all 55 observations to software since it fall under others main sector

companies_1[x,11] <- "software"

#now again we will  try to merge the files

map_companies <- merge(companies_1,mapping,by.x = "primary_sector",by.y = "category_list",all = FALSE)

#we see that both the map and companies_1 files have the same obs

#now we have a cleaner data frame

#but before merge we would first drop unique from rounds1 data sets

rounds4 <- subset(rounds1,select = -c(unique))

#also we require to exclude blank sector from our companies

map_companies$main_sector <- trimws(tolower(map_companies$main_sector))

#for that we will replace blanks with the most occurred sector
#we will put the most occured sector in DF with frequency

j <- as.data.frame(table(map_companies$main_sector))

k <- which(j$Freq == max(j$Freq))

most_occu_sec <- as.character(j[k,1])

#so the most occured sector is others

x <- which(map_companies$main_sector == "blanks")


map_companies[x,12] <- most_occu_sec


#we will create a new data frame master_frame through inner merge



master_frame <- merge(rounds4,map_companies,by.x = "company_permalink",by.y = "permalink",all = FALSE)

#after merge we see that there is difference in obs

diff_obs <- nrow(rounds4) - nrow(master_frame)

#so we will check which obs is not present in master frame

a <- which(rounds4$company_permalink %in% map_companies$permalink == FALSE)

#by which get the index of only the matching company that are not present
#and put it to DF b

b = rounds4[a,]

#we will then create a DF co which contain only names of the companies
#which are not present in master frame

co <- data.frame(rounds4[a,1])

#now we will get the index of all the missing companies from companies file
#by matching co with companies parmalink

z = which(companies$permalink %in% co$rounds4.a..1.)

#by using the index we will create a vector c01 which contain the status of the companies


co1 <- companies[z,5]

x <- factor(co1)

levels(x)

#by looking at the level we say that all obs are of closed companies
#so our merge is good to go





#now we will see how many "na" are there in raised amount usd

x = sum(is.na(master_frame$raised_amount_usd))

#now lets see what is the % of this na values

y <- nrow(master_frame)

na <- (x/y)*100

#since na is more than 10% of the total rows we cannot simply delete those rows
#we will now check for any outlies using boxplot and scatter plot

boxplot(master_frame$raised_amount_usd)

library(ggplot2)

ggplot(master_frame,aes(x = main_sector,y = raised_amount_usd)) + geom_point() 

#we see that there exist an outliers
#we will also veryfy this through normal distribution curve

x <- master_frame$raised_amount_usd

stand <- sd(master_frame$raised_amount_usd,na.rm = TRUE)
meann <- mean(master_frame$raised_amount_usd,na.rm = TRUE)

y = dnorm(master_frame$raised_amount_usd,mean = meann,sd = stand)

plot(x,y)

#so to normalize the outliers we would first sort the DF using decending order

z <- master_frame[with(master_frame,order(-raised_amount_usd)),]

#so for our analysis purpose we would consider any investment which is more
#than 1 billion will be treated as 1 billion only
#so we will replace all the amount greater than 1 billion to less than 1 billion

x = which(master_frame$raised_amount_usd > 999999999)

master_frame[x,6] <- 999999999

#now we will replace all the "NA" values with the mean of the raised amount usd

raised_mean <- mean(master_frame$raised_amount_usd,na.rm = TRUE)

x = which(is.na(master_frame$raised_amount_usd) == TRUE)

master_frame[x,6] <- as.integer(raised_mean)

#now we have to find out the average funding amount by funding round type

avg_ven <- aggregate(master_frame$raised_amount_usd,by = list(master_frame$funding_round_type),FUN = "mean" )

#now since spark fund wants to invest between 5 to 15 million usd we need to filter our search by
#usd 5 to 15 millions and which finding type receive most of those investments

suitable_type <- subset(master_frame,master_frame$raised_amount_usd >= 5 & master_frame$raised_amount_usd <= 15000000)

#so the investment type which received most investment is

d <- factor(suitable_type$funding_round_type)

e <- as.data.frame(table(d))

f <- which(e$Freq == max(e$Freq))

paste("So the most suitable investment type between 5 to 15 million usd will be", e[f,1])

#now we have to find out which are the top english speaking countries



#first we will subset based on top countries in the master frame

g <- master_frame$country_code

#we will store the table information in data frame h

h <- data.frame(table(g))

#then we will sord by descending order 

i <- h[with(h,order(-Freq)),]

#so the first, second and third top english speaking countries are USA,GBR and CAN respectively

paste("Top English Speaking Country as per most number of investments is", i[1,1])

paste("Second English Speaking Country as per most number of investments is", i[2,1])

paste("Third English Speaking Country as per most number of investments is", i[3,1])

#now we have to create three data frame each of the top three countries

top_country <- subset(master_frame,master_frame$country_code == i[1,1] & master_frame$funding_round_type == e[f,1] & master_frame$raised_amount_usd >= 5 & master_frame$raised_amount_usd <= 15000000) 

second_country <- subset(master_frame,master_frame$country_code == i[2,1] & master_frame$funding_round_type == e[f,1] & master_frame$raised_amount_usd >= 5 & master_frame$raised_amount_usd <= 15000000)

third_country <- subset(master_frame,master_frame$country_code == i[3,1] & master_frame$funding_round_type == e[f,1] & master_frame$raised_amount_usd >= 5 & master_frame$raised_amount_usd <= 15000000)

#now we will create three seperate df for count of total number of investments

top_inv_count <- top_country$main_sector

top_inv_count <- data.frame(table(top_inv_count))

colnames(top_inv_count) <- c("main_sector",as.character(i[1,1]))

second_inv_count <- second_country$main_sector

second_inv_count <- data.frame(table(second_inv_count))

colnames(second_inv_count) <- c("main_sector",as.character(i[2,1]))

third_inv.count <- third_country$main_sector

third_inv.count <- data.frame(table(third_inv.count))

colnames(third_inv.count) <- c("main_sector",as.character(i[3,1]))

#now once we have created the above three df for each country by no of inv sector
#we will merge all the three df in one df which will have total number of invest per sector in dirrerent country

no_inv_sec_cntry <- merge(top_inv_count,second_inv_count,by = "main_sector",all = FALSE)

no_inv_sec_cnt <- merge(no_inv_sec_cntry,third_inv.count,by = "main_sector",all = FALSE)


# now we will create sum of total investment by sector in top three countries

top_sum_coun <- aggregate(top_country$raised_amount_usd,by = list(top_country$main_sector),FUN = "sum")

colnames(top_sum_coun) <- c("main_sector", paste("sum of inv in",as.character(i[1,1])))

second_sum_coun <- aggregate(second_country$raised_amount_usd,by = list(second_country$main_sector),FUN = "sum")

colnames(second_sum_coun) <- c("main_sector", paste("sum of inv in",as.character(i[2,1])))

third_sum_coun <- aggregate(third_country$raised_amount_usd,by = list(third_country$main_sector),FUN = "sum")

colnames(third_sum_coun) <- c("main_sector", paste("sum of inv in",as.character(i[3,1])))

#now we will merge all the above three sum of inv in one DF

sum_inv_sec_cntry <- merge(top_sum_coun,second_sum_coun,by = "main_sector",all = FALSE)

sum_inv_sec_cnt <- merge(sum_inv_sec_cntry,third_sum_coun,by = "main_sector",all = FALSE)

#so the answers to the below questions are:- 

paste("Total number of Investments (count) by countries top second third are",sum(no_inv_sec_cnt$USA),sum(no_inv_sec_cnt$GBR),sum(no_inv_sec_cnt$CAN),"respectively")

paste("Total amount of investment (USD) by countries top second third are",sum(sum_inv_sec_cnt$`sum of inv in USA`),sum(sum_inv_sec_cnt$`sum of inv in GBR`),sum(sum_inv_sec_cnt$`sum of inv in CAN`),"respectively")

#to know the top sector name we will order the df "no_inv_sec_cnt" country wise


top_order_sector <- no_inv_sec_cnt[with(no_inv_sec_cnt,order(-no_inv_sec_cnt[,2])),]

second_order_sector <- no_inv_sec_cnt[with(no_inv_sec_cnt,order(-no_inv_sec_cnt[,3])),]

third_order_sector <- no_inv_sec_cnt[with(no_inv_sec_cnt,order(-no_inv_sec_cnt[,4])),]

#to answer the below questions: - 

paste("Top Sector name (no. of investment-wise) top second and third are", "'",top_order_sector[1,1],"'",",","'",second_order_sector[1,1],"'",",","'",third_order_sector[1,1],"'","respectively")


paste("Second Sector name (no. of investment-wise) top second and third are", "'",top_order_sector[2,1],"'",",","'",second_order_sector[2,1],"'",",","'",third_order_sector[2,1],"'","respectively")

paste("Third Sector name (no. of investment-wise) top second and third are", "'",top_order_sector[3,1],"'",",","'",second_order_sector[3,1],"'",",","'",third_order_sector[3,1],"'","respectively")

paste("Number of investments in top sector (3) of top second and third countries are","'",top_order_sector[1,2],"'",",","'",second_order_sector[1,3],"'",",","'",third_order_sector[1,4],"'","respectively")

paste("Number of investments in second sector (3) of top second and third countries are","'",top_order_sector[2,2],"'",",","'",second_order_sector[2,3],"'",",","'",third_order_sector[2,4],"'","respectively")

paste("Number of investments in third sector (3) of top second and third countries are","'",top_order_sector[3,2],"'",",","'",second_order_sector[3,3],"'",",","'",third_order_sector[3,4],"'","respectively")

#now we have find out the company which received highest investment in term of USD in each country

#so we will create a df with aggregate of investment based on company on each 3 country df

top_company <- aggregate(top_country$raised_amount_usd,by = list(top_country$name,top_country$main_sector),FUN = "sum")

colnames(top_company) <- c("Company Name","main_sector","Total Inv in USD")

top_company <- top_company[with(top_company,order(-top_company$`Total Inv in USD`)),]

#second country

second_company <- aggregate(second_country$raised_amount_usd,by = list(second_country$name,second_country$main_sector),FUN = "sum")

colnames(second_company) <- c("Company Name","main_sector","Total Inv in USD")

second_company <- second_company[with(second_company,order(-second_company$`Total Inv in USD`)),]

#third county

third_company <- aggregate(third_country$raised_amount_usd,by = list(third_country$name,third_country$main_sector),FUN = "sum")

colnames(third_company) <- c("Company Name","main_sector","Total Inv in USD")

third_company <- third_company[with(third_company,order(-third_company$`Total Inv in USD`)),]

#now we are ready to answer the below questions:-



c1_company <- top_company[which(top_company$main_sector == top_order_sector[1,1]),]

c2_company <- second_company[which(second_company$main_sector == second_order_sector[1,1]),]

c3_company <- third_company[which(third_company$main_sector == third_order_sector[1,1]),]

#Q9 For point 3 (top sector count-wise), which company received the highest investment?


c1_company[1,1]
c2_company[1,1]
c3_company[1,1]

#For point 4 (second best sector count-wise), which company received the highest investment?

c1_company[2,1]
c2_company[2,1]
c3_company[2,1]

#checkpoint 7: plots

#so to create below pie chart
#One pie chart showing the fraction of total investments (globally) in venture, seed and private equity and the average amount of investment in each funding type.
#we will first create a DF with total investments based on funding round type

total_inv_type <- aggregate(master_frame$raised_amount_usd,by = list(master_frame$funding_round_type),FUN ="sum")

avg_inv_type <- aggregate(master_frame$raised_amount_usd,by = list(master_frame$funding_round_type),FUN ="mean")

colnames(total_inv_type) <- c("Funding_Type","Sum_of_total_inv")

colnames(avg_inv_type) <- c("Funding_Type","Avg_of_total_inv")

funding_type <- merge(total_inv_type,avg_inv_type,by="Funding_Type",all = FALSE)


#now we will create a new df with only venture seed and private equity column with the percent of total investments

frac_total_type <- subset(funding_type,funding_type$Funding_Type == "private_equity" & funding_type$Funding_Type == "seed" & funding_type$Funding_Type == "venture")

total_inv_type$percent <-  prop.table(total_inv_type$Sum_of_total_inv)

sum(total_inv_type$percent)

write.table(total_inv_type,file = "piechart.txt",sep = "\t",row.names = FALSE)

library(ggplot2)

Plot1 <- ggplot(total_inv_type,aes(x="",y = percent,fill = Funding_Type)) + geom_bar(width = 1, stat = "identity") + coord_polar("y",start = 0)

