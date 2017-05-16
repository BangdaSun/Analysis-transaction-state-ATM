# Import data
setwd("C://Users//Bangda//Desktop//project-ATM exception detection")
part1 = read.csv("part1.csv", header = TRUE, as.is = TRUE)
part2 = read.csv("part2.csv", header = TRUE, as.is = TRUE)
part3 = read.csv("part3.csv", header = TRUE, as.is = TRUE)
part4 = read.csv("part4.csv", header = TRUE, as.is = TRUE)
part1$month = 1
part2$month = 2
part3$month = 3
part4$month = 4

summary(part1)
str(part1)

#####################
### Data cleaning ###
#####################

# Set the approiate columna name
transacData = rbind(rbind(rbind(part1, part2), part3), part4)
colnames(transacData)[1] = "date"

# Convert character into numeric
transacData$success_rate = unlist(strsplit(transacData$success_rate, split = "%"))
transacData$success_rate = as.numeric(transacData$success_rate) / 100

removeComma = function(char) {
  # @param - char, like "1,200"
  # @return - newchar, like 1200
  charvec = c(unlist(strsplit(char, split = ",")))
  newchar = paste(charvec, collapse = "")
  return(as.numeric(newchar))
}

transacData$response_time = sapply(transacData$response_time, removeComma)
transacData$tran_amount = sapply(transacData$tran_amount, removeComma)

########################
### Data exploration ###
########################

library(ggplot2)
table(transacData$date)
# Difference of transacction amount
# Take a small subset of first 16 days
transac_sub1 = subset(transacData, date%in%c("123", "124", "125", "126", 
                                             "127", "128", "129", "130", 
                                             "131", "201", "202", "203",
                                             "205", "206", "207", "208"))

ggplot(transac_sub1) + 
  geom_line(mapping = aes(x = time, y = tran_amount)) +
  facet_wrap(~ date, ncol = 4)

# Take another subset of 16 days
transac_sub2 = subset(transacData, date%in%c("323", "324", "325", "326", 
                                             "327", "328", "329", "330", 
                                             "331", "401", "402", "403",
                                             "405", "406", "407", "408"))

ggplot(transac_sub2) + 
  geom_line(mapping = aes(x = time, y = tran_amount)) +
  facet_wrap(~ date, ncol = 4)

transac_sub3 = subset(transacData, date%in%c("210", "211", "212", "213", 
                                             "214", "215", "216", "217", 
                                             "301", "302", "303", "304",
                                             "305", "306", "307", "308"))

ggplot(transac_sub3) + 
  geom_line(mapping = aes(x = time, y = tran_amount)) +
  facet_wrap(~ date, ncol = 4)

# Difference of response time
ggplot(transac_sub1, aes(response_time)) +
  geom_histogram(binwidth = 5)

ggplot(transac_sub1, aes(response_time)) + 
  geom_histogram(binwidth = 5) + 
  facet_wrap(~ date, nrow = 4)

# Difference of sucessful rate
ggplot(transac_sub1, aes(success_rate)) + 
  geom_histogram(binwidth = .01) + 
  facet_wrap(~ date, nrow = 4)


# Calculate the mean of every day index
tran_amount_mean = tapply(transacData$tran_amount, INDEX = transacData$date, mean)
success_rate_mean = tapply(transacData$success_rate, INDEX = transacData$date, mean)
response_time_mean = tapply(transacData$response_time, INDEX = transacData$date, mean)
meanValue_df = data.frame(date = names(tran_amount_mean),
                          tran_amount_mean = tran_amount_mean,
                          success_rate_mean = success_rate_mean,
                          response_time_mean = response_time_mean)

p1 = ggplot(meanValue_df, aes(x = date, y = tran_amount_mean)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

p2 = ggplot(meanValue_df, aes(x = date, y = success_rate_mean)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

p3 = ggplot(meanValue_df, aes(x = date, y = response_time_mean)) + 
  geom_bar(stat = "identity") +
  theme(axis.text.x = element_text(angle = 90))

library(grid)
library(gridExtra)
grid.arrange(p1, p2, p3, ncol = 1)

# Therefore we can see that success rate is almost same for all day, the difference 
# of success rate might show difference in different time in one day

# Next we try to distinguish different "type" of days
tran_amount_max = tapply(transacData$tran_amount, INDEX = transacData$date, max)
tran_amount_maxdf = data.frame(date = names(tran_amount_max),
                               tran_amount_max = tran_amount_max)
tran_amount_maxdf$month = as.numeric(substr(tran_amount_maxdf$date, start = 1, stop = 1))

ggplot(tran_amount_maxdf) + 
  geom_line(mapping = aes(x = as.numeric(date), y = tran_amount_max)) + 
  geom_point(mapping = aes(x = as.numeric(date), y = tran_amount_max)) + 
  labs(x = "date", y = "max transaction amount")

# Therefore we can see that the max transaction amount is high at the end of January

ggplot(subset(tran_amount_maxdf, month != 1)) + 
  geom_line(mapping = aes(x = as.numeric(date), y = tran_amount_max)) + 
  geom_point(mapping = aes(x = as.numeric(date), y = tran_amount_max)) + 
  labs(x = "date", y = "max transaction amount")

ggplot(subset(tran_amount_maxdf, month != 1)) +
  geom_line(mapping = aes(x = as.numeric(date), y = tran_amount_max)) + 
  geom_point(mapping = aes(x = as.numeric(date), y = tran_amount_max)) + 
  facet_wrap(~ month, nrow = 3) + 
  labs(x = "date", y = "max transaction amount")

#############################################################################################
