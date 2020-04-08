install.packages('ggplot2', dependencies = TRUE)
library(ggplot2)
install.packages("plotly")
library(plotly)



webforum = read.csv("webforum.csv")
head(webforum)

# checking for duplicate records
post_id_freq = as.data.frame(table(webforum$PostID))
check_duplicates_post_id = post_id_freq[which(post_id_freq$Freq > 1),]
check_duplicates_post_id

#removing anonymous authors
author_id_legit = webforum[which(webforum$AuthorID != -1),]
write.csv(author_id_legit, file = "author_id_legit.csv")
author_id_legit_check = author_id_legit[which(author_id_legit$AuthorID == -1),]
author_id_legit_check


#removing posts with WC = 0
wc_greater_zero = author_id_legit[which(author_id_legit$WC > 0),]
#wc_greater_zero_check = author_id_legit[which(author_id_legit$WC == 0),]
write.csv(wc_greater_zero, file = "wc_greater_zero.csv")
wc_greater_zero_check = wc_greater_zero[which(wc_greater_zero$WC == 0),]
wc_greater_zero_check



#checking for erroneous values in linguistic atributes
check_analytic = wc_greater_zero[which(wc_greater_zero$Analytic < 0 | wc_greater_zero$Analytic > 100),]
check_analytic

check_clout = wc_greater_zero[which(wc_greater_zero$Clout < 0 | wc_greater_zero$Clout > 100),]
check_clout

check_authentic = wc_greater_zero[which(wc_greater_zero$Authentic < 0 | wc_greater_zero$Authentic > 100),]
check_authentic


check_tone = wc_greater_zero[which(wc_greater_zero$Tone < 0 | wc_greater_zero$Tone > 100),]
check_tone

check_posemo = wc_greater_zero[which(wc_greater_zero$posemo < 0 | wc_greater_zero$posemo > 100),]
check_posemo

check_negemo = wc_greater_zero[which(wc_greater_zero$negemo < 0 | wc_greater_zero$negemo > 100),]
check_negemo

check_anx = wc_greater_zero[which(wc_greater_zero$anx < 0 | wc_greater_zero$anx > 100),]
check_anx

check_anger = wc_greater_zero[which(wc_greater_zero$anger < 0 | wc_greater_zero$anger > 100),]
check_anger

check_social = wc_greater_zero[which(wc_greater_zero$social < 0 | wc_greater_zero$social > 100),]
check_social

check_family = wc_greater_zero[which(wc_greater_zero$family < 0 | wc_greater_zero$family > 100),]
check_family

check_friend = wc_greater_zero[which(wc_greater_zero$friend < 0 | wc_greater_zero$friend > 100),]
check_friend
check_work

check_leisure = wc_greater_zero[which(wc_greater_zero$leisure < 0 | wc_greater_zero$leisure > 100),]
check_leisure

check_home = wc_greater_zero[which(wc_greater_zero$home < 0 | wc_greater_zero$home > 100),]
check_home

check_money = wc_greater_zero[which(wc_greater_zero$money < 0 | wc_greater_zero$money > 100),]
check_money

check_relig = wc_greater_zero[which(wc_greater_zero$relig < 0 | wc_greater_zero$relig > 100),]
check_relig

check_swear = wc_greater_zero[which(wc_greater_zero$swear < 0 | wc_greater_zero$swear > 100),]
check_swear


#max(wc_greater_zero$WC, na.rm = TRUE)
#min(wc_greater_zero$WC, na.rm = TRUE)



#calculating total WC for each Thread
thread_wc = wc_greater_zero[,c("ThreadID","WC")]
thread_wc_sum = aggregate(thread_wc$WC, by=list(thread_wc$ThreadID), sum)
thread_wc_sum
colnames(thread_wc_sum) <- c("ThreadID", "WC_Sum")

thread_wc_sum_des = thread_wc_sum[order(thread_wc_sum$WC_Sum, decreasing = TRUE),]
head(thread_wc_sum_des)
length(thread_wc_sum_des[,1])

write.csv(thread_wc_sum_des, file = "thread_wc_sum_des.csv")







#getting top 5 threads by word count from each quarter of the dataframe
divide_sets = function(thread_wc_sum_des){
  counter = 0
  temp_v = integer()
  
  while (counter != 500) {
    i = counter
    
    if(counter == 0){
      
      j = 1
      
      
      while (j != 11) {
        temp_v <- c(temp_v, thread_wc_sum_des[j,1])
        j = j + 1
        
      }
      
      
      counter = counter + 125
    }
    
    else{
      loop_end = i + 10
      k = i
      while (k != loop_end) {
        temp_v <- c(temp_v, thread_wc_sum_des[k,1])
        k = k + 1
        
      }
      counter = counter + 125
    }
    
  }
  return(temp_v)
}



divide_set_vector = divide_sets(thread_wc_sum_des)

thread_vector_wc_largest = divide_set_vector[1:5]
thread_vector_wc_largest

thread_vector_wc_large = divide_set_vector[11:15]
thread_vector_wc_large

thread_vector_wc_medium = divide_set_vector[21:25]
thread_vector_wc_medium

thread_vector_wc_small = divide_set_vector[31:35]
thread_vector_wc_small






#getting all records for each of the selected threads from each quarter of the dataset

get_all_data = function(thread_vector_wc_x){
  temp_df = wc_greater_zero[FALSE,]
  
  for (i in 1:length(thread_vector_wc_x))
  {temp_df <- rbind(temp_df, wc_greater_zero[(wc_greater_zero$ThreadID == thread_vector_wc_x[i]),])}
  return(temp_df)
}

all_data_thread_wc_largest = get_all_data(thread_vector_wc_largest)
head(all_data_thread_wc_largest)
write.csv(all_data_thread_wc_largest, file = "all_data_thread_wc_largest.csv")
all_data_thread_wc_largest_wcr = all_data_thread_wc_largest[,c(1,2,3,6,7,8,9,10,19,20,21,22,23,24,25,26,27,28,29,30,31)]
all_data_thread_wc_largest_wcr

all_data_thread_wc_large = get_all_data(thread_vector_wc_large)
head(all_data_thread_wc_large)
write.csv(all_data_thread_wc_large, file = "all_data_thread_wc_large.csv")
all_data_thread_wc_large_wcr = all_data_thread_wc_large[,c(1,2,3,6,7,8,9,10,19,20,21,22,23,24,25,26,27,28,29,30,31)]


all_data_thread_wc_medium = get_all_data(thread_vector_wc_medium)
head(all_data_thread_wc_medium)
write.csv(all_data_thread_wc_medium, file = "all_data_thread_wc_medium.csv")
all_data_thread_wc_medium_wcr = all_data_thread_wc_medium[,c(1,2,3,6,7,8,9,10,19,20,21,22,23,24,25,26,27,28,29,30,31)]



all_data_thread_wc_small = get_all_data(thread_vector_wc_small)
head(all_data_thread_wc_small)
write.csv(all_data_thread_wc_small, file = "all_data_thread_wc_small.csv")
all_data_thread_wc_small_wcr = all_data_thread_wc_small[,c(1,2,3,6,7,8,9,10,19,20,21,22,23,24,25,26,27,28,29,30,31)]



library(plyr)

#comparing number of posts made by each author in each set of threads


largest_count = count(all_data_thread_wc_largest_wcr, c("AuthorID", "ThreadID"))
largest_count
mean(largest_count$freq)
length(all_data_thread_wc_largest_wcr[,1])
#total posts = 1295  average posts per author = 2.69

large_count = count(all_data_thread_wc_large_wcr, c("AuthorID", "ThreadID"))
large_count
mean(large_count$freq)
length(all_data_thread_wc_large_wcr[,1])
#total posts = 166  average posts per author = 1.953


medium_count = count(all_data_thread_wc_medium_wcr, c("AuthorID", "ThreadID"))
medium_count
mean(medium_count$freq)
length(all_data_thread_wc_medium_wcr[,1])
#total posts = 133  average posts per author = 2.015


small_count = count(all_data_thread_wc_small_wcr, c("AuthorID", "ThreadID"))
small_count
mean(small_count$freq)
length(all_data_thread_wc_small_wcr[,1])
#total posts = 135  average posts per author = 1.57



#taking means of all the linguistic attributes for each author in chosen threads
all_data_thread_wc_largest_wcr_mean = aggregate(all_data_thread_wc_largest_wcr[4:21], by=list(all_data_thread_wc_largest_wcr$ThreadID,all_data_thread_wc_largest_wcr$AuthorID), mean)
write.csv(all_data_thread_wc_largest_wcr_mean, file = "all_data_thread_wc_largest_wcr_mean.csv")
colnames(all_data_thread_wc_largest_wcr_mean)[1] = "ThreadID"
colnames(all_data_thread_wc_largest_wcr_mean)[2] = "AuthorID"

all_data_thread_wc_large_wcr_mean = aggregate(all_data_thread_wc_large_wcr[4:21], by=list(all_data_thread_wc_large_wcr$ThreadID,all_data_thread_wc_large_wcr$AuthorID), mean)
colnames(all_data_thread_wc_large_wcr_mean)[1] = "ThreadID"
colnames(all_data_thread_wc_large_wcr_mean)[2] = "AuthorID"

all_data_thread_wc_small_wcr_mean = aggregate(all_data_thread_wc_small_wcr[4:21], by=list(all_data_thread_wc_small_wcr$ThreadID,all_data_thread_wc_small_wcr$AuthorID), mean)
colnames(all_data_thread_wc_small_wcr_mean)[1] = "ThreadID"
colnames(all_data_thread_wc_small_wcr_mean)[2] = "AuthorID"



#findings authors that have posts in multiple threads in first quarter
largest_authors_freq = as.data.frame(table(largest_count$AuthorID))
largest_authors_mthreads = largest_authors_freq[which(largest_authors_freq$Freq > 2),]
colnames(largest_authors_mthreads) <- c("AuthorID", "ThreadCount")
largest_authors_mthreads



#analysis of author 39170. Posts in 4/5 threads with highest word count
a_39170_t_252620 = all_data_thread_wc_largest_wcr[which(all_data_thread_wc_largest_wcr$AuthorID == 39170 & all_data_thread_wc_largest_wcr$ThreadID == 252620),]
a_39170_t_252620

p1 <- plot_ly(a_39170_t_252620, x = ~PostID, y = ~Tone, type = 'scatter', mode = 'markers' ) %>%
  layout(title = "Author 39170 Posts in Thread 252620" , yaxis = list(title = "Tone"))
p1


t_252620_wo_39170 = all_data_thread_wc_largest_wcr[which(all_data_thread_wc_largest_wcr$AuthorID != 39170 & all_data_thread_wc_largest_wcr$ThreadID == 252620),]
t_252620_wo_39170

p2 <- plot_ly(t_252620_wo_39170, x = ~AuthorID, y = ~Tone, type = 'scatter', mode = 'markers' ) %>%
  layout(title = "Posts in Thread 252620 Without Author 39170" , yaxis = list(title = "Tone"))
p2



a_39170_t_145223 = all_data_thread_wc_largest_wcr[which(all_data_thread_wc_largest_wcr$AuthorID == 39170 & all_data_thread_wc_largest_wcr$ThreadID == 145223),]
a_39170_t_145223

p3 <- plot_ly(a_39170_t_145223, x = ~PostID, y = ~Tone, type = 'scatter', mode = 'markers', marker = list(color = 'red')) %>%
  layout(title = "Author 39170 Posts in Thread 145223" , yaxis = list(title = "Tone"))
p3


t_145223_wo_39170 = all_data_thread_wc_largest_wcr[which(all_data_thread_wc_largest_wcr$AuthorID != 39170 & all_data_thread_wc_largest_wcr$ThreadID == 145223),]
t_145223_wo_39170

p4 <- plot_ly(t_145223_wo_39170, x = ~AuthorID, y = ~Tone, type = 'scatter', mode = 'markers', marker = list(color = 'red')) %>%
  layout(title = "Posts in Thread 145223 Without Author 39170" , yaxis = list(title = "Tone"))
p4




a_39170_t_127115 = all_data_thread_wc_largest_wcr[which(all_data_thread_wc_largest_wcr$AuthorID == 39170 & all_data_thread_wc_largest_wcr$ThreadID == 127115),]
a_39170_t_127115

p5 <- plot_ly(a_39170_t_127115, x = ~PostID, y = ~Tone, type = 'scatter', mode = 'markers', marker = list(color = 'green')) %>%
  layout(title = "Author 39170 Posts in Thread 127115" , yaxis = list(title = "Tone"))
p5


t_127115_wo_39170 = all_data_thread_wc_largest_wcr[which(all_data_thread_wc_largest_wcr$AuthorID != 39170 & all_data_thread_wc_largest_wcr$ThreadID == 127115),]
t_127115_wo_39170

p6 <- plot_ly(t_127115_wo_39170, x = ~AuthorID, y = ~Tone, type = 'scatter', mode = 'markers', marker = list(color = 'green')) %>%
  layout(title = "Posts in Thread 127115 Without Author 39170" , yaxis = list(title = "Tone"))
p6



a_39170_t_472752 = all_data_thread_wc_largest_wcr[which(all_data_thread_wc_largest_wcr$AuthorID == 39170 & all_data_thread_wc_largest_wcr$ThreadID == 472752),]
a_39170_t_472752

p7 <- plot_ly(a_39170_t_472752, x = ~PostID, y = ~Tone, type = 'scatter', mode = 'markers', marker = list(color = 'purple')) %>%
  layout(title = "Author 39170 Posts in Thread 472752" , yaxis = list(title = "Tone"))
p7

t_472752_wo_39170 = all_data_thread_wc_largest_wcr[which(all_data_thread_wc_largest_wcr$AuthorID != 39170 & all_data_thread_wc_largest_wcr$ThreadID == 472752),]
t_472752_wo_39170

p8 <- plot_ly(t_472752_wo_39170, x = ~AuthorID, y = ~Tone, type = 'scatter', mode = 'markers', marker = list(color = 'purple')) %>%
  layout(title = "Posts in Thread 472752 Without Author 39170" , yaxis = list(title = "Tone"))
p8




posts_largest_mean_39170 = all_data_thread_wc_largest_wcr_mean[which(all_data_thread_wc_largest_wcr_mean$AuthorID == 39170),]
posts_largest_mean_39170



all_data_wo_39170 = all_data_thread_wc_largest_wcr[which(all_data_thread_wc_largest_wcr$AuthorID != 39170),]

mean_all_threads_largest_wo_39170 = aggregate(all_data_wo_39170[4:21], by=list(all_data_wo_39170$ThreadID), mean)
mean_all_threads_largest_wo_39170



# 39170 mean authentic for thread 127115: 33.087
# 39170 mean authentic for thread 145223: 37.96 
# 39170 mean authentic for thread 252620: 45.21 
# 39170 mean authentic for thread 472752: 34.18 


# mean Authentic all posts in thread 127115 exclusing posts by author 39170: 34.30
# mean Authentic all posts in thread 145223 exclusing posts by author 39170: 24.67
# mean Authentic all posts in thread 252620 exclusing posts by author 39170: 37.42
# mean Authentic all posts in thread 472752 exclusing posts by author 39170: 56.12


plot_scatter_graph = function(q, ThID) {
  selected_thread_data = q[which(q$ThreadID == ThID),]
  ttitle = paste("Thread", as.character(ThID), sep=" ")
  xtitle = paste("Authors in Thread", as.character(ThID), sep=" ")
  
  g <- plot_ly(selected_thread_data, y = ~`anx`, type = 'scatter', mode = 'lines+markers', name = 'anxiety') %>%
    add_trace(y = ~`anger`, name = 'anger', mode = 'lines+markers') %>%
    add_trace(y = ~`work`, name = 'work', mode = 'lines+markers') %>%
    add_trace(y = ~`money`, name = 'money', mode = 'lines+markers') %>%
    layout(title = ttitle, yaxis = list(title = 'Mean %'), xaxis = list(title = xtitle), barmode = 'stack')
  g
}



plot_bar_graph = function(q, ThID){
  selected_thread_data = q[which(q$ThreadID == ThID),]
  
  ttitle = paste("Thread", as.character(ThID), sep=" ")
  
  p <- plot_ly(selected_thread_data, x = ~AuthorID, y = ~Tone, type = 'bar') %>%
    layout(title = ttitle, yaxis = list(title = "Mean Tone"),  bargap = 0.2)
  
  return(p)
}




#creating bar graph and scatter graph to compare mean Authentic and Tone for all 5 threads in each chosen quarter
#and to comapre anx, anger, work and money in all 5 threads in each chosen quarter

#quarter1 (largest wc)
unique_threads_largest = thread_vector_wc_largest
unique_threads_largest

plot_bar_graph(all_data_thread_wc_largest_wcr_mean, 127115)
plot_bar_graph(all_data_thread_wc_largest_wcr_mean, 145223)
plot_bar_graph(all_data_thread_wc_largest_wcr_mean, 252620)
plot_bar_graph(all_data_thread_wc_largest_wcr_mean, 254138)
plot_bar_graph(all_data_thread_wc_largest_wcr_mean, 472752)


plot_scatter_graph(all_data_thread_wc_largest_wcr_mean, 127115)
plot_scatter_graph(all_data_thread_wc_largest_wcr_mean, 145223)
plot_scatter_graph(all_data_thread_wc_largest_wcr_mean, 252620)
plot_scatter_graph(all_data_thread_wc_largest_wcr_mean, 254138)
plot_scatter_graph(all_data_thread_wc_largest_wcr_mean, 472752)


#quarter2 (large wc)
unique_threads_large = thread_vector_wc_large
unique_threads_large

plot_bar_graph(all_data_thread_wc_large_wcr_mean, 330904)
plot_bar_graph(all_data_thread_wc_large_wcr_mean, 767538)
plot_bar_graph(all_data_thread_wc_large_wcr_mean, 365246)
plot_bar_graph(all_data_thread_wc_large_wcr_mean, 190589)
plot_bar_graph(all_data_thread_wc_large_wcr_mean, 813754)

plot_scatter_graph(all_data_thread_wc_large_wcr_mean, 330904)
plot_scatter_graph(all_data_thread_wc_large_wcr_mean, 767538)
plot_scatter_graph(all_data_thread_wc_large_wcr_mean, 365246)
plot_scatter_graph(all_data_thread_wc_large_wcr_mean, 190589)
plot_scatter_graph(all_data_thread_wc_large_wcr_mean, 813754)



#quarter4 (small wc)
unique_threads_small = thread_vector_wc_small
unique_threads_small


plot_bar_graph(all_data_thread_wc_small_wcr_mean, 389413)
plot_bar_graph(all_data_thread_wc_small_wcr_mean, 255772)
plot_bar_graph(all_data_thread_wc_small_wcr_mean, 395454)
plot_bar_graph(all_data_thread_wc_small_wcr_mean, 547927)
plot_bar_graph(all_data_thread_wc_small_wcr_mean, 450327)

plot_scatter_graph(all_data_thread_wc_small_wcr_mean, 389413)
plot_scatter_graph(all_data_thread_wc_small_wcr_mean, 255772)
plot_scatter_graph(all_data_thread_wc_small_wcr_mean, 395454)
plot_scatter_graph(all_data_thread_wc_small_wcr_mean, 547927)
plot_scatter_graph(all_data_thread_wc_small_wcr_mean, 450327)



#timeseries analysis

#finding threads from q1 with the highest number of posts 
post_count = integer()
for (thread in thread_vector_wc_largest) {
  subset = all_data_thread_wc_largest[which(all_data_thread_wc_largest$ThreadID == thread),]
  
  tcount = count(subset, "ThreadID")
  print(thread)
  print(as.integer(tcount$freq))
  
  post_count <- c(post_count, as.integer(tcount$freq))
}
#top 2 threads with highest number of posts in q1: 252620 , 127115



#finding author with highest number of posts in each of the threads(252620, 127115)
chosen_threads = thread_vector_wc_largest[1:2]
auth_count = integer()
for (t in chosen_threads) {
  cthread_data = all_data_thread_wc_largest[which(all_data_thread_wc_largest$ThreadID == thread),]
  
  authcount = count(cthread_data, "AuthorID")
  print(t)
  print(authcount)
  print(max(authcount$freq))
  #auth_count <- c(auth_count, as.integer(authcount$freq))
}
#chosen author 39170. 16 posts in each of the threads(252620, 127115)


#building time series for thread 252620 with extreme values(Dates) removed. Chosen variables Clout & Analytic
data252620 = all_data_thread_wc_largest[which(all_data_thread_wc_largest$ThreadID == 252620),]
data252620



data252620$Date <- as.Date(data252620$Date, "%Y-%m-%d")
data252620$Date
data252620_exv_rm = data252620[which(data252620$Date < "2006-03-30"),]
min(data252620$Date)
max(data252620$Date)



data252620_date_dec = data252620_exv_rm[order(data252620_exv_rm$Date),]
data252620_date_dec


ts252620 <- plot_ly(x = ~data252620_date_dec$Date, y = ~data252620_date_dec$Clout, type = 'scatter', mode = 'lines+markers')%>%
  layout(title = "Thread 252620", yaxis = list(title = "Clout"), xaxis = list(title = "Date"))
ts252620



#building time series for thread 127115. Chosen variables Clout & Analytic
data127115 = all_data_thread_wc_largest[which(all_data_thread_wc_largest$ThreadID == 127115),]
data127115


data127115$Date <- as.Date(data127115$Date, "%Y-%m-%d")
data127115$Date
min(data127115$Date)
max(data127115$Date)


data127115_date_dec = data127115[order(data127115$Date),]
data127115_date_dec


ts127115 <- plot_ly(x = ~data127115_date_dec$Date, y = ~data127115_date_dec$Clout, type = 'scatter', mode = 'lines+markers')%>%
  layout(title = "Thread 127115", yaxis = list(title = "Clout"), xaxis = list(title = "Date"))
ts127115



#building time series for author 39170. Chosen variables Clout & Analytic
author39170_data = all_data_thread_wc_largest[which(all_data_thread_wc_largest$AuthorID == 39170),]
head(author39170_data)
author39170_data$Date <- as.Date(author39170_data$Date, "%Y-%m-%d")
min(author39170_data$Date)
max(author39170_data$Date)

author39170_data_asc = author39170_data[order(author39170_data$Date),]
author39170_data_asc

ts39170 <- plot_ly(x = ~author39170_data_asc$Date, y = ~author39170_data_asc$Clout, type = 'scatter', mode = 'lines+markers')%>%
  layout(title = "Author 39170", yaxis = list(title = "Clout"), xaxis = list(title = "Date"))
ts39170
