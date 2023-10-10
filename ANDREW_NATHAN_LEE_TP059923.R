# ANDREW NATHAN LEE
# TP059923

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data import~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
install.packages("readr")
library(readr)
# read CSV file from source location
dataset = read_delim(file = "C:\\Users\\andrew\\Desktop\\UNIVERSITY\\2nd year sem 1\\PFDA\\Assignment\\student.csv", delim = ";", quote = "\"")
dataset
View(dataset, "Dataset for three-year final scores of degree students")
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data cleaning~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# display names for each column
names(dataset)

# load package 
library(dplyr)
# change names to snake_case
dataset <- rename(dataset, p_status = Pstatus, m_edu = Medu, f_edu = Fedu, m_job = Mjob, f_job = Fjob, d_alc = Dalc, w_alc = Walc, g1 = G1, g2 = G2, g3 = G3)
names(dataset)

# provide info for data set
str(dataset)

# initialization
stu_dataset <- data.frame(index = 1:nrow(dataset))
# remove double quotes
for (col in names(dataset)){
  temp_col <- c()
  for(row in dataset[,col]){
    temp_row <- gsub("\"", "", row)
    temp_col <- c(temp_col, temp_row)
  }
  stu_dataset <- cbind(stu_dataset, temp_col)
}
# rename column name
names(stu_dataset) <- c("index", names(dataset))
View(stu_dataset, "Dataset for three-year final scores of degree students")

# provide info for updated data set
str(stu_dataset)

# convert to numeric data type
convert <- c("age", "m_edu", "f_edu", "traveltime", "studytime",
             "failures", "famrel", "freetime", "goout", "d_alc", 
             "w_alc", "health", "absences", "g1", "g2", "g3")
for (col in convert){
  stu_dataset[[col]] <- as.numeric(stu_dataset[[col]])
}
str(stu_dataset)
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data pre-processing~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# check for each columns' missing data
mapply(function(x){sum(is.na(x))}, dataset)

# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~Data analysis~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 1: How can students' family background influence his or her average grade?
stu_dataset$average = round((stu_dataset$g1 + stu_dataset$g2 + stu_dataset$g3) / 3, 2)
View(stu_dataset, "Dataset for three-year final scores of degree students")

# Analysis 1-1: Find the relationship between students' family size and their average grades
install.packages("ggplot2") # install
library(ggplot2) # load
# create the mean function
mean.n <- function(x){
  return(c(y = 0.97 * median(x), label = round(mean(x),2)))
}
# create box plot
ggplot(stu_dataset, aes(x = famsize, y = average, color = famsize)) + 
  geom_boxplot(fill = "white", width = 0.5, outlier.color = "red") +
  stat_summary(fun.data = mean.n, geom = "point", fun = mean, color = "deeppink3", size = 2) + # assign the mean mark
  stat_summary(fun.data = mean.n, geom = "text", fun = mean, color = "darkgreen", size = 5) + # display the mean mark
  ggtitle("Family size versus average grade") + xlab("Family size") + ylab("Average grade") +
  theme(plot.title = element_text(hjust = 0.5, color = "sienna3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "slateblue2", size = 14),
        axis.title.y = element_text(color = "mediumorchid3", size = 14),
        axis.text.x = element_text(color = "slateblue2", size = 10),
        axis.text.y = element_text(color = "mediumorchid3", size = 10),
        legend.title = element_text(color = "dodgerblue3", size = 14),
        legend.text = element_text(color = "dodgerblue2", size = 12))# design box plot

# Analysis 1-2: Identify the relationship between students' family size, parents' cohabitation status and their average grades
library(ggplot2) # load
# create heatmap
ggplot(stu_dataset, aes(x = famsize, y = p_status, fill = average)) +
  geom_raster() + scale_fill_gradient(low = "pink2", high = "blue") + # average ranges from pink to blue
  labs(title = "Average grade against parents' cohabitation status and students' family size", 
       x = "Students' family size", y = "Parents' cohabitation status", fill = "Average grade") +
  theme(plot.title = element_text(hjust = 0.5, color = "orange2", size = 18, face = "bold"),
        axis.title.x = element_text(color = "red4", size = 14),
        axis.title.y = element_text(color = "green3", size = 14),
        axis.text.x = element_text(color = "red4", size = 10),
        axis.text.y = element_text(color = "green3", size = 10),
        legend.title = element_text(color = "grey4", size = 14),
        legend.text = element_text(color = "grey3", size = 12)) # design heatmap

# Analysis 1-3: Find the relationship between mother's education, mother's job and students' average grades
install.packages("hexbin") # install
# load
library(hexbin)
library(ggplot2) 
# create hexagon plot
ggplot(stu_dataset, aes(x = m_edu, y = average, fill = factor(m_job))) +
  geom_hex() + 
  labs(title = "Mother's education and mother's job versus students' average grades", 
       x = "Mother's education level", y = "Average grade", fill = "Mother's job") +
  theme(plot.title = element_text(hjust = 0.5, color = "purple2", size = 18, face = "bold"),
        axis.title.x = element_text(color = "pink4", size = 14),
        axis.title.y = element_text(color = "red3", size = 14),
        axis.text.x = element_text(color = "pink4", size = 10),
        axis.text.y = element_text(color = "red3", size = 10),
        legend.title = element_text(color = "orange4", size = 14),
        legend.text = element_text(color = "orange3", size = 12)) # design hexagon plot

# Analysis 1-4: Analyse the relationship between father's education, father's job and students' average grades
library(ggplot2) # load
# create scatter plot
ggplot(stu_dataset, aes(x = f_edu, y = average)) +
  geom_point(aes(color = factor(f_job))) + 
  labs(title = "Father's education and father's job versus students' average grades", 
       x = "Father's education level", y = "Average grade", color = "Father's job") +
  theme(plot.title = element_text(hjust = 0.5, color = "blue2", size = 18, face = "bold"),
        axis.title.x = element_text(color = "red2", size = 14),
        axis.title.y = element_text(color = "maroon4", size = 14),
        axis.text.x = element_text(color = "red2", size = 10),
        axis.text.y = element_text(color = "maroon4", size = 10),
        legend.title = element_text(color = "orange4", size = 14),
        legend.text = element_text(color = "orange3", size = 12)) # design scatter plot

# Analysis 1-5: Identify the relationship between parents' cohabitation status, students' guardian and their average grades
library(ggplot2) # load
# create jitter plot
ggplot(stu_dataset, aes(x = p_status, y = average)) +
  geom_jitter(aes(color = factor(guardian))) + 
  labs(title = "Students' average grades against parents' cohabitation status and guardian", 
       x = "Parents' cohabitation status", y = "Average grade", color = "Students' guardian") +
  theme(plot.title = element_text(hjust = 0.5, color = "pink2", size = 18, face = "bold"),
        axis.title.x = element_text(color = "maroon2", size = 14),
        axis.title.y = element_text(color = "orange4", size = 14),
        axis.text.x = element_text(color = "maroon2", size = 10),
        axis.text.y = element_text(color = "orange4", size = 10),
        legend.title = element_text(color = "blue4", size = 14),
        legend.text = element_text(color = "blue3", size = 12)) # design jitter plot

# Analysis 1-6: Find the relationship between quality of family relationship, students' guardian and their average grades
library(ggplot2) # load
# create panel-wise scatter plot
ggplot(stu_dataset, aes(x = famrel, y = average)) +
  geom_point(aes(color = factor(guardian))) + 
  facet_grid(~ guardian) + # separate the graph into grids based on guardian
  labs(title = "Panel-wise scatter plot for relationship between students' family relationship, guardian and average grade", 
       x = "Students' family relationship", y = "Average grade", color = "Students' guardian") +
  theme(plot.title = element_text(hjust = 0.5, color = "green2", size = 18, face = "bold"),
        axis.title.x = element_text(color = "red4", size = 14),
        axis.title.y = element_text(color = "blue3", size = 14),
        axis.text.x = element_text(color = "red4", size = 10),
        axis.text.y = element_text(color = "blue3", size = 10),
        legend.title = element_text(color = "coral4", size = 14),
        legend.text = element_text(color = "coral3", size = 12),
        strip.text.x = element_text(color = "darkorchid3", size = 10), panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),    
        strip.background = element_rect(color = "black", size = 1)) # design panel-wise scatter plot

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 2: How can school choice affect students who score the highest in their finals?
q2_dataset <- stu_dataset[(stu_dataset$g3 >= stu_dataset$g2) & (stu_dataset$g3 >= stu_dataset$g1),]
View(q2_dataset, "Dataset for three-year final scores of degree students who score highest in finals")

# Analysis 2-1: Find the relationship between school and number of students scoring highest in finals
library(ggplot2) # load
q2_df <- data.frame(school = unique(q2_dataset$school), total = 
                 mapply(function(x){sum(x == q2_dataset$school)}, unique(q2_dataset$school)))
View(q2_df, "Dataset for School's distribution based on number of\n students scoring the highest final grade")
# create the pie chart 
plot_q2_1 <- ggplot(q2_df, aes(x = "", y = total, fill = school)) + 
             geom_bar(width = 0.5, stat = "identity") + 
             coord_polar("y", start = 0) + # starting from 0 at y coordinate
             labs(title = "School's distribution based on number of\n students scoring the highest final grade", 
                  x = NULL, y = NULL) +
             geom_text(aes(label = paste(round(total / sum(total) * 100, 2), "%")), 
                       position = position_stack(vjust = 0.5)) 
plot_q2_1 + theme(plot.title = element_text(color = "orange", size = 18, face = "bold")) # design the pie chart
  
# Analysis 2-2: Find the relationship between school and school support
library(ggplot2) # load
# create side-by-side bar chart 
ggplot(q2_dataset, aes(x = school, fill = schoolsup)) + 
  geom_bar(color = "blue", position = "dodge") + # "dodge" indicates side-by-side bar chart
  labs(title = "Relationship between school\n and school support", x = "School", y = "Count", fill = "School support") +
  theme(plot.title = element_text(hjust = 0.5, color = "orange", size = 18, face = "bold"),
        axis.title.x = element_text(color = "purple", size = 14),
        axis.title.y = element_text(color = "red", size = 14),
        axis.text.x = element_text(color = "purple", size = 10),
        axis.text.y = element_text(color = "red", size = 10),
        legend.title = element_text(color = "blue", size = 14),
        legend.text = element_text(color = "blue", size = 12)) # design side-by-side bar chart
  
# Analysis 2-3: Analyse the relationship between school and extra paid classes within course subject
library(ggplot2) # load
# create stacked bar chart 
ggplot(q2_dataset, aes(x = school, fill = paid)) + 
  geom_bar(color = "black", position = "stack") + # "stack" indicates stacked bar chart
  labs(title = "Relationship between school and extra\n paid classes within course subject", 
       x = "School", y = "Count", fill = "Extra paid classes") +
  theme(plot.title = element_text(hjust = 0.5, color = "green", size = 18, face = "bold"),
        axis.title.x = element_text(color = "red", size = 14),
        axis.title.y = element_text(color = "violet", size = 14),
        axis.text.x = element_text(color = "red", size = 10),
        axis.text.y = element_text(color = "violet", size = 10),
        legend.title = element_text(color = "blue", size = 14),
        legend.text = element_text(color = "blue", size = 12)) # design stacked bar chart

# Analysis 2-4: Identify the relationship between school, school support and extra paid classes within course subject
library(ggplot2) # load
# create panel-wise side-by-side bar chart 
ggplot(q2_dataset, aes(x = schoolsup, fill = paid)) + 
  geom_bar(color = "black", position = "dodge") + 
  facet_grid(~ school) + # separate the graph into grids based on school choice
  labs(title = "School support and extra paid classes within course subject versus school", 
       x = "School support", y = "Count", fill = "Extra paid classes") +
  theme(plot.title = element_text(hjust = 0.5, color = "blue3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "red3", size = 14),
        axis.title.y = element_text(color = "purple3", size = 14),
        axis.text.x = element_text(color = "red3", size = 10),
        axis.text.y = element_text(color = "purple3", size = 10),
        legend.title = element_text(color = "green3", size = 14),
        legend.text = element_text(color = "green2", size = 12)) # design panel-wise side-by-side bar chart

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 3: What are the factors which contribute to students scoring above or equal to 10 for each grade?
q3_dataset = stu_dataset[(stu_dataset$g1 >= 10) & (stu_dataset$g2 >= 10) & (stu_dataset$g3 >= 10),]
View(q3_dataset, "Dataset for three-year scores of degree students who score above or equal to 10")
# checking the range of average
factor(q3_dataset$average)
max(q3_dataset$average)

# Analysis 3-1: Find the relationship between students' reason in choosing a particular school and 
#               average grade based on average count
library("ggplot2") # load
# create scatter plot
ggplot(stu_dataset, aes(x = reason, y = average)) + 
  geom_point(aes(color = mapply(function(x){sum(x == average)}, average))) + 
  # different color represents different frequencies
  geom_segment(x = 0, y = 10, xend = 10, yend = 10) + # draw the standard line
  labs(title = "Relationship between students' reason in choosing a particular\n school and average grade based on average count", 
       x = "Reason in choosing a particular school", y = "Average grade", color = "Average count") +
  theme(plot.title = element_text(hjust = 0.5, color = "red2", size = 18, face = "bold"),
        axis.title.x = element_text(color = "orange2", size = 14),
        axis.title.y = element_text(color = "blue3", size = 14),
        axis.text.x = element_text(color = "orange2", size = 10),
        axis.text.y = element_text(color = "blue3", size = 10),
        legend.title = element_text(color = "green4", size = 14),
        legend.text = element_text(color = "green3", size = 12)) # design scatter plot

# Analysis 3-2: Identify the relationship between number of past failures and average grade
library("ggplot2") # load
# create box plot
ggplot(stu_dataset, aes(x = factor(failures), y = average, fill = factor(failures))) +
  geom_boxplot(color = "slategray4", width = 0.5, outlier.color = "green4", outlier.fill = "green4") +
  stat_summary(fun.data = mean.n, geom = "text", fun = mean, color = "purple3", size = 5) + # display the mean mark
  geom_segment(x = 0, y = 10, xend = 10, yend = 10) + # draw the standard line
  labs(title = "Students' number of past failures versus average grade", 
       x = "Students' number of past failures", y = "Average grade", fill = "Past failure count") +
  theme(plot.title = element_text(hjust = 0.5, color = "khaki3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "palevioletred2", size = 14),
        axis.title.y = element_text(color = "springgreen3", size = 14),
        axis.text.x = element_text(color = "palevioletred2", size = 10),
        axis.text.y = element_text(color = "springgreen3", size = 10),
        legend.title = element_text(color = "skyblue3", size = 14),
        legend.text = element_text(color = "skyblue2", size = 12)) # design box plot

# Analysis 3-3: Find the relationship between students' health status and average grade
library("ggplot2") # load
# create histogram
ggplot(stu_dataset, aes(x = average, fill = factor(health))) +
  geom_histogram(bins = 10, position = "dodge") +
  geom_vline(xintercept = 10, linetype = "dashed") + # draw the vertical line
  labs(title = "Students' average grade against health status", 
       x = "Students' average grade", y = "Count", fill = "Students' health condition") +
  theme(plot.title = element_text(hjust = 0.5, color = "burlywood3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "darkorange", size = 14),
        axis.title.y = element_text(color = "darkred", size = 14),
        axis.text.x = element_text(color = "darkorange", size = 10),
        axis.text.y = element_text(color = "darkred", size = 10),
        legend.title = element_text(color = "cadetblue4", size = 14),
        legend.text = element_text(color = "cadetblue3", size = 12)) # design histogram

# Analysis 3-4: Analyse the relationship between students' number of absent days and average grade
library("ggplot2") # load
# create dot plot
ggplot(stu_dataset, aes(x = average, fill = factor(absences))) +
  geom_dotplot(stackratio = 1) + 
  geom_vline(xintercept = 10, linetype = "dashed") + # draw the vertical line
  labs(title = "Students' number of days absent versus average grade", 
       x = "Students' average grade", y = "Count", fill = "Students' number of days absent") +
  theme(plot.title = element_text(hjust = 0.5, color = "seagreen4", size = 18, face = "bold"),
        axis.title.x = element_text(color = "violetred3", size = 14),
        axis.title.y = element_text(color = "slateblue2", size = 14),
        axis.text.x = element_text(color = "violetred3", size = 10),
        axis.text.y = element_text(color = "slateblue2", size = 10),
        legend.title = element_text(color = "orangered4", size = 14),
        legend.text = element_text(color = "orangered3", size = 12)) # design dot plot

# Analysis 3-5: Identify the relationship between students' health status, past failures and average grade
library(ggplot2) # load
# create heatmap
ggplot(stu_dataset, aes(x = factor(health), y = failures, fill = average)) + 
  geom_tile() + 
  scale_fill_gradient(low = "green4", high = "blue") + # average ranges from green to blue
  labs(title = "Students' average grade against health status and number of past failures", 
       x = "Students' health status", y = "Number of past failures", fill = "Average grade") +
  theme(plot.title = element_text(hjust = 0.5, color = "sienna3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "palevioletred2", size = 14),
        axis.title.y = element_text(color = "darkorange3", size = 14),
        axis.text.x = element_text(color = "palevioletred2", size = 10),
        axis.text.y = element_text(color = "darkorange3", size = 10),
        legend.title = element_text(color = "dodgerblue4", size = 14),
        legend.text = element_text(color = "dodgerblue3", size = 12)) # design heatmap

# Analysis 3-6: Analyse the relationship between students' health status, number of absences and average grade
library(ggplot2) # load
# create jitter plot
ggplot(stu_dataset, aes(x = health, y = absences)) + 
  geom_jitter(aes(color = factor(average))) + 
  geom_segment(x = 0, y = 9, xend = 6, yend = 9) + # create a standard indicator line
  labs(title = "Jitter plot for relationship between students' health status, number of absences and average grade", 
       x = "Students' health status", y = "Number of absences", color = "Average grade") +
  theme(plot.title = element_text(hjust = 0.5, color = "navyblue", size = 18, face = "bold"),
        axis.title.x = element_text(color = "mediumpurple4", size = 14),
        axis.title.y = element_text(color = "cyan3", size = 14),
        axis.text.x = element_text(color = "mediumpurple4", size = 10),
        axis.text.y = element_text(color = "cyan3", size = 10),
        legend.title = element_text(color = "deeppink4", size = 14),
        legend.text = element_text(color = "deeppink3", size = 12)) # design jitter plot

# Analysis 3-7: Find the relationship between students' health status, past failures, number of absences and average grade
library(ggplot2) # load
# create grid-wise scatter plot
ggplot(stu_dataset, aes(x = health, y = average, color = factor(absences))) + 
  geom_point() + 
  facet_grid(~failures) + # separate the graph into grids based on past failure count
  geom_segment(x = 0, y = 10, xend = 6, yend = 10) + # create a standard indicator line 
  labs(title = "Students' health status, number of past failures and number of absences versus average grade", 
       x = "Students' health status", y = "Average grade", color = "Students' number of absences") +
  theme(plot.title = element_text(hjust = 0.5, color = "mediumspringgreen", size = 18, face = "bold"),
        axis.title.x = element_text(color = "chocolate3", size = 14),
        axis.title.y = element_text(color = "olivedrab", size = 14),
        axis.text.x = element_text(color = "chocolate3", size = 10),
        axis.text.y = element_text(color = "olivedrab", size = 10),
        legend.title = element_text(color = "royalblue4", size = 14),
        legend.text = element_text(color = "royalblue3", size = 12), 
        panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(color = "black", size = 0.2, fill = NA),    
        strip.background = element_rect(color = "black", size = 1)) # design grid-wise scatter plot

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 4: How can students' learning passion and attitude help them score distinction for each grade?
library(dplyr) # load
temp <- subset(stu_dataset, g1 >= 15 & g2 >= 15 & g3 >= 15, 
               select = c("studytime", "nursery", "higher", "romantic", "g1", "g2", "g3", "average"))
q4_dataset <- data.frame(index = 1:nrow(temp), temp) # convert temp to data frame
View(q4_dataset, "Dataset for students who score distinction for each grade")
# checking the range of average
factor(q4_dataset$average)
max(q4_dataset$average)

# Analysis 4-1: Analyse and visualize distribution of students who want to pursue in higher education
library(dplyr)
library(ggplot2) # load
q4_dataset <- mutate(q4_dataset, 
                     no_higher = mapply(function(x){sum(x == q4_dataset$higher)}, unique(q4_dataset$higher))) %>% # pipelining
              select(c("higher", "no_higher")) # count occurrence of students' option in pursuing higher education
# create pie chart
ggplot(q4_dataset, aes(x = "", y = no_higher, fill = higher)) + 
  geom_bar(width = 2, stat = "identity") + 
  coord_polar("y", start = 0) + # starting from 0 at y coordinate
  labs(title = "Distribution of students who want to pursue in higher education", 
       x = NULL, y = NULL) + 
  theme(plot.title = element_text(hjust = 0.5, color = "darkorange4", size = 18, face = "bold"),  
        legend.title = element_text(color = "seagreen4", size = 14),
        legend.text = element_text(color = "seagreen3", size = 12)) # design pie chart

# Analysis 4-2: Analyse and visualize distribution of students who attended nursery school
library(ggplot2)# load
# create histogram
ggplot(q4_dataset, aes(x = average, fill = nursery)) + 
  geom_histogram(bins = 30) + 
  geom_vline(xintercept = 18, linetype = "dashed", color = "violetred3") + # add vertical line
  labs(title = "Distribution of students who score distinction and have attended nursery school ", 
       x = "Students' average grade", y = "Count", color = "Students' nursery school attendance") +
  theme(plot.title = element_text(hjust = 0.5, color = "mediumpurple3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "maroon2", size = 14),
        axis.title.y = element_text(color = "royalblue3", size = 14),
        axis.text.x = element_text(color = "maroon2", size = 10),
        axis.text.y = element_text(color = "royalblue3", size = 10),
        legend.title = element_text(color = "sienna4", size = 14),
        legend.text = element_text(color = "sienna3", size = 12)) # design histogram

# Analysis 4-3: Analyse and visualize distribution of students who attended nursery school and had no romantic relationship
library(ggplot2) # load
# create scatter plot
ggplot(q4_dataset, aes(x = nursery, y = average)) + 
  geom_point(aes(color = factor(romantic))) + 
  geom_hline(yintercept = 18, linetype = "dashed", color = "navy") + # add horizontal line
  labs(title = "Distribution of students who attended nursery school and had no romantic relationship", 
       x = "Nursery school attendance", y = "Students' average grade", color = "Romantic relationship") +
  theme(plot.title = element_text(hjust = 0.5, color = "coral3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "chartreuse4", size = 14),
        axis.title.y = element_text(color = "darkorange3", size = 14),
        axis.text.x = element_text(color = "chartreuse4", size = 10),
        axis.text.y = element_text(color = "darkorange3", size = 10),
        legend.title = element_text(color = "darkorchid4", size = 14),
        legend.text = element_text(color = "darkorchid3", size = 12)) # design scatter plot

# Analysis 4-4: Analyse and visualize distribution of students who attended nursery school, had no romantic relationship 
#               and had more study time
library(ggplot2) # load
# create grid-wise heatmap
ggplot(q4_dataset, aes(x = nursery, y = romantic, fill = average)) + 
  geom_tile() + 
  facet_grid(~ studytime) + # separate the graph into grids based on study time 
  labs(title = "Distribution of students who attended nursery school, had no romantic relationship and had more study time", 
       x = "Nursery school attendance", y = "Romantic relationship", fill = "Students' average grade") +
  theme(plot.title = element_text(hjust = 0.5, color = "springgreen3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "orangered3", size = 14),
        axis.title.y = element_text(color = "darkorange1", size = 14),
        axis.text.x = element_text(color = "orangered3", size = 10),
        axis.text.y = element_text(color = "darkorange1", size = 10),
        legend.title = element_text(color = "violetred2", size = 14),
        legend.text = element_text(color = "violetred", size = 12), panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),    
        strip.background = element_rect(color = "black", size = 1)) # design grid-wise heatmap

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 5: How does students' address or living location affect the average of their first two grades?
library(dplyr) # load
q5_dataset <- mutate(stu_dataset, g1_2_average = (g1 + g2) / 2) %>% # pipelining
              select(c("address", "traveltime", "studytime", "internet", "g1", "g2", "g1_2_average"))
View(q5_dataset, "Dataset for first and second years' Math grade of degree students")

# Analysis 5-1: Identify the relationship between students' address and their average of first two Math grades
library(ggplot2)# load
# create box plot
ggplot(q5_dataset, aes(x = address, y = g1_2_average)) + 
  geom_boxplot(color = "darkorange4", width = 0.5, outlier.color = "brown", aes(fill = address)) + 
  stat_summary(fun.data = mean.n, geom = "text", fun = mean, color = "darkred", size = 4) + # display the mean mark
  labs(title = "Average of g1 and g2 against students' address", 
       x = "Students' address", y = "Average of g1 and g2", fill = "Students' address") +
  theme(plot.title = element_text(hjust = 0.5, color = "coral3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "chartreuse4", size = 14),
        axis.title.y = element_text(color = "darkorange3", size = 14),
        axis.text.x = element_text(color = "chartreuse4", size = 10),
        axis.text.y = element_text(color = "darkorange3", size = 10),
        legend.title = element_text(color = "darkorchid4", size = 14),
        legend.text = element_text(color = "darkorchid3", size = 12)) # design box plot

# Analysis 5-2: Find the relationship between students' address, travel time and their average Math grade of g1 and g2
library(ggplot2) 
library(hexbin) # load
# create hexagon plot
ggplot(q5_dataset, aes(x = address, y = g1_2_average, fill = factor(traveltime))) + 
  geom_hex() + 
  labs(title = "Students' g1 and g2 average grade against address and travel time", 
       x = "Students' address", y = "Average of g1 and g2", fill = "Students' travel time") +
  theme(plot.title = element_text(hjust = 0.5, color = "goldenrod3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "cyan4", size = 14),
        axis.title.y = element_text(color = "chartreuse2", size = 14),
        axis.text.x = element_text(color = "cyan4", size = 10),
        axis.text.y = element_text(color = "chartreuse2", size = 10),
        legend.title = element_text(color = "coral4", size = 14),
        legend.text = element_text(color = "coral3", size = 12)) # design hexagon plot

# Analysis 5-3: Find the relationship between students' travel time, study time and their first two average grade
library(ggplot2)# load
# create grid-wise scatter plot
ggplot(q5_dataset, aes(x = traveltime, y = g1_2_average)) + 
  geom_point(aes(color = factor(studytime))) + 
  facet_grid(~studytime) +# separate the graph into grids based on study time 
  labs(title = "Students' travel time and study time versus average grade of g1 and g2", 
       x = "Students' travel time", y = "Average of g1 and g2", color = "Students' study time") +
  theme(plot.title = element_text(hjust = 0.5, color = "darkmagenta", size = 18, face = "bold"),
        axis.title.x = element_text(color = "gold3", size = 14),
        axis.title.y = element_text(color = "firebrick2", size = 14),
        axis.text.x = element_text(color = "gold3", size = 10),
        axis.text.y = element_text(color = "firebrick2", size = 10),
        legend.title = element_text(color = "goldenrod4", size = 14),
        legend.text = element_text(color = "goldenrod3", size = 12), panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),    
        strip.background = element_rect(color = "black", size = 1)) # design grid-wise scatter plot

# Analysis 5-4: Identify the relationship between students' address, internet availability and their average of first two grades
library(ggplot2)# load
# create heatmap
ggplot(q5_dataset, aes(x = address, y = internet, fill = factor(g1_2_average))) + 
  geom_tile() + 
  labs(title = "Relationship between students' address, internet availability and average grade of g1 and g2", 
       x = "Students' address", y = "Internet availability", fill = "Average of g1 and g2") +
  theme(plot.title = element_text(hjust = 0.5, color = "darkcyan", size = 18, face = "bold"),
        axis.title.x = element_text(color = "darkviolet", size = 14),
        axis.title.y = element_text(color = "dodgerblue3", size = 14),
        axis.text.x = element_text(color = "darkviolet", size = 10),
        axis.text.y = element_text(color = "dodgerblue3", size = 10),
        legend.title = element_text(color = "gray4", size = 14),
        legend.text = element_text(color = "gray3", size = 12)) # design heatmap

# Analysis 5-5: Identify the relationship between students' study time, travel time, internet and average grade of g1 and g2
library(ggplot2)# load
# create grid-wise jitter plot
ggplot(q5_dataset, aes(x = traveltime, y = g1_2_average)) + 
  geom_jitter(aes(color = factor(internet))) + 
  facet_grid(~ studytime) + # separate the graph into grids based on study time
  geom_hline(yintercept = 10, linetype = "dashed") +# create a horizontal line
  labs(title = "Students' g1 and g2 average grade against study time, internet availability and travel time", 
       x = "Students' travel time", y = "Average of g1 and g2", color = "Internet availability") +
  theme(plot.title = element_text(hjust = 0.5, color = "deeppink3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "darkred", size = 14),
        axis.title.y = element_text(color = "darkorchid", size = 14),
        axis.text.x = element_text(color = "darkred", size = 10),
        axis.text.y = element_text(color = "darkorchid", size = 10),
        legend.title = element_text(color = "chocolate4", size = 14),
        legend.text = element_text(color = "chocolate3", size = 12), panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),    
        strip.background = element_rect(color = "black", size = 1)) # design grid-wise jitter plot

#~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# Question 6: From students' time management perspective, what are the reasons which cause them to fail the final grade?
library(dplyr) # load
temp <- filter(stu_dataset, g3 <= 5) %>% # pipelining
        select(c("studytime", "activities", "freetime", "goout", "d_alc", "w_alc", "g3"))
q6_dataset <- data.frame(index = 1:nrow(temp), temp) # convert temp to data frame
View(q6_dataset, "Dataset for students who fail in the finals")

# Analysis 6-1: Identify the relationship between students' study time and free time
library(ggplot2)# load
# create bar chart
ggplot(q6_dataset, aes(x = freetime)) + 
  geom_bar(aes(fill = factor(studytime))) + 
  labs(title = "Students' study time against free time", 
       x = "Students' free time", y = "Number of students", fill = "Students' study time") +
  theme(plot.title = element_text(hjust = 0.5, color = "royalblue2", size = 18, face = "bold"),
        axis.title.x = element_text(color = "mediumspringgreen", size = 14),
        axis.title.y = element_text(color = "violet", size = 14),
        axis.text.x = element_text(color = "mediumspringgreen", size = 10),
        axis.text.y = element_text(color = "violet", size = 10),
        legend.title = element_text(color = "chocolate4", size = 14),
        legend.text = element_text(color = "chocolate3", size = 12)) # design bar chart

# Analysis 6-2: Analyse the relationship between students' free time and outing time
library(ggplot2)# load
# create violin plot
ggplot(q6_dataset, aes(x = factor(freetime), y = goout, fill = factor(freetime))) + 
  geom_violin() +
  labs(title = "Students' free time versus outing time", 
       x = "Students' free time", y = "Students' outing time", fill = "Students' free time") +
  theme(plot.title = element_text(hjust = 0.5, color = "peachpuff3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "magenta2", size = 14),
        axis.title.y = element_text(color = "cyan3", size = 14),
        axis.text.x = element_text(color = "magenta2", size = 10),
        axis.text.y = element_text(color = "cyan3", size = 10),
        legend.title = element_text(color = "darkgreen", size = 14),
        legend.text = element_text(color = "chartreuse4", size = 12)) # design violin plot

# Analysis 6-3: Find the relationship between students' free time, outing time and study time
library(ggplot2) # load
# create grid-wise jitter plot
ggplot(q6_dataset, aes(x = factor(freetime), y = goout)) + 
  geom_jitter(aes(color = factor(studytime))) +
  facet_grid(~studytime) +  # separate the graph into grids based on study time
  labs(title = "Grid-wise jitter plot for relationship between students' free time, outing time and study time", 
       x = "Students' free time", y = "Students' outing time", color = "Students' study time") +
  theme(plot.title = element_text(hjust = 0.5, color = "darkorchid3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "paleturquoise4", size = 14),
        axis.title.y = element_text(color = "salmon3", size = 14),
        axis.text.x = element_text(color = "paleturquoise4", size = 10),
        axis.text.y = element_text(color = "salmon3", size = 10),
        legend.title = element_text(color = "coral4", size = 14),
        legend.text = element_text(color = "coral3", size = 12),  panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),    
        strip.background = element_rect(color = "black", size = 1)) # design grid-wise jitter plot

# Analysis 6-4: Analyse the relationship between students' free time and school activity participation
library(ggplot2)# load
# create violin plot
ggplot(q6_dataset, aes(x = freetime, y = activities, fill = factor(activities))) + 
  geom_violin() +
  labs(title = "Violin plot for relationship between students' free time and school activity participation", 
       x = "Students' free time", y = "Students' school activity participation", fill = "Students' school\nactivity participation") +
  theme(plot.title = element_text(hjust = 0.5, color = "blueviolet", size = 18, face = "bold"),
        axis.title.x = element_text(color = "lightskyblue4", size = 14),
        axis.title.y = element_text(color = "mistyrose4", size = 14),
        axis.text.x = element_text(color = "lightskyblue4", size = 10),
        axis.text.y = element_text(color = "mistyrose4", size = 10),
        legend.title = element_text(color = "seagreen", size = 14),
        legend.text = element_text(color = "mediumseagreen", size = 12)) # design violin plot

# Analysis 6-5: Find the relationship between students' free time, outing time and daily alcohol consumption
library(ggplot2) # load
# create grid-wise bar chart
ggplot(q6_dataset, aes(x = freetime, fill = factor(d_alc))) + 
  geom_bar() +
  facet_grid(~goout) + # separate the graph into grids based on outing time
  labs(title = "Students' free time and outing time versus daily alcohol consumption", 
       x = "Students' free time", y = "Number of students", fill = "Students' daily\nalcohol consumption") +
  theme(plot.title = element_text(hjust = 0.5, color = "mediumturquoise", size = 18, face = "bold"),
        axis.title.x = element_text(color = "indianred4", size = 14),
        axis.title.y = element_text(color = "hotpink2", size = 14),
        axis.text.x = element_text(color = "indianred4", size = 10),
        axis.text.y = element_text(color = "hotpink2", size = 10),
        legend.title = element_text(color = "darkorange4", size = 12),
        legend.text = element_text(color = "darkorange3", size = 10),  panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),    
        strip.background = element_rect(color = "black", size = 1)) # design grid-wise bar chart

# Analysis 6-6: Analyse the relationship between students' outing time and weekend alcohol consumption
library(ggplot2) # load
# create dot plot
ggplot(q6_dataset, aes(x = goout, fill = factor(w_alc))) + 
  geom_dotplot(stackratio = 1.5) +
  labs(title = "Students' weekend alcohol consumption against their outing time", 
       x = "Students'outing time", y = "Number of students", fill = "Students' weekend\nalcohol consumption") +
  theme(plot.title = element_text(hjust = 0.5, color = "lightslateblue", size = 18, face = "bold"),
        axis.title.x = element_text(color = "maroon3", size = 14),
        axis.title.y = element_text(color = "orangered2", size = 14),
        axis.text.x = element_text(color = "maroon3", size = 10),
        axis.text.y = element_text(color = "orangered2", size = 10),
        legend.title = element_text(color = "goldenrod4", size = 12),
        legend.text = element_text(color = "goldenrod3", size = 10)) # design dot plot

# Analysis 6-7: Identify the relationship between students' free time, study time, outing time and weekend alcohol consumption
library(ggplot2)# load
# create panel-wise heatmap
ggplot(q6_dataset, aes(x = freetime, y = studytime, fill = factor(goout))) + 
  geom_tile() +
  facet_wrap(~ w_alc) + # separate the graph into grids based on weekend alcohol consumption
  labs(title = "Grid-wise heatmap for relationship between students' free time, study time, \nouting time and weekend alcohol consumption", 
       x = "Students' free time", y = "Students' study time", fill = "Students' outing time") +
  theme(plot.title = element_text(hjust = 0.5, color = "gold3", size = 18, face = "bold"),
        axis.title.x = element_text(color = "darkorange2", size = 14),
        axis.title.y = element_text(color = "seagreen3", size = 14),
        axis.text.x = element_text(color = "darkorange2", size = 10),
        axis.text.y = element_text(color = "seagreen3", size = 10),
        legend.title = element_text(color = "palevioletred4", size = 14),
        legend.text = element_text(color = "palevioletred3", size = 12),  panel.spacing = unit(0.5, "lines"),
        panel.border = element_rect(color = "black", size = 1, fill = NA),    
        strip.background = element_rect(color = "black", size = 1)) # design panel-wise heatmap