
options(repos = c(CRAN = "https://cran.rstudio.com/"))

install.packages("ggfortify")
install.packages("mvnormtest")
install.packages("datarium")
install.packages("ggplot2")
install.packages("caret")
install.packages("mvtnorm")
install.packages("pROC")
install.packages("tinytex")
install.packages("scales")
install.packages("janitor")

library(MASS) 
library(datarium)
library(ggplot2)
library(broom) 
library(ggfortify)
library(tidyverse)
library(mvnormtest)
library(data.table)
library(gridExtra)
library(dplyr)
library(tinytex)
library(tidyr)
library(scales)

# higher education dataset 
# https://archive.ics.uci.edu/dataset/697/predict+students+dropout+and+academic+success

data1 <- read.csv("C:/Users/harip/Downloads/predict+students+dropout+and+academic+success/data.csv", sep=";")
head(data1)

colnames(data1)
names(data1) <- c("Marital status",
                 "Application mode", 
                 "Application order",
                 "Course",
                 "Daytime/evening attendance",
                 "Previous qualification",
                 "Previous qualification grade",
                 "Nationality",
                 "Mother's qualification",
                 "Father's qualification",
                 "Mother's occupation",
                 "Father's occupation",
                 "Admission grade",
                 "Displaced",
                 "Educational special needs",
                 "Debtor",
                 "Tuition fees up to date",
                 "Gender",
                 "Scholarship holder",
                 "Age at enrollment",
                 "International",
                 "Semester 1 credited units",
                 "Semester 1 enrolled units",
                 "Semester 1 evaluations",
                 "Semester 1 approved units",
                 "Semester 1 grade",
                 "Semester 1 units without evaluations",
                 "Semester 2 credited units",
                 "Semester 2 enrolled units",
                 "Semester 2 evaluations",
                 "Semester 2 approved units",
                 "Semester 2 grade",
                 "Semester 2 units without evaluations",
                 "Unemployment rate",
                 "Inflation rate",
                 "GDP",
                 "Target")

data1 <- select(data1, "Marital status",
                  "Application mode", 
                  "Application order",
                  "Course",
                  "Daytime/evening attendance",
                  "Previous qualification",
                  "Previous qualification grade",
                  "Nationality",
                  "Mother's qualification",
                  "Father's qualification",
                  "Admission grade",
                  "Displaced",
                  "Educational special needs",
                  "Debtor",
                  "Gender",
                  "Scholarship holder",
                  "Age at enrollment",
                  "International",
                  "Semester 1 enrolled units",
                  "Semester 1 approved units",
                  "Semester 1 grade",
                  "Semester 2 enrolled units",
                  "Semester 2 approved units",
                  "Semester 2 grade",
                  "Unemployment rate",
                  "Inflation rate",
                  "GDP",
                  "Target")

data1 <- data1 |>
  mutate(
    `Unemployment rate` = as.numeric(`Unemployment rate`)/100,
    `Inflation rate` = as.numeric(`Inflation rate`)/100,
    `GDP` = as.numeric(`GDP`)/100
         ) 
head(data1)

#assumption: students do not vary over time, no intrinsic distribution change 


data1$"Marital status" <- ifelse(data1$"Marital status" == 1, "single", 
                          ifelse(data1$"Marital status" == 2, "married",
                          ifelse(data1$"Marital status" == 3, "widower",
                          ifelse(data1$"Marital status" == 4, "divorced",
                          ifelse(data1$"Marital status" == 5, "facto union",
                          ifelse(data1$"Marital status" == 6, "legally separated", NA)))))) 

data1$"Application mode" <- ifelse(data1$"Application mode" == 1, "1st phase - general contingent", 
                            ifelse(data1$"Application mode" == 2, "Ordinance No. 612/93",
                            ifelse(data1$"Application mode" == 5, "1st phase - special contingent (Azores Island)",
                            ifelse(data1$"Application mode" == 7, "Holders of other higher courses",
                            ifelse(data1$"Application mode" == 10, "Ordinance No. 854-B/99",
                            ifelse(data1$"Application mode" == 15, "International student (bachelor)", 
                            ifelse(data1$"Application mode" == 16, "1st phase - special contingent (Madeira Island)",
                            ifelse(data1$"Application mode" == 17, "2nd phase - general contingent",
                            ifelse(data1$"Application mode" == 18, "3rd phase - general contingent",
                            ifelse(data1$"Application mode" == 26, "Ordinance No. 533-A/99, item b2) (Different Plan)",
                            ifelse(data1$"Application mode" == 27, "Ordinance No. 533-A/99, item b3 (Other Institution)",
                            ifelse(data1$"Application mode" == 39, "Over 23 years old",
                            ifelse(data1$"Application mode" == 42, "Transfer",
                            ifelse(data1$"Application mode" == 43, "Change of course",
                            ifelse(data1$"Application mode" == 44, "Technological specialization diploma holders",
                            ifelse(data1$"Application mode" == 51, "Change of institution/course",
                            ifelse(data1$"Application mode" == 53, "Short cycle diploma holders",
                            ifelse(data1$"Application mode" == 57, "Change of institution/course (International)", NA)))))))))))))))))) 

data1$"Application order" <- ifelse(data1$"Application order" == 0, "1st choice",
                             ifelse(data1$"Application order" == 1, "2nd choice",
                             ifelse(data1$"Application order" == 2, "3rd choice",
                             ifelse(data1$"Application order" == 3, "4th choice",
                             ifelse(data1$"Application order" == 4, "5th choice",
                             ifelse(data1$"Application order" == 5, "6th choice",
                             ifelse(data1$"Application order" == 6, "7th choice",
                             ifelse(data1$"Application order" == 7, "8th choice",
                             ifelse(data1$"Application order" == 8, "9th choice choice",
                             ifelse(data1$"Application order" == 9, "Last choice", NA)))))))))) 

data1$"Course" <- ifelse(data1$"Course" == 33, "Biofuel Production Technologies", 
                  ifelse(data1$"Course" == 171, "Animation and Multimedia Design", 
                  ifelse(data1$"Course" == 8014, "Social Service (evening attendance)", 
                  ifelse(data1$"Course" == 9003, "Agronomy", 
                  ifelse(data1$"Course" == 9070, "Communication Design", 
                  ifelse(data1$"Course" == 9085, "Veterinary Nursing", 
                  ifelse(data1$"Course" == 9119, "Informatics Engineering", 
                  ifelse(data1$"Course" == 9130, "Equinculture", 
                  ifelse(data1$"Course" == 9147, "Management", 
                  ifelse(data1$"Course" == 9238, "Social Service", 
                  ifelse(data1$"Course" == 9254, "Tourism", 
                  ifelse(data1$"Course" == 9500, "Nursing", 
                  ifelse(data1$"Course" == 9556, "Oral Hygiene", 
                  ifelse(data1$"Course" == 9670, "Advertising and Marketing Management", 
                  ifelse(data1$"Course" == 9773, "Journalism and Communication", 
                  ifelse(data1$"Course" == 9853, "Basic Education", 
                  ifelse(data1$"Course" == 9991, "Management (evening attendance)", NA))))))))))))))))) 

data1$"Daytime/evening attendance" <- ifelse(data1$"Daytime/evening attendance" == 1, "daytime", "evening") 

data1$"Previous qualification" <- ifelse(data1$"Previous qualification" == 1, "Secondary education",
                                  ifelse(data1$"Previous qualification" == 2, "Higher education - bachelors degree",
                                  ifelse(data1$"Previous qualification" == 3, "Higher education - degree",
                                  ifelse(data1$"Previous qualification" == 4, "Higher education - masters",
                                  ifelse(data1$"Previous qualification" == 5, "Higher education - doctorate",
                                  ifelse(data1$"Previous qualification" == 6, "Frequency of higher education",
                                  ifelse(data1$"Previous qualification" == 9, "12th year of schooling - not completed",
                                  ifelse(data1$"Previous qualification" == 10, "11th year of schooling - not completed",
                                  ifelse(data1$"Previous qualification" == 12, "Other - 11th year of schooling",
                                  ifelse(data1$"Previous qualification" == 14, "10th year of schooling",
                                  ifelse(data1$"Previous qualification" == 15, "10th year of schooling - not completed",
                                  ifelse(data1$"Previous qualification" == 19, "Basic education 3rd cycle (9th/10th/11th year) or equiv.",
                                  ifelse(data1$"Previous qualification" == 38, "Basic education 2nd cycle (6th/7th/8th year) or equiv.",
                                  ifelse(data1$"Previous qualification" == 39, "Technological specialization course",
                                  ifelse(data1$"Previous qualification" == 40, "Higher education - degree (1st cycle)",
                                  ifelse(data1$"Previous qualification" == 42, "Professional higher technical course",
                                  ifelse(data1$"Previous qualification" == 43, "Higher education - master (2nd cycle)", NA))))))))))))))))) 

data1$"Nationality" <- ifelse(data1$"Nationality" == 1, "Portuguese", 
                       ifelse(data1$"Nationality" == 2, "German", 
                       ifelse(data1$"Nationality" == 6, "Spanish", 
                       ifelse(data1$"Nationality" == 11, "Italian", 
                       ifelse(data1$"Nationality" == 13, "Dutch", 
                       ifelse(data1$"Nationality" == 14, "English", 
                       ifelse(data1$"Nationality" == 17, "Lithuanian",
                       ifelse(data1$"Nationality" == 21, "Angolan", 
                       ifelse(data1$"Nationality" == 22, "Cape Verdean", 
                       ifelse(data1$"Nationality" == 24, "Guinean", 
                       ifelse(data1$"Nationality" == 25, "Mozambican", 
                       ifelse(data1$"Nationality" == 26, "Santomean", 
                       ifelse(data1$"Nationality" == 32, "Turkish", 
                       ifelse(data1$"Nationality" == 41, "Brazilian", 
                       ifelse(data1$"Nationality" == 62, "Romanian", 
                       ifelse(data1$"Nationality" == 100, "Moldova (Republic of)", 
                       ifelse(data1$"Nationality" == 101, "Mexican", 
                       ifelse(data1$"Nationality" == 103, "Ukrainian", 
                       ifelse(data1$"Nationality" == 105, "Russian", 
                       ifelse(data1$"Nationality" == 108, "Cuban", 
                       ifelse(data1$"Nationality" == 109, "Columbian", NA)))))))))))))))))))))

data1$"Mother's qualification" <- ifelse(data1$"Mother's qualification" == 1, "Secondary Education - 12th Year of Schooling or Eq.",
                                  ifelse(data1$"Mother's qualification" == 2, "Higher Education - Bachelor's Degree",
                                  ifelse(data1$"Mother's qualification" == 3, "Higher Education - Degree",
                                  ifelse(data1$"Mother's qualification" == 4, "Higher Education - Master's",
                                  ifelse(data1$"Mother's qualification" == 5, "Higher Education - Doctorate",
                                  ifelse(data1$"Mother's qualification" == 6, "Frequency of Higher Education",
                                  ifelse(data1$"Mother's qualification" == 9, "12th Year of Schooling - Not Completed",
                                  ifelse(data1$"Mother's qualification" == 10, "11th Year of Schooling - Not Completed",
                                  ifelse(data1$"Mother's qualification" == 11, "7th Year (Old)",
                                  ifelse(data1$"Mother's qualification" == 12, "Other - 11th Year of Schooling",
                                  ifelse(data1$"Mother's qualification" == 14, "10th Year of Schooling",
                                  ifelse(data1$"Mother's qualification" == 18, "General commerce course",
                                  ifelse(data1$"Mother's qualification" == 19, "Basic Education 3rd Cycle (9th/10th/11th Year) or Equiv.",
                                  ifelse(data1$"Mother's qualification" == 22, "Technical-professional course",
                                  ifelse(data1$"Mother's qualification" == 26, "7th year of schooling",
                                  ifelse(data1$"Mother's qualification" == 27, "2nd cycle of the general high school course",
                                  ifelse(data1$"Mother's qualification" == 29, "9th Year of Schooling - Not Completed",
                                  ifelse(data1$"Mother's qualification" == 30, "8th year of schooling",
                                  ifelse(data1$"Mother's qualification" == 34, "Unknown",
                                  ifelse(data1$"Mother's qualification" == 35, "Can't read or write",
                                  ifelse(data1$"Mother's qualification" == 36, "Can read without having a 4th year of schooling",
                                  ifelse(data1$"Mother's qualification" == 37, "Basic education 1st cycle (4th/5th year) or equiv.",
                                  ifelse(data1$"Mother's qualification" == 38, "Basic Education 2nd Cycle (6th/7th/8th Year) or Equiv.",
                                  ifelse(data1$"Mother's qualification" == 39, "Technological specialization course",
                                  ifelse(data1$"Mother's qualification" == 40, "Higher education - degree (1st cycle)",
                                  ifelse(data1$"Mother's qualification" == 41, "Specialized higher studies course",
                                  ifelse(data1$"Mother's qualification" == 42, "Professional higher technical course",
                                  ifelse(data1$"Mother's qualification" == 43, "Higher Education - Master (2nd cycle)",
                                  ifelse(data1$"Mother's qualification" == 44, "Higher Education - Doctorate (3rd cycle)", NA))))))))))))))))))))))))))))) 

data1$"Father's qualification" <- ifelse(data1$"Father's qualification" == 1, "Secondary Education - 12th Year of Schooling or Eq.",
                                  ifelse(data1$"Father's qualification" == 2, "Higher Education - Bachelor's Degree",
                                  ifelse(data1$"Father's qualification" == 3, "Higher Education - Degree",
                                  ifelse(data1$"Father's qualification" == 4, "Higher Education - Master's",
                                  ifelse(data1$"Father's qualification" == 5, "Higher Education - Doctorate",
                                  ifelse(data1$"Father's qualification" == 6, "Frequency of Higher Education",
                                  ifelse(data1$"Father's qualification" == 9, "12th Year of Schooling - Not Completed",
                                  ifelse(data1$"Father's qualification" == 10, "11th Year of Schooling - Not Completed",
                                  ifelse(data1$"Father's qualification" == 11, "7th Year (Old)",
                                  ifelse(data1$"Father's qualification" == 12, "Other - 11th Year of Schooling",
                                  ifelse(data1$"Father's qualification" == 13, "2nd year complementary high school course", 
                                  ifelse(data1$"Father's qualification" == 14, "10th Year of Schooling",
                                  ifelse(data1$"Father's qualification" == 18, "General commerce course",
                                  ifelse(data1$"Father's qualification" == 19, "Basic Education 3rd Cycle (9th/10th/11th Year) or Equiv.",
                                  ifelse(data1$"Father's qualification" == 20, "Complementary High School Course", 
                                  ifelse(data1$"Father's qualification" == 22, "Technical-professional course",
                                  ifelse(data1$"Father's qualification" == 25, "Complementary High School Course - not concluded", 
                                  ifelse(data1$"Father's qualification" == 26, "7th year of schooling",
                                  ifelse(data1$"Father's qualification" == 27, "2nd cycle of the general high school course",
                                  ifelse(data1$"Father's qualification" == 29, "9th Year of Schooling - Not Completed",
                                  ifelse(data1$"Father's qualification" == 30, "8th year of schooling",
                                  ifelse(data1$"Father's qualification" == 31, "General Course of Administration and Commerce", 
                                  ifelse(data1$"Father's qualification" == 33, "Supplementary Accounting and Administration", 
                                  ifelse(data1$"Father's qualification" == 34, "Unknown",
                                  ifelse(data1$"Father's qualification" == 35, "Can't read or write",
                                  ifelse(data1$"Father's qualification" == 36, "Can read without having a 4th year of schooling",
                                  ifelse(data1$"Father's qualification" == 37, "Basic education 1st cycle (4th/5th year) or equiv.",
                                  ifelse(data1$"Father's qualification" == 38, "Basic Education 2nd Cycle (6th/7th/8th Year) or Equiv.",
                                  ifelse(data1$"Father's qualification" == 39, "Technological specialization course",
                                  ifelse(data1$"Father's qualification" == 40, "Higher education - degree (1st cycle)",
                                  ifelse(data1$"Father's qualification" == 41, "Specialized higher studies course",
                                  ifelse(data1$"Father's qualification" == 42, "Professional higher technical course",
                                  ifelse(data1$"Father's qualification" == 43, "Higher Education - Master (2nd cycle)",
                                  ifelse(data1$"Mother's qualification" == 44, "Higher Education - Doctorate (3rd cycle)", NA)))))))))))))))))))))))))))))))))) 

data1$"Displaced" <- ifelse(data1$"Displaced" == 1, "yes", "no") 

data1$"Educational special needs" <- ifelse(data1$"Educational special needs" == 1, "yes", "no") 

data1$"Debtor" <- ifelse(data1$"Debtor" == 1, "yes", "no") 

data1$"Gender" <- ifelse(data1$"Gender" == 1, "Male", "Female") 

data1$"Scholarship holder" <- ifelse(data1$"Scholarship holder" == 1, "yes", "no") 

data1$"International" <- ifelse(data1$"International" == 1, "yes", "no") 

categorical_cols <- c("Marital status",
                      "Application mode", 
                      "Application order",
                      "Course",
                      "Daytime/evening attendance",
                      "Previous qualification",
                      "Nationality",
                      "Mother's qualification",
                      "Father's qualification",
                      "Displaced",
                      "Educational special needs",
                      "Debtor",
                      "Gender",
                      "Scholarship holder",
                      "International")

library(ggplot2)
library(scales)

for (i in categorical_cols) {
  plot <- ggplot(data1, aes(x = .data[[i]])) +
    geom_bar(fill = "steelblue", color = "black") +
    labs(
      title = paste("Frequency of", i), 
      x = i,
      y = "Count"
    ) +
    theme_minimal() + 
    scale_x_discrete(
      labels = label_wrap(150) 
    ) + 
    coord_flip() + theme(
      axis.text = element_text(size = 6) 
    )
  print(plot)
}

#for (i in categorical_cols) {
#  plot <- ggplot(data1, aes(x = .data[[i]], fill = Target)) +
#    geom_bar(position = "identity", alpha=0.5) +
#    labs(
#      title = paste("Frequency of", i, "by Target"),
#      x = i,
#      y = "Count"
#    ) +
#    theme_minimal() + 
#    scale_x_discrete(
#      labels = label_wrap(150) 
#    ) + 
#    coord_flip() + theme(
#      axis.text = element_text(size = 6) 
#    )
#  print(plot)
#}

for (i in categorical_cols) {
  plot <- ggplot(data1, aes(x = .data[[i]], fill = Target)) +
  geom_bar(position = "dodge") +
  labs(
    title = paste("Frequency of", i, "by Target"),
    x = i,
    y = "Count"
    ) +
    theme_minimal() + 
    scale_x_discrete(
      labels = label_wrap(150) 
    ) + 
    coord_flip() + theme(
      axis.text = element_text(size = 6) 
    )
  print(plot)
}

ggplot(data1, aes(x = `Marital status`, fill = Target)) +
  geom_bar(position = "dodge", alpha = 0.7) +
  facet_wrap(~Target, scales = "free_y") +
  labs(y = "Count") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))

numeric_cols <- names(data1)[sapply(data1, is.numeric)]

for (i in numeric_cols) {
  plot <- ggplot(data1, aes(x = .data[[i]])) +
    geom_histogram(bins = 45, fill = "steelblue", color = "black") +
    labs(title = paste("Histogram of", i), x = i, y = "Frequency") +
    theme_minimal() 
  print(plot)
}

#for (i in numeric_cols) {
#  plot <- ggplot(data1, aes(x = .data[[i]], fill = Target)) +
#  geom_histogram(bins = 45, color = "black") +
#    labs(title = paste("Stacked Histogram of", i, "by Target"), x = i, y = "Frequency") +
#    theme_minimal() 
# print(plot)
#}

for (i in numeric_cols) {
  plot <- ggplot(data1, aes(x = .data[[i]], fill = Target)) +
    geom_histogram(bins = 45, position = "identity", alpha = 0.5) +
    labs(title = paste("Overlaid Histogram of", i, "by Target"), x = i, y = "Frequency") +
    theme_minimal() 
  print(plot)
}

for (i in numeric_cols) {
  plot <- ggplot(data1, aes(x = .data[[i]], fill = Target)) +
    geom_histogram(bins = 15, position = "dodge") +
    labs(title = paste("Dodged Histogram of", i, "by Target"), x = i, y = "Frequency") +
    theme_minimal() 
  print(plot)
}

ggplot(data1,aes(x=`Unemployment rate`, color=Target)) + 
  facet_wrap(~Target,scales="free") +
  geom_histogram(aes(y=after_stat(density)), bins = 50, alpha=0.5,
                 position="identity", fill="white")+
  geom_density(alpha=.2)

ggplot(data1,aes(x=`Inflation rate`, color=Target)) + 
  facet_wrap(~Target,scales="free") +
  geom_histogram(aes(y=after_stat(density)), bins = 50, alpha=0.5,
                 position="identity", fill="white")+
  geom_density(alpha=.2)

ggplot(data1,aes(x=`GDP`, color=Target)) + 
  facet_wrap(~Target,scales="free") +
  geom_histogram(aes(y=after_stat(density)), bins = 50, alpha=0.5,
                 position="identity", fill="white")+
  geom_density(alpha=.2)

data1$`Marital status` <- ifelse(
  data1$`Marital status` == "single", 
  "single", 
  "not single"
)

data1$`Application order` <- ifelse(
  data1$`Application order` == "2nd choice", 
  "2nd choice", 
  "not 2nd choice"
)

data1$`Nationality` <- ifelse(
  data1$`Nationality` == "Portuguese", 
  "Portuguese", 
  "not Portuguese"
)

data1$`Previous qualification` <- ifelse(
  data1$`Previous qualification` == "Other − 11th year of schooling" | 
    data1$`Previous qualification` == "10th year of schooling"| 
    data1$`Previous qualification` == "10th year of schooling − not completed" | 
    data1$`Previous qualification` == "11th year of schooling − not completed" |
    data1$`Previous qualification` == "12th year of schooling − not completed" |
    data1$`Previous qualification` == "Basic education 2nd cycle (6th/7th/8th year) or equiv." |
    data1$`Previous qualification` == "Basic education 3rd cycle (9th/10th/11th year) or equiv.", 
  "underqualified", 
  ifelse(
    data1$`Previous qualification` == "Secondary education",
    "qualified", 
    "overqualified"
  )
)

data1$`Application mode` <- ifelse(
  data1$`Application mode` == "3rd phase − general contingent" | 
    data1$`Application mode` == "2nd phase − general contingent" | 
    data1$`Application mode` == "1st phase − general contingent", 
  "General contingent", 
  ifelse(
    data1$`Application mode` == "Over 23 years old",
    "Over 23 years old", 
    "Other"
  )
)

data1$`Mother's qualification` <- ifelse(
  data1$`Mother's qualification` == "Unknown" | 
  data1$`Mother's qualification` == "Can read without having a 4th year of schooling" | 
  data1$`Mother's qualification` == "Can't read or write" |
  data1$`Mother's qualification` == "9th Year of Schooling − Not Completed" |
  data1$`Mother's qualification` == "10th Year of Schooling" |
  data1$`Mother's qualification` == "11th Year of Schooling − Not Completed" |
  data1$`Mother's qualification` == "12th Year of Schooling − Not Completed" |
  data1$`Mother's qualification` == "2nd cycle of the general high school course" |
  data1$`Mother's qualification` == "7th Year (Old)" |
  data1$`Mother's qualification` == "7th year of schooling" |
  data1$`Mother's qualification` == "8th year of schooling",
    "Other", 
  ifelse(
    data1$`Mother's qualification` == "Basic education 1st cycle (4th/5th year) or equiv." |
    data1$`Mother's qualification` == "Basic Education 2nd Cycle (6th/7th/8th Year) or Equiv." |
    data1$`Mother's qualification` == "Basic Education 3rd Cycle (9th/10th/11th Year) or Equiv." |
    data1$`Mother's qualification` == "Other − 11th Year of Schooling",
    "Basic education", 
    "Secondary education and above"
  )
)

data1$`Father's qualification` <- ifelse(
  data1$`Father's qualification` == "NA" | 
  data1$`Father's qualification` == "Unknown" | 
  data1$`Father's qualification` == "Can read without having a 4th year of schooling" | 
  data1$`Father's qualification` == "Can't read or write" | 
  data1$`Father's qualification` == "10th Year of Schooling" | 
  data1$`Father's qualification` == "11th Year of Schooling − Not Completed" | 
  data1$`Father's qualification` == "12th Year of Schooling − Not Completed" | 
  data1$`Father's qualification` == "2nd cycle of the general high school course" | 
  data1$`Father's qualification` == "2nd year complementary high school course" | 
  data1$`Father's qualification` == "7th Year (Old)" | 
  data1$`Father's qualification` == "7th year of schooling" | 
  data1$`Father's qualification` == "8th year of schooling" | 
  data1$`Father's qualification` == "9th Year of Schooling − Not Completed",
  "Other", 
  ifelse(
    data1$`Father's qualification` == "Basic Education 3rd Cycle (9th/10th/11th Year) or Equiv." |
    data1$`Father's qualification` == "Basic Education 2nd Cycle (6th/7th/8th Year) or Equiv." | 
    data1$`Father's qualification` == "Basic education 1st cycle (4th/5th year) or equiv." | 
    data1$`Father's qualification` == "Other − 11th Year of Schooling" | 
    data1$`Father's qualification` == "Complementary High School Course − not concluded",
    "Basic education", 
    "Secondary education and above"
  )
)

head(data1)

for (col_name in categorical_cols) {
  a <- data.frame(x1 = data1[[col_name]], x2 = data1[["Target"]])
  table(a) 
  b <- table(a) 
  c <- b/rowSums(b) #% 
  print(c)
}

# multiclass classification 

#define helper for decision boundary visualization 
decisionplot <- function(model, data, class = NULL, predict_type = "class",
                         resolution = 200, ...) {
  require(data.table)
  require(ggplot2)
  
  if (!is.data.table(data)) data <- as.data.table(data)
  
  if (!is.null(class)) {
    cl <- data[[class]]
  } else {
    stop("You must provide the class column name using `class =`")
  }
  
  data_xy <- data[, 1:2, with = FALSE]
  k <- length(unique(cl))
  
  # Build grid
  r <- sapply(data_xy, range, na.rm = TRUE)
  grid_x1 <- seq(r[1, 1], r[2, 1], length.out = resolution)
  grid_x2 <- seq(r[1, 2], r[2, 2], length.out = resolution)
  grid <- as.data.table(expand.grid(x1 = grid_x1, x2 = grid_x2))
  
  # Predict over grid
  p <- predict(model, newdata = grid, type = predict_type)
  if (is.list(p)) p <- p$class
  grid[, yhat := as.factor(p)]
  
  # Return ggplot object
  plt <- ggplot() +
    geom_point(data = grid, aes(x1, x2, color = yhat), alpha = 0.05, shape = 15) +
    geom_point(data = data, aes(x1, x2, color = get(class)), shape = 1) +
    labs(title = "Decision Boundary", color = "Class") +
    theme_minimal()
  
  print(plt)
  invisible(plt)
}

library(tidyverse)
library(caret)
library(nnet)
library(pROC)
library(dplyr)
library(janitor) 

data2 <- data1 
data2 <- select(data2, "Previous qualification grade",   
                     "Admission grade", 
                     "Age at enrollment",                   
                     "Semester 1 enrolled units",           
                     "Semester 1 approved units",           
                     "Semester 1 grade",                    
                     "Semester 2 enrolled units",           
                     "Semester 2 approved units",           
                     "Semester 2 grade",
                     "Target")

names(data2) <- c("previous_qualification_grade",
                 "admission_grade",
                 "age_at_enrollment",
                 "semester_1_enrolled_units",
                 "semester_1_approved_units",
                 "semester_1_grade",
                 "semester_2_enrolled_units",
                 "semester_2_approved_units",
                 "semester_2_grade",
                 "Target")

head(data2)

data <- data2 

data$Target <- ifelse(data$Target == "Enrolled", 1,
                              ifelse(data$Target == "Dropout", 2,
                                     ifelse(data$Target == "Graduate", 3, NA))) 

head(data)

y <- data$Target 

X1 <- filter(data, y == 1)
head(X1) 
X2 <- filter(data, y == 2)
head(X2) 
X3 <- filter(data, y == 3)
head(X3) 

X <- rbind(X1, X2, X3)
head(X)
head(y)

data <- data.table(x1 = X[, 1], x2 = X[, 2], y = y)
head(data)

data$y <- factor(data$y)

ggplot(data, aes(x = x1, y = x2, color = y)) +
  geom_point(size = 2, alpha = 0.7) +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
  ggtitle("") +
  labs(color = "True Class", shape = "Predicted Class") +
  theme(legend.position = "bottom")

#LDA model and decision boundary 

lda_model <- lda(y ~ x1 + x2, data = data)
lda_pred <- predict(lda_model, data)
table("LDA" = lda_pred$class, "True" = data$y)

decisionplot(lda_model, data, class="y")

#QDA and comparison 

qda_model <- qda(y ~ x1 + x2, data = data)
qda_pred <- predict(qda_model, data)
table("QDA" = qda_pred$class, "True" = data$y)

decisionplot(qda_model, data, class="y")

#RDA 
library(mvtnorm)
library(data.table)

K <- 3

mu_list <- lapply(1:K, function(k) {
  colMeans(data[y == k, .(x1, x2)])
})

n_k <- as.numeric(table(y))
prior <- prop.table(n_k)

cov_list <- lapply(1:K, function(k) {
  X_k <- data[y == k, .(x1, x2)]
  cov(X_k)
})

Sigma_pooled <- Reduce("+", lapply(1:2, function(k) (n_k[k]-1)*cov_list[[k]])) / (sum(n_k) - K)

sigma2_hat <- mean(sapply(cov_list, function(S) mean(diag(S))))

#compute sigma_k^tilde and final RDA covariances 
lambda <- 0.5
gamma <- 0.5

tilde_covs <- lapply(1:K, function(k) {
  lambda * cov_list[[k]] + (1 - lambda) * sigma2_hat * diag(2)
})

rda_covs <- lapply(1:K, function(k) {
  gamma * tilde_covs[[k]] + (1 - gamma) * Sigma_pooled
})

#RDA prediction function 
rda_predict <- function(Xnew, mu_list, cov_list, prior) {
  log_post <- sapply(1:length(mu_list), function(k) {
    dmvnorm(Xnew, mean = mu_list[[k]], sigma = cov_list[[k]], log = TRUE) + log(prior[k])
  })
  apply(log_post, 1, which.max)
}

#plot 
grid_x1 <- seq(min(data$x1), max(data$x1), length.out = 200)
grid_x2 <- seq(min(data$x2), max(data$x2), length.out = 200)
grid <- as.data.table(expand.grid(x1 = grid_x1, x2 = grid_x2))

yhat <- rda_predict(as.matrix(grid), mu_list, rda_covs, prior)
grid[, yhat := as.factor(yhat)]

ggplot() +
  geom_point(data = grid, aes(x1, x2, color = yhat), alpha = 0.05, shape = 15) +
  geom_point(data = data, aes(x1, x2, color = y), shape = 1) +
  labs(title = "Regularized Discriminant Analysis", color = "Class") +
  theme_minimal()

#computation for QDA via eigen-decomposition 
#compute eigen-decomposition for sigma_k
cov1 <- cov(X1)
eig1 <- eigen(cov1)

cat("Eigenvalues (class 1):", eig1$values, "\n")
cat("Orthonormal matrix U_k (class 1):\n")
print(eig1$vectors)

#reduced-rank LDA 
#mean vectors for each class 
means <- by(data[, .(x1, x2)], data$y, colMeans)
means_matrix <- do.call(rbind, means)

#compute between-class and within-class scatter matrices 
grand_mean <- colMeans(data[, .(x1, x2)])
B <- t(means_matrix - grand_mean) %*% (means_matrix - grand_mean)
X <- select(X, x1, x2)
W <- var(X)

#eigen-decomposition of W^{-1}B 
eig <- eigen(solve(W) %*% B)
eig$vectors  # Discriminant directions

#single linear regression 
linear_model <- lm(as.numeric(y) ~ x1 + x2, data = data)
linear_pred <- as.numeric(predict(linear_model, data))

linear_pred_class <- ifelse(linear_pred < 1.5, 1, ifelse(linear_pred < 2.5, 2, 3))
table("Linear Regression" = linear_pred_class, "True" = data$y)

result1 <- cbind(data,Linear_Regression=as.factor(linear_pred_class))
ggplot(result1, aes(x = x1, y = x2, color = y)) +
  geom_point(aes(shape = Linear_Regression), size = 2, alpha = 0.7) +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
  ggtitle("Single Linear Regression") +
  labs(color = "True Class", shape = "Predicted Class") +
  theme(legend.position = "bottom")

#one-vs-all linear regression 

#linear regression for class 1 
linear_model_1 <- lm(I(y == 1) ~ x1 + x2, data = data)
linear_pred_1 <- predict(linear_model_1, data)

#for class 2 
linear_model_2 <- lm(I(y == 2) ~ x1 + x2, data = data)
linear_pred_2 <- predict(linear_model_2, data)

linear_model_3 <- lm(I(y == 3) ~ x1 + x2, data = data)
linear_pred_3 <- predict(linear_model_3, data)

#combine
linear_pred <- data.frame(
  class1 = linear_pred_1,
  class2 = linear_pred_2,
  class3 = linear_pred_3
)

#determine class with highest predicted value 
linear_pred_class <- apply(linear_pred, 1, function(x) names(x)[which.max(x)])
linear_pred_class <- gsub("class","",linear_pred_class)

table("Linear Regression" = linear_pred_class, "True" = data$y)

result2 <- cbind(data,Linear_Regression=linear_pred_class)
ggplot(result2, aes(x = x1, y = x2, color = y)) +
  geom_point(aes(shape = Linear_Regression), size = 2, alpha = 0.7) +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
  ggtitle("One-vs-all Linear Regression") +
  labs(color = "True Class", shape = "Predicted Class") +
  theme(legend.position = "bottom")

#confusion matrices 
dc_data <- as.matrix(data[,1:2])%*%as.matrix(lda_model$scaling)
dc_data <- as.data.frame(dc_data)
dc_data$True <- data$y
dc_data$LDA <- lda_pred$class
dc_data$Linear_Regression <- factor(linear_pred_class)
head(dc_data)

ggplot(dc_data, aes(x = LD1, y = LD2, color = True)) +
  geom_point() +
  geom_point(aes( shape = LDA), size = 4, alpha = 0.3) +
  geom_point(aes(shape = Linear_Regression), size = 2, alpha = 0.7) +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
  ggtitle("Comparison of LDA and Linear Regression") +
  labs(color = "True Class", shape = "Predicted Class") +
  theme(legend.position = "bottom")

#logistic regression for multinomial 
library(tidyverse)
library(caret)
library(nnet)
library(pROC)

str(X)

set.seed(100)
train.sample1 <- sample(seq_len(nrow(X)), size=0.8*nrow(X))

length(train.sample1)

set.seed(100)
train.sample2 <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
sum(train.sample2)

train.data <- data[train.sample1, ]
dim(train.data)
test.data <- data[-train.sample1, ] 
dim(test.data)

#one vs. all 
y.uni <- as.character(unique(data$y))
y.uni

#create an empty list to hold each binary classifier 
classifiers_ova <- list()
train.data2 <- train.data
head(train.data2)

#loop to train a classifier for each class 
#for (k in y.uni) {
#  y_binary <- ifelse(train.data2$y == k, 1, 0)
#  assign("train.data2", within(train.data2, assign(k, y_binary)))
#  formula.glm <- as.formula(paste(k,"~ x1 + x2"))
#  fit <- glm(formula.glm, data = train.data2, family = binomial)
#  classifiers_ova[[k]] <- fit
#}

classifiers_ova <- lapply(y.uni, function(k) {
  y_binary <- ifelse(train.data2$y == k, 1, 0)
  temp_data <- data.frame(y_binary = y_binary, x1 = train.data2$x1, x2 = train.data2$x2)
  glm(y_binary ~ x1 + x2, data = temp_data, family = binomial)
})

names(classifiers_ova) <- y.uni

#prediction function for OvA 
predict_OvA <- function(newdata, classifiers) {
  scores <- sapply(classifiers, function(fit) predict(fit, newdata = data.frame(newdata), type = "response"))
  return(y.uni[apply(scores, 1, which.max)])
}

#test the prediction function 
OvA_pred <- predict_OvA(test.data, classifiers_ova)

mean(OvA_pred == test.data$y)

#confusion matrix 
conf.ova <- confusionMatrix(as.factor(OvA_pred), test.data$y)
print(conf.ova)

#multinomial/softmaxx 
logit_model <- nnet::multinom(y ~., data = train.data)
summary(logit_model)

logit_pred <- logit_model %>% predict(test.data)
head(logit_pred)

#levels 
mean(logit_pred == test.data$y)
conf.logit <- confusionMatrix(as.factor(logit_pred), test.data$y)

print(conf.logit)

#LDA 
lda_model <- lda(y ~ ., data = train.data)

lda_pred <- lda_model %>% 
  predict(test.data) %>%
  `[[`("class")

lda_pred <- lda_model %>% 
  predict(test.data) %>%
  pluck("class")

mean(lda_pred==test.data$y)
conf.lda <- confusionMatrix(as.factor(lda_pred), test.data$y)
print(conf.lda)

#linear 
linear_model_1 <- lm(I(y == 1) ~ ., data = train.data)
linear_model_2 <- lm(I(y == 2) ~ ., data = train.data)
linear_model_3 <- lm(I(y == 3) ~ ., data = train.data)


linear_pred_1 <- predict(linear_model_1, test.data)
linear_pred_2 <- predict(linear_model_2, test.data)
linear_pred_3 <- predict(linear_model_3, test.data) 

linear_pred <- data.frame(
  1 == linear_pred_1,
  2 == linear_pred_2,
  3 == linear_pred_3 
)

linear_pred_class <- apply(linear_pred, 1, function(x) names(x)[which.max(x)])
mean(linear_pred_class==test.data$y)

#plot 
dc_data <- as.matrix(test.data[,1:2])%*%as.matrix(lda_model$scaling)
dc_data <- as.data.frame(dc_data)
dc_data$True <- test.data$y
dc_data$Logit <- logit_pred
dc_data$LDA <- lda_pred
dc_data$Linear <- factor(linear_pred_class)
head(dc_data)

ggplot(dc_data, aes(x = LD1, y = LD2, color = True)) +
  geom_point(aes(shape = Logit),  size = 4, alpha = 0.3) +
  geom_point(aes(shape = LDA),  size = 2, alpha = 0.9) +
  # geom_point(aes(shape = Linear), size = 2, alpha = 0.7) +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
  ggtitle("PCA Visualization of Actual and Predicted Classes") +
  labs(shape = "Predicted Class",
       color = "Actual Class") +
  theme(legend.position = "bottom")
