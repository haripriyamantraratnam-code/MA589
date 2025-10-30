
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
library(ggplot2)
library(tidyr)
library(dplyr)
library(scales)

# histograms for higher education dataset 
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

data1$"Mother's occupation" <- ifelse(data1$"Mother's occupation" == 0, "Student",
                               ifelse(data1$"Mother's occupation" == 1, "Representatives of the Legislative Power and Executive Bodies, Directors, Directors and Executive Managers",
                               ifelse(data1$"Mother's occupation" == 2, "Specialists in Intellectual and Scientific Activities",
                               ifelse(data1$"Mother's occupation" == 3, "Intermediate Level Technicians and Professions",
                               ifelse(data1$"Mother's occupation" == 4, "Administrative staff",
                               ifelse(data1$"Mother's occupation" == 5, "Personal Services, Security and Safety Workers and Sellers",
                               ifelse(data1$"Mother's occupation" == 6, "Farmers and Skilled Workers in Agriculture, Fisheries and Forestry",
                               ifelse(data1$"Mother's occupation" == 7, "Skilled Workers in Industry, Construction and Craftsmen",
                               ifelse(data1$"Mother's occupation" == 8, "Installation and Machine Operators and Assembly Workers",
                               ifelse(data1$"Mother's occupation" == 9, "Unskilled Workers",
                               ifelse(data1$"Mother's occupation" == 10, "Armed Forces Professions 90 - Other Situation",
                               ifelse(data1$"Mother's occupation" == 99, "(Blank)",
                               ifelse(data1$"Mother's occupation" == 122, "Health professionals",
                               ifelse(data1$"Mother's occupation" == 123, "Teachers",
                               ifelse(data1$"Mother's occupation" == 125, "Specialists in information and communication technologies (ICT)",
                               ifelse(data1$"Mother's occupation" == 131, "Intermediate level science and engineering technicians and professions",
                               ifelse(data1$"Mother's occupation" == 132, "Technicians and professionals, of intermediate level of health",
                               ifelse(data1$"Mother's occupation" == 134, "Intermediate level technicians from legal, social, sports, cultural and similar services",
                               ifelse(data1$"Mother's occupation" == 141, "Office workers, secretaries in general and data processing operators",
                               ifelse(data1$"Mother's occupation" == 143, "Data, accounting, statistical, financial services and registry-related operators",
                               ifelse(data1$"Mother's occupation" == 144, "Other administrative support staff",
                               ifelse(data1$"Mother's occupation" == 151, "Personal service workers",
                               ifelse(data1$"Mother's occupation" == 152, "Sellers",
                               ifelse(data1$"Mother's occupation" == 153, "Personal care workers and the like",
                               ifelse(data1$"Mother's occupation" == 171, "Skilled construction workers and the like, except electricians",
                               ifelse(data1$"Mother's occupation" == 173, "Skilled workers in printing, precision instrument manufacturing, jewelers, artisans and the like",
                               ifelse(data1$"Mother's occupation" == 175, "Workers in food processing, woodworking, clothing and other industries and crafts",
                               ifelse(data1$"Mother's occupation" == 191, "Cleaning workers",
                               ifelse(data1$"Mother's occupation" == 192, "Unskilled workers in agriculture, animal production, fisheries and forestry",
                               ifelse(data1$"Mother's occupation" == 193, "Unskilled workers in extractive industry, construction, manufacturing and transport", 
                               ifelse(data1$"Mother's occupation" == 194, "Meal preparation assistants", NA)))))))))))))))))))))))))))))))

data1$"Father's occupation" <- ifelse(data1$"Father's occupation" == 0, "Student",
                               ifelse(data1$"Father's occupation" == 1, "Representatives of the Legislative Power and Executive Bodies, Directors, Directors and Executive Managers",
                               ifelse(data1$"Father's occupation" == 2, "Specialists in Intellectual and Scientific Activities",
                               ifelse(data1$"Father's occupation" == 3, "Intermediate Level Technicians and Professions",
                               ifelse(data1$"Father's occupation" == 4, "Administrative staff",
                               ifelse(data1$"Father's occupation" == 5, "Personal Services, Security and Safety Workers and Sellers",
                               ifelse(data1$"Father's occupation" == 6, "Farmers and Skilled Workers in Agriculture, Fisheries and Forestry",
                               ifelse(data1$"Father's occupation" == 7, "Skilled Workers in Industry, Construction and Craftsmen",
                               ifelse(data1$"Father's occupation" == 8, "Installation and Machine Operators and Assembly Workers",
                               ifelse(data1$"Father's occupation" == 9, "Unskilled Workers",
                               ifelse(data1$"Father's occupation" == 10, "Armed Forces Professions 90 - Other Situation",
                               ifelse(data1$"Father's occupation" == 99, "(Blank)",
                               ifelse(data1$"Father's occupation" == 101, "Armed Forces Officers", 
                               ifelse(data1$"Father's occupation" == 102, "Armed Forces Sergeants",
                               ifelse(data1$"Father's occupation" == 103, "Other Armed Forces personnel",
                               ifelse(data1$"Father's occupation" == 112, "Directors of administrative and commercial services",
                               ifelse(data1$"Father's occupation" == 114, "Hotel, catering, trade and other services directors",  
                               ifelse(data1$"Father's occupation" == 121, "Specialists in the physical sciences, mathematics, engineering and related techniques", 
                               ifelse(data1$"Father's occupation" == 122, "Health professionals",
                               ifelse(data1$"Father's occupation" == 123, "Teachers",
                               ifelse(data1$"Father's occupation" == 124, "Specialists in finance, accounting, administrative organization, public and commercial relations",
                               ifelse(data1$"Father's occupation" == 131, "Intermediate level science and engineering technicians and professions",
                               ifelse(data1$"Father's occupation" == 132, "Technicians and professionals, of intermediate level of health",
                               ifelse(data1$"Father's occupation" == 134, "Intermediate level technicians from legal, social, sports, cultural and similar services",
                               ifelse(data1$"Father's occupation" == 135, "Information and communication technology technicians", 
                               ifelse(data1$"Father's occupation" == 141, "Office workers, secretaries in general and data processing operators",
                               ifelse(data1$"Father's occupation" == 143, "Data, accounting, statistical, financial services and registry-related operators",
                               ifelse(data1$"Father's occupation" == 144, "Other administrative support staff",
                               ifelse(data1$"Father's occupation" == 151, "Personal service workers",
                               ifelse(data1$"Father's occupation" == 152, "Sellers",
                               ifelse(data1$"Father's occupation" == 153, "Personal care workers and the like",
                               ifelse(data1$"Father's occupation" == 154, "Protection and security services personnel", 
                               ifelse(data1$"Father's occupation" == 161, "Market-oriented farmers and skilled agricultural and animal production workers", 
                               ifelse(data1$"Father's occupation" == 163, "Farmers, livestock keepers, fishermen, hunters and gatherers, subsistence", 
                               ifelse(data1$"Father's occupation" == 171, "Skilled construction workers and the like, except electricians",
                               ifelse(data1$"Father's occupation" == 172, "Skilled workers in metallurgy, metalworking and similar", 
                               ifelse(data1$"Father's occupation" == 174, "Skilled workers in electricity and electronics",
                               ifelse(data1$"Father's occupation" == 175, "Workers in food processing, woodworking, clothing and other industries and crafts",
                               ifelse(data1$"Father's occupation" == 181, "Fixed plant and machine operators", 
                               ifelse(data1$"Father's occupation" == 182, "Assembly workers", 
                               ifelse(data1$"Father's occupation" == 183, "Vehicle drivers and mobile equipment operators",
                               ifelse(data1$"Father's occupation" == 192, "Unskilled workers in agriculture, animal production, fisheries and forestry",
                               ifelse(data1$"Father's occupation" == 193, "Unskilled workers in extractive industry, construction, manufacturing and transport", 
                               ifelse(data1$"Father's occupation" == 194, "Meal preparation assistants", 
                               ifelse(data1$"Father's occupation" == 195, "Street vendors (except food) and street service providers", NA)))))))))))))))))))))))))))))))))))))))))))))

data1$"Displaced" <- ifelse(data1$"Displaced" == 1, "yes", "no") 

data1$"Educational special needs" <- ifelse(data1$"Educational special needs" == 1, "yes", "no") 

data1$"Debtor" <- ifelse(data1$"Debtor" == 1, "yes", "no") 

data1$"Tuition fees up to date" <- ifelse(data1$"Tuition fees up to date" == 1, "yes", "no") 

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
                      "Mother's occupation",
                      "Father's occupation",
                      "Displaced",
                      "Educational special needs",
                      "Debtor",
                      "Tuition fees up to date",
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

for (i in categorical_cols) {
  plot <- ggplot(data1, aes(x = .data[[i]], fill = Target)) +
    geom_bar(position = "identity", alpha=0.5) +
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

# these categorical variables show little variation: 
# marital status (single) 
# application order (2nd) 
# daytime/evening attendance (daytime) 
# previous qualification (secondary education) 
# nationality (Portuguese)
# educational special needs (no) 
# debtor (no) 
# tuition fees up to date (yes) 
# international (no) 

# other notes: 
# course with highest frequency is nursing 
# more displaced than not, but almost half and half 
# about 2/3 women, 1/3 men 
# about 3/4 without scholarship, 1/4 with scholarship 

numeric_cols <- names(data1)[sapply(data1, is.numeric)]

for (i in numeric_cols) {
  plot <- ggplot(data1, aes(x = .data[[i]])) +
    geom_histogram(bins = 45, fill = "steelblue", color = "black") +
    labs(title = paste("Histogram of", i), x = i, y = "Frequency") +
    theme_minimal() 
  print(plot)
}

# numeric columns with almost all zeros 
# Semester 1 credited units
# Semester 1 units without evaluations 
# Semester 2 credited units 
# Semester 2 units without evaluations 

for (i in numeric_cols) {
  plot <- ggplot(data1, aes(x = .data[[i]], fill = Target)) +
    geom_histogram(bins = 45, color = "black") +
    labs(title = paste("Stacked Histogram of", i, "by Target"), x = i, y = "Frequency") +
    theme_minimal() 
  print(plot)
}

# variables that seem important 
# Debtor (for Dropout) 
# Tuition fees up to date (for Dropout) 
# Scholarship holder 
# Semester 1 approved units 
# Semester 1 grade 
# Semester 2 approved units 
# Semester 2 grade 

for (i in numeric_cols) {
  plot <- ggplot(data1, aes(x = .data[[i]], fill = Target)) +
    geom_histogram(bins = 45, position = "identity", alpha = 0.5) +
    labs(title = paste("Overlaid Histogram of", i, "by Target"), x = i, y = "Frequency") +
    theme_minimal() 
  print(plot)
}

# additional variables that seem important 
# Semester 1 evaluations 
# Semester 2 evaluations

for (i in numeric_cols) {
  plot <- ggplot(data1, aes(x = .data[[i]], fill = Target)) +
    geom_histogram(bins = 15, position = "dodge") +
    labs(title = paste("Dodged Histogram of", i, "by Target"), x = i, y = "Frequency") +
    theme_minimal() 
  print(plot)
}


# histogram for income dataset 
# https://archive.ics.uci.edu/dataset/2/adult
# https://www.kaggle.com/code/yashhvyass/adult-census-income-logistic-reg-explained-86-2 

data2 <- read.csv("C:/Users/harip/Downloads/adult/adult.data", header=FALSE)
head(data2)

# final weight is the number of people the Census believes the entry represents 
names(data2) <- c("Age", 
                  "Employment status", 
                  "Final weight", 
                  "Education", 
                  "Years of education", 
                  "Marital status", 
                  "Occupation", 
                  "Relationship", 
                  "Race", 
                  "Sex", 
                  "Capital gain", 
                  "Capital loss", 
                  "Hours worked per week", 
                  "Country of origin", 
                  "Target")

categorical_cols <- c("Employment status", "Education", "Marital status", "Occupation", "Relationship", "Race", "Sex", "Country of origin") 

library(ggplot2) 
library(scales)

for (i in categorical_cols) {
  plot <- ggplot(data2, aes(x = .data[[i]])) +
    geom_bar(fill = "steelblue", color = "black") +
    labs(
      title = paste("Frequency of", i), 
      x = i,
      y = "Count"
    ) +
    theme_minimal() + 
    scale_x_discrete(
      labels = label_wrap(150) # Wrap labels at approximately 10 characters
    ) + 
    coord_flip() + theme(
      axis.text = element_text(size = 6) # Sets all axis text to size 8
    )
  print(plot)
}

# employment status is mostly private but has variation 
# race is mostly white but has variation 
# country of origin is mostly US 
# 2/3 male, 1/3 female 

for (i in categorical_cols) {
  plot <- ggplot(data2, aes(x = .data[[i]], fill = Target)) +
    geom_bar(position = "identity", alpha=0.5) +
    labs(
      title = paste("Frequency of", i, "by Target"),
      x = i,
      y = "Count"
    ) +
    theme_minimal() + 
    scale_x_discrete(
      labels = label_wrap(150) # Wrap labels at approximately 10 characters
    ) + 
    coord_flip() + theme(
      axis.text = element_text(size = 6) # Sets all axis text to size 8
    )
  print(plot)
}

# variables that seem important 
# relationship 
# occupation 
# marital status 

for (i in categorical_cols) {
  plot <- ggplot(data2, aes(x = .data[[i]], fill = Target)) +
    geom_bar(position = "dodge") +
    labs(
      title = paste("Frequency of", i, "by Target"),
      x = i,
      y = "Count"
    ) +
    theme_minimal() + 
    scale_x_discrete(
      labels = label_wrap(150) # Wrap labels at approximately 10 characters
    ) + 
    coord_flip() + theme(
      axis.text = element_text(size = 6) # Sets all axis text to size 8
    )
  print(plot)
}

numeric_cols <- names(data2)[sapply(data2, is.numeric)]
numeric_cols 

for (i in numeric_cols) {
  plot <- ggplot(data2, aes(x = .data[[i]])) +
    geom_histogram(bins = 45, fill = "steelblue", color = "black") +
    labs(title = paste("Histogram of", i), x = i, y = "Frequency") +
    theme_minimal() 
  print(plot)
}

# hours worked per week is mostly 40, with some variation 
# capital loss and gain are each mostly zero 
# age may be truncated 

for (i in numeric_cols) {
  plot <- ggplot(data2, aes(x = .data[[i]], fill = Target)) +
    geom_histogram(bins = 45, color = "black") +
    labs(title = paste("Stacked Histogram of", i, "by Target"), x = i, y = "Frequency") +
    theme_minimal() 
  print(plot)
}

for (i in numeric_cols) {
  plot <- ggplot(data2, aes(x = .data[[i]], fill = Target)) +
    geom_histogram(bins = 45, position = "identity", alpha = 0.5) +
    labs(title = paste("Overlaid Histogram of", i, "by Target"), x = i, y = "Frequency") +
    theme_minimal() 
  print(plot)
}

# Age is skewed to the right for <=50K 
# Hours worked per week: <=50K has a larger spread 

for (i in numeric_cols) {
  plot <- ggplot(data2, aes(x = .data[[i]], fill = Target)) +
    geom_histogram(bins = 15, position = "dodge") +
    labs(title = paste("Dodged Histogram of", i, "by Target"), x = i, y = "Frequency") +
    theme_minimal() 
  print(plot)
}

# multiclass classification for higher education dataset 
# Target classes: Enrolled, Dropout, Graduate 
# predictors:  Previous.qualification..grade., Admission.grade 
# I will add more predictors after the code runs as is 
# here I attempt the LDA, QDA, and RDA code 
# along with the previous logistic regression from hw 4 

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
library(janitor) # Best practice for cleaning names

data <- data1 %>%
  # Clean the names first, making them easier to work with
  clean_names() %>%
  # Now select using the clean, tidy names
  select(
    previous_qualification_grade,
    admission_grade,
    age_at_enrollment,
    semester_1_enrolled_units,
    semester_1_evaluations,
    semester_1_approved_units,
    semester_1_grade,
    semester_2_enrolled_units,
    semester_2_evaluations,
    semester_2_approved_units,
    semester_2_grade,
    target
  )


head(data1)
data <- data1 |> 
        select(`Previous qualification grade`, 
               `Admission grade`, 
               `Age at enrollment`, 
               `Semester 1 enrolled units`,
               `Semester 1 evaluations`,
               `Semester 1 approved units`,
               `Semester 1 grade`,
               `Semester 2 enrolled units`,
               `Semester 2 evaluations`,
               `Semester 2 approved units`,
               `Semester 2 grade`,
               Target
               )
names(data) <- c("marital_status",
                 "application_mode", 
                 "application_order",
                 "course",
                 "daytime_evening_attendance",
                 "previous_qualification",
                 "previous_qualification_grade",
                 "nationality",
                 "mothers_qualification",
                 "fathers_qualification",
                 "mothers_occupation",
                 "fathers_occupation",
                 "admission_grade",
                 "displaced",
                 "educational_special_needs",
                 "debtor",
                 "tuition_fees_up_to_date",
                 "gender",
                 "scholarship_holder",
                 "age_at_enrollment",
                 "international",
                 "semester_1_credited_units",
                 "semester_1_enrolled_units",
                 "semester_1_evaluations",
                 "semester_1_approved_units",
                 "semester_1_grade",
                 "semester_1_units_without_evaluations",
                 "semester_2_credited_units",
                 "semester_2_enrolle_units",
                 "semester_2_evaluations",
                 "semester_2_approved_units",
                 "semester_2_grade",
                 "semester_2_units_without_evaluations",
                 "unemployment_rate",
                 "inflation_rate",
                 "GDP",
                 "Target")
data <- select(data, `previous_qualification_grade`, admission_grade, age_at_enrollment, semester_1_enrolled_units, semester_1_evaluations, semester_1_approved_units, semester_1_grade, semester_2_enrolle_units, semester_2_evaluations, semester_2_approved_units, semester_2_grade, Target)
head(data)

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
y

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

# running into problems here, need to fix 

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
  # geom_point() +
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

str(data)

set.seed(100)
train.sample1 <- sample(seq_len(nrow(data)), size=0.8*nrow(data))

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
for (k in y.uni) {
  y_binary <- ifelse(train.data2$y == k, 1, 0)
  assign("train.data2", within(train.data2, assign(k, y_binary)))
  formula.glm <- paste(k,"~ Previous.qualification..grade. + Admission.grade")
  fit <- glm(formula.glm, data = train.data2, family = binomial)
  classifiers_ova[[k]] <- fit
}

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
dc_data <- as.matrix(test.data[,1:4])%*%as.matrix(lda_model$scaling)
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

# below this point is what was already submitted in hw 4 for number 3

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

#logistic regression for multinomial 
library(tidyverse)
library(caret)
library(nnet)
library(pROC)

data <- read.csv("C:/Users/harip/Downloads/predict+students+dropout+and+academic+success/data.csv", sep=";")
data <- select(data, Previous.qualification..grade., Admission.grade, Target)

str(data)

set.seed(100)
train.sample1 <- sample(seq_len(nrow(data)), size=0.8*nrow(data))

length(train.sample1)

set.seed(100)
train.sample2 <- sample(c(TRUE, FALSE), nrow(data), replace=TRUE, prob=c(0.8,0.2))
sum(train.sample2)

train.data <- data[train.sample1, ]
dim(train.data)
test.data <- data[-train.sample1, ] 
dim(test.data)

#one vs. all 
Target.uni <- as.character(unique(data$Target))
Target.uni

#create an empty list to hold each binary classifier 
classifiers_ova <- list()
train.data2 <- train.data
head(train.data2)

#loop to train a classifier for each class 
for (k in Target.uni) {
  Target_binary <- ifelse(train.data2$Target == k, 1, 0)
  assign("train.data2", within(train.data2, assign(k, Target_binary)))
  formula.glm <- paste(k,"~ Previous.qualification..grade. + Admission.grade")
  fit <- glm(formula.glm, data = train.data2, family = binomial)
  classifiers_ova[[k]] <- fit
}

#prediction function for OvA 
predict_OvA <- function(newdata, classifiers) {
  scores <- sapply(classifiers, function(fit) predict(fit, newdata = data.frame(newdata), type = "response"))
  return(Target.uni[apply(scores, 1, which.max)])
}

#test the prediction function 
OvA_pred <- predict_OvA(test.data, classifiers_ova)

mean(OvA_pred == test.data$Target)

all_possible_classes <- c("Dropout", "Enrolled", "Graduate")
OvA_pred <- factor(OvA_pred, levels = all_possible_classes)
test.data$Target <- factor(test.data$Target, levels = all_possible_classes)

#confusion matrix 
conf.ova <- confusionMatrix(as.factor(OvA_pred), as.factor(test.data$Target)) 
print(conf.ova)

#multinomial/softmaxx 
logit_model <- nnet::multinom(Target ~., data = train.data)
summary(logit_model)

logit_pred <- logit_model %>% predict(test.data)
head(logit_pred)

#levels 
mean(logit_pred == test.data$Target)
conf.logit <- confusionMatrix(as.factor(logit_pred), test.data$Target)
print(conf.logit)

#LDA 
lda_model <- lda(Target ~ ., data = train.data)

lda_pred <- lda_model %>% 
  predict(test.data) %>%
  `[[`("class")

lda_pred <- lda_model %>% 
  predict(test.data) %>%
  pluck("class")

mean(lda_pred==test.data$Target)
conf.lda <- confusionMatrix(as.factor(lda_pred), test.data$Target)
print(conf.lda)

#linear 
linear_model_Dropout <- lm(I(Target == "Dropout") ~ ., data = train.data)
linear_model_Enrolled <- lm(I(Target == "Enrolled") ~ ., data = train.data)
linear_model_Graduate <- lm(I(Target == "Graduate") ~ ., data = train.data)

linear_pred_Dropout <- predict(linear_model_Dropout, test.data)
linear_pred_Enrolled <- predict(linear_model_Enrolled, test.data)
linear_pred_Graduate <- predict(linear_model_Graduate, test.data)

linear_pred <- data.frame(
  Dropout = linear_pred_Dropout,
  Enrolled = linear_pred_Enrolled,
  Graduate = linear_pred_Graduate
)

linear_pred_class <- apply(linear_pred, 1, function(x) names(x)[which.max(x)])
mean(linear_pred_class==test.data$Target)

#plot 
dc_data <- as.matrix(test.data[,1:2])%*%as.matrix(lda_model$scaling)
dc_data <- as.data.frame(dc_data)
dc_data$True <- test.data$Target
dc_data$Logit <- logit_pred
dc_data$LDA <- lda_pred
dc_data$Linear <- factor(linear_pred_class)
head(dc_data)

ggplot(dc_data, aes(x = LD1, y = LD2, color = True)) +
  geom_point(aes(shape = Logit),  size = 4, alpha = 0.3) +
  geom_point(aes(shape = LDA),  size = 2, alpha = 0.9) +
  geom_point(aes(shape = Linear), size = 2, alpha = 0.7) +
  scale_shape_manual(values = c(16, 17, 18, 19, 20, 21)) +
  ggtitle("PCA Visualization of Actual and Predicted Classes") +
  labs(shape = "Predicted Class",
       color = "Actual Class") +
  theme(legend.position = "bottom")
