# Data Cleaning

# load libraries
library(tidyverse)
library(dplyr)
library(lubridate)

# read in data
dir("data")
dew_data <- read.csv("data/DEW Rates.csv")
sy20_data <- read.csv("data/Course Schedule Overview - SY20.csv")
sy21_data <- read.csv("data/Course Schedule Overview - SY21.csv")

glimpse(sy20_data)
glimpse(sy21_data)
glimpse(dew_data)

head(sy20_data)
summary(sy20_data)
# remove commas and convert total student count to integer value
dew_data$Total.Student.Count <- as.integer(gsub(",", "", dew_data$Total.Student.Count))

# change some column names
names(sy21_data)[names(sy21_data) == "ï..Term"] <- "Term"
names(sy20_data)[names(sy20_data) == "ï..Term"] <- "Term"
names(dew_data)[names(dew_data) == "ï..College"] <- "College"
names(dew_data)[names(dew_data) == "PercentÂ.D.Grade"] <- "D_Grade_Percent"
names(dew_data)[names(dew_data) == "PercentÂ.E.Grade"] <- "E_Grade_Percent"
names(dew_data)[names(dew_data) == "PercentÂ.W.Grade"] <- "W_Grade_Percent"

# remove class with less than 5 enrolled in dew
dew_data <- dew_data %>% 
  filter(Total.Student.Count > 5)

unique(dew_data$TERM_LD)
unique(sy21_data$Term)
unique(sy20_data$Term)

# remove winter terms and fall 2018
dew_data <- dew_data %>% 
  filter(TERM_LD != "Winter 2020", TERM_LD != "Winter 2018", TERM_LD != "Winter 2019", TERM_LD != "Fall 2018", TERM_LD != "Summer 2019", TERM_LD != "Spring 2019", TERM_LD != "Summer 2020", TERM_LD != "Summer 2021")

sy20_data <- sy20_data %>% 
  filter(Term != "Winter 2020")

sy21_data <- sy21_data %>% 
  filter(Term != "Winter 2021")

# create a merged column for a way to join
# subject + catalog
sy20_data <- sy20_data %>% 
  unite('Merged', Subject:Cat.., remove = FALSE)
sy21_data <- sy21_data %>% 
  unite("Merged", Subject:Cat.., remove = FALSE)
dew_data <- dew_data %>% 
  unite("Merged", Subject.Code:Catalog.Number, remove = FALSE)

# merged + term
sy20_data <- sy20_data %>% 
  unite("Course.Identifier", Term,Merged, remove = FALSE)
sy21_data <- sy21_data %>% 
  unite("Course.Identifier", Term,Merged, remove = FALSE)
dew_data <- dew_data %>% 
  unite("Course.Identifier", TERM_LD,Merged, remove = FALSE)

# remove dup colmuns from sy data in prep for joining
modified_sy20 <- select(sy20_data, -c(Term, Merged, Subject, Cat.., Min.Units, Max.Units, Course))
modified_sy21 <- select(sy21_data, -c(Term, Merged, Subject, Cat.., Min.Units, Max.Units, Course))

# what are the components and mode
unique(modified_sy20$Component)
unique(modified_sy21$Component)
unique(modified_sy20$Mode)
unique(modified_sy21$Mode)

# remove all independent study course
modified_sy20 <- modified_sy20 %>% 
  filter(Component != "Ind Study")
modified_sy21 <- modified_sy21 %>% 
  filter(Component != "Ind Study")

# remove total enrollment that = 0
modified_sy20 <- modified_sy20 %>% 
  filter(Total.Enroll > 0)
modified_sy21 <- modified_sy21 %>% 
  filter(Total.Enroll > 0)

# I am noticing a lot of dups that have same data so will dedup the df
deduped_sy20 <- unique(modified_sy20[,1:22])
deduped_sy21 <- unique(modified_sy21[,1:22])

# there are still dups but they have a unique facility name or end date
# the plan to remove course data with the flex facility that run for 1 week at end on semester
deduped_sy20 <- deduped_sy20 %>% 
  filter(Facility != "FLEX")
deduped_sy21 <- deduped_sy21 %>% 
  filter(Facility != "FLEX")


# Many labs have 2 entries - 1 for the first 14 wks(mode has a room listed) and 1 for the last 2 wks(mode is Live Online) this is unique to sy21
# my plan is to remove the last two week one and then adjust the end date for the other
deduped_sy21 <- deduped_sy21 %>% 
  filter(Start.Date != "11/26/2020")
# fix end of 11/25 to end of semester
deduped_sy21$End.Date <- gsub("11/25/2020", "12/9/2020", deduped_sy21$End.Date)

# remove class with NA in units
deduped_sy21 <- deduped_sy21 %>% 
  filter(Units != "NA")
deduped_sy20 <- deduped_sy20 %>% 
  filter(Units != "NA")

length(unique(deduped_sy21$Course.Identifier))
length(unique(deduped_sy20$Course.Identifier))

# Change time to a numeric value based on start time
deduped_sy20<- deduped_sy20 %>% 
  separate(Start, remove = FALSE, into = c("Start_hour","Start_min", "Am/PM"))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Start_hour = as.numeric(Start_hour))
deduped_sy21<- deduped_sy21 %>% 
  separate(Start, remove = FALSE, into = c("Start_hour","Start_min", "Am/PM"))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Start_hour = as.numeric(Start_hour))

# use a number system for time of day I think 6 columns - early morning, mid morning, early afternoon, mid afternoon, evening, asnyc
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Early_Morning = case_when(Start_hour >= 8 & Start_hour < 10 ~ 1,
                                   TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Mid_Morning = case_when(Start_hour >= 10 & Start_hour < 12 ~ 1,
                                 TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Early_Afternoon = case_when(Start_hour < 2  ~ 1,
                                     Start_hour == 12 & `Am/PM`== "PM" ~ 1, 
                                     TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Mid_Afternoon = case_when(Start_hour >= 2 & Start_hour < 4 ~ 1,
                                   TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Evening = case_when(Start_hour >= 4 & Start_hour < 8 ~ 1,
                             TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Asynchronous = case_when(Start_hour == 12 & `Am/PM` == "AM" ~ 1,
                                  TRUE ~ 0))

deduped_sy21 <- deduped_sy21 %>% 
  mutate(Early_Morning = case_when(Start_hour >= 8 & Start_hour < 10 ~ 1,
                                   TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Mid_Morning = case_when(Start_hour >= 10 & Start_hour < 12 ~ 1,
                                 TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Early_Afternoon = case_when(Start_hour < 2  ~ 1,
                                     Start_hour == 12 & `Am/PM`== "PM" ~ 1, 
                                     TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Mid_Afternoon = case_when(Start_hour >= 2 & Start_hour < 4 ~ 1,
                                   TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Evening = case_when(Start_hour >= 4 & Start_hour < 8 ~ 1,
                             TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Asynchronous = case_when(Start_hour == 12 & `Am/PM` == "AM" ~ 1,
                                  TRUE ~ 0))
# Now remove unneeded cloums
deduped_sy20 <- select(deduped_sy20, -c(Start, End, Start_hour, Start_min, `Am/PM`))
deduped_sy21 <- select(deduped_sy21, -c(Start, End, Start_hour, Start_min, `Am/PM`))


# Make columns for days of the week that can be numeric
# check day values
unique(deduped_sy20$Meeting.Days)
unique(deduped_sy21$Meeting.Days)

deduped_sy20 <- deduped_sy20 %>% 
  mutate(Monday = case_when(str_detect(Meeting.Days, "M") ~ 1,
                            TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Tuesday = case_when(str_detect(Meeting.Days, "T") ~ 1,
                             TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Wednesday = case_when(str_detect(Meeting.Days, "W") ~ 1,
                               TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Thursday = case_when(str_detect(Meeting.Days, "R") ~ 1,
                              TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Friday = case_when(str_detect(Meeting.Days, "F") ~ 1,
                            TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Saturday = case_when(str_detect(Meeting.Days, "S") ~ 1,
                              TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Sunday = case_when(str_detect(Meeting.Days, "U") ~ 1,
                            TRUE ~ 0))

deduped_sy21 <- deduped_sy21 %>% 
  mutate(Monday = case_when(str_detect(Meeting.Days, "M") ~ 1,
                            TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Tuesday = case_when(str_detect(Meeting.Days, "T") ~ 1,
                             TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Wednesday = case_when(str_detect(Meeting.Days, "W") ~ 1,
                               TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Thursday = case_when(str_detect(Meeting.Days, "R") ~ 1,
                              TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Friday = case_when(str_detect(Meeting.Days, "F") ~ 1,
                            TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Saturday = case_when(str_detect(Meeting.Days, "S") ~ 1,
                              TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Sunday = case_when(str_detect(Meeting.Days, "U") ~ 1,
                            TRUE ~ 0))

deduped_sy20 <- select(deduped_sy20, -c(Meeting.Days))
deduped_sy21 <- select(deduped_sy21, -c(Meeting.Days))

# # numeric values for components
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Laboratory = case_when(Component == "Laboratory" ~ 1,
                                TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Lecture = case_when(Component == "Lecture" ~ 1,
                             TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Colloquim = case_when(Component == "Colloquim" ~ 1,
                               TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Seminar = case_when(Component == "Seminar" ~ 1,
                             TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Workshop = case_when(Component == "Workshop" ~ 1,
                              TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Discussion = case_when(Component == "Discussion" ~ 1,
                                TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Studio = case_when(Component == "Studio" ~ 1,
                            TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Practicum = case_when(Component == "Practicum" ~ 1,
                               TRUE ~ 0))
# 
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Laboratory = case_when(Component == "Laboratory" ~ 1,
                                TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Lecture = case_when(Component == "Lecture" ~ 1,
                             TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Colloquim = case_when(Component == "Colloquim" ~ 1,
                               TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Seminar = case_when(Component == "Seminar" ~ 1,
                             TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Workshop = case_when(Component == "Workshop" ~ 1,
                              TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Discussion = case_when(Component == "Discussion" ~ 1,
                                TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Studio = case_when(Component == "Studio" ~ 1,
                            TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Practicum = case_when(Component == "Practicum" ~ 1,
                               TRUE ~ 0))

deduped_sy20 <- select(deduped_sy20, -c(Component))
deduped_sy21<- select(deduped_sy21, -c(Component))

# Now create a numeric value for mode
unique(deduped_sy20$Mode)
deduped_sy20 <- deduped_sy20 %>% 
  mutate(In_Person = case_when(Mode == "In Person" ~ 1,
                               TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Full_Online = case_when(Mode == "FullOnline" ~ 1,
                                 TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(IntractTV = case_when(Mode == "IntractTV" ~ 1,
                               TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Hybrid = case_when(Mode == "Hybrid" ~ 1,
                            TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Live_Online = case_when(Mode == "Live Onln" ~ 1,
                                 TRUE ~ 0))
unique(deduped_sy21$Mode)
deduped_sy21 <- deduped_sy21 %>% 
  mutate(In_Person = case_when(Mode == "In Person" ~ 1,
                               TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Full_Online = case_when(Mode == "FullOnline" ~ 1,
                                 TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(IntractTV = case_when(Mode == "IntractTV" ~ 1,
                               TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Hybrid = case_when(Mode == "Hybrid" ~ 1,
                            TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Live_Online = case_when(Mode == "Live Onln" ~ 1,
                                 TRUE ~ 0))

deduped_sy20 <- select(deduped_sy20, -c(Mode))
deduped_sy21<- select(deduped_sy21, -c(Mode))

# create numeric value for session
unique(deduped_sy20$Session)
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Reg_Session = case_when(Session == "Regular Academic Session" ~ 1,
                                 TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(First_Half_Session = case_when(Session == "Seven Week - First" | Session == "Fall 8 Week 1st" | Session == "Summer 8 Week 1st" ~ 1,
                                        TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Second_Half_Session = case_when(Session == "Seven Week - Second" | Session == "Fall 8 Week 2nd" | Session == "Summer 8 Week 2nd" ~ 1,
                                         TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(First_Third_Session = case_when(Session == "Five Week - First" ~ 1,
                                         TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Second_Third_Session = case_when(Session == "Five Week - Second" ~ 1,
                                          TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Third_Third_Session = case_when(Session == "Five Week - Third" ~ 1,
                                         TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Ten_Week = case_when(Session == "10 Week" ~ 1,
                              TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Thirteen_Week = case_when(Session == "13 Week" ~ 1,
                                   TRUE ~ 0))
deduped_sy20 <- deduped_sy20 %>% 
  mutate(Other = case_when(Session == "Carry-over Course"| Session == "Dynamically Dated Session"| Session == "Pre-session" | Session == "-" ~ 1,
                           TRUE ~ 0))

unique(deduped_sy21$Session)
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Reg_Session = case_when(Session == "Regular Academic Session" ~ 1,
                                 TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(First_Half_Session = case_when(Session == "Seven Week - First" | Session == "Fall 8 Week 1st" | Session == "Summer 8 Week 1st" | Session == "Spring 8 Week 1st" ~ 1,
                                        TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Second_Half_Session = case_when(Session == "Seven Week - Second" | Session == "Fall 8 Week 2nd" | Session == "Summer 8 Week 2nd" | Session == "Spring 8 Week 2nd" ~ 1,
                                         TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(First_Third_Session = case_when(Session == "Five Week - First" | Session == "Six Week - First" ~ 1,
                                         TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Second_Third_Session = case_when(Session == "Five Week - Second" | Session == "Six Week - Second" ~ 1,
                                          TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Third_Third_Session = case_when(Session == "Five Week - Third" ~ 1,
                                         TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Ten_Week = case_when(Session == "10 Week" ~ 1,
                              TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Thirteen_Week = case_when(Session == "13 Week" ~ 1,
                                   TRUE ~ 0))
deduped_sy21 <- deduped_sy21 %>% 
  mutate(Other = case_when(Session == "Carry-over Course"| Session == "Dynamically Dated Session"| Session == "Pre-session" | Session == "-" ~ 1,
                           TRUE ~ 0))


# now the dup identifiers need to be combined into one row
combined_deduped_sy20 <- deduped_sy20 %>% 
  group_by(Course.Identifier, P.F.Opt, Units)  %>% 
  summarise(across(where(is.numeric), sum))
  
combined_deduped_sy21 <- deduped_sy21 %>% 
  group_by(Course.Identifier, P.F.Opt, Units) %>% 
  summarise(across(where(is.numeric), sum))
 
length(unique(combined_deduped_sy20$Course.Identifier))
length(unique(combined_deduped_sy21$Course.Identifier))

# remove all course with less than 5 enrolled to match dew data
combined_deduped_sy20 <- combined_deduped_sy20 %>% 
  filter(Total.Enroll > 5)
combined_deduped_sy21 <- combined_deduped_sy21 %>% 
  filter(Total.Enroll > 5)

length(unique(dew_data$Course.Identifier))

# merging to see how many stray dogs in each
course_data <- rbind(combined_deduped_sy20, combined_deduped_sy21)

combined_data <- merge(dew_data, course_data, by = "Course.Identifier", all = TRUE)

study_data <- combined_data %>% 
  drop_na()

# now that data is combined remove all graduate level courses as we will only consider undergraduate course 
study_data <- study_data %>% 
  filter(Course.Level != "Graduate")

# create a numeric variable for college
unique(study_data$College)
study_data <- study_data %>% 
  mutate(College_Number = case_when(College == "College of Agric and Life Sci" ~ 1,
                                    College == "Eller College of Management" ~ 2,
                                    College == "College of Applied Sci & Tech" ~ 3,
                                    College == "College of Humanities" ~ 4,
                                    College == "College of Social & Behav Sci" ~ 5,
                                    College == "Graduate College" ~ 6,
                                    College == "College of Engineering" ~ 7,
                                    College == "College of Fine Arts" ~ 8,
                                    College == "College of Science" ~ 9,
                                    College == "College of Public Health" ~ 10,
                                    College == "College of Medicine - Tucson" ~ 11,
                                    College == "College of Education" ~ 12,
                                    College == "Colleges of Letters Arts & Sci" ~ 13,
                                    College == "W.A. Franke Honors College" ~ 14,
                                    College == "James E Rogers College of Law" ~ 15,
                                    College == "College of Nursing" ~ 16,
                                    College == "R Ken Coit College of Pharmacy" ~ 17))



study_data <- rename(study_data, c("Sections" = "Meet.."))

# remove courses that are independent study/directed research style courses
study_data <- study_data %>% filter(Course.Description != "Internship",
                                    Course.Description != "Directed Research",
                                    Course.Description != "Independent Study",
                                    Course.Description != "Senior Capstone",
                                    Course.Description != "Honors Thesis",
                                    Course.Description != "Special Topics",
                                    Course.Description != "Practicum",
                                    Course.Description != "Honors Independent Study", 
                                    Course.Description != "Honors Colloquium",
                                    Course.Description != "Senior Mentoring Workshop", 
                                    Course.Description != "Preceptorship",
                                    Course.Description != "Honors Preceptorship",
                                    Course.Description != "Honors Directed Research")


write.csv(study_data, "data/study_data.csv", row.names = TRUE)

