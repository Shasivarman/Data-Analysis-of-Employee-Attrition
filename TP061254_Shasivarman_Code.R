#TP061254
#Shasivarman A/L Jeyaraman

# IMPORT DATA
library(skimr)
library(readxl)
employee_attrition <- read_excel("C:/Users/jeyar/OneDrive/Desktop/PFDA/employee_attrition.xlsx")
View(employee_attrition)

#Data processing
library(dplyr)
library(readxl)
employee_attrition = employee_attrition %>% 
  group_by(employee_attrition$EmployeeID) %>%     
  filter(age == max(age))

employee_attrition$`employee_attrition$EmployeeID` = NULL
employee_attrition$gender_short = NULL

employee_attrition$AgeGroup <- cut(employee_attrition$age,
                         breaks = c (-Inf
                                     ,19,30,40,50,60,70
                                     , Inf),
                         labels = c("Under19","19-29 years","30-39 years","40-49 years","50-59 years","60-69 years"
                                    , Inf),
                         right = FALSE)

employee_attrition$termreason_desc <- as.factor(employee_attrition$termreason_desc)
count(employee_attrition, termreason_desc)
employee_attrition <- mutate(employee_attrition, termreason_desc = recode(.x=termreason_desc, "Resignaton"="Resignation"))

employee_attrition$city_name <- as.factor(employee_attrition$city_name)
count(employee_attrition, city_name)
employee_attrition <- mutate(employee_attrition, city_name = recode(.x=city_name, "New Westminister"="New Westminster"))

#data exploration
summary(employee_attrition)
summary(employee_attrition$length_of_service)
summary(employee_attrition$age)
summary(employee_attrition$job_title)
summary(employee_attrition$city_name)
skim(employee_attrition)


is.na(employee_attrition)
colSums(is.na(employee_attrition))
which(colSums(is.na(employee_attrition))>0)
names(which(colSums(is.na(employee_attrition))>0))

#DATA VISUALIZATION
# Q1 - Why would staff get terminated?
#Analysis 1 - Relationship of term reason and number
library(ggplot2)
library(dplyr)
library(tidyverse)
library(vtree)




employee_attrition %>% as.data.frame %>%
  group_by(termreason_desc) %>% summarise(N=n()) %>%
  ggplot(aes(x=termreason_desc,y=N,fill=termreason_desc))+
  geom_bar(stat = 'identity',color='black')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  labs(x = 'Term Reason', y = 'Number of Staff', title = 'Relationship of term reason and number of staff')+
  geom_text(aes(label=N),vjust=-0.25,fontface='bold')+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))

#relationship between term reason and Average length of service 
employee_attrition %>% as.data.frame %>%
  group_by(termreason_desc,length_of_service) %>%
  filter(termreason_desc != "Not Applicable") %>%
  ggplot(aes(x=termreason_desc,y=length_of_service,fill=termreason_desc))+
  geom_bar(stat = 'summary',position= "dodge",fun = "mean",color='black')+
  labs(x = 'Term Reason', y = 'Length of service', title = 'Relationship of term reason and Length of service')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))

#Relationship between department name and number of people
employee_attrition %>% as.data.frame %>%
  group_by(termreason_desc,department_name) %>%summarise(N=n()) %>%
  filter(termreason_desc != "Not Applicable") %>%
  ggplot(aes(x=termreason_desc,y=N,fill=department_name))+
  geom_bar(stat = 'identity',color='black')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  labs(x = 'Term Reason', y = 'Number of staff', title = '- Relationship between department name and termination reason and number of staff getting    terminated ')+
  facet_wrap(department_name~.)+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))

#Relationship between department name and age 
employee_attrition %>% as.data.frame %>%
  group_by(termreason_desc,department_name,age) %>%
  filter(department_name != "Accounting") %>%
  filter(department_name != "Accounts Payable") %>%
  filter(department_name != "Accounts Receiveable") %>%
  filter(department_name != "Audit") %>%
  filter(department_name != "Compensation") %>%
  filter(department_name != "Customer Service") %>%
  filter(department_name != "Employee Records") %>%
  filter(department_name != "HR Technology") %>%
  filter(department_name != "Information Technology") %>%
  filter(department_name != "Investment") %>%
  filter(department_name != "Labor Relations") %>%
  filter(department_name != "Legal") %>%
  filter(department_name != "Processed Foods") %>%
  filter(department_name != "Recruitment") %>%
  filter(department_name != "Store Management") %>%
  filter(department_name != "Training") %>%
  filter(termreason_desc == "Retirement") %>%
  ggplot(aes(x=termreason_desc,y=age,fill=department_name))+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  geom_bar(stat = 'summary',position= "dodge",fun = "mean",color='black')+
  labs(x = 'Term Reason', y = 'Age', title = 'Relationship between department name and age and termination reason')+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))


#Q2 - Why staff with short length of service are getting terminated?

#Relationship between the Status and the business unit and number of people in each bussiness unit
employee_attrition %>% as.data.frame %>%
  group_by(STATUS,BUSINESS_UNIT) %>% summarise(N=n()) %>%
  filter(STATUS != "ACTIVE") %>%
  ggplot(aes(x=BUSINESS_UNIT,y=N,fill=STATUS))+
  geom_bar(stat = 'identity',color='black')+
  geom_text(aes(label=N),vjust=-0.25,fontface='bold')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  labs(x = 'Business Unit', y = 'Number of staff', title = 'Relationship between the Status and the business unit and number of people in each bussiness unit')+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))

#Relationship between Average length of service and age with status
  employee_attrition %>% as.data.frame %>%
    group_by(length_of_service,age,STATUS) %>% summarise(length_of_service = mean(length_of_service)) %>%
    filter(STATUS != "ACTIVE") %>%
    ggplot(aes(x = age, y = length_of_service, color = STATUS))+
    geom_point()+
    labs(x = 'Age', y = 'Average Length of service', title = 'Relationship between Age and Average Length of Service')+
    geom_smooth(method = 'lm',se = FALSE)+
    theme_bw()+
    theme(panel.border = element_rect(color = "black",
                                      fill = NA,
                                      size = 5))

#Relationship between job title and Average length of service
employee_attrition %>% as.data.frame %>%
  group_by(length_of_service,job_title,STATUS) %>%
  filter( STATUS!= "ACTIVE") %>%
  ggplot(aes(x=STATUS,y=length_of_service,fill=job_title))+
  geom_bar(stat = 'summary',position= "dodge",fun = "mean",color='black')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  labs(x = 'Status', y = 'Average Length of service', title = 'Relationship between job title and average length of service')+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))

#Relationship between the Termination type and job title and Term reason and Number of staff
employee_attrition %>% as.data.frame %>%
  group_by(termtype_desc,job_title,termreason_desc) %>%summarise(N=n()) %>%
  filter(job_title %in% c("Baker","Cashier","Dairy Person","Meat Cutter","Produce Clerk","Shelf Stocker")) %>%
  filter( termreason_desc != "Not Applicable") %>%
  filter(termreason_desc != "Retirement") %>%
  ggplot(aes(x = termreason_desc,y=N,fill=termtype_desc))+
  geom_bar(stat = 'summary',position= "dodge",fun = "mean",color='black')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  labs(x = 'Term reason', y = 'Number Of staff', title = 'Relationship between the Termination type and job title and Term reason and Number of staff')+
  facet_wrap(~job_title,scales="free")+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))

#Relationship between the Job title and the Age group of people working in that particular job and termination reason Resignation
employee_attrition %>% as.data.frame %>%
  group_by(termreason_desc,job_title,AgeGroup) %>%summarise(N=n()) %>%
  filter(job_title %in% c("Baker","Cashier","Dairy Person","Meat Cutter","Produce Clerk","Shelf Stocker")) %>%
  filter(AgeGroup != "60-69 years")%>%
  filter(termreason_desc == "Resignation") %>%
  ggplot(aes(x = termreason_desc,y=N,fill=job_title))+
  geom_bar(stat = 'summary',position= "dodge",fun = "mean",color='black')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  labs(x = 'Termination Reason (Resignation)', y = 'Number of Staff', title = 'Relationship between the Job title and the Age group of people working in that particular job and termination reason Resignation')+
  facet_wrap(~AgeGroup,scales="free")+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))

#Relationship between the Job title and the Age group of people working in that particular job and termination reason Layoff
employee_attrition %>% as.data.frame %>%
  group_by(termreason_desc,job_title,AgeGroup) %>%summarise(N=n()) %>%
  filter(job_title %in% c("Baker","Cashier","Dairy Person","Meat Cutter","Produce Clerk","Shelf Stocker")) %>%
  filter(AgeGroup != "60-69 years")%>%
  filter(termreason_desc == "Layoff") %>%
  ggplot(aes(x = termreason_desc,y=N,fill=job_title))+
  geom_bar(stat = 'summary',position= "dodge",fun = "mean",color='black')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  labs(x = 'Termination Reason (Layoff)', y = 'Number of Staff', title = 'Relationship between the Job title and the Age group of people working in that particular job and termination reason Layoff')+
  facet_wrap(~AgeGroup,scales="free")+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))

# Q3 - Which gender gets terminated more and why the particular gender is getting terminated 
#Relationship between gender and the reason of the termination and Number of staff
employee_attrition %>% as.data.frame %>%
  group_by(gender_full,termreason_desc) %>% summarise(N=n()) %>%
  filter(termreason_desc != "Not Applicable") %>%
  ggplot(aes(x=termreason_desc,y=N,fill=gender_full))+
  geom_bar(stat = 'identity',color='black')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  labs(x = 'Term Reason', y = 'Number of Staff', title = 'Relationship between gender and the reason of the termination and Number of staff')+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))

#Relationship between gender(female) and Age Group with  Term Reason(except retirement)
vtree(employee_attrition,"gender_full AgeGroup termreason_desc",horiz = FALSE)
vtree(employee_attrition,"gender_full AgeGroup termreason_desc",prune = list(termreason_desc = "Not Applicable",AgeGroup = "60-69 years",gender_full = "Male"))


#Relationship between gender female and age group and term reason (resignation) and city_name (top 5 cities)
employee_attrition %>% as.data.frame %>%
  group_by(gender_full,city_name,termreason_desc,AgeGroup) %>% summarise(N=n()) %>%
  filter(gender_full == 'Female')%>%
  filter(termreason_desc == "Resignation") %>%
  filter(AgeGroup != "60-69 years")%>%
  filter(city_name %in% c("Vancouver","Victoria","Prince George","Kelowna","New Westminster"))%>%
  ggplot(aes(x=AgeGroup,y=N,fill=termreason_desc))+
  geom_bar(stat = 'identity',color='black')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  labs(x = 'Term Reason', y = 'Number of Staff', title = 'Relationship between gender female and age group and term reason (resignation) and city_name (top 5 cities)')+
  facet_wrap(city_name~.)+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))

# Data manipulation
employee_attrition %>%
  group_by(gender_full,city_name,STATUS) %>%
  filter(gender_full == "Female",termreason_desc == "Resignation")%>%
  summarize(Freq=n())%>%
  arrange(desc(Freq))%>%
  print(n = 5)  


#Relationship between gender(male) and Age Group with  Term Reason(except retirement)
vtree(employee_attrition,"gender_full AgeGroup termreason_desc",horiz = FALSE)
vtree(employee_attrition,"gender_full AgeGroup termreason_desc",prune = list(termreason_desc = "Not Applicable",AgeGroup = "60-69 years",gender_full = "Female"))


#Relationship between male and age group and term reason (resignation) and city name (top 5 cities)
employee_attrition %>% as.data.frame %>%
  group_by(gender_full,city_name,termreason_desc,AgeGroup) %>% summarise(N=n()) %>%
  filter(gender_full == 'Male')%>%
  filter(termreason_desc == "Resignation") %>%
  filter(city_name %in% c("Vancouver","Victoria","Nanaimo","New Westminster","Kamloops"))%>%
  ggplot(aes(x=AgeGroup,y=N,fill=termreason_desc))+
  geom_bar(stat = 'identity',color='black')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  labs(x = 'Age Group', y = 'Number of Staff', title = 'Relationship between gender female and age group and term reason (resignation) and city_name (top 5 cities)')+
  facet_wrap(city_name~.)+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))


# Data manipulation
employee_attrition %>%
  group_by(gender_full,city_name,STATUS) %>%
  filter(gender_full == "Male",termreason_desc == "Resignation")%>%
  summarize(Freq=n())%>%
  arrange(desc(Freq))%>%
  print(n = 5)  



#Q4 - Which Cities has most number of people terminated and why is that?
#Relationship between city name and the number of people in the city
employee_attrition %>% as.data.frame %>% 
  group_by(city_name,STATUS) %>%  summarise(N=n()) %>%
  filter( STATUS == "TERMINATED")%>%
  filter(city_name %in% c("Vancouver","Victoria","New Westminster","Kelowna","Nanaimo")) %>%
  ggplot(aes(x=city_name,y=N,fill=STATUS))+
  geom_bar(stat = 'identity',color='black')+
  geom_text(aes(label=N),vjust=-0.25,fontface='bold')+
  labs(x = 'City Name', y = 'Number of Staff', title = 'Relationship between Number of staff and City name')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))

employee_attrition %>%
  group_by(city_name) %>%
  summarize(Freq=n())%>%
  arrange(desc(Freq))%>%
  print(n = 5)  

# Relationship between the reason of termination and city name and number of staff

employee_attrition %>% as.data.frame %>%
  group_by(city_name, termreason_desc) %>% summarise(N=n()) %>%
  filter(termreason_desc %in% c("Layoff","Resignation"))%>%
  filter(city_name %in% c("Vancouver","Victoria","New Westminster","Kelowna","Nanaimo")) %>%
  ggplot(aes(x=termreason_desc,y=N,fill = termreason_desc))+
  geom_bar(stat = 'identity',color='black')+
  labs(x = 'Terminattion Reason', y = 'Number of Staff', title = 'Relationship between Number of staff and City name and termination reason')+
  stat_summary(fun = "mean", geom = "point", shape = 8,
              size = 2, color = "black")+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  facet_wrap(~city_name,scales="free")+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))

#Data manipulation
employee_attrition %>%
  group_by(city_name,termreason_desc) %>%
  filter(termreason_desc %in% c("Layoff","Resignation"))%>%
  summarize(Freq=n())%>%
  arrange(desc(Freq))%>%
  print(n = 10)  

#Relationship between city_name and job title and term reason resignation

employee_attrition %>% as.data.frame %>%
  group_by(city_name, termreason_desc,job_title) %>% summarise(N=n()) %>%
  filter(termreason_desc == "Resignation")%>%
  filter(city_name %in% c("Vancouver","Victoria","New Westminster","Kelowna","Nanaimo")) %>%
  ggplot(aes(x=termreason_desc,y=N,fill =city_name))+
  geom_bar(stat = 'identity',color='black')+
  labs(x = 'Termination Reason', y = 'Number of Staff', title = 'Relationship between city_name and job title and term reason resignation')+
  scale_y_continuous(labels = scales::comma_format(accuracy = 2))+
  facet_wrap(~job_title,scales="free")+
  theme_bw()+
  theme(axis.text = element_text(color='black',face='bold'),
        axis.title = element_text(color='black',face='bold'),
        legend.text = element_text(color='black',face='bold'),
        legend.title = element_text(color='black',face='bold'))
     




 



