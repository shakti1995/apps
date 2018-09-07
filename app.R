library(shiny)
library(dplyr)
library(DT)
library(DBI)
library(ggplot2)
library(rredis)
library(shinythemes)
library(RMySQL)
library(lubridate)
library(dbConnect)
library(RPostgreSQL)
library(reshape)
library(shinyjs)
library(as_tibble)


    
    drv <- dbDriver("PostgreSQL")
    con <- dbConnect(drv, dbname = 'credi_health_production',
                     host='credi-health-prod-db-read.cvhedgajwcqg.ap-south-1.rds.amazonaws.com',
                     user='root', password='credi_prod_db'
    )
    # query <- paste("Select id, created_at, updated_at, app_type, appointment_status_id from appointments")
    # appt2 <- dbGetQuery(con, query)
    doctors <- dbGetQuery(con, "Select id as doctor_id, designation, firstname as doctor_name from doctors")
    doctors_table <- doctors
    doctors_table$designation <- NULL
    
    operationalCities <- dbGetQuery(con, "Select id as city_id, title as city_name from operational_cities")
    
    hospitals <- dbGetQuery(con, "Select id as healthcare_centre_id, name as hospital_name from healthcare_centres")
    
    speciality <- read.csv("speciality.csv")
    names(speciality) <- c("speciality_id", "Spec","speciality")
    speciality$Spec <- NULL
    speciality$speciality <- as.character(speciality$speciality)
    
    admin_hospital_group_relations <- dbGetQuery(con, "Select id as admin_hospital_group_relations_id, healthcare_centre_id,
                                                 admin_hospital_group_id from admin_hospital_group_relations")
    
    prime_membership <- dbGetQuery(con, "Select user_id, prime_membership_id, promo_code_id, status from prime_memberships_users")
    
    patients <- dbGetQuery(con, "Select id as patient_id, phone_no as code from patients")
    
    address <- dbGetQuery(con, "select area, addressible_id as healthcare_centre_id from addresses where addressible_type = 'HealthcareCentre' ")
    code <- dbGetQuery(con,"select name,code from country_Codes")
    colnames(code)<-c("country","code")
    #cities
    cities <- c()
    cities <- unique(operationalCities$city_name)
    
    
    ui <-  fluidPage(
        
        titlePanel(h3("Appointment Analysis Dashboard", align = "center")),
        sidebarLayout(
            
            #Input Variables
            sidebarPanel( width = 4,
                          useShinyjs(),
                          br(),
                          dateRangeInput(inputId = "dateRange", label = strong("Enter the Date range :"),
                                         start = Sys.Date() - 6, end = Sys.Date()
                                         
                          ),
                          checkboxGroupInput(inputId = "userLocality", label = strong("Select a Category :"),
                                             choices = c("Domestic","International"), 
                                             selected = c("Domestic","International")),
                          
                          checkboxGroupInput(inputId = "sourceChannel", label = strong("Select a User Category :"), 
                                             choices = c("Internally_Created", "Not Internally_Created"),
                                             selected = c("Internally_Created", "Not Internally_Created")),
                          
                          checkboxGroupInput(inputId = "userCategory", label = strong("Select the User Category :"),
                                             choices = c("Credi+", "Non_Credi+"), 
                                             selected = c("Credi+", "Non_Credi+")),
                          
                          selectInput(inputId = "dataChoices", label = strong("Select Basis of Data Representation :"),
                                      choices = c("Date Wise" = "date_wise", "City Wise"="city_wise", "Hospital Wise"="hospital_wise",
                                                  "Doctor Wise" = "doctor_wise", "Speciality Wise" = "speciality_wise",
                                                  "Hospital Network Wise" = "hospital_network", "City + Hospital Wise"= "city_hospital_wise",
                                                  "Appt Type Wise" = "appt_type_wise")),
                          br(),
                          
                          uiOutput("filter_box"),
                          # selectInput(inputId = "City_wise", label = strong("Select City :"),
                          #             choices = c(cities), selected = "", multiple = T),
                          # selectInput(inputId = "Hospital_wise", label = strong("Select Hospital :"),
                          #             choices = c(unique(hospitals$hospital_name)), selectize = T, selected = "", multiple = T),
                          # selectInput(inputId = "Speciality_wise", label = strong("Select Speciality :"),
                          #             choices = c(unique(speciality$speciality)), selectize = T, selected = "", multiple = T),
                          # selectInput(inputId = "Doctor_wise",label = strong("Select Doctors : "),
                          #             choices = c(unique(doctors$doctor_name)), selectize = T,selected = "", multiple = T),
                          br(),
                          submitButton(),
                          br(),
                          actionButton(inputId = "Submit", label = "Apply Changes")
                          
            ),
            
            #Output 
            mainPanel(
                tabsetPanel(type = "tabs", id = "tabs",
                            tabPanel(title = "Date Wise",
                                     br(),
                                     dataTableOutput(outputId = "dateWiseTable")
                                     
                            ),
                            tabPanel(title = "City Wise", br(),
                                     dataTableOutput(outputId = "cityWise")
                                     
                            ),
                            tabPanel(title = "Country Wise", br(),
                                     dataTableOutput(outputId = "countryWise")
                                     
                            ),
                            tabPanel(title = "Doctor Wise", br(),
                                     dataTableOutput(outputId = "doctorWise")
                                     
                            ),
                            tabPanel(title = "Hospital Wise", br(),
                                     dataTableOutput(outputId = "hospitalWise")
                                     
                            ),
                            tabPanel(title = "Speciality Wise", br(),
                                     dataTableOutput(outputId = "specialityWise")
                                     
                            ),
                            tabPanel(title = "OPD Wise", br(),
                                     dataTableOutput(outputId = "OPDWise")
                                     
                            ),
                            tabPanel(title = "Test Panel", br(),
                                     dataTableOutput(outputId = "testPanel"))
                            
                )   
            )
        )
    )
    
    server <- function(input,output){
        
        ######################  JOIN  #######################
        join_function <- function(q_out){
            q_out <- left_join(q_out, operationalCities, by = "city_id")
            q_out <- left_join(q_out, doctors_table, by = "doctor_id")
            q_out <- left_join(q_out, hospitals, by = "healthcare_centre_id")
            q_out <- left_join(q_out, speciality, by = "speciality_id")
            q_out <- left_join(q_out, admin_hospital_group_relations, by  = "healthcare_centre_id")
            q_out <- left_join(q_out, prime_membership, by = "user_id")
            q_out <- left_join(q_out, patients, by = "patient_id")
            q_out <- left_join(q_out, address, by = "healthcare_centre_id")
        }
        # ann <- function(df,inp){
        #   if(input$tabs == "Date Wise"){
        #     if(inp == "city_wise") {
        #       data <- df %>% filter(city_name %in% c(input$City_wise))
        #     }else if(inp == "hospital_wise"){
        #       data <- df %>% filter(hospital_name %in% c(input$Hospital_wise))
        #     }else if(inp == "speciality_wise"){
        #       data <- df %>% filter(speciality %in% c(input$Speciality_wise))
        #     }else if(inp == "doctor_wise"){
        #       data <- df %>% filter(doctor_name %in% c(input$Doctor_wise))
        #     }else {
        #       data <- df
        #     } 
        #   }else if(input$tabs == "City Wise"){
        #     if(inp == "date_wise") {
        #       data <- df %>% filter(Date %in% c(input$Date_wise))
        #     }else if(inp == "hospital_wise"){
        #       data <- df %>% filter(hospital_name %in% c(input$Hospital_wise))
        #     }else if(inp == "speciality_wise"){
        #       data <- df %>% filter(speciality %in% c(input$Speciality_wise))
        #     }else if(inp == "doctor_wise"){
        #       data <- df %>% filter(doctor_name %in% c(input$Doctor_wise))
        #     }else{
        #       data <- df
        #     }
        #   }else if(input$tabs == "Doctor Wise"){
        #     if(inp == "city_wise") {
        #       data <- df %>% filter(city_name %in% c(input$City_wise))
        #     }else if(inp == "hospital_wise"){
        #       data <- df %>% filter(hospital_name %in% c(input$Hospital_wise))
        #     }else if(inp == "speciality_wise"){
        #       data <- df %>% filter(speciality %in% c(input$Speciality_wise))
        #     }else if(inp == "doctor_wise"){
        #       data <- df %>% filter(doctor_name %in% c(input$Doctor_wise))
        #     }else if(inp == "date_wise") {
        #       data <- df %>% filter(Date %in% c(input$Date_wise))
        #     }else{
        #       data <- df
        #     }
        #   }else if(input$tabs == "Country Wise"){
        #     if(inp == "city_wise") {
        #       data <- df %>% filter(city_name %in% c(input$City_wise))
        #     }else if(inp == "hospital_wise"){
        #       data <- df %>% filter(hospital_name %in% c(input$Hospital_wise))
        #     }else if(inp == "speciality_wise"){
        #       data <- df %>% filter(speciality %in% c(input$Speciality_wise))
        #     }else if(inp == "doctor_wise"){
        #       data <- df %>% filter(doctor_name %in% c(input$Doctor_wise))
        #     }else if(inp == "date_wise") {
        #       data <- df %>% filter(Date %in% c(input$Date_wise))
        #     }else{
        #       data <- df
        #     }
        #   }else if(input$tabs == "Hospital Wise"){
        #     if(inp == "city_wise") {
        #       data <- df %>% filter(city_name %in% c(input$City_wise))
        #     }else if(inp == "hospital_wise"){
        #       data <- df %>% filter(hospital_name %in% c(input$Hospital_wise))
        #     }else if(inp == "speciality_wise"){
        #       data <- df %>% filter(speciality %in% c(input$Speciality_wise))
        #     }else if(inp == "doctor_wise"){
        #       data <- df %>% filter(doctor_name %in% c(input$Doctor_wise))
        #     }else if(inp == "date_wise") {
        #       data <- df %>% filter(Date %in% c(input$Date_wise))
        #     }else{
        #       data <- df
        #     }
        #   }else if(input$tabs == "Speciality Wise"){
        #     if(inp == "city_wise") {
        #       data <- df %>% filter(city_name %in% c(input$City_wise))
        #     }else if(inp == "hospital_wise"){
        #       data <- df %>% filter(hospital_name %in% c(input$Hospital_wise))
        #     }else if(inp == "speciality_wise"){
        #       data <- df %>% filter(speciality %in% c(input$Speciality_wise))
        #     }else if(inp == "doctor_wise"){
        #       data <- df %>% filter(doctor_name %in% c(input$Doctor_wise))
        #     }else if(inp == "date_wise") {
        #       data <- df %>% filter(Date %in% c(input$Date_wise))
        #     }else{
        #       data <- df
        #     }
        #   }
        #   else if(input$tabs == "OPD Wise"){
        #     if(inp == "city_wise") {
        #       data <- df %>% filter(city_name %in% c(input$City_wise))
        #     }else if(inp == "hospital_wise"){
        #       data <- df %>% filter(hospital_name %in% c(input$Hospital_wise))
        #     }else if(inp == "speciality_wise"){
        #       data <- df %>% filter(speciality %in% c(input$Speciality_wise))
        #     }else if(inp == "doctor_wise"){
        #       data <- df %>% filter(doctor_name %in% c(input$Doctor_wise))
        #     }else if(inp == "date_wise") {
        #       data <- df %>% filter(Date %in% c(input$Date_wise))
        #     }else{
        #       data <- df
        #     }
        #   }
        #   
        # }
        
        bookingDB <- reactive({
            
            date1 <- as.Date(input$dateRange[1])
            date2 <- as.Date(input$dateRange[2])
            query <- sprintf("Select id as appt_id, created_at at TIME ZONE 'UTC' as booking_date, is_contracted as contract, app_type, 
                             appointment_status_id, operational_city_id as city_id, patient_country_type, 
                             source_channel, speciality_id, track_promo_code_id as member, patient_id,
                             healthcare_centre_id , doctor_id, user_id from appointments
                             where date(created_at at time zone 'utc+5:30') between '%s' and '%s' ", date1, date2)
            bookingDB <- dbGetQuery(con, query)
        })
        
        fetch_booking <- reactive({
            q_out <- bookingDB()
            
            #JOIN
            q_out <- join_function(q_out)
            
            q_out$city_id <- NULL
            q_out$doctor_id <- NULL
            q_out$speciality_id <- NULL
            
            #################################
            
            q_out$code<-sub("-.*", "", q_out$code)
            
            q_out<-q_out %>% left_join(code,by='code')
            q_out$code<-NULL
            q_out$country[is.na(q_out$country)] <- "Others"
            
            q_out<-q_out %>% filter(contract==TRUE | is.na(contract))
            q_out$contract<-NULL
            q_out<-as_tibble(q_out)
            
            q_out$source_channel<-ifelse(q_out$app_type=='request_callback' & q_out$source_channel=='internally_created',"portal",q_out$source_channel)
            
            q_out$source_channel<-ifelse(q_out$source_channel == "internally_created","Internally_Created","Not Internally_Created")
            q_out$source_channel[is.na(q_out$source_channel)] <- "Not Internally_Created"
            
            q_out$speciality[which(is.na(q_out$speciality))] <- "Others"
            
            q_out$member <- ifelse(!is.na(q_out$member),'Credi+',"Non_Credi+")
            
            q_out$patient_country_type <- ifelse(q_out$patient_country_type==0,"Domestic","International")
            
            q_out$hospital_name <- paste(q_out$hospital_name, q_out$area, sep=", ")
            
            q_out$doctor_name[which(is.na(q_out$doctor_name))] <- "Others"
            
            q_out$hospital_name<-ifelse(q_out$hospital_name == "NA, NA","",q_out$hospital_name)
            q_out$hospital_name[which(q_out$hospital_name == "")]<-"Request Callback"
            q_out$hospital_name[which(is.na(q_out$hospital_name))]<-"Request Callback"
            q_out<-q_out[!duplicated(q_out$appt_id),]
            q_out <- select(q_out,appt_id, booking_date, app_type, appointment_status_id, patient_country_type,city_name,hospital_name,
                            doctor_name,speciality,member,source_channel,country)  %>% mutate(Date = as.Date(as.POSIXct(q_out$booking_date)))
            
            # q_out <- ann(q_out,input$dataChoices)
            
            q_out <- as_tibble(q_out)
        })
        
        fetch_appointment <- reactive({
            date1 <- as.Date(input$dateRange[1])
            date2 <- as.Date(input$dateRange[2])
            
            query <- sprintf("Select id as appt_id, appt_at at TIME ZONE 'UTC' as appointment_date, is_contracted as contract, app_type, 
                             appointment_status_id, operational_city_id as city_id, patient_country_type, 
                             source_channel, speciality_id, track_promo_code_id as member, patient_id,
                             healthcare_centre_id , doctor_id, user_id from appointments
                             where date(appt_at at time zone 'utc+5:30') between '%s' and '%s' ", date1, date2)
            
            appointmentDB <- dbGetQuery(con, query)
            q_out <- appointmentDB
            
            #JOIN
            q_out <- join_function(q_out)
            
            q_out$city_id <- NULL
            q_out$doctor_id <- NULL
            q_out$healthcare_centre_id <- NULL
            q_out$speciality_id <- NULL
            
            ######################################
            
            q_out$code<-sub("-.*", "", q_out$code)
            
            # code<-dbGetQuery(con,"select name,code from country_Codes")
            # colnames(code)<-c("country","code")
            
            q_out<-q_out %>% left_join(code,by='code')
            q_out$code<-NULL
            q_out$country[is.na(q_out$country)] <- "Others"
            
            q_out<-q_out %>% filter(contract==TRUE | is.na(contract))
            q_out$contract<-NULL
            q_out<-as_tibble(q_out)
            
            q_out$source_channel<-ifelse(q_out$app_type=='request_callback' & q_out$source_channel=='internally_created',"portal",q_out$source_channel)
            
            q_out$source_channel<-ifelse(q_out$source_channel == "internally_created","Internally_Created","Not Internally_Created")
            q_out$source_channel[is.na(q_out$source_channel)] <- "Not Internally_Created"
            
            q_out$speciality[which(is.na(q_out$speciality))] <- "Others"
            
            q_out$hospital_name <- paste(q_out$hospital_name, q_out$area, sep=", ")
            
            q_out$member <- ifelse(!is.na(q_out$member),'Credi+',"Non_Credi+")
            
            q_out$patient_country_type <- ifelse(q_out$patient_country_type==0,"Domestic","International")
            
            q_out$doctor_name[which(is.na(q_out$doctor_name))] <- "Others"
            
            q_out$hospital_name<-ifelse(q_out$hospital_name == "NA, NA","",q_out$hospital_name)
            q_out$hospital_name[which(q_out$hospital_name == "")]<-"Request Callback"
            q_out$hospital_name[which(is.na(q_out$hospital_name))]<-"Request Callback"
            q_out<-q_out[!duplicated(q_out$appt_id),]
            q_out <- select(q_out,appt_id,  appointment_date, app_type, appointment_status_id, patient_country_type,city_name,hospital_name,
                            doctor_name,speciality,member,source_channel, country) %>% mutate(Date = as.Date(as.POSIXct(q_out$appointment_date)))
            
            # q_out <- ann(q_out,input$dataChoices)
            q_out <- as_tibble(q_out)
            # 
        })
        
        
        fetch_admission <- reactive({
            date1 <- as.Date(input$dateRange[1])  
            date2 <- as.Date(input$dateRange[2])
            
            query <- sprintf("Select id as appt_id, ipd_admission_date  as admission_date, is_contracted as contract, app_type, 
                             appointment_status_id, operational_city_id as city_id, patient_country_type, 
                             source_channel, speciality_id, track_promo_code_id as member, patient_id,
                             healthcare_centre_id , doctor_id, user_id from appointments
                             where date(ipd_admission_date at time zone 'utc') between '%s' and '%s' ", date1, date2)
            admissionDB <- dbGetQuery(con, query)
            q_out <- admissionDB
            
            #JOIN
            q_out <- join_function(q_out)
            
            q_out$city_id <- NULL
            q_out$doctor_id <- NULL
            q_out$healthcare_centre_id <- NULL
            q_out$speciality_id <- NULL
            
            #################################
            
            q_out$code<-sub("-.*", "", q_out$code)
            
            # code<-dbGetQuery(con,"select name,code from country_Codes")
            # colnames(code)<-c("country","code")
            
            q_out<-q_out %>% left_join(code,by='code')
            q_out$code<-NULL
            q_out$country[is.na(q_out$country)] <- "Others"
            
            q_out<-q_out %>% filter(contract==TRUE | is.na(contract))
            q_out$contract<-NULL
            q_out<-as_tibble(q_out)
            
            q_out$source_channel<-ifelse(q_out$app_type=='request_callback' & q_out$source_channel=='internally_created',"portal",q_out$source_channel)
            
            q_out$source_channel<-ifelse(q_out$source_channel == "internally_created","Internally_Created","Not Internally_Created")
            q_out$source_channel[is.na(q_out$source_channel)] <- "Not Internally_Created"
            
            q_out$speciality[which(is.na(q_out$speciality))] <- "Others"
            
            q_out$hospital_name <- paste(q_out$hospital_name, q_out$area, sep=", ")
            
            q_out$member <- ifelse(!is.na(q_out$member),'Credi+',"Non_Credi+")
            
            q_out$patient_country_type <- ifelse(q_out$patient_country_type==0,"Domestic","International")
            
            q_out$doctor_name[which(is.na(q_out$doctor_name))] <- "Others"
            
            q_out$hospital_name<-ifelse(q_out$hospital_name == "NA, NA","",q_out$hospital_name)
            q_out$hospital_name[which(q_out$hospital_name == "")]<-"Request Callback"
            q_out$hospital_name[which(is.na(q_out$hospital_name))]<-"Request Callback"
            q_out<-q_out[!duplicated(q_out$appt_id),]
            q_out <- select(q_out,appt_id, admission_date, app_type, appointment_status_id, patient_country_type,city_name,hospital_name,
                            doctor_name,speciality,member,source_channel, country) %>% mutate(Date = as.Date(as.POSIXct(q_out$admission_date)))
            
            # q_out <- ann(q_out,input$dataChoices) 
            q_out <- as_tibble(q_out)
        })
        
        ######################################### Date WISE ############################################
        requestData1_date <- reactive({
            requestData2 <- fetch_appointment() %>% filter(source_channel %in% c(input$sourceChannel) & !app_type %in% c('request_callback','quote_request')
                                                           & appointment_status_id != 9 & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory)
            ) %>% group_by(Date) %>% summarise(Request=n())
            colnames(requestData2)<-c('Date','Request')
            requestData2
        })
        requestData2_date <- reactive({
            requestData1 <- fetch_booking() %>% filter(source_channel %in% c(input$sourceChannel) & app_type %in% c('request_callback','quote_request')
                                                       & appointment_status_id != 9 & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory)
            ) %>% group_by(Date) %>% summarise(Request=n())
            colnames(requestData1)<-c('Date','Request')
            requestData1
        })
        bookingData_date <- reactive({
            bookingData <- fetch_appointment() %>%  
                filter( source_channel %in% c(input$sourceChannel) & appointment_status_id %in% c(2,8,6,7,10,11,12,13) & app_type %in% c('opd', 'opd_walkin', 'probable_ipd') & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory))%>% #& patient_country_type %in% c(input$userLocality) & source_channel %in% c(input$sourceChannel) & member %in% c(input$userCategory)) %>% 
                select( Date , app_type) %>% group_by(Date) %>% summarize(Booking = n())
            bookingData <- bookingData[!duplicated(bookingData),]
            bookingData
        })
        opdData_date <- reactive({
            opdData <- fetch_appointment() %>%  
                filter( source_channel %in% c(input$sourceChannel) & appointment_status_id == 8 & app_type %in% c('opd','opd_walkin','probable_ipd') 
                        & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) & source_channel %in% c(input$sourceChannel))  %>% #& patient_country_type %in% c(input$userLocality) & source_channel %in% c(input$sourceChannel) & member %in% c(input$userCategory)) %>%
                select( Date, app_type) %>% group_by(Date) %>% summarize(OPD = n())
            opdData <- opdData[!duplicated(opdData),]
            opdData
        })
        dayCareData_date <- reactive({
            dayCareData <- fetch_appointment() %>%  
                filter(source_channel %in% c(input$sourceChannel) & appointment_status_id == 8, app_type == "day_care" & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) ) %>%
                select( Date, app_type) %>% group_by(Date) %>% summarize(DayCare = n())
            dayCareData <- dayCareData[!duplicated(dayCareData),]
            dayCareData
        })
        ipdData_date <- reactive({
            ipdData <- fetch_admission() %>%  
                filter(source_channel %in% c(input$sourceChannel) & appointment_status_id == 8, app_type == "ipd" & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) ) %>%
                select( Date, app_type) %>% group_by(Date) %>% summarize(IPD = n())
            ipdData <- ipdData[!duplicated(ipdData),]
            ipdData
        })
        
        
        fetchDate <- reactive({
            
            requestData<-left_join(requestData2_date(),requestData1_date(),by='Date',all = TRUE)
            requestData<-transform(requestData,Request = rowSums(requestData[, 2:3], na.rm = TRUE))
            requestData<-requestData[,c(1,4)]
            
            ############## Merge Data ##############
            nData <- left_join(requestData,bookingData_date(), by = c('Date'), all = TRUE)
            data1 <- left_join(nData,opdData_date(), by = c('Date'), all = TRUE)
            data1 <- left_join(data1, dayCareData_date(), by = c('Date'), all = TRUE)
            data1 <- left_join(data1,ipdData_date(), by = c('Date'), all = TRUE)
            
            ############# Mutate Data ##############
            data1 <- mutate(data1, Day = weekdays(Date)) %>% select(Date, Day, everything()) 
            data1 <- data1 %>% mutate( "booking%" = round((Booking/Request)*100,2), "opd%"= round((OPD/Booking)*100,2), "dayCare%" = round((DayCare/OPD)*100,2) , "ipd%" = round((IPD/OPD)*100,2) )
            row <- nrow(data1)
            data1 <- data1[2:row, ]
        })
        
        
        ########################################### CITY WISE #############################################
        requestData1_city <- reactive({
            requestData1 <- fetch_booking() %>% filter(source_channel %in% c(input$sourceChannel) & app_type %in% c('request_callback','quote_request')
                                                       & appointment_status_id != 9 & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory)
            ) %>% group_by(city_name) %>% summarise(Request=n())
            colnames(requestData1)<-c('city_name','Request')
            requestData1
        })
        requestData2_city <- reactive({
            requestData2 <- fetch_appointment() %>% filter(source_channel %in% c(input$sourceChannel) & !app_type %in% c('request_callback','quote_request')
                                                           & appointment_status_id != 9 & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory)
            ) %>% group_by(city_name) %>% summarise(Request=n())
            colnames(requestData2)<-c('city_name','Request')
            requestData2
        })
        bookingData_city <- reactive({
            bookingData <- fetch_appointment() %>%  
                filter( source_channel %in% c(input$sourceChannel) & patient_country_type %in% c(input$userLocality) &  member %in% c(input$userCategory) & appointment_status_id %in% c(2,8,6,7,10,11,12,13), app_type %in% c('opd', 'opd_walkin', 'probable_ipd') ) %>%
                select( city_name, app_type) %>% group_by(city_name) %>% summarize(Booking = n())
            bookingData <- bookingData[!duplicated(bookingData),]
            bookingData
        })
        opdData_city <- reactive({
            opdData <- fetch_appointment() %>%  
                filter( source_channel %in% c(input$sourceChannel) & appointment_status_id == 8 & app_type %in% c('opd','opd_walkin','probable_ipd') 
                        & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) & source_channel %in% c(input$sourceChannel))  %>% #& patient_country_type %in% c(input$userLocality) & source_channel %in% c(input$sourceChannel) & member %in% c(input$userCategory)) %>%
                select( city_name, app_type) %>% group_by(city_name) %>% summarize(OPD = n())
            opdData <- opdData[!duplicated(opdData),]
            opdData
        })
        dayCareData_city <- reactive({
            dayCareData <- fetch_appointment() %>%  
                filter(source_channel %in% c(input$sourceChannel) & appointment_status_id == 8, app_type == "day_care" & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) ) %>%
                select( city_name, app_type) %>% group_by(city_name) %>% summarize(DayCare = n())
            dayCareData <- dayCareData[!duplicated(dayCareData),]
            dayCareData
        })
        ipdData_city <- reactive({
            ipdData <- fetch_admission() %>%  
                filter(source_channel %in% c(input$sourceChannel) & appointment_status_id == 8, app_type == "ipd" & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) ) %>%
                select( city_name, app_type) %>% group_by(city_name) %>% summarize(IPD = n())
            ipdData <- ipdData[!duplicated(ipdData),]
            ipdData
        })
        
        fetchCity <- reactive({
            
            requestData<-merge(requestData1_city(),requestData2_city(),by='city_name',all = TRUE)
            requestData<-transform(requestData,Request = rowSums(requestData[, 2:3], na.rm = TRUE))
            requestData<-requestData[,c(1,4)]
            
            ############## Merge Data ##############
            nData <- merge(requestData,bookingData_city(), by = c('city_name'), all = TRUE)
            data1 <- merge(nData,opdData_city(), by = c('city_name'), all = TRUE)
            data1 <- merge(data1, dayCareData_city(), by = c('city_name'), all = TRUE)
            data1 <- merge(data1,ipdData_city(), by = c('city_name'), all = TRUE)
            
            ############# Mutate Data ##############
            data1 <- select(data1,city_name, everything()) 
            data1 <- data1 %>% mutate( "booking%" = round((Booking/Request)*100,2), "opd%"= round((OPD/Booking)*100,2), "dayCare%" = round((DayCare/OPD)*100,2) , "ipd%" = round((IPD/OPD)*100,2) )
            
        })
        
        ########################################## COUNTRY WISE ###########################################
        requestData1_country <- reactive({
            requestData1 <- fetch_booking() %>% filter(source_channel %in% c(input$sourceChannel) & app_type %in% c('request_callback','quote_request')
                                                       & appointment_status_id != 9 & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory)
            ) %>% group_by(country) %>% summarise(Request=n())
            colnames(requestData1)<-c('country','Request')
            requestData1
        })
        requestData2_country <- reactive({
            requestData2 <- fetch_appointment() %>% filter(source_channel %in% c(input$sourceChannel) & !app_type %in% c('request_callback','quote_request')
                                                           & appointment_status_id != 9 & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory)
            ) %>% group_by(country) %>% summarise(Request=n())
            colnames(requestData2)<-c('country','Request')
            requestData2
        })
        bookingData_country <- reactive({
            bookingData <- fetch_appointment() %>%  
                filter( source_channel %in% c(input$sourceChannel) & patient_country_type %in% c(input$userLocality) &  member %in% c(input$userCategory) & appointment_status_id %in% c(2,8,6,7,10,11,12,13), app_type %in% c('opd', 'opd_walkin', 'probable_ipd') ) %>%
                select( country, app_type) %>% group_by(country) %>% summarize(Booking = n())
            bookingData <- bookingData[!duplicated(bookingData),]
            bookingData
        })
        opdData_country <- reactive({
            opdData <- fetch_appointment() %>%  
                filter( source_channel %in% c(input$sourceChannel) & appointment_status_id == 8 & app_type %in% c('opd','opd_walkin','probable_ipd') 
                        & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) & source_channel %in% c(input$sourceChannel))  %>% #& patient_country_type %in% c(input$userLocality) & source_channel %in% c(input$sourceChannel) & member %in% c(input$userCategory)) %>%
                select( country, app_type) %>% group_by(country) %>% summarize(OPD = n())
            opdData <- opdData[!duplicated(opdData),]
            opdData
        })
        dayCareData_country <- reactive({
            dayCareData <- fetch_appointment() %>%  
                filter(source_channel %in% c(input$sourceChannel) & appointment_status_id == 8, app_type == "day_care" & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) ) %>%
                select( country, app_type) %>% group_by(country) %>% summarize(DayCare = n())
            dayCareData <- dayCareData[!duplicated(dayCareData),]
            dayCareData
        })
        ipdData_country <- reactive({
            ipdData <- fetch_admission() %>%  
                filter(source_channel %in% c(input$sourceChannel) & appointment_status_id == 8, app_type == "ipd" & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) ) %>%
                select( country, app_type) %>% group_by(country) %>% summarize(IPD = n())
            ipdData <- ipdData[!duplicated(ipdData),]
            ipdData
        })
        fetchCountry <- reactive({
            
            requestData<-merge(requestData1_country(),requestData2_country(),by='country',all = TRUE)
            requestData<-transform(requestData,Request = rowSums(requestData[, 2:3], na.rm = TRUE))
            requestData<-requestData[,c(1,4)]
            
            ############## Merge Data ##############
            nData <- merge(requestData,bookingData_country(), by = c('country'), all = TRUE)
            data1 <- merge(nData,opdData_country(), by = c('country'), all = TRUE)
            data1 <- merge(data1, dayCareData_country(), by = c('country'), all = TRUE)
            data1 <- merge(data1,ipdData_country(), by = c('country'), all = TRUE)
            
            ############# Mutate Data ##############
            data1 <- select(data1,country, everything()) 
            data1 <- data1 %>% mutate( "booking%" = round((Booking/Request)*100,2), "opd%"= round((OPD/Booking)*100,2), "dayCare%" = round((DayCare/OPD)*100,2) , "ipd%" = round((IPD/OPD)*100,2) )
            
        })
        
        ##################################################################################################
        ######################################## DOCTOR WISE #############################################
        requestData1_doctor <- reactive({
            requestData1 <- fetch_booking() %>% filter(source_channel %in% c(input$sourceChannel) & app_type %in% c('request_callback','quote_request')
                                                       & appointment_status_id != 9 & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory)
            ) %>% group_by(doctor_name) %>% summarise(Request=n())
            colnames(requestData1)<-c('doctor_name','Request')
            requestData1
        })
        requestData2_doctor <- reactive({
            requestData2 <- fetch_appointment() %>% filter(source_channel %in% c(input$sourceChannel) & !app_type %in% c('request_callback','quote_request')
                                                           & appointment_status_id != 9 & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory)
            ) %>% group_by(doctor_name) %>% summarise(Request=n())
            colnames(requestData2)<-c('doctor_name','Request')
            requestData2
        })
        bookingData_doctor <- reactive({
            bookingData <- fetch_appointment() %>%  
                filter( source_channel %in% c(input$sourceChannel) & patient_country_type %in% c(input$userLocality) &  member %in% c(input$userCategory) & appointment_status_id %in% c(2,8,6,7,10,11,12,13), app_type %in% c('opd', 'opd_walkin', 'probable_ipd') ) %>%
                select( doctor_name, app_type) %>% group_by(doctor_name) %>% summarize(Booking = n())
            bookingData <- bookingData[!duplicated(bookingData),]
            bookingData
        })
        opdData_doctor <- reactive({
            opdData <- fetch_appointment() %>%  
                filter( source_channel %in% c(input$sourceChannel) & appointment_status_id == 8 & app_type %in% c('opd','opd_walkin','probable_ipd') 
                        & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) & source_channel %in% c(input$sourceChannel))  %>% #& patient_country_type %in% c(input$userLocality) & source_channel %in% c(input$sourceChannel) & member %in% c(input$userCategory)) %>%
                select( doctor_name, app_type) %>% group_by(doctor_name) %>% summarize(OPD = n())
            opdData <- opdData[!duplicated(opdData),]
            opdData
        })
        dayCareData_doctor <- reactive({
            dayCareData <- fetch_appointment() %>%  
                filter(source_channel %in% c(input$sourceChannel) & appointment_status_id == 8, app_type == "day_care" & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) ) %>%
                select( doctor_name, app_type) %>% group_by(doctor_name) %>% summarize(DayCare = n())
            dayCareData <- dayCareData[!duplicated(dayCareData),]
            dayCareData
        })
        ipdData_doctor <- reactive({
            ipdData <- fetch_admission() %>%  
                filter(source_channel %in% c(input$sourceChannel) & appointment_status_id == 8, app_type == "ipd" & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) ) %>%
                select( doctor_name, app_type) %>% group_by(doctor_name) %>% summarize(IPD = n())
            ipdData <- ipdData[!duplicated(ipdData),]
            ipdData
        })
        
        fetchDoctor <- reactive({
            
            requestData<-merge(requestData2_doctor(),requestData1_doctor(),by='doctor_name',all = TRUE)
            requestData<-transform(requestData,Request = rowSums(requestData[, 2:3], na.rm = TRUE))
            requestData<-requestData[,c(1,4)]
            
            ############## Merge Data ##############
            nData <- merge(requestData,bookingData_doctor(), by = c('doctor_name'), all = TRUE)
            data1 <- merge(nData,opdData_doctor(), by = c('doctor_name'), all = TRUE)
            data1 <- merge(data1, dayCareData_doctor(), by = c('doctor_name'), all = TRUE)
            data1 <- merge(data1,ipdData_doctor(), by = c('doctor_name'), all = TRUE)
            
            ############# Mutate Data ##############
            data1 <- select(data1, doctor_name, everything()) 
            data1 <- data1 %>% mutate( "booking%" = round((Booking/Request)*100,2), "opd%"= round((OPD/Booking)*100,2), "dayCare%" = round((DayCare/OPD)*100,2) , "ipd%" = round((IPD/OPD)*100,2) )
            
        })
        
        ################################### Hospital Wise #################################################
        requestData1_hospital <- reactive({
            requestData1 <- fetch_booking() %>% filter(source_channel %in% c(input$sourceChannel) & app_type %in% c('request_callback','quote_request')
                                                       & appointment_status_id != 9 & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory)
            ) %>% group_by(hospital_name) %>% summarise(Request=n())
            colnames(requestData1)<-c('hospital_name','Request')
            requestData1
        })
        requestData2_hospital <- reactive({
            requestData2 <- fetch_appointment() %>% filter(source_channel %in% c(input$sourceChannel) & !app_type %in% c('request_callback','quote_request')
                                                           & appointment_status_id != 9 & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory)
            ) %>% group_by(hospital_name) %>% summarise(Request=n())
            colnames(requestData2)<-c('hospital_name','Request')
            requestData2
        })
        bookingData_hospital <- reactive({
            bookingData <- fetch_appointment() %>%  
                filter( source_channel %in% c(input$sourceChannel) & patient_country_type %in% c(input$userLocality) &  member %in% c(input$userCategory) & appointment_status_id %in% c(2,8,6,7,10,11,12,13), app_type %in% c('opd', 'opd_walkin', 'probable_ipd') ) %>%
                select( hospital_name, app_type) %>% group_by(hospital_name) %>% summarize(Booking = n())
            bookingData <- bookingData[!duplicated(bookingData),]
            bookingData
        })
        opdData_hospital <- reactive({
            opdData <- fetch_appointment() %>%  
                filter( source_channel %in% c(input$sourceChannel) & appointment_status_id == 8 & app_type %in% c('opd','opd_walkin','probable_ipd') 
                        & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) & source_channel %in% c(input$sourceChannel))  %>% #& patient_country_type %in% c(input$userLocality) & source_channel %in% c(input$sourceChannel) & member %in% c(input$userCategory)) %>%
                select( hospital_name, app_type) %>% group_by(hospital_name) %>% summarize(OPD = n())
            opdData <- opdData[!duplicated(opdData),]
            opdData
        })
        dayCareData_hospital  <- reactive({
            dayCareData <- fetch_appointment() %>%  
                filter(source_channel %in% c(input$sourceChannel) & appointment_status_id == 8, app_type == "day_care" & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) ) %>%
                select( hospital_name, app_type) %>% group_by(hospital_name) %>% summarize(DayCare = n())
            dayCareData <- dayCareData[!duplicated(dayCareData),]
            dayCareData
        })
        ipdData_hospital <- reactive({
            ipdData <- fetch_admission() %>%  
                filter(source_channel %in% c(input$sourceChannel) & appointment_status_id == 8, app_type == "ipd" & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) ) %>%
                select( hospital_name, app_type) %>% group_by(hospital_name) %>% summarize(IPD = n())
            ipdData <- ipdData[!duplicated(ipdData),]
            ipdData
        })
        
        fetchHospital <- reactive({
            date1 <- input$dateRange[1]
            date2 <- input$dateRange[2]
            
            requestData<-merge(requestData2_hospital(),requestData1_hospital(),by='hospital_name',all = TRUE)
            requestData<-transform(requestData,Request = rowSums(requestData[, 2:3], na.rm = TRUE))
            requestData<-requestData[,c(1,4)]
            
            ############## Merge Data ##############
            nData <- merge(requestData,bookingData_hospital(), by = c('hospital_name'), all = TRUE)
            data1 <- merge(nData,opdData_hospital(), by = c('hospital_name'), all = TRUE)
            data1 <- merge(data1, dayCareData_hospital(), by = c('hospital_name'), all = TRUE)
            data1 <- merge(data1,ipdData_hospital (), by = c('hospital_name'), all = TRUE)
            
            ############# Mutate Data ##############
            data1 <- select(data1, hospital_name, everything()) 
            data1 <- data1 %>% mutate( "booking%" = round((Booking/Request)*100,2), "opd%"= round((OPD/Booking)*100,2), "dayCare%" = round((DayCare/OPD)*100,2) , "ipd%" = round((IPD/OPD)*100,2) )
            
        })
        
        ##################################### SPECIALITY WISE #############################################
        requestData1_spec <- reactive({
            requestData1 <- fetch_booking() %>% filter(source_channel %in% c(input$sourceChannel) & app_type %in% c('request_callback','quote_request')
                                                       & appointment_status_id != 9 & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory)
            ) %>% group_by(speciality) %>% summarise(Request=n())
            colnames(requestData1)<-c('speciality','Request')
            requestData1
        })
        requestData2_spec <- reactive({
            requestData2 <- fetch_appointment() %>% filter(source_channel %in% c(input$sourceChannel) & !app_type %in% c('request_callback','quote_request')
                                                           & appointment_status_id != 9 & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory)
            ) %>% group_by(speciality) %>% summarise(Request=n())
            colnames(requestData2)<-c('speciality','Request')
            requestData2
        })
        bookingData_spec <- reactive({
            bookingData <- fetch_appointment() %>%  
                filter( source_channel %in% c(input$sourceChannel) & patient_country_type %in% c(input$userLocality) &  member %in% c(input$userCategory) & appointment_status_id %in% c(2,8,6,7,10,11,12,13), app_type %in% c('opd', 'opd_walkin', 'probable_ipd') ) %>%
                select( speciality, app_type) %>% group_by(speciality) %>% summarize(Booking = n())
            bookingData <- bookingData[!duplicated(bookingData),]
            bookingData
        })
        opdData_spec <- reactive({
            opdData <- fetch_appointment() %>%  
                filter( source_channel %in% c(input$sourceChannel) & appointment_status_id == 8 & app_type %in% c('opd','opd_walkin','probable_ipd') 
                        & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) & source_channel %in% c(input$sourceChannel))  %>% #& patient_country_type %in% c(input$userLocality) & source_channel %in% c(input$sourceChannel) & member %in% c(input$userCategory)) %>%
                select( speciality, app_type) %>% group_by(speciality) %>% summarize(OPD = n())
            opdData <- opdData[!duplicated(opdData),]
            opdData
        })
        dayCareData_spec  <- reactive({
            dayCareData <- fetch_appointment() %>%  
                filter(source_channel %in% c(input$sourceChannel) & appointment_status_id == 8, app_type == "day_care" & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) ) %>%
                select( speciality, app_type) %>% group_by(speciality) %>% summarize(DayCare = n())
            dayCareData <- dayCareData[!duplicated(dayCareData),]
            dayCareData
        })
        ipdData_spec <- reactive({
            ipdData <- fetch_admission() %>%  
                filter(source_channel %in% c(input$sourceChannel) & appointment_status_id == 8, app_type == "ipd" & patient_country_type %in% c(input$userLocality) & member %in% c(input$userCategory) ) %>%
                select( speciality, app_type) %>% group_by(speciality) %>% summarize(IPD = n())
            ipdData <- ipdData[!duplicated(ipdData),]
            ipdData
        })
        
        fetchSpeciality <- reactive({
            date1 <- input$dateRange[1]
            date2 <- input$dateRange[2]
            
            requestData<-merge(requestData1_spec(),requestData2_spec(),by='speciality',all = TRUE)
            requestData<-transform(requestData,Request = rowSums(requestData[, 2:3], na.rm = TRUE))
            requestData<-requestData[,c(1,4)]
            
            ############## Merge Data ##############
            nData <- merge(requestData,bookingData_spec(), by = c('speciality'), all = TRUE)
            data1 <- merge(nData,opdData_spec(), by = c('speciality'), all = TRUE)
            data1 <- merge(data1, dayCareData_spec(), by = c('speciality'), all = TRUE)
            data1 <- merge(data1,ipdData_spec(), by = c('speciality'), all = TRUE)
            
            ############# Mutate Data ##############
            data1 <- select(data1, speciality, everything()) 
            data1 <- data1 %>% mutate( "booking%" = round((Booking/Request)*100,2), "opd%"= round((OPD/Booking)*100,2), "dayCare%" = round((DayCare/OPD)*100,2) , "ipd%" = round((IPD/OPD)*100,2) )
            
        }) 
        
        #######################################################################
        
        main_data_feed <- reactive({
            main_data_feed <- fetch_booking()
            main_data_feed <- main_data_feed %>% mutate(Date = as.Date(as.POSIXct(main_data_feed$booking_date))) %>%
                select(Date,app_type,appointment_status_id,patient_country_type,city_name,hospital_name,
                       doctor_name,speciality,member,source_channel)
        })
        
        data_req <- reactive({ 
            data_req <-main_data_feed() %>% filter(source_channel %in% c(input$sourceChannel) & app_type %in% c('opd','opd_walkin','probable_ipd') & appointment_status_id != 9 & patient_country_type %in% c(input$userLocality) &  member %in% c(input$userCategory) ) %>%
                group_by(Date) %>% summarise(Request = n())
            data_req <- data_req[!duplicated(data_req),]
        })
        
        data_new <- reactive({ 
            data_new <- main_data_feed() %>% filter(source_channel %in% c(input$sourceChannel) & app_type %in% c('opd','opd_walkin','probable_ipd') & appointment_status_id == 5 & patient_country_type %in% c(input$userLocality) &  member %in% c(input$userCategory) 
            ) %>% group_by(Date) %>% summarise(New = n())
            data_new <- data_new[!duplicated(data_new),]
        })
        
        data_processing <- reactive({ 
            data_processing <-main_data_feed() %>% filter(source_channel %in% c(input$sourceChannel) & appointment_status_id == 4 & app_type %in% c('opd','opd_walkin','probable_ipd') & patient_country_type %in% c(input$userLocality) &  member %in% c(input$userCategory) 
            ) %>% group_by(Date) %>% summarise(Processing = n())
            data_processing <- data_processing[!duplicated(data_processing),]
        })
        
        data_confirm <- reactive({ 
            data_confirm <-main_data_feed() %>% filter(source_channel %in% c(input$sourceChannel) & app_type %in% c('opd','opd_walkin','probable_ipd') & appointment_status_id == 2 & patient_country_type %in% c(input$userLocality) &  member %in% c(input$userCategory) 
            ) %>% group_by(Date) %>% summarise(Confirm = n())
            data_confirm <- data_confirm[!duplicated(data_confirm),]
        })
        
        data_converted <- reactive({ 
            data_converted <-main_data_feed() %>% filter(source_channel %in% c(input$sourceChannel) & app_type %in% c('opd','opd_walkin','probable_ipd') &  appointment_status_id == 8 & patient_country_type %in% c(input$userLocality) &  member %in% c(input$userCategory) 
            ) %>% group_by(Date) %>% summarise(Converted = n())
            data_converted <- data_converted[!duplicated(data_converted),]
        })
        
        data_pending <- reactive({ 
            data_pending <- main_data_feed() %>% filter(source_channel %in% c(input$sourceChannel) & app_type %in% c('opd','opd_walkin','probable_ipd') & appointment_status_id ==1 & patient_country_type %in% c(input$userLocality) &  member %in% c(input$userCategory) 
            ) %>% group_by(Date) %>% summarise(Pending = n())
            data_pending <- data_pending[!duplicated(data_pending),]
        })
        
        data_cancelled <- reactive({ 
            data_cancelled <-main_data_feed() %>% filter(source_channel %in% c(input$sourceChannel) & app_type %in% c('opd','opd_walkin','probable_ipd') & appointment_status_id == 3 & patient_country_type %in% c(input$userLocality) &  member %in% c(input$userCategory) 
            ) %>% group_by(Date) %>% summarise(Cancelled = n())
            data_cancelled <- data_cancelled[!duplicated(data_cancelled),]
        })
        
        data_other <- reactive({ 
            data_other <- main_data_feed() %>% filter(source_channel %in% c(input$sourceChannel) & app_type %in% c('opd','opd_walkin','probable_ipd') & !(appointment_status_id %in% c(1:5,8,9)) & patient_country_type %in% c(input$userLocality) &  member %in% c(input$userCategory) 
            ) %>% group_by(Date) %>% summarise(Others = n())
            data_other <- data_other[!duplicated(data_other),]
        })
        
        fetchOPD <- reactive({
            n1 <- left_join(data_req(), data_new(), by='Date')
            n2 <- left_join(n1, data_processing(), by='Date')
            n3 <- left_join(n2, data_confirm(), by='Date')
            n4 <- left_join(n3, data_converted(), by='Date')
            n5 <- left_join(n4, data_pending(), by='Date')
            n6 <- left_join(n5, data_cancelled(), by='Date')
            all_data <- left_join(n6, data_other(), by='Date')
            
            all_data <- all_data %>% mutate(Day = weekdays(Date)) %>% select(Date, Day, everything())
            row <- nrow(all_data)
            all_data <- all_data[2:row, ]
        })
        ###########################################################################
        # Testing Purpose #
        fetchTest <- reactive({
            date1 <- as.Date(input$dateRange[1])
            date2 <- as.Date(input$dateRange[2])
            
            query <- sprintf("Select *
                             from quote_requests
                             where date(created_at at time zone 'utc+5:30') between '%s' and '%s' ", date1, date2)
            quote_req <- dbGetQuery(con, query)
            #id as quote_id, appointment_id as appt_id, healthcare_centre_id
        })
        
        ######################################   
        
        opts <- list(
            dom = 'Bfrtip', buttons = list('copy','csv','excel','pdf'),
            footerCallback = JS(
                "function( tfoot, data, start, end, display ) {",
                "var api = this.api(), data;",
                "$( api.column(5).footer()).html('SubTotal:  '+",
                "api.column(5).data().reduce( function ( a, b ) {",
                "return a + b;",
                "} )",
                ");",
                "$( api.column(4).footer()).html('SubTotal: '+",
                "api.column(4).data().reduce( function ( a, b ) {",
                "return a + b;",
                "} )",
                ");","}")
        )
        output$dateWiseTable <- renderDataTable(
            datatable(extensions = 'Buttons',options = opts,
                      data = fetchDate(), 
                      rownames = FALSE)
        )
        output$cityWise <- renderDataTable(
            datatable(extensions = 'Buttons',options = opts,
                      data = fetchCity(), 
                      rownames = FALSE)
        )
        output$countryWise <- renderDataTable(
            datatable(extensions = 'Buttons',options = opts,
                      data = fetchCountry(),
                      rownames = FALSE)
        )
        output$doctorWise <- renderDataTable(
            datatable(extensions = 'Buttons',options = opts,
                      data = fetchDoctor(),
                      rownames = FALSE)
        )
        output$hospitalWise <- renderDataTable(
            datatable(extensions = 'Buttons',options = opts,
                      data = fetchHospital(),
                      rownames = FALSE)
        )
        output$specialityWise <- renderDataTable(
            datatable(extensions = 'Buttons',options = opts,
                      data = fetchSpeciality(),
                      rownames = FALSE)
        )
        output$OPDWise <- renderDataTable(
            datatable(extensions = 'Buttons',options = opts,
                      data = fetchOPD(),
                      rownames = FALSE)
        )
        output$testPanel <- renderDataTable(
            datatable(extensions = 'Buttons',options = opts,
                      data = fetchTest(),
                      rownames = FALSE)
        )
        
        output$filter_box <- renderUI({
            if(input$dataChoices == "speciality_wise"){
                selectInput(inputId = "Speciality_wise", label = strong("Select Speciality :"),
                            choices = c(unique(speciality$speciality)), selectize = T, selected = "", multiple = T)
            }else if(input$dataChoices == "City Wise"){
                selectInput(inputId = "City_wise", label = strong("Select City :"),
                            choices = c(cities), selected = "", multiple = T)
            }
        })
        
    } 
    
    
    shinyApp(ui = ui, server = server)
