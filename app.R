library(shiny)
library(sf)
library(leaflet)
library(dplyr)
library(shinydashboard)
library(htmltools)
library(htmlwidgets)
library(reshape2)
library(rsconnect)
library(ggplot2)
library(plotly)
library(RColorBrewer)
library(ggiraph)
library(tidyverse)

# Load the GeoJSON file for India states
india_states <- st_read("https://raw.githubusercontent.com/ShrutiReddy21/statedata/main/INDIA_STATES.geojson")


india_states1 <- st_read("https://raw.githubusercontent.com/ShrutiReddy21/statedata/main/INDIA_STATES.geojson")

anemia_men_other_factors<-read.csv("https://raw.githubusercontent.com/hanuzalte/Nutrition-Dashboard//main/anemia%20men's%20population%20groupwise.csv")
anemia_women_other_factors<-read.csv("https://raw.githubusercontent.com/hanuzalte/Nutrition-Dashboard/main/women%20anemia%20population%20group-wise.csv")
anemia_children_other_factors<-read.csv("https://raw.githubusercontent.com/hanuzalte/Nutrition-Dashboard/main/children%20anemia%20other%20factorscsv.csv")
# Load the CSV files for anemia prevalence data
anemia_data_women <- read.csv("https://raw.githubusercontent.com/ShrutiReddy21/anemia_data/main/women%20anemia%20prevalence_2019-21.csv")
anemia_data_men <- read.csv("https://raw.githubusercontent.com/ShrutiReddy21/anemia_data/main/men_anemia.csv")
anemia_data_children <- read.csv("https://raw.githubusercontent.com/ShrutiReddy21/anemia_data/main/children_anemia_2019-21.csv")
anemia_data_women
anemia_data_men
anemia_data_children

# Load India shapefile for first code
india_sf <- st_read("https://raw.githubusercontent.com/ShrutiReddy21/statedata/main/INDIA_STATES.geojson")

# Sample data: region-wise consumption data for first code
data <- read.csv("https://raw.githubusercontent.com/ShrutiReddy21/Calorie-intake-data/main/Daily%20Calorie%20intake%20region%20wise.csv")

# Load the CSV files for anemia prevalence data
iodine_data <- read.csv("https://raw.githubusercontent.com/ShrutiReddy21/Iodine_test/main/Copy%20of%20Iodized_salt(1).csv")

# Load Data
women_data <- read.csv("https://raw.githubusercontent.com/hanuzalte/Nutrition-Dashboard/main/women%27s%20food%20consumption_%202019-21.csv")
men_data <- read.csv("https://raw.githubusercontent.com/hanuzalte/Nutrition-Dashboard/main/men%27s%20food%20consumption_2019-21.csv")
women_popgrpwise<-read.csv("https://raw.githubusercontent.com/hanuzalte/Nutrition-Dashboard/main/Women's%20Food%20Consumption%20Population%20Groupwise.csv")
men_popgrpwise<- read.csv("https://raw.githubusercontent.com/hanuzalte/Nutrition-Dashboard/main/Mens%20Food%20Consumption%20Population%20group%20wise.csv")
print(head(women_data))
print(head(men_data))

iodine_popgrpwise <- read.csv("https://raw.githubusercontent.com/hanuzalte/Nutrition-Dashboard/main/iodine%20other%20factors.csv")

caloric_intake <-  read.csv("https://raw.githubusercontent.com/haarshhhhh/NUTRITION-INDIA/main/caloric%20intake.csv")


women_data$STNAME <- as.character(women_data$STNAME)
men_data$STNAME <- as.character(men_data$STNAME)

india_map <- st_read("https://raw.githubusercontent.com/ShrutiReddy21/statedata/main/INDIA_STATES.geojson")

print(head(india_map))

men_data_map <- left_join(india_map, men_data, by = c("STNAME_SH" = "STNAME"))
women_data_map <- left_join(india_map, women_data, by = c("STNAME_SH"="STNAME"))

print(head(men_data_map))
print(head(women_data_map))

# Clean and select relevant columns
iodine_data_c <- iodine_data %>%
  filter(!is.na(Children_living_in_iodized_salt_households_below_5_years)) %>%
  select(STNAME, Children_living_in_iodized_salt_households_below_5_years)


yes_iodine_data <- iodine_data %>%
  filter(!is.na(Iodised_Salt)) %>%
  select(STNAME,Iodised_Salt)

not_iodine_data <- iodine_data %>%
  filter(!is.na(Non_Iodised_Salt)) %>%
  select(STNAME,Non_Iodised_Salt)


# Read data from file 
df <- read.csv("https://raw.githubusercontent.com//harshitahp//Nutrition-Prices//main//Copy%20of%20Nutrition_Prices(1).csv", row.names = 1)

# Convert row names to a column 
df$country <- rownames(df) 
rownames(df) <- NULL

# Define state-to-region mapping
state_to_region <- data.frame(
  state = c("Jammu & Kashmir", "Himachal Pradesh", "Punjab", "Uttarakhand", "Haryana", "Delhi", "Uttar Pradesh","Ladakh", 
            "Bihar", "Jharkhand", "Odisha", "West Bengal",
            "Assam", "Arunachal Pradesh", "Manipur", "Meghalaya", "Nagaland", "Tripura", "Sikkim",
            "Goa", "Gujarat", "Maharashtra", "Rajasthan",
            "Chhattisgarh", "Madhya Pradesh",
            "Andhra Pradesh", "Karnataka", "Kerala", "Tamil Nadu", "Telangana"),
  region = c(rep("North", 8),
             rep("East", 4),
             rep("North_East", 7),
             rep("West", 4),
             rep("Central", 2),
             rep("South", 5))
)

# Merge shapefile with state-to-region mapping
india_sf <- india_sf %>%
  left_join(state_to_region, by = c("STNAME_SH" = "state"))

# Merge with data
india_sf <- india_sf %>%
  left_join(data, by = c("region" = "Region"))


# Define a custom color palette
custom_colors <- c("#10477E1A", "#10477E33", "#10477E4D", "#10477E66", "#10477E80", "#10477E99", "#10477EB3", "#10477ECC", "#10477EE6", "#10477e")
# Define custom color function with explicit bins for first code
customColor1 <- colorBin(
  palette = c("#002A42","#10477E", "#3F8589","#97CA94","#FFA80B","#E54B4B"),
  domain = india_sf$Total,
  na.color = "transparent"
)


custom_colors3 <- c(  "#10477EE6", "#10477e",  "#10477EE6", "#10477e", "#10477EE6", "#10477e", "#10477EE6", "#10477e")


# UI


ui <- dashboardPage(
  dashboardHeader(
    title = tags$div(
      style = "white-space: nowrap; overflow: hidden; text-overflow: ellipsis; font-weight: bold;font-size: 25px;",
      "NUTRITION IN INDIA"
    ), 
    titleWidth = 350,
    tags$li(
      a(href = "https://cpc-analytics.com/home/ourwork",
        img(src = 'https://github.com/ShrutiReddy21/Iodine_test/blob/main/image.png?raw=true',
            title = "company Home", height = "50 px"),
        style = "padding-top:10px; padding-bottom:10px;"),
      class = "dropdown"
    )
  ),
  dashboardSidebar(disable = TRUE),
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .main-header .logo {
          background-color: #002A42 !important;
          color: white !important;
        }
        .main-header .navbar {
          background-color: #002A42 !important;
        }
        .box-header {
          background-color: #002A42 !important;
        }
        .box-title {
          color: white !important;
        }
        .tab-content {
          margin: 20px;
        }
        .description {
          margin-bottom: 15px;
          font-size: 14px;
          color: #555;
        }
      "))
    ),
    
    tags$div(
      style =  "padding-top:5px; padding-bottom:10px; font-size: 14px;",
      tags$p(div(style = "font-weight: bold; font-size: 16px;", "This dashboard provides insights into various aspects of nutrition in India based on data from NFHS 2021 and other sources. You can explore different dimensions of Food Consumption, Daily Caloric Intake, Cost Comparison of Food Groups, Anemia Prevalence, and Iodine Presence in Households.")),
      tags$p(div(style ="font-weight: bold; font-size: 16px;","Navigate through the tabs to explore detailed maps and visualizations."))
    ),
    
    tabBox(
      width = 14,
      tabPanel(div(style = "font-weight: bold; font-size: 20px;", "Food Consumption"),
               fluidRow(
                 box(
                   title =div(
                     style = "font-weight: bold; font-size: 20px;",
                     "State-Wise Food Consumption Map of India (NFHS 2021)"
                   ) ,
                   solidHeader = TRUE, 
                   width = 14,
                   tags$div(style = "font-size: 14px; color: #555; margin-bottom: 10px;", 
                            "The data from NFHS 5 (2019-21) reveals the percent distribution of men and women aged 15-49 consuming specific foods at least once a week across various states and union territories in India.Below is a map displaying categories and subcategories for gender and food groups. It illustrates the percentage of the population, categorized by gender, that consumes the selected food group at least once a week. For example, by selecting a specific gender and food group, you can see what proportion of men or women consume that food group weekly."),
                   sidebarLayout(
                     sidebarPanel(
                       
                       selectInput("variable", "Select Food Component:", 
                                   choices = c("Milk or curd" = "Milk_or_curd", "Pulses or beans" = "Pulses_or_beans",
                                               "Dark green leafy vegetables" = "Dark_green_leafy_vegetables", "Fruits" = "Fruits",
                                               "Eggs" = "Eggs", "Chicken or meat" = "Chicken_or_meat", 
                                               "Fish chicken or meat" = "Fish_chicken_or_meat",
                                               "Fried Foods" = "Fried_Foods", "Aerated drinks" = "Aerated_drinks")),
                       selectInput("gender", "Women/Men:", choices = c("Men", "Women")),
                       tags$p("Select a food component and gender to view the state-wise consumption map of India.", class = "description")
                     ),
                     mainPanel(
                       leafletOutput("map"),
                       
                     )
                   )
                 ),
                 box(
                   title = div(
                     style = "font-weight: bold; font-size: 20px;",
                     "Population Group-Wise Food Consumption India (NFHS 2021)"),
                   solidHeader = TRUE,
                   width = 14,tags$div(style = "font-size: 14px; color: #555; margin-bottom: 10px;", 
                                       "The NFHS 2021 data food groups consumption once a week atleast across diverse demographic groups in India. This analysis compares food consumption among men, women, and children across varying socio-economic and educational backgrounds.
Below is a bar chart depicting the distribution of choice of food groups among these population groups."),
                   sidebarLayout(
                     sidebarPanel (
                       selectInput("Sex", "Select Gender:",
                                   choices = c("Men","Women")),uiOutput("BG"),
                       uiOutput("rrb")
                       
                       
                     ),mainPanel(
                       plotlyOutput("barPlot1")
                     )
                   )
                 )
               ),
               tags$div(style = "text-align: center;font-size: 12px; margin-top: 10px;", "Source: NFHS 2021. Calculations by CPC Analytics.")
      ),
      
      tabPanel(div(style = "font-weight: bold; font-size: 20px;", "Daily Caloric Intake"),
               fluidRow(
                 box(
                   title =div(
                     style = "font-weight: bold; font-size: 20px;",
                     "Daily Region-Wise Per Capita Caloric Intake Comparison by Food Groups"
                   ) , 
                   
                   solidHeader = TRUE, 
                   width = 14, 
                   tags$p("This is India's per capita caloric intake data for different food groups, compared across six regions: East, West, North, South, Northeast, and Central. When you hover over the map of different regions, it shows a list of food groups and their average caloric intake in that region.", class = "description"),
                   leafletOutput("mymap"),
                   
                 )
                 
                 
                 ,
                 
                 box(
                   title = div(
                     style = "font-weight: bold; font-size: 20px;",
                     "Comparison of population group's diets with EAT Lancet Reference diet standards "
                   ),
                   solidHeader = TRUE,
                   width = 12,tags$p("The EAT-Lancet Commission on Food, Planet, Health suggested reference norms for adequate intake, measured as the count of calories across different food groups. Below is a comparison of different population groups and their daily calorie intake with the EAT-Lancet reference diet.", class = "description"),sidebarLayout(
                     sidebarPanel (
                       selectInput("Food", "Select Food Group",
                                   choices = c("Total","Whole_grains",
                                               "Potato_and_Cassava","Vegetables","Fruits","Dairy_Foods","Protein_sources","All_Animal_source_proteins","Beef_and_lamb",
                                               "Pork","Poultry","Eggs","Fish","Legumes","Tree_nuts","Added_fats","Palm_Oil","Unsaturated_Fats","Spices"
                                               
                                   ))
                     )
                     
                     ,mainPanel(
                       plotlyOutput("barPlot2")
                     )))),
               tags$div(style = "text-align: center; margin-top: 10px;", "Source: Sharma et al. BMC Public Health (2020). Calculations by CPC Analytics.")
      ),
      
      tabPanel(div(style = "font-weight: bold; font-size: 20px;", "Cost Comparison of Food Groups"),
               fluidRow(
                 box(
                   title =div(
                     style = "font-weight: bold; font-size: 20px;",
                     "Cost Comparison of Food Groups Between Countries (World Bank 2023)"
                   ),
                   subtitle = "Value is in $",
                   
                   solidHeader = TRUE, 
                   width = 14,
                   tags$div(style = "font-size: 14px; color: #555; margin-bottom: 10px;", 
                            "The comparative analysis between different food groups between given countries  highlights the relative costs of various food groups across different countries, with India as the baseline.
                            The costs of a nutrient-adequate diet and food groups like fruits, vegetables, and starchy staples in India are generally lower compared to countries such as the United States, Nepal, and Mexico. Conversely, some countries like Pakistan and Bangladesh offer certain food groups at cheaper rates than India.
                            These disparities reflect differences in agricultural productivity, economic conditions, and cost of living. A nutrient-adequate diet in Nepal is 20.7% more expensive than in India, Pakistan offers it at 8.4% cheaper. Similarly, fruits are significantly more expensive in Nepal (103.6% higher) but slightly more expensive in Nigeria (3.6% higher)."),
                   sidebarLayout(
                     sidebarPanel(
                       
                       selectInput("cost_type", "Select Cost Type:", 
                                   choices = c("Cost of a Nutrient Adequate Diet" = "Cost_of_a.nutrient_adequate_diet",
                                               "Cost of Fruits" = "Cost_of_fruits",  
                                               "Cost of Vegetables" = "Cost_of_vegetables", 
                                               "Cost of Starchy Staples" = "Cost_of_starchy_staples",
                                               "Cost of Animal-Source Foods" = "Cost_of_animal_source_foods", 
                                               "Cost of Legumes, Nuts, and Seeds" = "Cost_of_legumes_nuts_and_seeds", 
                                               "Cost of Oils and Fats" = "Cost_of_oils_and_fats")),
                       tags$p("Select a cost type to compare the costs of different food groups between countries.", class = "description")
                     ),
                     mainPanel(
                       plotlyOutput("barPlot")
                     )
                   )
                 )
               ),
               tags$div(style = "text-align: center;font-size: 12px; margin-top: 10px;", "Source: Herforth et al. (2022), adapted by World Bank (2023).")
      ),
      
      tabPanel(div(style = "font-weight: bold; font-size: 20px;", "Anemia Prevalence"),
               fluidRow(
                 box(
                   title =div(
                     style = "font-weight: bold; font-size: 20px;",
                     "State-Wise Anemia Prevalence in India (NFHS 2021)"
                   ),
                   
                   solidHeader = TRUE, 
                   width = 14,
                   tags$div(style = "font-size: 14px; color: #555; margin-bottom: 10px;", 
                            "Explore the state-wise prevalence of anemia in India among different population categories, based on NFHS 2021 data."),
                   tags$head(
                     tags$style(
                       HTML("
                  body {
                    font-family: 'Noto Sans', sans-serif;
                  }
                ")
                     )
                   ),
                   sidebarLayout(
                     sidebarPanel(
                       
                       selectInput("Category", "Select Population Category:", choices = c("Women", "Men", "Children")),
                       style = "background-color: #f8f9fa; padding: 20px;",
                       tags$p("Select a population category to view the state-wise anemia prevalence in India.", class = "description")
                     ),
                     mainPanel(
                       leafletOutput("indiaMap")
                     )
                   )
                 ),box(
                   title = div(
                     style = "font-weight: bold; font-size: 22px;",
                     "Population Group-Wise Anemia Prevalence of India (NFHS 2021)"),
                   
                   solidHeader = TRUE,
                   width = 18,tags$div(style = "font-size: 14px; color: #555; margin-bottom: 10px;", 
                                       "The NFHS 2021 data underscores the pervasive issue of anemia across diverse demographic groups in India. This analysis compares anemia prevalence among men, women, and children across varying socio-economic and educational backgrounds, as well as different levels of smoking and tobacco use.
Below is a bar chart depicting the distribution of severe, moderate, and mild anemia among these population groups."),
                   sidebarLayout(
                     sidebarPanel (
                       selectInput("Sex", "Select Category:",
                                   choices = c("Men","Women", "Children")),uiOutput("Cat"),
                       uiOutput("Type1")
                       
                       
                     ),
                     mainPanel(
                       plotlyOutput("barPlot3")
                     )
                   )
                 )
               ),
               tags$div(style = "text-align: center;font-size: 12px; margin-top: 10px;", "Source: NFHS 2021. Calculations by CPC Analytics.")
      ),
      
      tabPanel(div(style = "font-weight: bold; font-size: 20px;", "Iodine Presence in Households"),
               fluidRow(
                 box(
                   title = div(
                     style = "font-weight: bold; font-size: 20px;",
                     "State-Wise Iodine Presence in Children in India (NFHS 2021)"
                   ),
                   
                   solidHeader = TRUE, 
                   width = 14,
                   tags$div(style = "font-size: 14px; color: #555; margin-bottom: 10px; margin-top: 10px;", "The data from NFHS 5 on the usage of iodized salt in various Indian states. It includes the percentage of households using iodized salt, the percentage using non-iodized salt, and the percentage of children under 5 years old living in households using iodized salt.
                                     Iodized salt is essential for preventing iodine deficiency disorders (IDD), which can lead to severe health issues such as goiter, hypothyroidism, and cognitive impairments. According to the World Health Organization (WHO), more than 90% of households should use adequately iodized salt to prevent these disorders. 
                                     The data from various Indian states shows significant progress, with states like Arunachal Pradesh achieving 99.2% household usage of iodized salt and 99.3% of children under 5 years old living in households using iodized salt. However, some states like Chandigarh and Delhi have lower percentages, at 96.8%. 
                                     These figures indicate that while many states meet or exceed the WHO standard, there are still areas needing improvement. Targeted interventions, public awareness campaigns, and strict regulatory enforcement are necessary to increase the coverage of iodized salt in states with lower usage. 
                                     By addressing these gaps, India can further reduce the prevalence of IDD and improve the overall health and cognitive development of its population, particularly among vulnerable groups such as pregnant women and young children."),
                   tags$head(
                     tags$style(
                       HTML("
                  body {
                    font-family: 'Noto Sans', sans-serif;
                  }
                ")
                     )
                   ),
                   sidebarLayout(
                     sidebarPanel(
                       tags$div(style = "font-size: 14px; color: #555; margin-bottom: 10px;"
                       ),
                       selectInput("Category", "Select Household Category:", choices = c("Households with Iodized Salt", "Households without Iodized Salt", "Households with Children of Age 6-59 Months (With Iodized Salt)")),
                       style = "background-color: #f8f9fa; padding: 20px;",
                       tags$p("Select a household category to view the state-wise iodine presence in children in India.", class = "description")
                     ),
                     mainPanel(
                       leafletOutput("indiaMap1")
                     )
                   )
                 )
               ),
               tags$div(style = "text-align: center;font-size: 12px; margin-top: 10px;","Source: NFHS 2021. Calculations by CPC Analytics.")
      )
    ),
    
    fluidRow(
      tags$div(style = "text-align: left; margin-top: 20px; padding: 10px; background-color: #f8f9fa; font-size: 11px;color: #6c757d", 
               HTML("© 2024 CPC Analytics. All rights reserved.<br>
          All content included in this dashboard is strictly for informational purposes.<br>
          The information herein contains both original data and estimates which are subject to updation, correction, and revision.<br>
          Discrepancies between information contained in this dashboard and other analyses, studies, articles, or publications produced by the NFHS, the World Bank, or Sharma et al. BMC Public Health may exist due to the different timing and methodologies of processing the information.<br>
          The presentation of information on the map herein is not warranted to be error-free. It does not imply the expression of any opinion whatsoever on the part of the NFHS, the World Bank, or Sharma et al. BMC Public Health concerning the legal status of any country, area, or territory or of its authorities, or concerning the delimitation of its borders.")
      )
    )
  )
)


# Server 
server <- function(input, output, session) {
  output$mymap <- renderLeaflet({
    leaflet(india_sf) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~customColor1(Total),
        weight = 2,
        opacity = 1,
        color = 'white', 
        dashArray = '3', 
        fillOpacity = 0.7,
        label = ~lapply(paste0(
          "<strong>", region, "</strong><br/>",
          "Total: ", Total," Calories","<br/>",
          "<ul>",
          "<li>Added fats: ", Added_fats, "</li>",
          "<li>Animal source proteins: ", Animal_source_proteins, "</li>",
          "<li>Sweeteners: ", sweeteners, "</li>",
          "<li>Dairy Foods: ", Dairy_Foods, "</li>",
          "<li>Fruits: ", Fruits, "</li>",
          "<li>Legumes: ", Legumes, "</li>",
          "<li>Potato and Cassava: ", Potato_and_Cassava, "</li>",
          "<li>Processed Food: ", Processed_Food, "</li>",
          "<li>Spices: ", Spices, "</li>",
          "<li>Nuts: ", Nuts, "</li>",
          "<li>Vegetables: ", Vegetables, "</li>",
          "<li>Whole grains: ", Whole_grains, "</li>",
          "</ul>"
        ), HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "10px",
          direction = "auto",
          offset = c(0, 0),
          opacity = 1
        )
      )%>%
      addLegend(
        position = "topright",
        colors = c("#002A42","#10477E", "#3F8589","#97CA94","#FFA80B","#E54B4B"),
        labels = c("West", "North East","South","Central","East","North"),
        title = "REGIONS",
        opacity = 1)
  })
  
  
  anemia_data <- reactive({
    if (input$Category == "Women") {
      left_join(india_states, anemia_data_women, by = "STNAME")
    } else if (input$Category == "Men") {
      left_join(india_states, anemia_data_men, by = "STNAME")
    } else {
      left_join(india_states, anemia_data_children, by = "STNAME")
    }
  })
  
  
  observe({
    print(head(anemia_data()))
  })
  
  
  customColor <- reactive({
    if (input$Category == "Women") {
      colorBin(
        palette = c("#97CA94","#FFA80B","#E54B4B","#3F8589", "#10477e"),
        domain = anemia_data()$Any_Anaemia,
        bins = c(0, 30, 45, 60, 75, 100),
        na.color = "transparent"
      )
    } else if (input$Category == "Men") {
      colorBin(
        palette = c("#97CA94","#FFA80B","#E54B4B","#3F8589", "#10477e"),
        domain = anemia_data()$Any_Anemia,
        bins = c(0, 30, 45, 60, 75, 100),
        na.color = "transparent"
      )
    } else {
      colorBin(
        palette = c("#97CA94","#FFA80B","#E54B4B","#3F8589", "#10477e"),
        domain = anemia_data()$Any_Anemia,
        bins = c(0, 30, 45, 60, 75, 100),
        na.color = "transparent"
      )
    }
  })
  
  #Render UI according to Background
  output$BG <-renderUI({
    if(input$Sex == "Men"){selectInput("Group", "Select Population Group: ",choices = c("Residence","Religion","Caste","Wealth"))
    }
    else if (input$Sex == "Women"){selectInput("Group", "Select Population Group: ",choices = c("Residence","Religion","Caste","Wealth","Maternity Status"))}
    else {selectInput("Group", "Select Population Group: ",choices = c("Residence","Religion","Caste","Wealth"))}
  })
  output$rrb<-renderUI({
    if(input$Sex == "Men"){
      if (input$Group == "Residence"){selectInput("Type","Select the type of residence",choices = c("Urban" , "Rural"))}
      else if (input$Group == "Religion"){selectInput("Type","Select Religion",choices = c("Hindu","Muslim","Christian","Sikh","Buddhist","Other"))}
      else if (input$Group == "Caste"){selectInput("Type","Select Caste", choices = c("SC","ST","OBC","Other","Don't Know"))}
      else if (input$Group == "Wealth"){selectInput("Type","Select Wealth", choices = c("Lowest","Second","Middle","Fourth","Highest"))}}
    else
    {if (input$Group == "Residence"){selectInput("Type","Select the type of residence",choices = c("Urban" , "Rural"))}
      else if (input$Group == "Religion"){selectInput("Type","Select Religion",choices = c("Hindu","Muslim","Christian","Sikh","Buddhist","Other"))}
      else if (input$Group == "Caste"){selectInput("Type","Select Caste", choices = c("SC","ST","OBC","Other","Don't know"))}
      else if (input$Group == "Wealth"){selectInput("Type","Select Wealth", choices = c("Lowest","Second","Middle","Fourth","Highest"))}
      else {selectInput("Type","Choose Maternity Status",choices = c("Pregnant","Breastfeeding","Neither"))}}
  })
  
  
  #Render Bar Plot of food consumption population group wise
  # Plotting the graph
  output$barPlot1 <- renderPlotly({
    selected <- input$Type
    selected_sex <- input$Sex
    
    # Assuming men_popgrpwise is your data set and it is available in the global environment
    
    # Filter the data to get the relevant row based on the selected Background
    if (selected_sex == "Men") {
      filtered_data <- men_popgrpwise[men_popgrpwise$Background == selected, ]
    } else if (selected_sex == "Women") {
      filtered_data <- women_popgrpwise[women_popgrpwise$Background == selected, ]
    }
    else {filtered_data <- men_popgrpwise[men_popgrpwise$Background == selected, ]}
    # Melt the filtered data
    g_melted <- melt(filtered_data, id.vars = "Background")
    g_melted
    
    num_groups <- length(unique(g_melted$variable))
    # Create the plot
    pg <- ggplot(g_melted, aes(x = variable , y = value, fill = variable)) + 
      geom_bar(stat = "identity") + 
      labs(title = paste("Consumption for", gsub("_", " ", selected)), x = "Category", y = "% Consumption") + 
      scale_fill_manual(values = custom_colors3)+
      coord_flip() + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    # Convert to Plot ly
    ggplotly(pg, tooltip = "y")
    
  })
  output$barPlot2 <- renderPlotly({
    selected_food <- input$Food
    hg<-caloric_intake%>%filter(Population %in% c("Highest_MPCE_Urban","Highest_MPCE_Rural","Lowest_MPCE_Urban","Lowest_MPCE_Rural","EAT_Lancet"))
    hg_melted <- melt(hg, id.vars = "Population")
    hg_filtered <- hg_melted[hg_melted$variable == selected_food, ]
    
    
    custom_colors <- c("Highest_MPCE_Urban" = "#10477e", "Highest_MPCE_Rural" = "#10477EE6", 
                       "Lowest_MPCE_Urban" = "#10477ECC", "Lowest_MPCE_Rural" = "#10477EB3", 
                       "EAT_Lancet" = "red")
    
    p <- ggplot(hg_filtered, aes(x = Population, y = value, fill = Population)) + 
      geom_bar(stat = "identity") + 
      labs(title = paste("Caloric intake - ", gsub("_", " ", selected_food)), x = "Population", y = "Calories") + 
      scale_fill_manual(values =custom_colors) + 
      coord_flip() + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p, tooltip = "y") 
  })
  
  # Render the leaflet map
  output$indiaMap <- renderLeaflet({
    leaflet(st_as_sf(anemia_data())) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~customColor()(if(input$Category == "Women") anemia_data_women$Any_Anaemia else if (input$Category == "Men") anemia_data_men$Any_Anemia else anemia_data_children$Any_Anemia),
        weight = 1,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        label = ~lapply(if (input$Category == "Women") 
          paste0("<strong>", STNAME, "</strong><br/>",
                 "Any Anemia(<12.0 g/dl): ", 
                 anemia_data_women$Any_Anaemia, "<br/>",
                 "Severe Anemia(<8.0 g/dl): ",
                 anemia_data_women$Severe,  "<br/>",
                 "Moderate Anemia(8.0-10.9 g/dl): ",
                 anemia_data_women$Moderate, "<br/>",
                 "Mild Anemia(11.0-11.9 g/dl): ", 
                 anemia_data_women$Mild, 
                 "<br/>"
          )
          
          else if(input$Category == "Men")
            paste0("<strong>", STNAME, "</strong><br/>",
                   "Any Anemia(<13.0): ", 
                   anemia_data_men$Any_Anemia, "<br/>",
                   "Severe Anemia(<9.0 g/dl): ",
                   anemia_data_men$Severe, "<br/>",
                   "Moderate Anemia(9.0-11.9 g/dl): ",
                   anemia_data_men$Moderate, "<br/>",
                   "Mild Anemia(12.0-12.9 g/dl): ",
                   anemia_data_men$Mild,
                   
                   "%<br/>")
          else
            paste0("<strong>", STNAME, "</strong><br/>",
                   "Any Anemia(<11.0 g/dl): ", 
                   anemia_data_men$Any_Anemia, "<br/>",
                   "Severe Anemia(<7.0 g/dl): ",
                   anemia_data_men$Severe,"<br/>",
                   "Moderate Anemia(7.0-9.9 g/dl): ",
                   anemia_data_men$Moderate, "<br/>",
                   "Mild Anemia(10-10.9 g/dl): ",
                   anemia_data_men$Mild,
                   
                   "%<br/>"),
          HTML
        ),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "10px",
          direction = "auto",
          offset = c(0, 0),
          opacity = 1
        )
      ) %>%
      addLegend(
        position = "topright",
        colors = c("#97CA94","#FFA80B","#E54B4B","#3F8589", "#10477e"),
        labels = c("0-30", "30-45", "45-60", "60-75", "75-100"),
        opacity = 1
      )
  })
  
  #Render UI according to Background
  output$Cat <-renderUI({
    if(input$Sex == "Men"){selectInput("var", "Select Population Group: ",choices = c("Residence","Religion","Caste","Wealth","Schooling","Select Smoking status"))
    }
    else if (input$Sex == "Women"){selectInput("var", "Select Population Group: ",choices = c("Residence","Religion","Caste","Select Smoking status","Wealth","Schooling","Maternity Status"))}
    else if (input$Sex == "Children") {selectInput("var", "Select Population Group: ", choices = c("Age","Residence", "Religion","Caste", "Mother's Schooling", "Caste","Living Condition", "Anemia Status of Mother", "Wealth"))}
    else {selectInput("var", "Select Population Group: ",choices = c("Residence","Religion","Caste","Schooling","Wealth"))}
  })
  output$Type1<-renderUI({
    if(input$Sex == "Men"){
      if (input$var == "Residence"){selectInput("sub","Select the type of residence",choices = c("Urban" , "Rural"))}
      else if (input$var == "Religion"){selectInput("sub","Select Religion",choices = c("Hindu","Muslim","Christian","Sikh","Buddhist","Other"))}
      else if (input$var == "Caste"){selectInput("sub","Select Caste", choices = c("SC","ST","OBC","Other","Don't Know"))}
      else if (input$var == "Wealth"){selectInput("sub","Select Wealth", choices = c("Lowest","Second","Middle","Fourth","Highest"))}
      else if (input$var == "Schooling"){selectInput("sub","Schooling", choices = c("No schooling","<5 years complete","5-7 years complete","8-9 years complete","10-11 years complete", "12 or more years complete"))}
      else {selectInput("sub", "Select Smoking status", choices = c("Smokes cigarettes/tobacco", "Does not smoke"))}}
    else if (input$Sex == "Women") {
      if (input$var == "Residence"){selectInput("sub","Select the type of residence",choices = c("Urban" , "Rural"))}
      else if (input$var == "Religion"){selectInput("sub","Select Religion",choices = c("Hindu","Muslim","Christian","Sikh","Buddhist","Other"))}
      else if (input$var == "Caste"){selectInput("sub","Select Caste", choices = c("SC","ST","OBC","Other","Don't know"))}
      else if (input$var == "Wealth"){selectInput("sub","Select Wealth", choices = c("Lowest","Second","Middle","Fourth","Highest"))}
      else if (input$var == "Select Smoking status"){selectInput("Type","Select Smoking status", choices = c("Smokes cigarettes/tobacco","Does not smoke"))}
      else if (input$var== "Schooling"){selectInput("sub","Schooling", choices = c("No schooling","<5 years complete","5-7 years complete","8-9 years complete","10-11 years complete", "12 or more years complete"))}
      else{selectInput("sub","Choose Maternity Status",choices = c("Pregnant","Breastfeeding","Neither"))}}
    else if (input$Sex == "Children") {
      if (input$var == "Age"){selectInput("sub","Select the age group",choices = c("Age 6-8 months" , "Age 9-11 months", "Age 12-17 months", "Age 18-23 months", "Age 24-35 months", "Age 36-47 months", "Age 48-59 months"))}
      else if (input$var == "Residence"){selectInput("sub","Select the type of residence",choices = c("Urban" , "Rural"))}
      else if (input$var == "Religion"){selectInput("sub","Select Religion",choices = c("Hindu","Muslim","Christian","Sikh","Buddhist","Other"))}
      else if (input$var == "Caste"){selectInput("sub","Select the Caste", choices = c("SC","ST","OBC","Other","Don't know"))}
      else if (input$var == "Wealth"){selectInput("sub","Select Wealth group", choices = c("Lowest","Second","Middle","Fourth","Highest"))}
      else if (input$var == "Living Condition"){selectInput("sub","select Living status", choices = c("Living with both parents","Living only with mother", "Living only with father", "Living with neither parent"))}
      else if (input$var == "Mother's Schooling"){selectInput("sub","select the Mother's Schooling", choices = c("No schooling", "<5 years", "5-7 years", "8-9 years", "10-11 years", "12 or more years"))}
      else{selectInput("sub","Anemia Status of Mother",choices = c("Not anaemic(mother)","Mildly anaemic(mother)","Moderately anaemic(mother)", "Severely anaemic(mother)"))}}
  })
  
  
  
  #Render Bar Plot of Anemia Prevalence population group wise
  # Plotting the graph
  output$barPlot3 <- renderPlotly({
    selected_sub <- input$sub
    selected_sex <- input$Sex
    
    # Assuming men_popgrpwise is your data set and it is available in the global environment
    
    # Filter the data to get the relevant row based on the selected Background
    if (selected_sex == "Men") {
      filtered_data <- anemia_men_other_factors[anemia_men_other_factors$Background == selected_sub, ]
    } else if (selected_sex == "Women") {
      filtered_data <- anemia_women_other_factors[anemia_women_other_factors$Background == selected_sub, ]
    } else if (selected_sex == "Children") {
      filtered_data <- anemia_children_other_factors[anemia_children_other_factors$Background == selected_sub, ]
    }
    
    if (nrow(filtered_data) > 0) {
      
      g_melted <- melt(filtered_data, id.vars = "Background")
      
      palette1 <- colorRampPalette(c( "#10477EE6", "#10477e",  "#10477EE6", "#10477e"))(n = length(unique(g_melted$variable)))
      
      # Plot using ggplot2
      p <- ggplot(g_melted, aes(x = variable, y = value, fill = variable)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = palette1) +
        theme_minimal() +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1),
          plot.title = element_text(hjust = 0.5)
        ) +
        labs(
          title = paste("Anemia Factors by", selected_sex),
          x = "Factors",
          y = "Values",
          fill = "Factors"
        )
      
      # Convert to Plot ly
      ggplotly(p, tooltip = "y")
    } else {
      return(NULL)
    }
  })
  # Reactive expression to get the selected category data
  iodine_data <- reactive({
    if (input$Category == "Households with Iodized Salt") {
      left_join(india_states1, yes_iodine_data, by = "STNAME")
    } else if (input$Category == "Households without Iodized Salt") {
      left_join(india_states1, not_iodine_data, by = "STNAME")
    } else {
      left_join(india_states1, iodine_data_c, by = "STNAME")
    }
  })
  
  # Log output to debug
  observe({
    print(head(iodine_data()))
  })
  
  
  customColor2 <- reactive({
    if (input$Category == "Households with Iodized Salt") {
      colorBin(
        palette = c("#97CA94","#FFA80B","#E54B4B","#3F8589"),
        domain = iodine_data()$Iodised_Salt,
        bins = c(0, 80, 90, 95, 100),
        na.color = "transparent"
      )
    } else if (input$Category == "Households without Iodized Salt") {
      colorBin(
        palette = c("#97CA94","#FFA80B","#E54B4B","#3F8589"),
        domain = iodine_data()$Non_Iodised_Salt,
        bins = c(0, 5, 10, 20, 100),
        na.color = "transparent"
      )
    } else {
      colorBin(
        palette = c("#97CA94","#FFA80B","#E54B4B","#3F8589"),
        domain = iodine_data()$Children_living_in_iodized_salt_households_below_5_years,
        bins = c(0, 80, 90, 95, 100),
        na.color = "transparent"
      )
    }
  })
  
  # Render the leaflet map
  output$indiaMap1 <- renderLeaflet({
    leaflet(st_as_sf(iodine_data())) %>%
      addProviderTiles("CartoDB.Positron") %>%
      addPolygons(
        fillColor = ~customColor2()(if(input$Category == "Households with Iodized Salt") yes_iodine_data$Iodised_Salt else if (input$Category == "Households without Iodized Salt") not_iodine_data$Non_Iodised_Salt  else iodine_data_c$Children_living_in_iodized_salt_households_below_5_years),
        weight = 1,
        opacity = 1,
        color = 'white',
        dashArray = '3',
        fillOpacity = 0.7,
        highlight = highlightOptions(
          weight = 2,
          color = '#666',
          dashArray = '',
          fillOpacity = 0.7,
          bringToFront = TRUE
        ),
        label = ~paste0(STNAME, ": ",if(input$Category == "Households with Iodized Salt") yes_iodine_data$Iodised_Salt else if (input$Category == "Households without Iodized Salt") not_iodine_data$Non_Iodised_Salt  else iodine_data_c$Children_living_in_iodized_salt_households_below_5_years, "%"),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = customColor2(),
        values = if(input$Category == "Households with Iodized Salt") yes_iodine_data$Iodised_Salt else if (input$Category == "Households without Iodized Salt") not_iodine_data$Non_Iodised_Salt  else iodine_data_c$Children_living_in_iodized_salt_households_below_5_years,
        opacity = 0.7,
        position = "topright"
      ) %>%
      fitBounds(
        lng1 = min(st_bbox(iodine_data())$xmin, na.rm = TRUE),
        lat1 = min(st_bbox(iodine_data())$ymin, na.rm = TRUE),
        lng2 = max(st_bbox(iodine_data())$xmax, na.rm = TRUE),
        lat2 = max(st_bbox(iodine_data())$ymax, na.rm = TRUE)
      ) # Fit the map to the bounds of India
  })
  
  observe({
    data <- if (input$gender == "Men") men_data_map else women_data_map
    
    data[[paste0(input$variable, "_cat")]] <- cut(data[[input$variable]], 
                                                  breaks = c(-Inf, 20, 40, 60, 80, Inf), 
                                                  labels = c("Below 20", "20-40","40-60", "60-80", "Above 80"))
    
    pal <- colorFactor(palette = c( "#97CA94","#FFA80B","#E54B4B","#3F8589", "#10477e" ), domain = data[[paste0(input$variable, "_cat")]])
    
    labels <- sprintf(
      "<strong>%s</strong><br/>%s: %g%%",
      data$STNAME_SH, gsub("_", " ", input$variable), data[[input$variable]]
    ) %>% lapply(htmltools::HTML)
    
    output$map <- renderLeaflet({
      leaflet(data) %>%
        addTiles() %>%
        addPolygons(
          fillColor = ~pal(data[[paste0(input$variable, "_cat")]]),
          weight = 1,
          opacity = 1,
          color = "white",
          dashArray = "3",
          fillOpacity = 0.7,
          highlight = highlightOptions(
            weight = 2,
            color = "#666",
            dashArray = "",
            fillOpacity = 0.7,
            bringToFront = TRUE
          ),
          label = labels,
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "15px",
            direction = "auto"
          )
        ) %>%
        addLegend(pal = pal, values = data[[paste0(input$variable, "_cat")]], opacity = 0.7, title = NULL,
                  position = "topright")
    })
  })
  
  output$barPlot <- renderPlotly({
    selected_cost <- input$cost_type
    df_melted <- melt(df, id.vars = "country")
    df_filtered <- df_melted[df_melted$variable == selected_cost, ]
    
    custom_colors <- c("United States" = "#10477e",  "Pakistan" = "#10477ECC", "Nigeria" = "#10477e","Nepal" = "#10477ECC", "India" = "red","Mexico"= "#10477e", "Chile"= "#10477e", "Bangladesh"="#10477ECC" )
    
    custom_colors["India"] <- "red" 
    
    
    
    
    p <- ggplot(df_filtered, aes(x = country, y = value, fill = country)) + 
      geom_bar(stat = "identity") + 
      labs(title = paste("", gsub("_", " ", selected_cost)), x = "Country", y = "Cost in $") + 
      scale_fill_manual(values =custom_colors) + 
      coord_flip() + 
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    
    ggplotly(p, tooltip = "y") 
  })
}



shinyApp(ui,server)
