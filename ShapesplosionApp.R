#Loading Libraries
library(shiny)
library(shinythemes)
library(stringr)
library(readr)
library(ggplot2)
library(dplyr)
library(gdata)
library(broom)
 
#Shapesplosion Data
data <- read_csv("https://www.stat2games.sites.grinnell.edu/data/shapesplosion/getdata.php")

#Date Column
data <- data %>% mutate(Date = str_sub(Date, 1, 10))

#Changing NA to None for Vars
data[is.na(data)] <- "None"

#None to NA (For ANOVA)
data["None"] <- "None"
data$None <- as.factor(data$None)

#Lowercase
data$PlayerID <- tolower(data$PlayerID)
data$GroupID <- tolower(data$GroupID)

#Converting to Columns to Factor
data$PlayerID <- as.factor(data$PlayerID)
data$MatchingScheme <- as.factor(data$MatchingScheme)
data$Win <- as.factor(data$Win)
data$ReqTime <- as.factor(data$ReqTime)
data$DisplayTime <- as.factor(data$DisplayTime)
data$NumShapes <- as.factor(data$NumShapes)
data$Var1 <- as.factor(data$Var1)
data$Var2 <- as.factor(data$Var2)
data$Var3 <- as.factor(data$Var3)



##Creating Identifier Column

#Arranging Data
data <- data %>% arrange(GroupID, PlayerID, MatchingScheme, Game)

#Creating Identifier Column
data$Identifier <- NA

counter <- 1
data$Identifier[1] <- 1

for(i in 2:nrow(data)){
  
  if(data$GroupID[i] == data$GroupID[i-1] &
     data$PlayerID[i] == data$PlayerID[i-1]){
    
    data$Identifier[i] <- counter
    
  } else{
    counter <- counter + 1
    
    data$Identifier[i] <- counter
  }
}

#Creating Order Column
data$Order <- NA

counter <- 1
data$Order[1] <- 1

for(i in 2:nrow(data)){
  
  if(data$Identifier[i] == data$Identifier[i - 1]){
    counter <- counter + 1
    
    data$Order[i] <- counter 
    
  } else{
    counter <- 1
    data$Order[i] <- counter
  }
}


#### Paired MatchingScheme Data

#Only wins
dataMS <- data %>% filter(Win == "1")


#Remove players with only one row of data
#Removing players with only one type of matching scheme
Index <- numeric()

for(i in unique(dataMS$Identifier)){
  
  temp <- dataMS %>% filter(Identifier == i)
  
  if(nrow(temp) < 2){
    Index <- append(Index, i)
  }
  
  if(length(unique(temp$MatchingScheme)) == 1){
    Index <- append(Index, i)
  }
  
}

#Removing marked players
#At this point, all players left have at least 2 rows, and at least 2 different matching schemes
dataMS <- dataMS %>% filter(!(Identifier %in% Index))


#Keeping exactly two matching schemes per player
dataMS$Index <- 0

for(i in 2:nrow(dataMS)){
  
  #Set up for the very first person only
  if(i == 2){
    counter <- 1
    matchingscheme <- dataMS$MatchingScheme[1]
  }
  
  #If the players are the same and counter is 2
  if(counter == 2 & dataMS$Identifier[i - 1] == dataMS$Identifier[i]){
    dataMS$Index[i] <- 1
  }
  
  #If the players are the same and matching scheme matches the very first matching scheme for that player
  if(dataMS$Identifier[i - 1] == dataMS$Identifier[i] &
     matchingscheme == dataMS$MatchingScheme[i]){
    
    dataMS$Index[i] <- 1
  }
  
  #If the players are the same but the matching scheme does not match the very first matching scheme for that player
  if(dataMS$Identifier[i - 1] == dataMS$Identifier[i] & 
     matchingscheme != dataMS$MatchingScheme[i]){
    
    counter <- 2
  }
  
  #If the players are not the same
  if(dataMS$Identifier[i - 1] != dataMS$Identifier[i]){
    counter <-  1
    matchingscheme <- dataMS$MatchingScheme[i]
  }
}


#Keeping only the rows that were not marked
dataMS <- dataMS %>% filter(Index == 0)


#Making sure num shapes is the same
IndexNumShapes <- numeric()

for(i in unique(dataMS$Identifier)){
  
  temp <- dataMS %>% filter(Identifier == i)
  
  #One level of matching scheme
  if(nlevels(drop.levels(temp$NumShapes)) != 1){
    IndexNumShapes <- append(IndexNumShapes, i)
  }
}

#Removing marked players 
dataMS <- dataMS %>% filter(!(Identifier %in% IndexNumShapes))


#Creating Order Column for Matching Scheme Data (Sorting Finished)
dataMS$Order <- NA

counter <- 1
dataMS$Order[1] <- 1

for(i in 2:nrow(dataMS)){
  
  if(dataMS$Identifier[i] == dataMS$Identifier[i - 1]){
    counter <- counter + 1
    
    dataMS$Order[i] <- counter 
    
  } else{
    counter <- 1
    dataMS$Order[i] <- counter
  }
}


#### Paired NumShapes Data

#Only wins
dataNumS <- data %>% filter(Win == "1")

#Remove players with only one row of data
#Removing players with only one type of NumShapes
Index <- numeric()

for(i in unique(dataNumS$Identifier)){
  
  temp <- dataNumS %>% filter(Identifier == i)
  
  if(nrow(temp) < 2){
    Index <- append(Index, i)
  }
  
  if(length(unique(temp$NumShapes)) == 1){
    Index <- append(Index, i)
  }
  
}

#Removing marked players
#At this point, all players left have at least 2 rows, and at least 2 different num shapes
dataNumS <- dataNumS %>% filter(!(Identifier %in% Index))


#Keeping exactly two num shapes per player
dataNumS$Index <- 0

for(i in 2:nrow(dataNumS)){
  
  #Set up for the very first person only
  if(i == 2){
    counter <- 1
    numshape <- dataNumS$NumShapes[1]
  }
  
  #If the players are the same and counter is 2
  if(counter == 2 & dataNumS$Identifier[i - 1] == dataNumS$Identifier[i]){
    dataNumS$Index[i] <- 1
  }
  
  #If the players are the same and num shapes matches the very first num shapes for that player
  if(dataNumS$Identifier[i - 1] == dataNumS$Identifier[i] &
     numshape == dataNumS$NumShapes[i]){
    
    dataNumS$Index[i] <- 1
  }
  
  #If the players are the same but the num shapes does not match the very first num shapes for that player
  if(dataNumS$Identifier[i - 1] == dataNumS$Identifier[i] & 
     numshape != dataNumS$NumShapes[i]){
    
    counter <- 2
  }
  
  #If the players are not the same
  if(dataNumS$Identifier[i - 1] != dataNumS$Identifier[i]){
    counter <-  1
    numshape <- dataNumS$NumShapes[i]
  }
}

#Keeping only the rows that were not marked
dataNumS <- dataNumS %>% filter(Index == 0)


#Making sure matching scheme is the same
IndexMScheme <- numeric()

for(i in unique(dataNumS$Identifier)){
  
  temp <- dataNumS %>% filter(Identifier == i)
  
  #One level of matching scheme
  if(nlevels(drop.levels(temp$MatchingScheme)) != 1){
    IndexMScheme <- append(IndexMScheme, i)
  }
}

#Removing marked players 
dataNumS <- dataNumS %>% filter(!(Identifier %in% IndexMScheme))

#Creating Order Column for Num Shapes Data (Sorting Finished)
dataNumS$Order <- NA

counter <- 1
dataNumS$Order[1] <- 1

for(i in 2:nrow(dataNumS)){
  
  if(dataNumS$Identifier[i] == dataNumS$Identifier[i - 1]){
    counter <- counter + 1
    
    dataNumS$Order[i] <- counter 
    
  } else{
    counter <- 1
    dataNumS$Order[i] <- counter
  }
}

#### Paired Var Data

#Only wins
datavar <- data %>% filter(Win == "1")

#Remove players with only one row of data
#Keeping only players who played the same matching scheme more than once
Index1 <- numeric()
for(i in unique(datavar$Identifier)){
  
  temp <- datavar %>% filter(Identifier == i)
  
  #Only one row of data
  if(nrow(temp) < 2){
    Index1 <- append(Index1, i)
  }
  
  #Only one type of matching scheme
  if(sum(duplicated(temp$MatchingScheme)) == 0){
    Index1 <- append(Index1, i)
    
  }
}
#Removing marked players
datavar <- datavar %>% filter(!(Identifier %in% Index1))


#Keeping only matching schemes that are duplicated
datavar$Index <- 1

for(i in 2:nrow(datavar)){
  
  if(datavar$Identifier[i] == datavar$Identifier[i-1] &
     datavar$MatchingScheme[i] == datavar$MatchingScheme[i-1]){
    
    datavar$Index[i-1] <- 0 
    datavar$Index[i] <- 0
    
  }
}

#Removing rows with Index = 1
datavar <- datavar %>% filter(Index == 0)


#Keeping exactly one pair of data per player

#True/False column
tf <- duplicated(datavar$Identifier)
datavar <- cbind(datavar, tf)
datavar$tf <- as.factor(datavar$tf)

datavar$Index2 <- 1

for(i in 1:nrow(datavar)){
  
  if(datavar$tf[i] == "FALSE"){
    
    datavar$Index2[i] <-  0
    datavar$Index2[i + 1] <- 0
  }
  
  if(i == nrow(datavar)){
    break
  }
}
#Removing rows with Index2 == 1
datavar <- datavar %>% filter(Index2 == 0)


#Keeping only player data if the number of shapes are the same
IndexN <- numeric()

for(i in unique(datavar$Identifier)){
  
  temp <- datavar %>% filter(Identifier == i)
  
  #Not exactly one level for number of shapes
  if(nlevels(drop.levels(temp$NumShapes)) != 1){
    
    IndexN <- append(IndexN, i)
  }
}

#Removing marked players
datavar <- datavar %>% filter(!(Identifier %in% IndexN))


#Creating Order Column for datavar before creating dataset for each var
datavar$Order <- NA

counter <- 1
datavar$Order[1] <- 1

for(i in 2:nrow(datavar)){
  
  if(datavar$Identifier[i] == datavar$Identifier[i - 1]){
    counter <- counter + 1
    
    datavar$Order[i] <- counter 
    
  } else{
    counter <- 1
    datavar$Order[i] <- counter
  }
}



#Keeping if there are exactly 2 levels
#Separate checks for each of the three variables
Indexv1 <- numeric()
Indexv2 <- numeric()
Indexv3 <- numeric()


for(i in unique(datavar$Identifier)){
  
  temp <- datavar %>% filter(Identifier == i)
  
  #Var 1 Check
  if(nlevels(drop.levels(temp$Var1)) != 2){
    Indexv1 <- append(Indexv1, i)
  }
  
  #Var2 Check
  if(nlevels(drop.levels(temp$Var2)) != 2){
    Indexv2 <- append(Indexv2, i)
  }
  
  #Var3 Check
  if(nlevels(drop.levels(temp$Var3)) != 2){
    Indexv3 <- append(Indexv3, i)
  }
}

#Creating filtered Data set for each variable
datav1 <- datavar %>% filter(!(Identifier %in% Indexv1))
datav2 <- datavar %>% filter(!(Identifier %in% Indexv2))
datav3 <- datavar %>% filter(!(Identifier %in% Indexv3))




## Win Data
datawin <- data %>% filter(Win == "1")

#Creating Order Column for Win Data
datawin$Order <- NA

counter <- 1
datawin$Order[1] <- 1

for(i in 2:nrow(datawin)){
  
  if(datawin$Identifier[i] == datawin$Identifier[i - 1]){
    counter <- counter + 1
    
    datawin$Order[i] <- counter 
    
  } else{
    counter <- 1
    datawin$Order[i] <- counter
  }
}




#For UI Inputs
all_groups <- sort(unique(data$GroupID))




#### UI
ui <- fluidPage(
  
  theme = shinytheme("readable"),
  
  titlePanel("Shapesplosion Hypothesis Tests"),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput(inputId = "groupID",
                  label = "Group ID:", 
                  choices =  all_groups,
                  multiple = TRUE,
                  selectize = TRUE,
                  selected = "test"),
      
      uiOutput(outputId = "player_out"),
      
      selectInput(inputId = "xvar",
                  label = "X Variable:",
                  choices = c("MatchingScheme", "NumShapes", "Var1", "Var2", "Var3", "PlayerID"),
                  selected = "MatchingScheme",
                  multiple = FALSE),
      
      selectInput(inputId = "yvar",
                  label = "Y Variable:",
                  choices = c("Time", "NumClicks"),
                  selected = "Time",
                  multiple = FALSE),
      
      selectInput(inputId = "color",
                  label = "Color by:",
                  choices = c("MatchingScheme", "NumShapes", "Var1", "Var2", "Var3", 
                              "ReqTime", "DisplayTime", "Win"),
                  selected = "MatchingScheme",
                  multiple = FALSE),
      
      selectInput(inputId = "facets",
                  label = "Facet by:",
                  choices = c("None", "MatchingScheme", "NumShapes", "Var1", "Var2", "Var3", 
                              "ReqTime", "DisplayTime", "Win"),
                  selected = "None",
                  multiple = FALSE),
      
      checkboxInput(inputId = "bplot",
                    label = "Add Boxplot",
                    value = FALSE),
      
      
      selectInput(inputId = "datatype",
                  label = "Data Type:",
                  choices = c("All Data", "Paired MatchingScheme Data", "Paired NumShapes Data",
                              "Paired Var1 Data", "Paired Var2 Data", "Paired Var3 Data", "Win Data"),
                  selected = "All Data",
                  multiple = FALSE),
      
      selectInput(inputId = "tests",
                  label = "Statistical Tests:",
                  choices = c("None", "Two Sample T-Test", "Paired T-Test", "ANOVA", "Block Design"),
                  selected = "None",
                  multiple = FALSE),
      
      checkboxInput(inputId = "summarytable",
                    label = "Show Summary Table",
                    value = FALSE),
      
      downloadButton('downloadData', label = "Shapesplosion Data")),
    
    
    mainPanel(
      uiOutput("message"),
      plotOutput("plot"),
      verbatimTextOutput("tests_out"),
      tableOutput("summary")
     
    )
      
    
  ))
                  

  
#### Server
server <- function(input, output,session){
  
  
  ##Reactive Data
  plotDataR <- reactive({
    
    
    #All Data
    if(input$datatype == "All Data"){
      dataR <- data
      
      #Win Data
    } else if(input$datatype == "Win Data"){
      dataR <- datawin
      
      #Paired Matching Scheme Data
    } else if(input$datatype == "Paired MatchingScheme Data"){
       dataR <- dataMS
       
      #Paired NumShapes Data
    } else if(input$datatype == "Paired NumShapes Data"){
      dataR <- dataNumS
      
      #Paired Var 1 Data
    } else if(input$datatype == "Paired Var1 Data"){
      dataR <- datav1
      
      #Paired Var 2 Data
    } else if(input$datatype == "Paired Var2 Data"){
      dataR <- datav2
      
      #Paired Var 3 Data
    } else if(input$datatype == "Paired Var3 Data"){
      dataR <- datav3
    }
      
    dataR <- dataR %>% filter(GroupID %in% input$groupID, !(PlayerID %in% input$playerID))
    
    return(dataR)
  })
  
  
  ##Dynamic Remove Player Input
  output$player_out <- renderUI({
    
    input_data <- data %>% filter(GroupID %in% input$groupID)
    players <- sort(unique(input_data$PlayerID))
    
    selectInput(inputId = "playerID",
                label = "Remove Player ID:",
                choices =  players,
                multiple = TRUE,
                selectize = TRUE)
    
  })
  
  
  ##Visualization
  output$plot <- renderPlot({
    
    #Require
    req(input$groupID)
    
    #Reactive Data
    plotData <- plotDataR()
    
    #We must have data points
    if(nrow(plotData) > 0){
      
    #There must be data for the color variable
      colorcheck <- plotData %>% pull(input$color)
      
      if(all(is.na(colorcheck)) == FALSE){
      
      #General Plot
    
      #Boxplot checkbox not selected
      if(input$bplot == FALSE){
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color = input$color)) +
          geom_point(position=position_dodge(width = 0.75), size = 2) +
          labs(title = paste("Plot of",input$yvar, "by",input$xvar, "and Colored by", input$color)) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 70, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14)) 
        
        
        #Boxplot checkbox is selected
      } else{
        myplot <- ggplot(data = plotData, aes_string(x = input$xvar, y = input$yvar, color = input$color)) +
          geom_boxplot() +
          geom_point(position=position_dodge(width = 0.75), size = 2) +
          labs(title = paste("Plot of",input$yvar, "by",input$xvar, "and Colored by", input$color)) +
          theme_bw() +
          theme(axis.text.x = element_text(size = 18, angle = 70, hjust = 1), 
                axis.title = element_text(size = 20), 
                plot.title = element_text(size = 20, face = "bold", hjust = 0.5),
                legend.title = element_text(size = 18), 
                legend.text = element_text(size = 16), 
                axis.text.y = element_text(size = 14)) 
      }
      
      
      #Facet Option is selected
      if(input$facets != "None"){
        myplot <- myplot +
          facet_wrap(as.formula(paste("~", input$facets))) +
          labs(title = paste("Plot of",input$yvar, "by", input$xvar, "and Colored by", input$color, "and Faceted by", input$facets)) +
          theme(strip.text = element_text(size = 16)) 
      }
      
      return(myplot)
    
        
      #Error message because there is nothing to color by
      } else {
      
       output$message <- renderUI({
         HTML(paste(em("Please select a different Color Variable.")))
       })
         
    }
  }
})
  

  
##Statistical Tests
output$tests_out <- renderPrint({
  
  #Reactive Data
  plotData <- plotDataR()
  
  #We must have data points
  if(nrow(plotData) > 0){
  
    #Pulling 
    XVariable <- plotData %>% pull(input$xvar)
    XVariable <- drop.levels(XVariable)
    XLevels <- nlevels(XVariable)
    YVariable <- plotData %>% pull(input$yvar)
    ColorVariable <- plotData %>% pull(input$color)
    ColorVariable <- drop.levels(ColorVariable)
    ColorLevels <- nlevels(ColorVariable)
    
    
    ##Two sample T test
    if(input$tests == "Two Sample T-Test"){
      
      #Facet must be set to none
      if(input$facets == "None"){
        
        #X Variable and color variable must be the same
        if(input$xvar == input$color){
          
          #X Variable must have exactly two levels
          if(XLevels == 2){
            
            #Each group needs more than 1 observation
            check <- plotData %>% group_by_at(input$xvar) %>%
              summarize(Count = n())
            
            if(!(1 %in% check$Count)){
              t.test(YVariable ~ XVariable)
            
            } else {
              "Not enough observations to run the Two Sample T-Test."
              }
            
          } else{
            "T-Tests are only valid when there are exactly 2 groups."
          }
          
        } else{
          "The X Variable and the Color Variable must be the same."
        }
        
      } else{
        "Facet option must be set to None to run the Two Sample T-Test."
      }
    
  
      
    ##Paired T Test
    } else if(input$tests == "Paired T-Test"){
      
      #Data Type must be paired
      if(input$datatype %in% c("Paired MatchingScheme Data", "Paired NumShapes Data", 
                               "Paired Var1 Data", "Paired Var2 Data", "Paired Var3 Data")){
        
        #Facet must be set to none
        if(input$facets == "None"){
          
          #X Variable and color variable must be the same
          if(input$xvar == input$color){
            
            #X Variable must have exactly two levels
            if(XLevels == 2){
              
              #Each group needs more than 1 observation
              check <- plotData %>% group_by_at(input$xvar) %>%
                summarize(Count = n())
              
              if(!(1 %in% check$Count)){
              t.test(YVariable ~ XVariable, paired = TRUE)
                
              } else {
                "Not enough observations to run the Paired T-Test."
              } 
              
            } else{
              "T-Tests are only valid when there are exactly 2 groups."
            }
            
          } else{
            "The X Variable and the Color Variable must be the same."
          }
          
        } else{
          "Facet option must be set to None to run the Paired T-Test."
        }
        
      } else{
        "Paired Data must be selected to run the Paired T-Test."
      }
      
      
      
    ##ANOVA
    } else if(input$tests == "ANOVA"){
      
      #Pulling Facet Variable
      FacetVariable <- plotData %>% pull(input$facets)
      FacetVariable <- drop.levels(FacetVariable)
      FacetLevels <- nlevels(FacetVariable)
      
      
      #We need enough observations
      if(nrow(plotData) >= XLevels*ColorLevels*FacetLevels + 1){
      
        
        if(XLevels > 1 &  ColorLevels  > 1 & FacetLevels > 1){
      
          anovatest <- aov(YVariable ~ XVariable + ColorVariable + FacetVariable +
                             XVariable*ColorVariable +
                             XVariable*FacetVariable +
                             ColorVariable*FacetVariable)

        } else if(XLevels > 1 &  ColorLevels  > 1 & FacetLevels < 2){
          
          
          anovatest <-  aov(YVariable ~ XVariable + ColorVariable +
                              XVariable*ColorVariable)
          
          
          
        } else if(XLevels > 1 &  ColorLevels  < 2 & FacetLevels > 1 ){
          
          
          anovatest <-  aov(YVariable ~ XVariable + FacetVariable +
                              XVariable*FacetVariable)
          
          
          
        } else if(XLevels > 1 &  ColorLevels < 2 & FacetLevels < 2){
          
          anovatest <-  aov(YVariable ~ XVariable)
          
        } else if(XLevels < 2 &  ColorLevels  > 1 & FacetLevels > 1){
          
          
          anovatest <-  aov(YVariable ~ ColorVariable + FacetVariable +
                              ColorVariable*FacetVariable)
          
          
        } else if(XLevels < 2 &  ColorLevels > 1 & FacetLevels < 2){
          
          anovatest <-  aov(YVariable ~ ColorVariable)
          
        } else if(XLevels < 2 &  ColorLevels  < 2 & FacetLevels > 1){
          
          anovatest <-  aov(YVariable ~ FacetVariable)
          
        } else if(XLevels < 2 &  ColorLevels  < 2 & FacetLevels < 2){
          
          message <- "At least two levels are needed to run the ANOVA"
          
        }
        
        #Returning Error message
        if(XLevels < 2 & ColorLevels < 2 & FacetLevels < 2){
          
          return(message)
          
        } else{
          
          #Making Tidy table and adding columns/rows
          tidyanova = tidy(anovatest)
          sum_df = sum(tidyanova$df)
          sum_ss = sum(tidyanova$'sumsq')
          tidyanova = add_row(tidyanova,term = "Total", df = sum_df, sumsq = sum_ss)
          tidyanova$sumsq = round(tidyanova$sumsq, digits = 2)
          tidyanova$meansq = round(tidyanova$meansq, digits = 2)
          tidyanova$statistic = round(tidyanova$statistic, digits = 2)
          
          return(tidyanova)
        }
        
      #Error Message when there aren't enough observations   
      } else{
        "Not enough observations to run the ANOVA."
      }
        
      
     ##Block Design   
    } else if(input$tests == "Block Design"){
      
      #Pulling Facet Variable and PlayerID Variable
      FacetVariable <- plotData %>% pull(input$facets)
      FacetVariable <- drop.levels(FacetVariable)
      FacetLevels <- nlevels(FacetVariable)
      PlayerID <- plotData$PlayerID
      
      #We need enough observations
      if(nrow(plotData) >= XLevels*ColorLevels*FacetLevels + 1){
      
        if(XLevels > 1 &  ColorLevels  > 1 & FacetLevels > 1){
          
          anovatest <- aov(YVariable ~ PlayerID + XVariable + ColorVariable + FacetVariable + 
                             XVariable*ColorVariable + 
                             XVariable*FacetVariable +
                             ColorVariable*FacetVariable)
          
        } else if(XLevels > 1 &  ColorLevels  > 1 & FacetLevels < 2){
          
          
          anovatest <-  aov(YVariable ~ PlayerID + XVariable + ColorVariable +
                              XVariable*ColorVariable)
          
          
          
        } else if(XLevels > 1 &  ColorLevels  < 2 & FacetLevels > 1 ){
          
          
          anovatest <-  aov(YVariable ~ PlayerID + XVariable + FacetVariable +
                              XVariable*FacetVariable)
          
          
          
        } else if(XLevels > 1 &  ColorLevels < 2 & FacetLevels < 2){
          
          anovatest <-  aov(YVariable ~ PlayerID + XVariable)
          
        } else if(XLevels < 2 &  ColorLevels  > 1 & FacetLevels > 1){
          
          
          anovatest <-  aov(YVariable ~ PlayerID + ColorVariable + FacetVariable +
                              ColorVariable*FacetVariable)
          
          
        } else if(XLevels < 2 &  ColorLevels > 1 & FacetLevels < 2){
          
          anovatest <-  aov(YVariable ~ PlayerID + ColorVariable)
          
        } else if(XLevels < 2 &  ColorLevels  < 2 & FacetLevels > 1){
          
          anovatest <-  aov(YVariable ~ PlayerID + FacetVariable)
          
        } else if(XLevels < 2 &  ColorLevels  < 2 & FacetLevels < 2){
          
          message <- "At least two levels are needed to run the ANOVA"
          
        }
        
        #Returning Error message
        if(XLevels < 2 & ColorLevels < 2 & FacetLevels < 2){
          
          return(message)
          
        } else{
          
          #Making Tidy table and adding columns/rows
          tidyanova = tidy(anovatest)
          sum_df = sum(tidyanova$df)
          sum_ss = sum(tidyanova$'sumsq')
          tidyanova = add_row(tidyanova,term = "Total", df = sum_df, sumsq = sum_ss)
          tidyanova$sumsq = round(tidyanova$sumsq, digits = 2)
          tidyanova$meansq = round(tidyanova$meansq, digits = 2)
          tidyanova$statistic = round(tidyanova$statistic, digits = 2)
          
          return(tidyanova)
        }
        
      #Error Message when there aren't enough observations    
      } else{
        "Not enough observations to run the Block Design."
      }
        
    }
  } 
  
})


#Summary Table
output$summary <- renderTable({

  #Reactive Data
  plotData <- plotDataR()
  
  if(input$summarytable == TRUE){
  
  #Y Variable
  yvar <- sym(input$yvar)

  table <- plotData %>% group_by_at(input$xvar) %>%
    summarize(N = n(), Mean = mean(!!yvar), SD = sd(!!yvar))

  return(table)
  
  }
})


#Removing Error Message for ggplot
observeEvent(input$color, {
  
  #Reactive Data
  plotData <- plotDataR()
  
  #There must be data for the color variable
  colorcheck <- plotData %>% pull(input$color)
  
  #If there is at least one thing to color by
  if(all(is.na(colorcheck)) == FALSE){
    output$message <- renderUI({""})
  }
  
})


#Download Data
output$downloadData <- downloadHandler(
  filename = function() {
    paste('Data-', Sys.Date(), '.csv', sep="")
  },
  content = function(con) {
    write.csv(plotDataR(), con)
  })

  
#Closes Server
}


#Running Shiny App
shinyApp(ui = ui, server = server)