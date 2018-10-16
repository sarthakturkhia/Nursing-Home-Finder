# loading the different libraries required
library(leaflet)
library(DT)
library(dplyr)
library(ggplot2)
library(shiny)

# using the data set for state map and names required for filtering
data(state.map)
data(state)

# loading the data for Ratings of the Nursing Homes
ratings <- read.csv("data/Star_Ratings.csv", stringsAsFactors = FALSE)

# loading the data for the Penalties incurred by the Nursing Homes
penalty <- read.csv("data/Penalties.csv", stringsAsFactors = FALSE)

# loading the data for the Penalties incurred by the Nursing Homes
houses <- read.csv("data/Provider_Info.csv", stringsAsFactors = FALSE) 

# loading the data for the Penalties incurred by the Nursing Homes
houses.info <- houses %>% select(Federal.Provider.Number, Provider.Name, Provider.Phone.Number, 
                                 Provider.Address, Provider.City, Provider.State, 
                                 Number.of.Fines, Overall.Rating, Location)

# renaming the column names for user readability
colnames(houses.info) <- c("Federal.Provider.Number", "Name", "Phone.Number", "Address", "City", 
                           "State", "Number.of.Fines", "Overall.Rating", "Location")

# creating a new data set selecting the specific statistics for each house
general.info <- houses %>% select(Provider.Name, Provider.Phone.Number, 
                                  Provider.Address, Provider.City, Provider.State,
                                  Number.of.Certified.Beds, 
                                  Number.of.Residents.in.Certified.Beds,
                                  Reported.CNA.Staffing.Hours.per.Resident.per.Day,
                                  Reported.LPN.Staffing.Hours.per.Resident.per.Day,
                                  Reported.RN.Staffing.Hours.per.Resident.per.Day, 
                                  Reported.Total.Nurse.Staffing.Hours.per.Resident.per.Day,
                                  Expected.CNA.Staffing.Hours.per.Resident.per.Day,
                                  Expected.LPN.Staffing.Hours.per.Resident.per.Day,
                                  Expected.RN.Staffing.Hours.per.Resident.per.Day, 
                                  Expected.Total.Nurse.Staffing.Hours.per.Resident.per.Day)

# renaming the column names for user readability
colnames(general.info) <- c("name", "phone.number", "address", "city", "state", 
                            "certified.beds", "residents.in.beds", "reported.cna",
                            "reported.lpn", "reported.rn", "reported.total",
                            "expected.cna", "expected.lpn", "expected.rn", "expected.total")

# help assign whether the houses have a penalty or not, depending on the number of fines
fines <- c()

for(i in 1:nrow(houses.info)){
  if(houses.info$Number.of.Fines[i] == "0"){
    fines <- append(fines, "No")
  }else{
    fines <- append(fines, "Yes")
  }
}

# adding the above vector as a column in the provider information table
houses.info <- mutate(houses.info, Fines = fines) %>% select(-Number.of.Fines, 
                                                             -Federal.Provider.Number)

# defining the server function
server <- function(input, output) {
  
  # creating the table for display, which changes according to the state filter
  table.filter <- reactive({
    
    # changing the data set if it is not national, according to the input of the state
    if (input$state != "National") {
      houses.info <- filter(houses.info, State == state.abb[match(input$state, state.name)])
    }
    
    # displaying the contact information and basic rating and penalties for the nursing 
    # homes selected
    houses.info <- select(houses.info, Name, Phone.Number, Address, City, State, Fines, 
                          Overall.Rating, Location)
    
    # filtering the table according the filter chosen
    houses.info <- houses.info %>% filter(Overall.Rating >= input$ratings[1] & 
                                            Overall.Rating <= input$ratings[2]) 
    
    # changing the table on the basis of penalty option (with, without or all)
    if(input$radio == 1){
      houses.info<- houses.info %>% filter(Fines == "Yes")
    }else if(input$radio == 2){
      houses.info<- houses.info %>% filter(Fines == "No")
    }else{
      houses.info <- select(houses.info, Name, Phone.Number, Address, City, State, Fines, 
                            Overall.Rating, Location)
    }
    return(houses.info)
  })
  
  # displaying the introduction for our project tab
  output$introduction <- renderPrint({
    div (
      HTML("<h2> Our Mission: </h2>
           <br/>
           Nursing Homes are one of the fastest growing practices in the country today. With 60,000 
           nursing homes currently running in the country, it is difficult to find one that best suits 
           everyone's needs. The nursing home finder is the end to all of your problems. This project 
           is intended to ease the search process and help adults (25 and above) find a perfect 
           nursing home for their parent(s). We have based this project on the official datasets 
           available on the Medicare.gov   Nursing Home Compare Website provided by the Centers 
           for Medicare & Medicaid Services. This dataset allows one to compare the quality of care at 
           every Medicare and Medicaid-certified Nursing Home currently running in the country.
           <br/> <br/>
           <h2> Navigating through the application: </h2> <br/>
           This tool will help you compare nursing homes in 50 states. Initially you would see 
           the national map with a table showing the basic provider information (Name, Phone 
           Number, Address, City, State, Fines, and Overall Rating). You can choose a state from 
           the drop down on the side, and you would see a close-up map of your state, with location
           maps for each nursing home. You can hover over the location pins to see the names of 
           the nursing homes. Below the map, the table is filtered to the specific state's
           nursing homes. You can also filter the overall rating (from 1 to 5), and see the nursing 
           homes with/without fines or all of them, according to your choices. This will further 
           filter the location points and the table. 
           <br/> <br/>
           You can select a specific nursing home at a time, and then toggle between the General 
           Provider Information, Ratings and Penalties and Other Information for the specific 
           nursing home on the side. The General Information tab will show the Name, Address, 
           and the Phone Number of the nursing homes. The Ratings and Penalties tab will show 
           more specific ratings and penalties such as Overall Rating, Health Inspection Rating, 
           Staffing Rating, Registered Nurse Staffing Rating, and the Total Amount of fine due. 
           The Other Information tab has information on the maximum capacity of residents (that is,
           the number of certified beds), the current residents, the reported hours out of expected
           hours for Certified Nursing Assistant(CNA), Licensed Practical Nurses (LPN), 
           Registered Nurses(RN) and for all nurses. 
           <br/> <br/>
           You can select a specific nursing home at a time, and then toggle between the General 
           Provider Information, Ratings and Penalties and Other Information for the specific 
           nursing home on the side. The General Information tab will show the Name, Address 
           and the Phone Number of the nursing homes. The Ratings and Penalties tab will show more
           specific ratings and penalties such as Overall Rating, Health Inspection Rating, 
           Staffing Rating, Registered Nurse Staffing Rating and the Total Amount of fine due. 
           The Other Information tab has information on the maximum capacity of residents(that 
           is, the number of certified beds), the current residents, the reported hours out of 
           expected hours for Certified Nursing Assistant(CNA), Licesened Practical Nurses (LPN),
           Registered Nurses(RN) and for all nurses.
           <br/> <br/>
           You also have the ability to search the table and select the number of specific 
           entries you would like to see in the table. There is a glossary available for you to 
           help understand the difference between the different types of Nurses available and 
           their respective information given in the table.
           <br/> <br/>
           You also have the ability to search the table and select the number of specific entires
           you would like to see in the table. There is a glossary available for you to help 
           understand the difference between the different types of Nurses available and their 
           respective information given in the table.")
      )
  })
  
  # displays the summary of the pie chart for clear understanding of the pie chart
  output$pie.chart.summary <- renderPrint({
    
    # changes the data set according to the state chosen by the user
    if (input$state != "National") {
      houses.info <- filter(houses.info, State == state.abb[match(input$state, state.name)])
    }
    
    # selects the specific information required for the provider nursing homes in the chosen state
    houses.info <- select(houses.info, Name, Phone.Number, Address, City, State, Fines, 
                          Overall.Rating, Location)
    
    # filters the data based on each overall rating value
    rating.1 <- houses.info %>% filter(Overall.Rating == "1")
    
    rating.2 <- houses.info %>% filter(Overall.Rating == "2")
    
    rating.3 <- houses.info %>% filter(Overall.Rating == "3")
    
    rating.4 <- houses.info %>% filter(Overall.Rating == "4")
    
    rating.5 <- houses.info %>% filter(Overall.Rating == "5")
    
    # counts the number of rows for each filtered overall rating data set
    num.rows <- c(nrow(rating.1), nrow(rating.2), nrow(rating.3), nrow(rating.4), nrow(rating.5))
    
    # displays the summary of the pie chart according to the state chosen and the 
    # state's specific data
    cat("This is a pie chart showing the frequency of ratings in", input$state, 
        "that consists of 5 colors, 1 for each rating: red for 1 star rating, 
        orange for 2 star rating, yellow for 3 star rating, light green for 4 star rating, 
        and dark green for 5 star rating. The frequency of 1 star rating was", num.rows[1], 
        ", the frequency of 2 star rating was", num.rows[2], ", 
        the frequency of 3 star rating was", num.rows[3], 
        ", the frequency of 4 star rating was", num.rows[4], ", 
        the frequency of 5 star rating was", num.rows[5], ".")
  })
  
  # displays the summary of the scatter plot for clear understanding of the pie chart
  output$point.graph.summary <- renderPrint({
    
    # converts the string to the numeric form to help graph the scatter plot
    
    # strips the total amount of fine from the dollar sign
    col <- sub("\\$","", houses$Total.Amount.of.Fines.in.Dollars)
    
    #strips the decimal value from the above column
    col <- sub("\\.00", "", col)
    
    # adds a new column to the total amount of fines using the above nurmeric column change
    houses <- mutate(houses, Total.Fines = as.numeric(col))
    
    # filters the house data set according to the state chosen by the user
    if (input$state != "National") {
      data <- filter(houses, Provider.State == state.abb[match(input$state, state.name)])
      data <- na.omit(data)
    } else {
      data <- na.omit(houses)
    }
    
    # selects the specific information required for the provider nursing homes in the chosen state
    houses.info <- select(data, Provider.Name, Provider.Phone.Number, Provider.Address, 
                          Provider.City, Provider.State, Overall.Rating, 
                          Location, Total.Fines)
    
    # filters the data based on each overall rating value
    rating.1 <- houses.info %>% filter(Total.Fines != 0, Overall.Rating == "1")
    
    rating.2 <- houses.info %>% filter(Total.Fines != 0, Overall.Rating == "2")
    
    rating.3 <- houses.info %>% filter(Total.Fines != 0, Overall.Rating == "3")
    
    rating.4 <- houses.info %>% filter(Total.Fines != 0, Overall.Rating == "4")
    
    rating.5 <- houses.info %>% filter(Total.Fines != 0, Overall.Rating == "5")
    
    # counts the number of rows for each filtered overall rating data set
    num.rows <- c(nrow(rating.1), nrow(rating.2), nrow(rating.3), nrow(rating.4), nrow(rating.5))
    means <- c(mean(rating.1$Total.Fines), mean(rating.2$Total.Fines), mean(rating.3$Total.Fines),
               mean(rating.4$Total.Fines), mean(rating.5$Total.Fines))
    
    # displays the summary of the scatter plot depending on the state chosen and the state's 
    # nursing home data
    cat("This is a scatter plot that shows the relation between total 
        amount of fines and the home ratings in", input$state, ". The points on 
        the graph are colored in shades of blue based on the rating; the higher 
        the rating, the ligher shade of blue the point is. 1 star rating has", num.rows[1],
        "fines, with the mean amount of", "$", means[1], 
        ". 2 star rating has",  num.rows[2], "fines, with the mean amount of",
        "$", means[2], ". 3 star rating has", num.rows[3], "fines, with the mean amount of",
        "$", means[3], ". 4 star rating has",  num.rows[4], "fines, with the mean amount of",
        "$", means[4], ". And 5 star rating has", num.rows[5], "fines, with the mean amount of",
        "$", means[5], ". As shown on the graph, the higher the ratings 
        are, the fewer fines there are and less amount of money is owed. ")
    
  })
  
  # displays the logo 
  output$logo <- renderImage({
    list(src = "data/info201logo.png",
         contentType = 'image/png',
         width = 500,
         height = 200,
         alt = "Unable to display image")
  }, deleteFile = FALSE)
  
  # displaying the map of the country or state according to the filter chosen
  output$lemap <- renderLeaflet({
    
    # filtering the table according to the location chosen
    houses.data <- table.filter()
    houses.data.location <- houses.data$Location
    
    # vectors for longititude and latitude
    long <- vector()
    lat <- vector()
    
    # Extracts coordinates from given location information
    for (i in 1:length(houses.data$Location)) {
      long <- c(long, as.numeric(unlist(strsplit(unlist(strsplit(houses.data$Location[i],
                                                                 "\n"))[3], "[(),]"))[3]))
      lat <- c(lat, as.numeric(unlist(strsplit(unlist(strsplit(houses.data$Location[i],
                                                               "\n"))[3], "[(),]"))[2]))
    }
    
    # a data frame for the name and the overall rating of the nursing home
    points <- na.omit(data.frame(houses.data$Name, houses.data$Overall.Rating, long, lat,
                                 stringsAsFactors = FALSE))
    
    # sets the icons for the nursing houses shown on the map, according to state chosen by the user
    if (input$state != "National") {
      icon <- makeIcon(
        iconUrl = "data/pin.png",
        iconWidth = 60, iconHeight = 50
      )
      
      # sets the map according to the nursing homes, setting the view according to latitude 
      # and longitude chosen, adds markers and labels for each nursing home
      m <- leaflet(data = points) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(points$long[1], points$lat[1], zoom = 6) %>%
        addMarkers(~long, ~lat, label = ~houses.data.Name, icon = icon)
    } else {
      
      # setting icons for the nursing houses shown on the map, according to the country
      icon <- makeIcon(
        iconUrl = "data/pin.png",
        iconWidth = 30, iconHeight = 20
      )
      
      # sets the map according to the nursing homes, setting the view according to
      # latitude and longitude of the entire country, adds markers and labels for 
      # each nursing home
      m <- leaflet(data = points) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(-95.712891, 37.090240, zoom = 4) %>% 
        addMarkers(~long, ~lat, label = ~houses.data.Name, icon = icon)
    }
    return(m)
  })
  
  # summary for number of nursing homes shown in the map of the state chosen
  output$summary <- renderText({
    num.homes <- nrow(table.filter())
    paste("There are", num.homes, "homes in", input$state)
  })
  
  # shows the table with provider information for the nursing homes of the chosen states
  output$table <- DT::renderDataTable(select(table.filter(), -Location), server = TRUE,
                                      selection = "single")
  
  # prints the contact information summary for the chosen specific house
  output$general <- renderPrint({
    
    # prints the contact information summary for the chosen specific house
    s <- input$table_rows_selected
    
    # the name of the selected nursing home
    name <- houses$Provider.Name[s]
    
    # the address of the selected nursing home
    address <- houses$Provider.Address[s]
    
    # the city of the selected nursing home
    city <- houses$Provider.City[s]
    
    # the State of the selected nursing home
    state <- houses$Provider.State[s]
    
    # the contact phone number of the selected nursing home
    phone <- houses$Provider.Phone.Number[s]
    
    # the zip code of the location of the selected nursing home
    zip <- houses$Provider.Zip.Code[s]
    
    # outputs the summary of the address and phone number of the selected nursing home
    if (length(s)) {
      cat(name, "\n\n", 
          "Address: ",address, ", ", city, ", ", state, zip, "\n\n",
          "Phone Number: ", phone )
    }
    else {
      
      # output if no specific nursing home chosen by the user
      cat("Select an observation on the table below for general data summary.")
    }
  })
  
  # prints the rating summary of the selected nursing home by the user
  output$ratings <- renderPrint({
    
    # represents the selected nursing home by the user
    s <- input$table_rows_selected
    
    # the overall rating of the selected nursing home
    overall <- ratings$Overall.Rating[s]
    
    # the health inspection rating of the selected nursing home
    health <- ratings$Health.Inspection.Rating[s]
    
    # the staffing  rating of the selected nursing home
    staff <- ratings$Staffing.Rating[s]
    
    # the registered nurse rating of the selected nursing home
    rn <- ratings$RN.Staffing.Rating[s]
    
    # prints the summary of the overall rating, the health inspection rating, 
    # the staffing rating and the registered nurses staffing rating
    if (length(s)) {
      cat("Overall Rating: ",overall, "\n\n", 
          "Health Inspection Rating: ", health, "\n\n",
          "Staffing Rating: ",staff, "\n\n", 
          "RN Staffing Rating: ", rn)
    }
    else {
      
      # output if no selected nursing home is chosen by user
      cat("Select an observation on the table below for ratings/penalty data summary.")
    }
  })
  
  # prints the summary of the penalties charged to the selected nursing home by the user
  output$penalties <- renderPrint({
    
    # represents the selected nursing home by the user
    s <- input$table_rows_selected
    
    # the type of penalty charged by the selected nursing home
    type <- penalty$Penalty.Type[s]
    
    # the amount of fine paid by the selected nursing home
    amount <- penalty$Fine.Amount[s]
    
    # the total amount of fines in dollars paid by the selected nursing home
    total <- houses$Total.Amount.of.Fines.in.Dollars[s]
    
    # outputs the total amount of fines for the selected nursing home, depending 
    # whether the nursing home has a fine or not
    if (length(s)) {
      if(houses.info$Fines[s] == "Yes"){
        cat("Total amount of fines in dollars:", total)
      }else{
        cat("No fines")
      }
    }
  })
  
  # outputs the summary of the important relavent general information not covered in the 
  # above information for the selected nursing home by the user
  output$other <- renderPrint({
    
    #  represents the selected nursing home by the user
    s <- input$table_rows_selected
    
    # the maximum capacity in the selected nursing home
    number.beds <- general.info$certified.beds[s]
    
    # the number of current residents in the selected nursing home
    residents.in.beds <- general.info$residents.in.beds[s]
    
    # the reported working hours of the certified nurse for the selected nursing home
    reported.cna <- round(general.info$reported.cna[s], 2)
    
    # the reported working hours of the licensed practical nurse for the 
    # selected nursing home
    reported.lpn <- round(general.info$reported.lpn[s], 2)
    
    # the reported working hours of the registered nurse for the selected nursing home
    reported.rn <- round(general.info$reported.rn[s], 2)
    
    # the reported working hours of all of the nurses for the selected nursing home
    reported.total <- round(general.info$reported.total[s], 2)
    
    # the expectecd working hours for the certified nurses in the selected nursing home
    expected.cna <- round(general.info$expected.cna[s], 2)
    
    # the expectecd working hours for the licensed practical nurses in the selected nursing home
    expected.lpn <- round(general.info$expected.lpn[s], 2)
    
    # the expectecd working hours for the registered nurses in the selected nursing home
    expected.rn <- round(general.info$expected.rn[s], 2)
    
    # the expectecd working hours for all of the nurses in the selected nursing home
    expected.total <- round(general.info$expected.total[s], 2)
    
    # prints the above information for the selected nursing home
    if (length(s)) {
      cat("Number of Residents in Certified Beds: ", residents.in.beds, "\n\n",
          "Number of Certified Beds: ", number.beds, "\n\n",
          reported.cna, "reported CNA hours out of", expected.cna, "expected hours", "\n\n",
          reported.lpn, "reported LPN hours out of", expected.lpn, "expected hours", "\n\n",
          reported.rn, "reported RN hours out of", expected.rn, "expected hours", "\n\n",
          reported.total, "reported total nurse hours out of", expected.total, "expected hours", 
          "\n\n")
    }
    else {
      
      # output if no nursing home selected by user
      cat("Select an observation on the table below for data summary.")
    }
  })
  
  # outputs a pie chart for the frequencies of overall ratings for the nursing homes in the selected location
  output$pie <- renderPlot({
    
    # according to the selected state it filters the information for the bursing home
    if (input$state != "National") {
      data <- filter(houses.info, State == state.abb[match(input$state, state.name)])
      data <- na.omit(data)
    } else {
      data <- na.omit(houses.info)
    }
    
    # plots a pie chart for the overall rating frequency of nursing homes in each state
    pie <- ggplot(data, mapping = aes(x = factor(1), fill = factor(Overall.Rating))) + 
      geom_bar(width = 1) + 
      coord_polar(theta = "y") + scale_fill_brewer(palette = "RdYlGn") + 
      labs(x = "", y = "", fill = "Overall Rating")
    return(pie)
  })
  
  # outputs a scatter plot comparing the overall ratings with the maximum penalty frequency in that rating, 
  # for the nursing homes in the filtered state
  output$bar <- renderPlot({
    
    # converts the string to the numeric form to help graph the scatter plot
    
    # strips the total amount of fine from the dollar sign
    col <- sub("\\$","", houses$Total.Amount.of.Fines.in.Dollars)
    
    # strips the decimal value from the above column
    col <- sub("\\.00", "", col)
    
    # finally converts the string to the numeric and dividing by 1000 to help 
    # graph the numbers relatively
    col <- as.numeric(col) / 10000
    
    # adds a new column to the total amount of fines using the above nurmeric column change
    houses <- mutate(houses, Total.Fines = col)
    
    # filters the house data set according to the state chosen by the user
    if (input$state != "National") {
      data <- filter(houses, Provider.State == state.abb[match(input$state, state.name)])
      data <- na.omit(data)
    } else {
      data <- na.omit(houses)
    }
    
    # plots the scatter plot using the overall rating and the maximum penalty for the nursing
    # homes in the state chosen
    bar <- ggplot(data, mapping = aes(x = Overall.Rating, y = Total.Fines, 
                                      color = Overall.Rating)) + geom_point(size = 5) +
      labs(x = "Overall Rating", y = "Fine in (Ten-Thousand) Dollars", 
           fill = "Overall Rating") + guides(color = FALSE) +
      theme(axis.title = element_text(size = 16))
    return(bar)
  })
  
  # title and description for the pie chart plotted 
  output$viz <- renderText({
    return(paste("Frequency of Ratings in", input$state))
  })
  
  # title and description for the scatter plot plotted 
  output$viz2 <- renderText({
    return(paste("Relation Between Total Amount of Fines and Home Ratings in", input$state))
  })
  
}

shinyServer(server)