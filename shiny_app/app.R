library(shiny)
library(shiny.router)
library(dplyr)
library(cluster)
library(factoextra)

libs <- c("sf","tmap","tidyverse","maptools","spatstat","raster",
          "ggplot2","rgeos","rgdal","sp", "stringr")
lapply(libs, library, character.only = TRUE)

###########################################################################################

# assumption that data upload tool is done! 
# as such, referring to the datasets directly
# actually can we just use open street map as the base? 
# but we'd still need the study area boundaries right

# import geospatial data (boundaries of study area)
# input: shapefile
# output: simple features object

sg_sf <- st_read(dsn = "data/geospatial", 
                 layer="CostalOutline")
mpsz_sf <- st_read(dsn = "data/geospatial", 
                   layer = "MP14_SUBZONE_WEB_PL")
shan_sf <- st_read(dsn = "data/geospatial", 
                   layer = "myanmar_township_boundaries") %>%
    filter(ST %in% c("Shan (East)", "Shan (North)", "Shan (South)"))
london_sf <- st_read(dsn = "data/geospatial/statistical-gis-boundaries-london", 
                     layer = "London_Borough_Excluding_MHW")

# geospatial pre-processing function needs to:
# - check for invalid geometries
# - check for missing values
# - verify + transform CRS

# assumption that user's data upload is stored as input$filename

geospatial_processing <- function(input_sf, expected_crs){
    return_sf <- input_sf
    if(length(which(st_is_valid(input_sf) == FALSE)) == 1){
        return_sf <- st_make_valid(return_sf)
    }
    return_sf <- na.omit(return_sf) %>%
        st_transform(return_sf, crs=expected_crs)
    return(return_sf)
}

sg_sf <- geospatial_processing(sg_sf,3414)
mpsz_sf <- geospatial_processing(mpsz_sf,3414)

london_sf <- geospatial_processing(london_sf,27700)

###########################################################################################

# import aspatial data without lng/lat
# input: .csv with district names
# output: tibble data.frame

# assumption that user's data upload is stored as input$filename
# so to access, read_csv(input$filename$datapath)
ict <- read_csv ("data/aspatial/Shan-ICT.csv")
crime <- read_csv ("data/aspatial/crime-types.csv")

###########################################################################################

# import aspatial data with lng/lat (transform to geospatial)
# assumption that all Lng/Lat are ESPG 4326 i.e. WGS84, World Geodetic System 1984
# input: .csv with lng/lat
# output: simple features data frame

# pre-processing function needs to:
# - import data
# - remove missing values for relevant columns (lat, lng)
# - convert to sf objects
# - transform CRS (user input)

aspatial_processing <- function(input_filepath, expected_crs){
    filetype <- str_sub(input_filepath, -3, -1) 
    if(filetype=="csv"){
        return_sf <- read_csv(input_filepath)
    }else if(filetype=="rds"){
        return_sf <- read_rds(input_filepath)
    }
    
    return_sf$Lat <- as.numeric(return_sf$Lat)
    return_sf$Lng <- as.numeric(return_sf$Lng)
    
    if(sum(is.na(return_sf$Lat))>0){
        return_sf <- return_sf[!(is.na(return_sf$Lat)), ]
    }
    return_sf <- st_as_sf(return_sf,
                          coords = c("Lng", "Lat"),
                          crs = 4326) %>%
        st_transform(crs=expected_crs)
    return(return_sf)
}

listings_2019 <- aspatial_processing("data/aspatial/listings_30062019.csv", 3414)
listings_2021 <- aspatial_processing("data/aspatial/listings_29062021.csv", 3414)
childcare_sf <- aspatial_processing("data/aspatial/childcare.rds", 3414)

# deriving new variables - penetration rate
# need a tool for this?
ict_derived <- ict %>%
    mutate(`RADIO_PR` = `Radio`/`Total households`*1000) %>%
    mutate(`TV_PR` = `Television`/`Total households`*1000) %>%
    mutate(`LLPHONE_PR` = `Land line phone`/`Total households`*1000) %>%
    mutate(`MPHONE_PR` = `Mobile phone`/`Total households`*1000) %>%
    mutate(`COMPUTER_PR` = `Computer`/`Total households`*1000) %>%
    mutate(`INTERNET_PR` = `Internet at home`/`Total households`*1000) %>%
    rename(`DT_PCODE` =`District Pcode`,`DT`=`District Name`,
           `TS_PCODE`=`Township Pcode`, `TS`=`Township Name`,
           `TT_HOUSEHOLDS`=`Total households`,
           `RADIO`=`Radio`, `TV`=`Television`, 
           `LLPHONE`=`Land line phone`, `MPHONE`=`Mobile phone`,
           `COMPUTER`=`Computer`, `INTERNET`=`Internet at home`) 


# needs to left_join with geospatial data... which is user input? 'join on...'
# how to add that functionality though :-(
# or we can necessitate that they joined column must be of same name then go through both to find

shan_sf <- left_join(shan_sf, ict_derived, 
                      by=c("TS_PCODE"="TS_PCODE"))

###########################################################################################

# EDA SECTION

###########################################################################################


# CLUSTERING SECTION

## SELECT CLUSTER VARIABLES


cluster_vars <- shan_sf %>%
    st_set_geometry(NULL) %>% 
    dplyr::select("TS.x", "RADIO_PR", "TV_PR", "LLPHONE_PR", "MPHONE_PR", "COMPUTER_PR")

row.names(cluster_vars) <- cluster_vars$"TS.x"

shan_ict <- dplyr::select(cluster_vars, c(2:6))

## Convert dataframe into a matrix

shan_ict_mat <- data.matrix(shan_ict)

## Define the pages
### To create a new page, you have to define a new variable and set up the route in the router and hardcode it in the ui function of the shinyapp

homepage <- div(
    titlePanel("Homepage"),
    p("This is the Homepage")
)

eda_page <- div(
    titlePanel("EDA"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "room_type",
                        label = "Which room type?",
                        choices = c("Private Room" = "Private room",
                                    "Entire Home/Apartment" = "Entire home/apt",
                                    "Shared Room" = "Shared room"),
                        selected = "Private room",
                        multiple = TRUE),
            sliderInput(inputId = "price", 
                        label = "Price", 
                        min = 0,
                        max = 13999, 
                        value = c(100,1000)),
            selectInput(inputId = "var_of_interest",
                        label = "Summary by?",
                        choices = c("Room Type" = "room_type",
                                    "Neighbourhood Group" = "neighbourhood_group"),
                        selected = "Room Type",
                        multiple = FALSE),
            checkboxInput(inputId = "showData",
                          label = "Show data table",
                          value = TRUE)
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Map",
                         tmapOutput("mapPlot"),
                         DT::dataTableOutput(outputId = "aTable")
                ),
                tabPanel("Summary",
                         plotly::plotlyOutput("bar_plot")
                )
            )
        )
    )
)

basic_clustering_page <- div(
    titlePanel("Basic Clustering"),
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                condition = "input.tabselected==1",
                sliderInput(inputId = "max_clust", 
                            label = "Max number of Clusters", 
                            min = 1,
                            max = 20, 
                            value = 5)
            ),
            conditionalPanel(
                condition = "input.tabselected==2",
                selectInput(inputId = "clust_method",
                            label = "Which clustering method?",
                            choices = c("Ward D" = "ward.D",
                                        "Ward D2" = "ward.D2",
                                        "Single" = "single",
                                        "Complete" = "complete",
                                        "Average(UPGMA)" = "average",
                                        "Mcquitty(WPGMA)" = "mcquitty",
                                        "Median(WPGMC)" = "median",
                                        "Centroid(UPGMC)" = "centroid"),
                            selected = "Ward D",
                            multiple = FALSE),
                selectInput(inputId = "proximity_method",
                            label = "Which proximity method?",
                            choices = c("Euclidean" = "euclidean",
                                        "Maximum" = "maximum",
                                        "Manhattan" = "manhattan",
                                        "Canberra" = "canberra",
                                        "Binary" = "binary",
                                        "Minkowski" = "minkowski"),
                            selected = "Euclidean",
                            multiple = FALSE),
                sliderInput(inputId = "clust_num", 
                            label = "Number of Clusters", 
                            min = 1,
                            max = 20, 
                            value = 3)
            )
        ),
        mainPanel(
            tabsetPanel( 
                id = "tabselected",
                selected = 1,
                tabPanel(
                    "Parameter Tuning",
                    plotOutput("param_tune"),
                    value = 1
                ),
                tabPanel(
                    "Dendrogram and Cluster Map",
                    plotOutput("hier_dend"),
                    tmapOutput("hier_clust"),
                    value = 2
                )
            )
        )
    )

)

## Create the Router

router <- make_router(
    route("/", homepage),
    route("eda", eda_page),
    route("basic_clustering", basic_clustering_page)
)


ui <- fluidPage(
    tags$ul(
        tags$li(a(href = route_link("/"), "Homepage")),
        tags$li(a(href = route_link("eda"), "Explanatory Data Analysis")),
        tags$li(a(href = route_link("basic_clustering"), "Basic Clustering"))
    ),
    router$ui
)

server <- function(input, output, session){
    router$server(input, output, session)
    
    ## EDA 
    eda_dataset = reactive({
        listings_2019 %>%
            filter(room_type == input$room_type) %>%
            filter(price > input$price)
    })
    output$mapPlot <- renderTmap({
        tm_shape(shp = eda_dataset(),
                 bbox = st_bbox(listings_2019))+
            tm_bubbles(col = "room_type",
                       size = "price",
                       border.col = "black",
                       border.lwd = 0.5)
    })  
    output$bar_plot <- plotly::renderPlotly({
        data <- listings_2019
        if(input$var_of_interest == "room_type"){
            plot_bar <- ggplot(data, 
                               aes(room_type, fill=room_type))+
                                geom_bar()+
                                theme_minimal()
        } else {
            plot_bar <- ggplot(data, 
                               aes(neighbourhood_group, fill=neighbourhood_group))+
                                geom_bar()+
                                theme_minimal()
        } 
        return(plot_bar)
    })
    
    
    output$aTable <- DT::renderDataTable({
        if(input$showData){
            DT::datatable(data = eda_dataset() %>%
                              select(1:4),
                          options= list(pageLength = 10),
                          rownames = FALSE)
        }
    })   
    
    ## Basic Clustering
    basic_dataset <- shan_ict
    sec_dataset <- shan_sf
    
    output$param_tune <- renderPlot({
        set.seed(12345)
        gap_stat <- clusGap(shan_ict, 
                            FUN = hcut, 
                            nstart = 25, 
                            K.max = input$max_clust,
                            B = 50)
        fviz_gap_stat(gap_stat)
    })
    
    output$hier_dend <- renderPlot({
        proxmat <- dist(basic_dataset, method = input$proximity_method)
        
        hclust_ward <- hclust(proxmat, method = input$clust_method)
        
        plot(hclust_ward, cex=0.6)
        
    })
    
    output$hier_clust <- renderTmap({
        proxmat <- dist(basic_dataset, method = input$proximity_method)
        
        hclust_ward <- hclust(proxmat, method = input$clust_method)
        
        groups <- as.factor(cutree(hclust_ward, k=input$clust_num))
        
        shan_sf_cluster <- cbind(sec_dataset, as.matrix(groups)) %>%
            rename(`CLUSTER`=`as.matrix.groups.`)
        
        qtm(shan_sf_cluster, "CLUSTER")
    })
    
}

shinyApp (ui=ui, server=server)
