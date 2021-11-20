library(shiny)
library(shiny.router)
library(dplyr)
library(cluster)
library(factoextra)
library(leaflet)
library(shinythemes)
library(sf)
library(tmap)
library(tidyverse)
library(maptools)
library(spatstat)
library(raster)
library(corrplot)
library(tidyr)
library(ggplot2)
library(rgeos)
library(rgdal)
library(sp)
library(stringr)
library(ClustGeo)
library(spdep)
library(reshape2)
library(geodist)

###########################################################################################
#                            DATA IMPORT + PRE-PROCESSING
###########################################################################################

# import geospatial data (boundaries of study area)
# input: shapefile
# output: simple features object

shan_sf <- st_read(dsn = "data/geospatial", 
                   layer = "myanmar_township_boundaries") %>%
    filter(ST %in% c("Shan (East)", "Shan (North)", "Shan (South)"))
# london_sf <- st_read(dsn = "data/geospatial/statistical-gis-boundaries-london", 
#                      layer = "London_Borough_Excluding_MHW")

# geospatial pre-processing function will:
# - check for invalid geometries
# - check for missing values
# - verify + transform CRS

geospatial_processing <- function(input_sf, important_feature, expected_crs){
    return_sf <- input_sf
    if(length(which(st_is_valid(input_sf) == FALSE)) == 1){
        return_sf <- st_make_valid(return_sf)
    }
    return_sf %>% drop_na(important_feature) %>%
        st_transform(return_sf, crs=expected_crs)
    return(return_sf)
}

shan_sf <- geospatial_processing(shan_sf, "TS_PCODE", 4326)
# london_sf <- geospatial_processing(london_sf,27700)

# import aspatial data without lng/lat
# input: .csv with district names
# output: tibble data.frame

# assumption that user's data upload is stored as input$filename
# so to access, read_csv(input$filename$datapath)
ict <- read_csv("data/aspatial/final-Shan-ICT.csv")
# crime <- read_csv ("data/aspatial/crime-types.csv")

# import aspatial data with lng/lat (transform to geospatial)
# assumption that all Lng/Lat are ESPG 4326 i.e. WGS84, World Geodetic System 1984
# input: .csv with lng/lat
# output: simple features data frame
# currently not used as no aspatial data with lng/lat values

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

shan_sf <- left_join(shan_sf, ict, 
                      by=c("TS_PCODE"="TS_PCODE"))

# needs user input for selecting clustering variables
cluster_vars <- shan_sf %>%
    st_set_geometry(NULL) %>% 
    dplyr::select("TS.x", "RADIO_PR", "TV_PR", "LLPHONE_PR", "MPHONE_PR", "COMPUTER_PR", "INTERNET_PR")
row.names(cluster_vars) <- cluster_vars$"TS.x"
shan_ict <- dplyr::select(cluster_vars, c(2:7))

###########################################################################################
# PAGES
###########################################################################################

## Define the pages
### To create a new page, you have to define a new variable and set up the route in the router 
### and hardcode it in the ui function of the shinyapp

homepage <- div(
    titlePanel("About Project"),
    p("This is a regionalisation & geographical segmentation tool for our IS415 Project."),
    textOutput("choose")
)

df <- ict
shpdf <- shan_sf
sec_dataset <- shan_sf
basic_dataset <- shan_ict

#reference: https://shiny.rstudio.com/articles/upload.html
upload_page <- div(
    titlePanel("Upload your files"),
    sidebarLayout(
        sidebarPanel(
            fileInput("filecsv", "Choose CSV File",
                      multiple = TRUE,
                      accept = c("text/csv",
                                 "text/comma-separated-values,text/plain",
                                 ".csv")),
            fileInput(inputId = "filemap",
                      label = "Choose shapefile - needs 5 files (CST, DBF, PRJ, SHP and SHX)",
                      multiple = TRUE,
                      accept = c('.shp','.dbf','.sbn','.sbx','.shx','.prj')),
            tags$hr(),
            checkboxInput("header", "Header", TRUE),
            radioButtons("sep", "Separator",
                         choices = c(Comma = ",",
                                     Semicolon = ";",
                                     Tab = "\t"),
                         selected = ","),
            radioButtons("quote", "Quote",
                         choices = c(None = "",
                                     "Double Quote" = '"',
                                     "Single Quote" = "'"),
                         selected = '"'),
            tags$hr(),
            radioButtons("disp", "Display",
                         choices = c(Head = "head",
                                     All = "all"),
                         selected = "head")
            
        ),
        mainPanel(
            tableOutput("csvcontents"),
            tableOutput(outputId = "shpcontents")
        )
    )
    
)

preprocessing_page <- div(
  titlePanel("Preprocess your data"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId = "joinvar", 
                  label = "Join your geospatial and asptial data on... (assumes same feature name)",
                  choices = colnames(df),
                  selected = colnames(df)[3],
                  multiple = FALSE),
      selectInput(inputId = "townnamevar", 
                  label = "Select the geographically distinguishing feature (e.g. town name)",
                  choices = colnames(df),
                  selected = colnames(df)[4],
                  multiple = FALSE),
      selectInput(inputId = "clustervar", 
                  label = "Which clustering values are you using?",
                  choices = colnames(df),
                  selected = colnames(df)[12:18],
                  multiple = TRUE),
    ),
    mainPanel(
      "Data preprocessing for uploaded data."
    )
  )
)

eda_page <- div(
    titlePanel("EDA"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "column_select",
                        label = "Which column?",
                        choices = colnames(basic_dataset),
                        selected = colnames(basic_dataset)[1],
                        multiple = FALSE)
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Column Distribution",
                         plotly::plotlyOutput("distPlot")
                ),
                tabPanel("Correlation Matrix",
                         plotly::plotlyOutput("corrPlot")
                )
            )
        )
    )
)

hierarchical_clustering_page <- div(
    titlePanel("Hierarchical Clustering"),
    sidebarLayout(
        sidebarPanel(
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
                        value = 3),
            checkboxInput("param_tune_checkbox", "Show suggested number of clusters"),
            conditionalPanel(condition = "input.param_tune_checkbox",
                             sliderInput(inputId = "max_clust_num", 
                                         label = "Max number of Clusters", 
                                         min = 1,
                                         max = 20, 
                                         value = 3),
                             plotOutput("param_tune", height = 300))
        ),
        mainPanel(
            tabsetPanel( 
                tabPanel(
                    "Dendrogram and Cluster Map",
                    plotOutput("hier_dend"),
                    tmapOutput("hier_clust")
                ),
                tabPanel("About Hierarchical Clustering",
                         column(12,
                                h2("What is Hierarchical Clustering?"),
                                tags$br(),
                                h5("Hierarchical clustering is an algorithm that groups similar objects 
                                   into groups called clusters. The endpoint is a set of clusters, 
                                   where each cluster is distinct from each other cluster, 
                                   and the objects within each cluster are broadly similar to each other."),
                                h5("There are two types of Hierarchical clustering:"),
                                tags$ul(
                                    tags$li("Agglomerative clustering: Known as AGNES (Agglomerative Nesting), and works bottom-up.
                                    Each object is initially considered as a single-element cluster (leaf). 
                                    At each step of the algorithm, the two clusters that are the most similar 
                                    are combined into a new bigger cluster (nodes). This procedure is iterated until 
                                    all points are member of just one single big cluster (root). The result is a 
                                    tree which can be plotted as a dendrogram."),
                                    tags$li("Divisive hierarchical clustering: Known as DIANA (Divise Analysis),
                                    and it works top-down, inverse of AGNES. It begins with the root, in which all objects
                                    are included in a single cluster. At each step of iteration, the most heterogeneous 
                                    cluster is divided into two. The process is iterated until all objects are in their own cluster.")
                                ),
                                h5("These are agglomeration/linkage functions that groups the objects into hierarchical cluster tree based on the proximity matrix (similarity) generated:"),
                                tags$ul(
                                    tags$li("Ward: minimizes total within-cluster variance, the pair of clusters with minimum between-cluster distance are merged"),
                                    tags$li("Complete: computes all pairwise dissimilarities between elements two clusters, and considers the maximum value as the distance between the two clusters, tend to produce compact clusters"),
                                    tags$li("Average: computes all pairwise dissimilarities between elements two clusters, and considers the average value as the distance between the two clusters"),
                                    tags$li("Single: computes all pairwise dissimilarities between elements two clusters, and considers the minimum value as the distance between the two clusters, tend to produce loose clusters"),
                                    tags$li("Centroid: computes the dissimilarity between the centroid for cluster 1 and centroid for cluster 2 and considers it as the distance")
                                )
                         )
                )
            )
        )
    )
)

clustgeo_clustering_page <- div(
    titlePanel("ClustGeo Method"),
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                condition = "input.tabselected==1",
                tags$strong("ClustGeo spatially constrained clustering method"),
                selectInput("clustgeo_var",
                            label = "Choose your clustering variables:",
                            choices = colnames(basic_dataset),
                            selected = colnames(basic_dataset)[1],
                            multiple = TRUE),
                sliderInput("clustgeo_no_cluster",
                            label = "No. of clusters",
                            min = 3,
                            max = 10,
                            value = 5,
                            step = 1),
                sliderInput("clustgeo_alpha",
                            label = "alpha value",
                            min = 0.1,
                            max = 1.0,
                            value = 0.2,
                            step = 0.1),
                checkboxInput("clustgeo_sugg_alpha", "Show suggested alpha value for known no. of clusters"),
                conditionalPanel(condition = "input.clustgeo_sugg_alpha",
                                 plotOutput("clustgeo_sugg_alpha", height = 300),
                                 h6("Choose the intersection of D0 & D1 as the alpha value"))
            )
        ),
        mainPanel(
            tabsetPanel( 
                id = "tabselected",
                selected = 1,
                tabPanel(
                    "ClustGeo Method",
                    plotOutput("clustgeo_cluster_map"),
                     value = 1
                ),
                tabPanel("About ClustGeo Clustering",
                         column(12,
                                h2("What is ClustGeo clustering?"),
                                tags$br(),
                                h5("ClustGeo is an implementation of Ward-like hierarchical clustering.
                               The hclustgeo function of ClustGeo package takes two 
                               dissimilarity matrices, D0 & D1, and a mixing parameter alpha between 0 and 1.
                               The dissimilarities can be non-Euclidean 
                               and the weights of the observations can be non-uniform"),
                                tags$ul(
                                    tags$li("D0: euclidean distance matrix performed with socio-demographic/socio-economic continuous variables, obtained through dist() function"),
                                    tags$li("D1: second dissimilarity matrix to compute the geographical proximity between places in the study area, obtained through geodist() function")
                                ),
                                h5("The alpha value sets the importance of the contiguity constraint (distance matrix) in the clustering process."),
                                h5("The idea is to determine a value of alpha which increases the spatial contiguity 
                               without deteriorating too much the quality of the solution based on the variables 
                               of interest i.e. those of the feature space.")
                         )
                )
            )
        )
    )
)

spatially_constrained_clustering_page <- div(
    titlePanel("Spatially Constrained Clustering (SKATER Method)"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "proximity_method_1",
                        label = "Which proximity method?",
                        choices = c("Euclidean" = "euclidean",
                                    "Maximum" = "maximum",
                                    "Manhattan" = "manhattan",
                                    "Canberra" = "canberra",
                                    "Binary" = "binary",
                                    "Minkowski" = "minkowski"),
                        selected = "Euclidean",
                        multiple = FALSE),
            sliderInput(inputId = "clust_num_1", 
                        label = "Number of Clusters", 
                        min = 1,
                        max = 20, 
                        value = 3)
        ),
        mainPanel(
            tabsetPanel( 
                tabPanel(
                    "Minimum Spanning Tree",
                    plotOutput("mst_plot")
                ),
                tabPanel(
                    "Choropleth Map",
                    tmapOutput("chloropleth")
                ),
                tabPanel("About SKATER",
                         column(12,
                                h2("What is SKATER?"),
                                tags$br(),
                                h5("SKATER, also known as the Spatial Kluster analysis by Tree Edge Removal
                                Assuncao et al (2006) algorithm, is a contiguity-constrained clustering.
                                   It focuses on pruning a minimum spanning tree constructed from 
                                   the adjacency graph to to achieve maximum internal homogeneity."),
                                h5("Steps required involve: "),
                                tags$ul(
                                    tags$li("Building neighbour/contiguity list graph"),
                                    tags$li("Computing the minimum spanning tree, which minimizes a cost 
                                            function to minimize sum of dissimilarities over all nodes"),
                                    tags$li("Pruning/cutting edges of the tree for desired number of clusters")
                                )
                    )
                )
            )
        )
    )
)

## Create the Router
router <- make_router(
    route("/", homepage),
    route("upload", upload_page),
    route("preprocessing", preprocessing_page),
    route("eda", eda_page),
    route("hierarchical_clustering", hierarchical_clustering_page),
    route("clustgeo_clustering", clustgeo_clustering_page),
    route("spatially_constrained_clustering", spatially_constrained_clustering_page)
)

ui <- fluidPage(
    theme = shinytheme("flatly"),
    includeCSS("www/main.css"),
    
    navbarPage(
        title = div(img(src = 'logo.png', style = "margin-top: 0px; padding-right:6px;padding-bottom:20px", height = 55)),
        windowTitle = "Regionalisation and Geographical Segmentation Tool",
        
        tabPanel(tags$a(href = route_link("/"), "About Project")),
        tabPanel(tags$a(href = route_link("/upload"), "Data Upload")),
        tabPanel(tags$a(href = route_link("/preprocessing"), "Data Preprocessing")),
        tabPanel(tags$a(href = route_link("/eda"), "Explanatory Data Analysis")),
        tabPanel(tags$a(href = route_link("/hierarchical_clustering"), "Hierarchical Clustering")),
        tabPanel(tags$a(href = route_link("/spatially_constrained_clustering"), "Spatially Constrained Clustering")),
        tabPanel(tags$a(href = route_link("/clustgeo_clustering"), "ClustGeo Clustering"))
    ),
    router$ui
)

server <- function(input, output, session){
    ## DO NOT REMOVE THIS
    router$server(input, output, session)

    output$choose <- reactive({
        if((is.null(input$filecsv))&(is.null(input$filemap)))
        {
            "No input given yet."
        }
        else
        {
            "Data input passed."
        }
    })
    
    ## CSV DATA UPLOAD
    output$csvcontents <- renderTable({
          if(is.null(input$filecsv$datapath)){
            df <- ict
          } else {
            req(input$filecsv)
            
            df <- read.csv(input$filecsv$datapath,
                           header = input$header,
                           sep = input$sep,
                           quote = input$quote)
          }
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
    })
    
    ## SHAPEFILE DATA UPLOAD
    output$shpcontents <- renderTable({
        if(is.null(input$filemap$datapath)){
          map <- as_Spatial(shan_sf)
        } else {
          req(input$filemap)
          shpdf <- input$filemap
          
          # Name of the temporary directory where files are uploaded
          tempdirname <- dirname(shpdf$datapath[1])
          
          # Rename files
          for (i in 1:nrow(shpdf)) {
            file.rename(
              shpdf$datapath[i],
              paste0(tempdirname, "/", shpdf$name[i])
            )
          }
          # Now we read the shapefile with readOGR() of rgdal package
          # passing the name of the file with .shp extension.
          
          # We use the function grep() to search the pattern "*.shp$"
          # within each element of the character vector shpdf$name.
          # grep(pattern="*.shp$", shpdf$name)
          # ($ at the end denote files that finish with .shp,
          # not only that contain .shp)
          map <- readOGR(paste(tempdirname,
                               shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                               sep = "/"
          ))
        }
        map
    })
    
    ## Preprocessing
    
    # the sec_dataset
    sec_dataset_reactive <- reactive({
      
      #inFile <- input$filecsv
      #inMap <- input$filemap
      
      if((is.null(input$filecsv$datapath)) | (is.null(input$filemap$datapath))){
        df <- ict
        shpdf <- shan_sf
        joined_sf <- left_join(shpdf, df, 
                             by=c(input$joinvar))
        joined_sf <- geospatial_processing(joined_sf, input$joinvar, 4326)
      } else {
        req(input$filecsv)
        req(input$filemap)
        df <- read.csv(input$filecsv$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        shpdf <- input$filemap
        tempdirname <- dirname(shpdf$datapath[1])
        
        for (i in 1:nrow(shpdf)) {
          file.rename(
            shpdf$datapath[i],
            paste0(tempdirname, "/", shpdf$name[i])
          )
        }
        map <- readOGR(paste(tempdirname,
                             shpdf$name[grep(pattern = "*.shp$", shpdf$name)],
                             sep = "/"))
        shpdf <- st_as_sf(map)
        joined_sf <- left_join(shpdf, df, 
                             by=c(input$joinvar))
        joined_sf <- geospatial_processing(joined_sf, input$joinvar, 4326)
      }
      return(joined_sf)
    })
    
    # the basic_dataset
    basic_dataset_reactive <- reactive({
      
      #inFile <- input$filecsv
      #inMap <- input$filemap

      if((is.null(input$filecsv$datapath)) | (is.null(input$filemap$datapath))){
        new_ict <- shan_ict
      } else {
        sec_dataset <- sec_dataset_reactive()
        cluster_vars <- sec_dataset %>%
          st_set_geometry(NULL) %>%
          dplyr::select(input$townnamevar, input$clustervar)
        row.names(cluster_vars) <- cluster_vars$input$townnamevar
        new_ict <- dplyr::select(cluster_vars, c(2:7))
      }
      return(new_ict)
    })

    ## EDA 
    output$distPlot <- plotly::renderPlotly({
        col = input$column_select
        ggplot(data=basic_dataset, 
               aes_string(x=col)) +   #selected column
            geom_histogram(bins=20, 
                           color="black", 
                           fill="light blue")
    })
    
    output$corrPlot <- plotly::renderPlotly({
        correlation <- round(cor(basic_dataset), 2)
        # Get upper triangle of the correlation matrix
        get_upper_tri <- function(cormat){
            cormat[lower.tri(cormat)]<- NA
            return(cormat)
        }
        upper_tri <- get_upper_tri(correlation)
        melted_cormat <- melt(upper_tri, na.rm = TRUE)
        ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
            geom_tile(color = "white")+
            scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                                 midpoint = 0, limit = c(-1,1), space = "Lab", 
                                 name="Pearson\nCorrelation") +
            theme_minimal()+ # minimal theme
            theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                             size = 12, hjust = 1))+
            coord_fixed()
        ggheatmap + 
            geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
            theme(
                axis.title.x = element_blank(),
                axis.title.y = element_blank(),
                panel.grid.major = element_blank(),
                panel.border = element_blank(),
                panel.background = element_blank(),
                axis.ticks = element_blank(),
                legend.justification = c(1, 0),
                legend.position = c(0.6, 0.7),
                legend.direction = "horizontal")+
            guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                         title.position = "top", title.hjust = 0.5))
    })

    ## Hierarchical Clustering
    output$param_tune <- renderPlot({
        set.seed(12345)
        gap_stat <- clusGap(basic_dataset_reactive(), 
                            FUN = hcut, 
                            nstart = 25, 
                            K.max = input$max_clust_num,
                            B = 50)
        fviz_gap_stat(gap_stat)
    })
    
    output$hier_dend <- renderPlot({
        proxmat <- dist(basic_dataset_reactive(), method = input$proximity_method)
        hclust_ward <- hclust(proxmat, method = input$clust_method)
        
        plot(hclust_ward, cex=0.6)
        rect.hclust(hclust_ward, k=input$clust_num, border=2:5)
    })
    
    output$hier_clust <- renderTmap({
        proxmat <- dist(basic_dataset_reactive(), method = input$proximity_method)
        hclust_ward <- hclust(proxmat, method = input$clust_method)
        
        groups <- as.factor(cutree(hclust_ward, k=input$clust_num))
        shan_sf_cluster <- cbind(sec_dataset_reactive(), as.matrix(groups)) %>%
            rename(`CLUSTER`=`as.matrix.groups.`)
        qtm(shan_sf_cluster, "CLUSTER")
    })
    
    ## ClustGeo Clustering
    # https://cran.r-project.org/web/packages/ClustGeo/vignettes/intro_ClustGeo.html
 
    # Clustgeo alpha plot
    output$clustgeo_sugg_alpha <- renderPlot({
        
        D0 <- dist(basic_dataset[,input$clustgeo_var])
        
        shan_map <- as_Spatial(sec_dataset)
        coords <- coordinates(shan_map)
        row.names(coords) <- shan_map$TS.x
        colnames(coords) <- c("lon", "lat")
        
        D1 <- geodist(coords, measure = "vincenty")
        D1 <- as.dist(D1)
        
        range.alpha <- seq(0,1,0.01)
        cr <- choicealpha(D0, D1, range.alpha,
                          input$clustgeo_no_cluster, graph = TRUE)
        return(cr)
    })
    
    # ClustGeo clustering map
    output$clustgeo_cluster_map <- renderPlot({
        
        # the socio-economic distances
        D0 <- dist(basic_dataset[,input$clustgeo_var])
        # tree <- hclustgeo(D0)
        
        shan_map <- as_Spatial(sec_dataset)
        coords <- coordinates(shan_map)
        row.names(coords) <- shan_map$TS.x
        colnames(coords) <- c("lon", "lat")
        
        # the geographic distances between the municipalities
        D1 <- geodist(coords, measure = "vincenty")
        D1 <- as.dist(D1)
        
        # needs to be an input for the alpha here
        tree <- hclustgeo(D0,D1,input$clustgeo_alpha)
        P5bis <- cutree(tree,input$clustgeo_no_cluster)
        
        plot(shan_map, border = "grey", col = P5bis, 
                 main = "Partition P5bis obtained with alpha=0.2 and geographical distances")
        legend("topleft", legend=paste("cluster",1:5), 
               fill=1:5, bty="n",border="white")
    })

    ## Spatially Constrained Clustering
    
    shan_sp <- as_Spatial(sec_dataset)
    shan.nb <- poly2nb(shan_sp) # neighbour list
    lcosts <- nbcosts(shan.nb, basic_dataset) # cost
    shan.w <- nb2listw(shan.nb, 
                       lcosts, 
                       style="B")
    shan.mst <- mstree(shan.w)
    
    output$mst_plot <- renderPlot({
        
        clust6 <- skater(edges = shan.mst[,1:2], 
                                  data = basic_dataset, 
                                  method = input$proximity_method_1, 
                                  ncuts = input$clust_num_1-1)
        plot(shan_sp, border=gray(.5))
        plot(clust6, 
             coordinates(shan_sp), 
             cex.lab=.7,
             groups.colors=rainbow(input$clust_num_1),
             cex.circles=0.005,
             add=TRUE)
    })
    
    output$chloropleth <- renderTmap({
        
        clust6 <- skater(edges = shan.mst[,1:2], 
                         data = basic_dataset, 
                         method = input$proximity_method_1, 
                         ncuts = input$clust_num_1-1)
        proxmat <- dist(basic_dataset, method = input$proximity_method_1)
        hclust_ward <- hclust(proxmat, method = 'ward.D')
        groups <- as.factor(cutree(hclust_ward, k=input$clust_num_1))
        groups_mat <- as.matrix(clust6$groups)
        shan_sf_cluster <- cbind(sec_dataset, as.matrix(groups)) %>%
            rename(`CLUSTER`=`as.matrix.groups.`)
        shan_sf_spatialcluster <- cbind(shan_sf_cluster, as.factor(groups_mat)) %>%
            rename(`SP_CLUSTER`=`as.factor.groups_mat.`)
        qtm(shan_sf_spatialcluster, "SP_CLUSTER")
    })
}

shinyApp (ui=ui, server=server)
