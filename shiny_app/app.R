library(shiny)
library(shiny.router)
library(dplyr)
library(cluster)
library(factoextra)
library(leaflet)

libs <- c("sf","tmap","tidyverse","maptools","spatstat","raster","corrplot",
          "ggplot2","rgeos","rgdal","sp", "stringr", "ClustGeo","spdep","reshape2")
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

# for ClustGeo
# from https://cran.r-project.org/web/packages/ClustGeo/vignettes/intro_ClustGeo.html
library(ClustGeo)
data(estuary)
dat <- estuary$dat
D.geo <- estuary$D.geo
map <- estuary$map
sel <- map$NOM_COMM%in% c("BORDEAUX", "ARCACHON", "ROYAN") # label of 3 municipalities

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
### To create a new page, you have to define a new variable and set up the route in the router 
### and hardcode it in the ui function of the shinyapp

homepage <- div(
    titlePanel("Homepage"),
    p("This is the Homepage")
)

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
                      label = "Upload map. Choose shapefile",
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

eda_page <- div(
    titlePanel("EDA"),
    sidebarLayout(
        sidebarPanel(
            selectInput(inputId = "column_select",
                        label = "Which column?",
                        choices = c("RADIO" = "RADIO",
                                    "TV" = "TV",
                                    "LLPHONE" = "LLPHONE",
                                    "MPHONE" = "MPHONE",
                                    "COMPUTER" = "COMPUTER",
                                    "INTERNET" = "INTERNET"),
                        selected = "RADIO",
                        multiple = FALSE)
        ),
        mainPanel(
            tabsetPanel(
                tabPanel("Column Distribution",
                         plotly::plotlyOutput("distPlot")
                ),
                tabPanel("Correlationn Matrix",
                         plotly::plotlyOutput("corrPlot")
                )
            )
        )
    )
)

# eda_page <- div(
#     titlePanel("EDA"),
#     sidebarLayout(
#         sidebarPanel(
#             selectInput(inputId = "room_type",
#                         label = "Which room type?",
#                         choices = c("Private Room" = "Private room",
#                                     "Entire Home/Apartment" = "Entire home/apt",
#                                     "Shared Room" = "Shared room"),
#                         selected = "Private room",
#                         multiple = TRUE),
#             sliderInput(inputId = "price", 
#                         label = "Price", 
#                         min = 0,
#                         max = 13999, 
#                         value = c(100,1000)),
#             selectInput(inputId = "var_of_interest",
#                         label = "Summary by?",
#                         choices = c("Room Type" = "room_type",
#                                     "Neighbourhood Group" = "neighbourhood_group"),
#                         selected = "Room Type",
#                         multiple = FALSE),
#             checkboxInput(inputId = "showData",
#                           label = "Show data table",
#                           value = TRUE)
#         ),
#         mainPanel(
#             tabsetPanel(
#                 tabPanel("Map",
#                          tmapOutput("mapPlot"),
#                          DT::dataTableOutput(outputId = "aTable")
#                 ),
#                 tabPanel("Summary",
#                          plotly::plotlyOutput("bar_plot")
#                 )
#             )
#         )
#     )
# )

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
                )
            )
        )
    )
)

clustgeo_page <- div(
    titlePanel("ClustGeo Method"),
    sidebarLayout(
        sidebarPanel(
            conditionalPanel(
                condition = "input.tabselected==1",
                tags$strong("ClustGeo spatially constrained clustering method"),
                # selectInput("clustgeo_var",
                #             label = "Choose your clustering variables:",
                #             choices = sel,
                #             selected = c("Bordeaux" = "BORDEAUX"),
                #             multiple = TRUE),
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
            )
        )
    )
)

spatially_constrained_clustering_page <- div(
    titlePanel("Spatially Constrained Clustering"),
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
                )
            )
        )
    )
    
)

## Create the Router

router <- make_router(
    route("/", homepage),
    route("upload", upload_page),
    route("eda", eda_page),
    route("hierarchical_clustering", hierarchical_clustering_page),
    route("clustgeo", clustgeo_page),
    route("spatially_constrained_clustering", spatially_constrained_clustering_page)
)


ui <- fluidPage(
    tags$ul(
        tags$li(a(href = route_link("/"), "Homepage")),
        tags$li(a(href = route_link("/upload"), "Data Upload")),
        tags$li(a(href = route_link("eda"), "Explanatory Data Analysis")),
        tags$li(a(href = route_link("hierarchical_clustering"), "Hierarchical Clustering")),
        tags$li(a(href = route_link("clustgeo"), "ClustGeo")),
        tags$li(a(href = route_link("spatially_constrained_clustering"), "Spatially Constrained Clustering"))
    ),
    router$ui
)

server <- function(input, output, session){
    ## DO NOT REMOVE THIS
    router$server(input, output, session)
    
    ## CSV DATA UPLOAD
    output$csvcontents <- renderTable({
        req(input$filecsv)
        df <- read.csv(input$filecsv$datapath,
                       header = input$header,
                       sep = input$sep,
                       quote = input$quote)
        
        if(input$disp == "head") {
            return(head(df))
        }
        else {
            return(df)
        }
    })
    
    ## SHAPEFILE DATA UPLOAD
    output$shpcontents <- renderTable({
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
        
        map
    })
    
    ## EDA 
    output$distPlot <- plotly::renderPlotly({
        col = input$column_select
        ggplot(data=ict_derived, 
               aes_string(x=col)) +   #selected column
            geom_histogram(bins=20, 
                           color="black", 
                           fill="light blue")
    })
    output$corrPlot <- plotly::renderPlotly({
        correlation <- round(cor(ict_derived[,12:17]), 2)
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
    
    
    # eda_dataset = reactive({
    #     listings_2019 %>%
    #         filter(room_type == input$room_type) %>%
    #         filter(price > input$price)
    # })
    # output$mapPlot <- renderTmap({
    #     tm_shape(shp = eda_dataset(),
    #              bbox = st_bbox(listings_2019))+
    #         tm_bubbles(col = "room_type",
    #                    size = "price",
    #                    border.col = "black",
    #                    border.lwd = 0.5)
    # })  
    # output$bar_plot <- plotly::renderPlotly({
    #     data <- listings_2019
    #     if(input$var_of_interest == "room_type"){
    #         plot_bar <- ggplot(data, 
    #                            aes(room_type, fill=room_type))+
    #                             geom_bar()+
    #                             theme_minimal()
    #     } else {
    #         plot_bar <- ggplot(data, 
    #                            aes(neighbourhood_group, fill=neighbourhood_group))+
    #                             geom_bar()+
    #                             theme_minimal()
    #     } 
    #     return(plot_bar)
    # })
    # 
    # 
    # output$aTable <- DT::renderDataTable({
    #     if(input$showData){
    #         DT::datatable(data = eda_dataset() %>%
    #                           select(1:4),
    #                       options= list(pageLength = 10),
    #                       rownames = FALSE)
    #     }
    # })   
    
    ## Hierarchical Clustering
    basic_dataset <- shan_ict
    sec_dataset <- shan_sf
    
    output$param_tune <- renderPlot({
        set.seed(12345)
        gap_stat <- clusGap(shan_ict, 
                            FUN = hcut, 
                            nstart = 25, 
                            K.max = input$max_clust_num,
                            B = 50)
        fviz_gap_stat(gap_stat)
    })
    
    output$hier_dend <- renderPlot({
        proxmat <- dist(basic_dataset, method = input$proximity_method)
        
        hclust_ward <- hclust(proxmat, method = input$clust_method)
        
        plot(hclust_ward, cex=0.6)
        rect.hclust(hclust_ward, k=input$clust_num, border=2:5)
        
    })
    
    output$hier_clust <- renderTmap({
        proxmat <- dist(basic_dataset, method = input$proximity_method)
        
        hclust_ward <- hclust(proxmat, method = input$clust_method)
        
        groups <- as.factor(cutree(hclust_ward, k=input$clust_num))
        
        shan_sf_cluster <- cbind(sec_dataset, as.matrix(groups)) %>%
            rename(`CLUSTER`=`as.matrix.groups.`)
        
        qtm(shan_sf_cluster, "CLUSTER")
    })
    
    # ClustGeo Clustering
    # Currently is hardcoded to the estuary dataset to understand clustgeo
    # https://cran.r-project.org/web/packages/ClustGeo/vignettes/intro_ClustGeo.html
    # alternative reference: https://github.com/erikaaldisa/IS415_T14_Project/blob/master/app/app.R
    # https://erika-aldisa-gunawan.shinyapps.io/IS415_T14_EastKalimantan_New_JTown/
    
    # Clustgeo alpha plot
    output$clustgeo_sugg_alpha <- renderPlot({
        D0 <- dist(dat)
        D1 <- as.dist(D.geo) 
        range.alpha <- seq(0,1,0.01)
        cr <- choicealpha(D0, D1, range.alpha,
                          input$clustgeo_no_cluster, graph = TRUE)
        return(cr)
    })
    
    # ClustGeo clustering map
    output$clustgeo_cluster_map <- renderPlot({
        # the socio-economic distances
        D0 <- dist(dat)
        tree <- hclustgeo(D0)
        
        # the geographic distances between the municipalities
        D1 <- as.dist(D.geo) 
        
        # needs to be an input for the alpha here
        tree <- hclustgeo(D0,D1,input$clustgeo_alpha)
        P5bis <- cutree(tree,input$clustgeo_no_cluster)
        
        plot(map, border = "grey", col = P5bis, 
                 main = "Partition P5bis obtained with alpha=0.2 
         and geographical distances")
        legend("topleft", legend=paste("cluster",1:5), 
               fill=1:5, bty="n",border="white")
    })

    ## Spatially Constraied Clustering
    basic_dataset <- shan_ict
    sec_dataset <- shan_sf
    shan_sp <- as_Spatial(shan_sf)
    shan.nb <- poly2nb(shan_sp) # neighbour list
    lcosts <- nbcosts(shan.nb, shan_ict) # cost
    shan.w <- nb2listw(shan.nb, 
                       lcosts, 
                       style="B")
    shan.mst <- mstree(shan.w)
    
    output$mst_plot <- renderPlot({
        clust6 <- skater(edges = shan.mst[,1:2], 
                                  data = shan_ict, 
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
                         data = shan_ict, 
                         method = input$proximity_method_1, 
                         ncuts = input$clust_num_1-1)
        proxmat <- dist(shan_ict, method = input$proximity_method_1)
        hclust_ward <- hclust(proxmat, method = 'ward.D')
        groups <- as.factor(cutree(hclust_ward, k=input$clust_num_1))
        groups_mat <- as.matrix(clust6$groups)
        shan_sf_cluster <- cbind(shan_sf, as.matrix(groups)) %>%
            rename(`CLUSTER`=`as.matrix.groups.`)
        shan_sf_spatialcluster <- cbind(shan_sf_cluster, as.factor(groups_mat)) %>%
            rename(`SP_CLUSTER`=`as.factor.groups_mat.`)
        qtm(shan_sf_spatialcluster, "SP_CLUSTER")
        
    })
    
}

shinyApp (ui=ui, server=server)
