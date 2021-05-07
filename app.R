## Loading packages

library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(dplyr)
library(tidyr)
library(ggplot2)
library(plotly)
library(visNetwork)
library(igraph)
library(forcats)
library(DT)
library(maps)
library(ggiraph)
library(RColorBrewer)
library(scales)
library(chorddiag)
library(htmltools)




## Data Manipulation

country_industry<-read.csv("https://raw.githubusercontent.com/albina23/Data-analysis/main/country_sector2.csv")
#country_industry <- read.csv("country_sector2.csv")

country_industry <- country_industry %>% 
  mutate(domestic="")

country_industry$domestic[country_industry$Country==country_industry$ShareholderCountry] <- "Domestic"
country_industry$domestic[country_industry$Country!=country_industry$ShareholderCountry] <- "International"
country_industry$domestic <- factor(country_industry$domestic, levels = c("Domestic", "International"))

country_industry$EquityShare <- round(country_industry$EquityShare,2)

country_finance <- country_industry %>% 
  filter(Sh_NACEDesc=="finance") %>% 
  filter(domestic=="Domestic") %>% 
  select(Country, Country_Name, Continent, NACEDesc, Equity, EquityOwned, EquityShare, EquityCountry)%>% 
  arrange(Country_Name)


country_foreign <- country_industry %>% 
  group_by(Country, NACEDesc, domestic) %>%
  mutate(EquityForeign=sum(EquityOwned)) %>%
  select(Country, Country_Name, Continent, NACEDesc, Equity, domestic, EquityForeign, EquityCountry) %>% 
  distinct() %>% 
  mutate(EquityShare= round((EquityForeign/Equity),2)) %>% 
  arrange(Country_Name)


country_foreign2 <- country_industry %>%
  group_by(Country, NACEDesc, ShareholderCountry, domestic) %>%
  mutate(EquityForeign=sum(EquityOwned)) %>%
  mutate(EquityForeignShare=EquityForeign/Equity) %>%
  select(Country, Country_Name, Continent, NACEDesc, ShareholderCountry, Sh_Country_Name,Sh_Continent,domestic, Equity,EquityForeign,EquityForeignShare,EquityCountry) %>% 
  distinct() %>% 
  mutate(EquityForeignShare=round(EquityForeignShare,2))%>% 
  arrange(Country_Name)


#hex_codes1 <- hue_pal()(19) 
#hex_codes1
#show_col(hex_codes1) 

scale_fill_industry <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(c("#F8766D", "#E9842C", "#D69100", "#BC9D00", "#9CA700", "#6FB000", "#00B813", "#00BD61", "#00C08E", "#00C0B4", "#00BDD4", "#00B5EE",
                        "#00A7FF", "#7F96FF", "#BC81FF", "#E26EF7", "#F863DF", "#FF62BF", "#FF6A9A"), 
                      c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","individual","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail")), 
    ...
  )
}


country_scatterplot <- country_industry %>%
  group_by(Country, NACEDesc, Sh_NACEDesc, domestic) %>%
  mutate(EquityForeign=sum(EquityOwned)) %>%
  mutate(EquityForeignShare=round(EquityForeign/Equity,3)) %>%
  select(Country, Country_Name, Continent,NACEDesc, Sh_NACEDesc, domestic, EquityForeignShare, EquityCountry) %>% 
  distinct() %>% 
  mutate(NACEDesc=ifelse(NACEDesc=="health social work","health_social_work",
                  ifelse(NACEDesc=="electricity water waste","electricity_water_waste",
                  ifelse(NACEDesc=="accomodation food","accomodation_food",
                  ifelse(NACEDesc=="arts entertainment","arts_entertainment",
                  ifelse(NACEDesc=="wholesale and retail","wholesale_retail",
                  ifelse(NACEDesc=="real estate","real_estate",
                  ifelse(NACEDesc=="transport storage","transport_storage",
                  ifelse(NACEDesc=="finance","finance",
                  ifelse(NACEDesc=="individual","individual",
                  ifelse(NACEDesc=="other","other",
                  ifelse(NACEDesc=="agriculture","agriculture",
                  ifelse(NACEDesc=="mining","mining",
                  ifelse(NACEDesc=="education","education",
                  ifelse(NACEDesc=="government","government",
                  ifelse(NACEDesc=="construction","construction",
                  ifelse(NACEDesc=="manufacturing","manufacturing",
                  ifelse(NACEDesc=="admin","admin",
                  ifelse(NACEDesc=="information","information",
                  ifelse(NACEDesc=="professional","professional")))))))))))))))))))) %>% 
  spread(NACEDesc, EquityForeignShare)%>%
  arrange(Country_Name)


country_scatterplot2 <- country_industry %>%
  group_by(Country, NACEDesc, Sh_NACEDesc) %>%
  mutate(EquityForeign=sum(EquityOwned)) %>%
  mutate(EquityForeignShare=round(EquityForeign/Equity,3)) %>%
  select(Country, Country_Name, Continent,NACEDesc, Sh_NACEDesc, domestic, EquityForeignShare, EquityCountry) %>% 
  mutate(domestic="Global") %>% 
  distinct() %>% 
  mutate(NACEDesc=ifelse(NACEDesc=="health social work","health_social_work",
                         ifelse(NACEDesc=="electricity water waste","electricity_water_waste",
                         ifelse(NACEDesc=="accomodation food","accomodation_food",
                         ifelse(NACEDesc=="arts entertainment","arts_entertainment",
                         ifelse(NACEDesc=="wholesale and retail","wholesale_retail",
                         ifelse(NACEDesc=="real estate","real_estate",
                         ifelse(NACEDesc=="transport storage","transport_storage",
                         ifelse(NACEDesc=="finance","finance",
                         ifelse(NACEDesc=="individual","individual",
                         ifelse(NACEDesc=="other","other",
                         ifelse(NACEDesc=="agriculture","agriculture",
                         ifelse(NACEDesc=="mining","mining",
                         ifelse(NACEDesc=="education","education",
                         ifelse(NACEDesc=="government","government",
                         ifelse(NACEDesc=="construction","construction",
                         ifelse(NACEDesc=="manufacturing","manufacturing",
                         ifelse(NACEDesc=="admin","admin",
                         ifelse(NACEDesc=="information","information",
                         ifelse(NACEDesc=="professional","professional")))))))))))))))))))) %>% 
  spread(NACEDesc, EquityForeignShare)%>%
  arrange(Country_Name)


country_scatterplot <- bind_rows(country_scatterplot, country_scatterplot2)


nonfinance <- country_industry%>%
  mutate(nonfinance=ifelse(NACEDesc=="finance", "finance", "nonfinance")) %>% 
  group_by(Country, nonfinance,Sh_NACEDesc,domestic) %>% 
  mutate(EquityForeign=sum(EquityOwned)) %>%
  select(Country, Sh_NACEDesc, nonfinance, Equity, domestic, EquityForeign, EquityCountry) %>%
  distinct() %>% 
  mutate(EquityTotal=sum(Equity)) %>%
  mutate(EquityForeignShare= round((EquityForeign/EquityTotal),3)) %>%
  select(Country, Sh_NACEDesc, nonfinance, domestic, EquityForeignShare) %>%
  distinct() %>%
  ungroup() %>% 
  filter(Country!="") %>% 
  filter(nonfinance=="nonfinance") %>%
  select(-nonfinance) %>% 
  rename(nonfinance=EquityForeignShare)


nonfinance2 <- country_industry%>%
  mutate(nonfinance=ifelse(NACEDesc=="finance", "finance", "nonfinance")) %>% 
  group_by(Country, nonfinance,Sh_NACEDesc,domestic) %>% 
  mutate(EquityForeign=sum(EquityOwned)) %>%
  select(Country, Sh_NACEDesc, nonfinance, Equity, domestic, EquityForeign, EquityCountry) %>%
  distinct() %>% 
  mutate(EquityTotal=sum(Equity)) %>%
  mutate(EquityForeignShare=round(EquityForeign/EquityTotal,3)) %>% 
  select(Country, Sh_NACEDesc, nonfinance, domestic, EquityForeignShare) %>%
  distinct() %>%
  ungroup() %>%
  filter(nonfinance=="nonfinance") %>%
  group_by(Country,Sh_NACEDesc) %>% 
  mutate(EquityForeignShare=sum(EquityForeignShare)) %>% 
  mutate(domestic="Global") %>%
  distinct() %>% 
  ungroup() %>%
  filter(Country!="") %>% 
  select(-nonfinance) %>% 
  rename(nonfinance=EquityForeignShare) 
  

nonfinance <- bind_rows(nonfinance, nonfinance2)

country_scatterplot <- left_join(country_scatterplot,nonfinance, by=c("Country","Sh_NACEDesc","domestic"))

country_scatterplot <- country_scatterplot %>% 
  relocate(nonfinance, .after = wholesale_retail)

## Map

world_data <- read.csv("https://raw.githubusercontent.com/albina23/Data-analysis/main/world_data.csv")

country_scatterplot_ <- country_industry %>%
  group_by(Country, NACEDesc, Sh_NACEDesc, domestic) %>%
  mutate(EquityForeign=sum(EquityOwned)) %>%
  mutate(EquityForeignShare=round(EquityForeign/Equity,3)) %>%
  select(Country, Country_Name, Continent,NACEDesc, Sh_NACEDesc, domestic, EquityForeignShare, EquityCountry) %>% 
  distinct() %>% 
  arrange(Country_Name)


country_scatterplot2_ <- country_industry %>%
  group_by(Country, NACEDesc, Sh_NACEDesc) %>%
  mutate(EquityForeign=sum(EquityOwned)) %>%
  mutate(EquityForeignShare=round(EquityForeign/Equity,3)) %>%
  select(Country, Country_Name, Continent,NACEDesc, Sh_NACEDesc, domestic, EquityForeignShare, EquityCountry) %>% 
  mutate(domestic="Global") %>% 
  distinct() %>% 
  arrange(Country_Name)


country_scatterplot_ <- bind_rows(country_scatterplot_, country_scatterplot2_)


country_scatterplot_ <- country_scatterplot_ %>% 
  select(Country, Country_Name, NACEDesc, Sh_NACEDesc, domestic, EquityForeignShare)



nonfinance_ <- country_industry%>%
  mutate(nonfinance=ifelse(NACEDesc=="finance", "finance", "nonfinance")) %>% 
  group_by(Country, nonfinance,Sh_NACEDesc,domestic) %>% 
  mutate(EquityForeign=sum(EquityOwned)) %>%
  select(Country, Country_Name, nonfinance, Sh_NACEDesc,Equity, domestic, EquityForeign, EquityCountry) %>%
  distinct() %>% 
  mutate(EquityTotal=sum(Equity)) %>%
  mutate(EquityForeignShare= round((EquityForeign/EquityTotal),3)) %>%
  select(Country, Country_Name, nonfinance, Sh_NACEDesc,  domestic, EquityForeignShare) %>%
  distinct() %>%
  ungroup() %>% 
  filter(Country!="") %>% 
  filter(nonfinance=="nonfinance")


nonfinance2_ <- country_industry%>%
  mutate(nonfinance=ifelse(NACEDesc=="finance", "finance", "nonfinance")) %>% 
  group_by(Country, nonfinance,Sh_NACEDesc,domestic) %>% 
  mutate(EquityForeign=sum(EquityOwned)) %>%
  select(Country, Country_Name, nonfinance, Sh_NACEDesc,  Equity, domestic, EquityForeign, EquityCountry) %>%
  distinct() %>% 
  mutate(EquityTotal=sum(Equity)) %>%
  mutate(EquityForeignShare=round(EquityForeign/EquityTotal,3)) %>% 
  select(Country, Country_Name, nonfinance, Sh_NACEDesc, domestic, EquityForeignShare) %>%
  distinct() %>%
  ungroup() %>%
  filter(nonfinance=="nonfinance") %>%
  group_by(Country,Sh_NACEDesc) %>% 
  mutate(EquityForeignShare=sum(EquityForeignShare)) %>% 
  mutate(domestic="Global") %>%
  distinct() %>% 
  ungroup() %>%
  filter(Country!="")


nonfinance_ <- bind_rows(nonfinance_, nonfinance2_)
nonfinance_ <- nonfinance_ %>% 
  rename(NACEDesc=nonfinance)

country_scatterplot_ <- bind_rows(country_scatterplot_,nonfinance_)


df <- country_scatterplot_ %>% 
  rename(ISO2=Country) %>% 
  rename(Value=EquityForeignShare)



worldMaps <- function(map_data, world_data, NACEDesc, Sh_NACEDesc, domestic){
  
  # Function for setting the aesthetics of the plot
  my_theme <- function () { 
    theme_bw() + theme(axis.text = element_blank(),
                       axis.title = element_text(size = 14),
                       strip.text = element_text(size = 14),
                       panel.grid.major = element_blank(), 
                       panel.grid.minor = element_blank(),
                       panel.background = element_blank(), 
                       axis.ticks = element_blank(),
                       legend.position = "right",
                       panel.border = element_blank(), 
                       strip.background = element_rect(fill = 'white', colour = 'white'))
  }
  
  # Select only the data that the user has selected to view
  plotdf <- df[df$NACEDesc == NACEDesc & df$Sh_NACEDesc == Sh_NACEDesc & df$domestic == domestic,]
  plotdf <- plotdf[!is.na(plotdf$ISO2), ]
  
  # Add the data the user wants to see to the geographical world data
  world_data['NACEDesc'] <- rep(NACEDesc, nrow(world_data))
  world_data['Sh_NACEDesc'] <- rep(Sh_NACEDesc, nrow(world_data))
  world_data['domestic'] <- rep(domestic, nrow(world_data))
  world_data['Value'] <- plotdf$Value[match(world_data$ISO2, plotdf$ISO2)]
  
  
  # Specify the plot for the world map

  g <- ggplot() + 
    geom_polygon_interactive(data = world_data, color = 'gray70', size = 0.1,
                             aes(x = long, y = lat, fill = Value, group = group, 
                                 tooltip = sprintf("%s<br/>%s", Country_Name, Value))) + 
    scale_fill_gradientn(colours = brewer.pal(5, "YlOrRd"), na.value = 'white') + 
    labs(fill = Sh_NACEDesc, color = Sh_NACEDesc, 
         #title = "Share of capital owned by a sector in a given industry & country", 
         x = NULL, y = NULL) + 
    my_theme()
  
  return(g)
}



## Network files

country_network <- country_industry %>% 
  mutate(Country=replace(Country,Country=="",NA)) %>% 
  mutate(ShareholderCountry=replace(ShareholderCountry,ShareholderCountry=="",NA)) %>% 
  mutate(from=paste(ShareholderCountry,"-",Sh_NACEDesc)) %>% 
  mutate(to=paste(Country,"-",NACEDesc))%>% 
  arrange(Country_Name)     


nodes1 <- country_network %>% 
  select(to, Country, Country_Name, Continent, NACEDesc)

nodes2 <- country_network %>% 
  select(from, ShareholderCountry, Sh_Country_Name,Sh_Continent, Sh_NACEDesc) %>% 
  rename(to=from,Country=ShareholderCountry,Country_Name=Sh_Country_Name,Continent=Sh_Continent,NACEDesc=Sh_NACEDesc)

node_file <- bind_rows(nodes1,nodes2) %>% 
  distinct() %>% 
  rename(id=to) %>% 
  mutate(label=NACEDesc) %>% 
  mutate(group=NACEDesc)


edge_list <- country_network %>% 
  select(from, to, EquityOwned, domestic,Country,Country_Name, Continent, NACEDesc,ShareholderCountry, Sh_Country_Name,Sh_Continent,Sh_NACEDesc) %>% 
  rename(weight=EquityOwned) %>%
  mutate(width=0.00000002*weight) %>% 
  distinct()

edge_list$internal[edge_list$from==edge_list$to] <- "Internal (self-loops)"
edge_list$internal[edge_list$from!=edge_list$to] <- "External"

### calculate "domestic" node degree 
node_degree <- edge_list %>% 
  group_by(from, domestic) %>% 
  mutate(degree=sum(weight)) %>%  #domestic degree
  filter(domestic=="Domestic") %>% 
  select(from,degree,domestic) %>%
  distinct() %>% 
  rename(id=from) %>% 
  rename(value=degree)


node_file <- left_join(node_file, node_degree, by = "id") 

node_file <- node_file %>% select(-domestic)

node_file <- node_file %>% 
  mutate(label = replace(label,is.na(label),"N.A.")) %>% 
  mutate(group = replace(group,is.na(group),"N.A."))


country_networkf <- country_industry %>% 
  mutate(from=paste(ShareholderCountry)) %>% 
  mutate(to=paste(Country)) %>% 
  arrange(Country_Name)     


node_filef <- country_networkf %>% 
  filter(from!="II") %>% 
  select(from, Sh_Country_Name, Sh_Continent) %>% 
  distinct() %>%
  rename(id=from) %>%
  mutate(label=id) %>% 
  rename(title=Sh_Country_Name) %>% 
  rename(group=Sh_Continent)



edge_listf <- country_networkf %>% 
  select(from, to, EquityOwned, domestic,Country_Name, Sh_Country_Name) %>%
  rename(weight=EquityOwned) %>%
  group_by(from, to,domestic,Country_Name, Sh_Country_Name) %>% 
  mutate(weight=sum(weight)) %>% 
  mutate(width=0.00000002*weight) %>% 
  distinct() %>% 
  arrange(Country_Name)


### calculate "domestic" node degree 
node_degreef <- edge_listf %>% 
  filter(domestic=="International") %>% 
  group_by(from) %>% 
  mutate(degree=sum(weight)) %>%  #domestic degree
  select(from,degree) %>%
  distinct() %>% 
  rename(id=from) %>% 
  rename(value=degree)

### 
node_filef <- left_join(node_filef, node_degreef, by = "id") 



country_industry <- country_industry %>% 
  arrange(Country_Name) 


### UI portion

ui <- dashboardPage(
  skin="purple",
  dashboardHeader(title = "Geographies of Financialization",
                  titleWidth = 350),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      
      menuItem("Project description", tabName = "project", icon = icon("search plus")),
      
      #menuItem("Data description", tabName = "data", icon = icon("table")),
      
      menuItem("National comparison charts", tabName = "charts", icon = icon("chart-bar"),startExpanded = TRUE,
               menuSubItem("Domestic shareholders", tabName = "domestic"),
               menuSubItem("Domestic finance", tabName = "finance"),
               menuSubItem("Domestic industries", tabName = "shareholder"),
               menuSubItem("Foreign shareholders", tabName = "foreign"),
               menuSubItem("Scatterplot of countries", tabName = "scatterplot"),
               menuSubItem("Map of shareholders", tabName = "map")
      ),
      
      
      menuItem("Network charts", tabName = "national_network", icon = icon("network-wired"),startExpanded = TRUE,
               menuSubItem("Country's domestic investments by sector", tabName = "domestic_network"),
               menuSubItem("Comparing countries' domestic investments", tabName = "domestic_network2"),
               menuSubItem("Country's FDI network", tabName = "foreign_network"),
               menuSubItem("Comparing countries' FDI networks", tabName = "foreign_network2"),
               menuSubItem("Global corporate ownership network", tabName = "country_network")
      ),
      
      #menuItem("US-centric charts", tabName = "us_network", icon = icon("flag-usa"),startExpanded = TRUE,
               #menuSubItem("Domestic ownership", tabName = "domestic_ownership"),
               #menuSubItem("Foreign ownership", tabName = "foreign_ownership"),
               #menuSubItem("Global ownership", tabName = "global_ownership")
      #),
      
      #menuItem("Author description", tabName = "author", icon = icon("glasses")),
      
      menuItem("References", tabName = "resources", icon = icon("question-circle"))

    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML('
      .main-header .logo {
                              font-weight: bold;
                              font-size: 20px;
                              }
                              '))),
    
    tags$head(tags$style(HTML('
      .content-wrapper {
        background-color: #ffffff;
      }
    '))),
    
    tags$head(tags$style('h1 {color:black;}')),
    
    tabItems(
      
      # Intro tab content - description of project
      tabItem(tabName = "project",
              
              fluidPage(
              br(), 
              h1("Who Owns the Means of Production? Uneven Geographies of Financialization"),
              
              br(),
              
              fluidRow(
                
                column(width=6, h2(strong("Project description and rationale"))),
                
                column(width=6, h2(strong("Data and methods")))),
              
              br(), 
              
              fluidRow(
                
                column(width=6, 
                       
                       h4(p("Financialization is a term used to broadly describe the rising importance of finance in the global economy and society at large (Epstein, 2005). In the United States, one of the primary characteristics of the post-1980 financial turn has been the rising share of corporate profits accrued by the US financial sector (Krippner, 2005). This led to the conceptualization of financialization as a new regime of accumulation where profits increasingly accrue through financial rather than productive channels (ibid)."),
                          p(" By examining the primary profit-generating activities performed by the US financial sector as a whole and the composition of its income sources and assets, in my Master's thesis I demonstrated that the increased profitability of US finance can largely be attributed to a transition from credit intermediation (i.e. lending) to the management and ownership of capital. By showing how the share of US capital directly owned and managed by US financial firms has grown from 3 percent in 1945 to at least 62 percent in 2018, I proposed that financialization in the United States should be primarily understood as a new regime of property relations, in which the class of financiers have established themselves as the direct owners of the means of production, having at their discretion ultimate control over the US economy by way of collectively holding the most shares by far in American corporations."),
                          p("This analysis project expands on the research conducted in my Master's thesis by examining two main questions. First, it explores the extent to which financiers have established themselves as the new, dominant owners of capital in other countries or whether financialization as a new regime of property relations has been a US-only phenomenon. Secondly, it develops visualizations to show the global dominance of American financial firms in this global corporate ownership network. The empirical analysis involves examining 6.4 million ownership ties of 2.9 million firms around world from 2018 that add up to $114.4 trillion in owned equity. The project aims to expose a staggering consolidation of power obtained by the US financial sector through a series of static and interactive visualizations and advance our understanding of the influence exerted by American finance in the global economy, contributing to the literatures of financialization, corporate networks, and geographies of advanced producer services.")
                       )),
                
                column(width=6, 
                       
                       h4(p("The project examines corporate ownership data provided by the Orbis database (Bureau van Dijk), which offers the most comprehensive co-ownership dataset of firms (both public and private) and state enterprises available to date, covering over 375 million entities around the world, and providing detailed financial and geographical information for each firm and quantifiable ownership ties between them. Although the data coverage is uneven with significantly less information available on firms located in the Global South (see Garcia-Bernardo and Takes, 2018), for each firm Orbis tries to provide basic information on firm's location and industry, financial information from firm's balance-sheets and income statements, as well as data on corporate ownership ties by listing everyone who owns a particular firm and everyone who a particular firm owns."),
                          p("While Orbis advertises that it has some information on 375 million firms, in 2018 only 8.9 million firms had available information on total assets (a metric relevant to estimating the value of equity and quantifying each ownership tie in dollar terms). As often the case with financial data, the distribution of the total assets variable was highly skewed with a small number of firms accounting for a large share of total assets. I filtered my sample of firms based on the total assets variable, collecting information on all firms with at least $1 million in total assets in 2018. My dataset features 2.9 million unique firms located in 202 countries. I estimate that cumulatively these 2.9 million firms account for 99% of total assets in the Orbis database. For each firm, I collected basic and financial information, as well as information on all of its shareholders (and their respective basic and financial information). My core list of 2.9 million firms has 6.4 million unique shareholders, featuring 6.7 million weighted ownership ties between them, totalling $114.4 trillion in owned equity. Given that it is next to impossible to properly visualize 6.6 million nodes with 6.7 million edges, I have aggregated information in the derived dataset at the level of countries and sectors. This aggregation allows me to visualize the dominance of the domestic financial sector within each country as well as visualize the power of the US financial sector in the global corporate network."),
                          p("Visualizations were developed using the following R packages: dplyr, tidyr, ggplot2, plotly, visNetwork, igraph, forcats, DT, maps, ggiraph, RColorBrewer, scales, chorddiag, and htmltools. The project webpage and interactive visualizations and tables were developed using shiny and shinydashboard."))
                       
                )),
              
              br(),
              
              h2(strong("Overview of visualizations")),
              
              br(),
              
              fluidRow(
                column(width=6,
                       h3(strong("Domestic owners of capital in a given industry in a given country"))),
                
                column(width=6,
                       h3(strong("Share of capital in a given industry in a given country owned by domestic financial sector")))),
              
              
              fluidRow(
                
                column(width=5,
                       h4("This stacked bar chart shows the share of capital owned domesticially (i.e. by shareholders located in the same country) in a given industry for a selected country. The visualization allows one to easily compare rates of domestic capital ownership between different industries, while the facets make it easy to compare these values between different countries.")),
                
                column(width=1,h4("")),
                
                column(width=5,
                       h4("This stacked bar chart shows the share of capital owned by the domestic financial sector (i.e. by financial firms located in the same country) in a given industry for a selected country. The visualization allows one to easily compare rates of financialization of capital ownership for different industries, while the facets make it easy to compare these values between different countries."))),
              
              
              
              fluidRow(
                column(width=5,
                       htmlOutput("picture1"),height = 600),
                
                column(width=1,h4("")),
                
                column(width=5,
                       htmlOutput("picture2"),height = 600)),
              
              br(),
              
              fluidRow(
                column(width=6,
                       h3(strong("Share of capital in a given industry/given country owned by type of domestic shareholder"))),
                
                
                column(width=6,
                       h3(strong("Share of capital owned by a sector in a given industry and country")))),
              
              
              fluidRow(
                
                column(width=5, 
                       h4("This stacked bar chart breaks down domestic shareholders by different industries, showing the share of capital that each type of domestic industry owns in each sector in a given country . The visualization allows one to compare the primary domestic shareholders within each industry and between countries.")),
                
                column(width=1,h4("")),
                
                column(width=5,
                       h4("This map visualizes the country-level differences in the rates of capital ownership for different types of shareholders for each economic sector. The user has an option to specify the industry that they want to examine, the industry of the shareholder, and the type of shareholder: domestic, foreign, or global."))),
              
              
              fluidRow(
                
                column(width=5,
                       htmlOutput("picture3"),height = 600),
                
                column(width=1,h4("")),
                
                column(width=5,
                       htmlOutput("picture4"),height = 600)),
              
              
              br(),
              
              column(width=12,
                     h3(strong("Share of capital in a given industry in a given country owned by foreign investors"))),
              
              column(width=7,
                     h4("This stacked bar chart provides information on the composition of foreign investors in each industry for a given country, showing the share of capital that other countries own in each sector of the selected country.")),
              
              
              column(width=9,
                     htmlOutput("picture5"),height = 600),
              
              
              br(),
              
              
              column(width=12,
                     h3(strong("Share of capital owned by a selected shareholder in chosen industries"))),
              
              column(width=7,
                     h4("This scatterplot visualizes the country-level differences in the rates of capital ownership for different types of shareholders for two selected economic sectors (one on the x-axis, another on the y-axis). The user has an option to specify the two sectors that they want to examine, the industry of the shareholder, and the type of shareholder: domestic, foreign, or global.")),
              
              column(width=12,
                     htmlOutput("picture6"),height = 600),
              
              br(),
              
              
              column(width=12,
                              h3(strong("Domestic investments by sector"))),
                       
              column(width=7,
                       h4("These two visualizations show domestic ownership ties between industries for a selected country. The information on the domestic investment flows are represented into views: a chord diagram (shown on the left) and a network (shown on the right).")),
               
              column(width=8,
                       htmlOutput("picture7"),height = 600),
                

              br(), 
              
              
              column(width=12,
                     h3(strong("Network of incoming and outgoing FDI"))),
              
              column(width=7,
                     h4("This network shows the incoming and outgoing foreign direct investments (FDI) for a selected country. The outgoing investments are shown in blue, the incoming investments are shown in red.")),
              
              column(width=8,
                     htmlOutput("picture8"),height = 600),
              
              br(), 
              
              
              column(width=12,
                     h3(strong("Global investments by countries"))),
              
              column(width=7,
                     h4("These two visualizations show global ownership ties between countries. The information on the global investment flows are represented into views: a chord diagram (shown on the left) and a network (shown on the right).")
              ),
              
              column(width=9,
                     htmlOutput("picture9"),height = 600)
              
      )
      ),
      
      
      # Tab content - description of data
      tabItem(tabName = "data"
              
              
              
      ),
      
      

      # Chart tab content - domestic and foreign owners
      tabItem(tabName = "domestic",
              
              fluidRow(
                
                box(width=2,
                    pickerInput(inputId="country0", label="Choose a country",  choices=unique(country_foreign$Country_Name),selected = c("United States","United Kingdom","China"),options = list(`actions-box` = TRUE),multiple = T)),
                
                box(width=2,
                    pickerInput(inputId="continent0", label="Choose a continent",  choices=c("Africa","Asia","Europe","North America","Oceania","South America"),selected = c("Africa","Asia","Europe","North America","Oceania","South America"),options = list(`actions-box` = TRUE),multiple = T)),
                
                
                box(width=2,
                    pickerInput(inputId="industry0", label="Choose an industry",   choices=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"), selected=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"),options = list(`actions-box` = TRUE),multiple = T)),
                
                box(width=4,
                    sliderInput(inputId="equity0", label="Select countries based on their degree (in th USD)", min=0, max=20000000000,
                                value = c(2000000000, 20000000000))),
                
                box(width=1,
                    actionButton("reset_input", "Reset all inputs"))
              ),  
              
              fluidRow(
                
                box(width=3,
                    sliderInput(inputId="height0", label="Change the height of graphs by a factor of ...", min=0.5, max=10,value = 1,step=0.1)),
                
                box(width=3,
                    sliderInput(inputId="cols0", label="Select the number of columns", min=1, max=10,value = 3)),
                
                box(width=3,
                    sliderInput(inputId="font_sector0", label="Change the size of a font for economic sectors by a factor of ...", min=0.2, max=2,value = 1, step=0.1)),
                
                box(width=3,
                    sliderInput(inputId="font_num0", label="Change the size of a font for percentages by a factor of ...", min=0.2, max=2,value = 1, step=0.1))
              
                ),
              

              fluidRow(
                
                column(width=12,
                       h1("Domestic owners of capital in a given industry in a given country")),
                
                box(
                  width=12,
                  plotOutput("chart_0")))
      ),
      
              
              
      # Chart tab content - Types of Shareholders (domestic, foreign, all)
      tabItem(tabName = "shareholder",

                fluidRow(
                
                box(width=2,
                    pickerInput(inputId="country2", label="Choose a country",  choices=unique(country_industry$Country_Name),selected = c("United States","United Kingdom","China"),options = list(`actions-box` = TRUE),multiple = T)),
                
                box(width=2,
                    pickerInput(inputId="continent2", label="Choose a continent",  choices=c("Africa","Asia","Europe","North America","Oceania","South America"),selected = c("Africa","Asia","Europe","North America","Oceania","South America"),options = list(`actions-box` = TRUE),multiple = T)),
                
                box(width=2,
                    pickerInput(inputId="industry2", label="Choose an industry",   choices=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"), selected=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"),options = list(`actions-box` = TRUE),multiple = T)),
                
                box(width=2,
                    pickerInput(inputId="sh_industry2", label="Choose an industry of the shareholder",   choices=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","individual","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"), selected=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","individual","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"),options = list(`actions-box` = TRUE),multiple = T)),
                
                box(width=2,
                    sliderInput(inputId="equity2", label="Select countries based on their degree (in th USD)", min=0, max=20000000000,
                                value = c(2000000000, 20000000000))),
                
                box(width=1,
                    actionButton("reset_input2", "Reset all inputs"))
                
              ),  
              
              fluidRow(
                
                box(width=3,
                    sliderInput(inputId="height2", label="Change the height of graphs by a factor of ...", min=0.5, max=10,value = 1,step=0.1)),
                
                box(width=3,
                    sliderInput(inputId="cols2", label="Select the number of columns", min=1, max=10,value = 3)),
                
                box(width=3,
                    sliderInput(inputId="font_sector2", label="Change the size of a font for economic sectors by a factor of ...", min=0.2, max=2,value = 1, step=0.1)),
                
                box(width=3,
                    sliderInput(inputId="font_num2", label="Change the size of a font for percentages by a factor of ...", min=0.2, max=2,value = 1, step=0.1))
              
              ),  
              
              fluidRow(
                
                column(width=12,
                       h1("Share of capital in a given industry in a given country owned by type of domestic shareholder")),
                
                box(
                  width=12,
                  plotOutput("chart_2")))
      ),
      
      
      
      
      # Chart tab content - finance
      tabItem(tabName = "finance",
              
              fluidRow(
                
                box(width=2,
                    pickerInput(inputId="country1", label="Choose a country",  choices=unique(country_finance$Country_Name),selected = c("United States","United Kingdom","China"),options = list(`actions-box` = TRUE),multiple = T)),
                
                box(width=2,
                    pickerInput(inputId="continent1", label="Choose a continent",  choices=c("Africa","Asia","Europe","North America","Oceania","South America"),selected = c("Africa","Asia","Europe","North America","Oceania","South America"),options = list(`actions-box` = TRUE),multiple = T)),
                
                
                box(width=2,
                    pickerInput(inputId="industry1", label="Choose an industry",   choices=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"), selected=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"),options = list(`actions-box` = TRUE),multiple = T)),
                
                box(width=4,
                    sliderInput(inputId="equity1", label="Select countries based on their degree (in th USD)", min=0, max=20000000000,
                                value = c(2000000000, 20000000000))),
                
                box(width=1,
                    actionButton("reset_input1", "Reset all inputs"))
              ),  
              
              fluidRow(
                
                box(width=3,
                    sliderInput(inputId="height1", label="Change the height of graphs by a factor of ...", min=0.5, max=10,value = 1,step=0.1)),
                
                box(width=3,
                    sliderInput(inputId="cols1", label="Select the number of columns", min=1, max=10,value = 3)),
                
                box(width=3,
                    sliderInput(inputId="font_sector1", label="Change the size of a font for economic sectors by a factor of ...", min=0.2, max=2,value = 1, step=0.1)),
                
                box(width=3,
                    sliderInput(inputId="font_num1", label="Change the size of a font for percentages by a factor of ...", min=0.2, max=2,value = 1, step=0.1))),
              
              fluidRow(
                
                column(width=12,
                       h1("Share of capital in a given industry in a given country owned by domestic financial sector")),
                
                
                box(
                  width=12,
                  plotOutput("chart_1")))
      ),
      
      
      
      # Chart tab content - Foreign Ownership by industry
      tabItem(tabName = "foreign",
              
              
              fluidRow(
                
                box(width=3,
                    selectInput(inputId="country3", label="Choose a country",  choices=unique(country_foreign2$Country_Name),selected = c("United States"),multiple=FALSE)),
                
                box(width=3,
                    pickerInput(inputId="industry3", label="Choose an industry",   choices=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"), selected=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"),options = list(`actions-box` = TRUE),multiple = T)),
                
                box(width=2,
                    pickerInput(inputId="sh_country3", label="Select countries for shareholders",  choices=unique(country_foreign2$Country_Name),selected = country_foreign2$Country_Name, options = list(`actions-box` = TRUE),multiple = T)),
                
                box(width=2,
                    pickerInput(inputId="continent3", label="Select continents for shareholders",  choices=c("Africa","Asia","Europe","North America","Oceania","South America"),selected = c("Africa","Asia","Europe","North America","Oceania","South America"),options = list(`actions-box` = TRUE),multiple = T)),
                
                box(width=1,
                    actionButton("reset_input3", "Reset all inputs"))
                
              ),  
              
              
              fluidRow(
                
                box(width=3,
                    sliderInput(inputId="height3", label="Change the height of graphs by a factor of ...", min=0.5, max=2,value = 1,step=0.1)),
                
                box(width=3,
                    sliderInput(inputId="cols3", label="Select the number of columns", min=1, max=10,value = 9)),
                
                box(width=3,
                    sliderInput(inputId="font_sector3", label="Change the size of a font for shareholder countries by a factor of ...", min=0.2, max=2,value = 1, step=0.1)),
                
                box(width=3,
                    sliderInput(inputId="font_num3", label="Change the size of a font for percentages by a factor of ...", min=0.2, max=2,value = 1, step=0.1))),
              
              
              
              fluidRow(
                
                column(width=12,
                       h1("Share of capital in a given industry in a given country owned by foreign investors (showing investments above 0.1%)")),
                
                
                box(
                  width=12,
                  plotOutput("chart_3")))
      ),
              
              
              
              
              
              
      
      
      # Chart tab content - scatterplot comparing different countries
      tabItem(tabName = "scatterplot",
              
              fluidRow(
              
              box(width=3,
                    pickerInput(inputId="country4", label="Choose a country",  choices=unique(country_scatterplot$Country_Name), selected =unique(country_scatterplot$Country_Name), options = list(`actions-box` = TRUE),multiple = T)),
              
              box(width=2,
                  pickerInput(inputId="continent4", label="Choose a continent",  choices=c("Africa","Asia","Europe","North America","Oceania","South America"),selected = c("Africa","Asia","Europe","North America","Oceania","South America"),options = list(`actions-box` = TRUE),multiple = T)),
              
              
              box(width=2,
                  selectInput(inputId="shareholder4", label="Choose industry of the shareholder",   choices=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","individual","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"), selected ="finance", multiple=FALSE)),
              
              box(width=3,
                  sliderInput(inputId="equity4", label="Select countries based on their degree (in th USD)", min=0, max=20000000000,
                              value = c(50000000, 20000000000))),
              
              box(width=1,
                  actionButton("reset_input4", "Reset all inputs"))
              
              ),
              
              fluidRow(
              
                box(width=4,
                    selectInput(inputId="domestic4", label="Choose to show domestic, foreign, or global shareholders",  choices=c("Domestic","International","Global"), selected ="Domestic",multiple=FALSE)),
                
                
                
              # Select variable for x-axis
              box(width=4,
                  selectInput(inputId = "y", label = "Y-axis: Choose an industry",
                            choices = colnames(country_scatterplot)[7:25],
                            selected = "finance")),
              
                  # Select variable for x-axis
              box(width=4,
                  selectInput(inputId = "x", label = "X-axis: Choose an industry",
                              choices = colnames(country_scatterplot)[7:25],
                              selected = "nonfinance"))
              
              ),
              
              
              fluidRow(
                
                column(width=12,
                       h1("Share of capital owned by a selected shareholder in chosen industries")),
                
                
                box(
                  width=12,
                  plotlyOutput("chart_4",height = 850)))
        ),
      
      
      
      
      
      # Map
      tabItem(tabName = "map",
      
              fluidPage(
                
                fluidRow(box(width=3,
                    selectInput(inputId = "Sh_NACEDesc",
                                label = "Choose industry for the shareholder",
                                choices=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","individual","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"), selected ="finance",multiple=FALSE)),
                    
                box(width=3,
                    selectInput(inputId = "NACEDesc",
                                label = "Choose your industry",
                                choices=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail","nonfinance"), selected ="finance",multiple=FALSE)),
                    
                    
                box(width=3,
                    selectInput(inputId = "domestic",
                                label = "Choose to show domestic, foreign, or global shareholders",  choices=c("Domestic","International","Global"), selected ="Global",multiple=FALSE)),
                
                box(width=1,
                    actionButton("reset_input5", "Reset all inputs"))),
                    
                
                fluidRow(
                column(width=12,
                       h1("Share of capital owned by a sector in a given industry and country")),
                
                  
                column(6,
                  # Hide errors
                    tags$style(type = "text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"),
                    
                    # Output: interactive world map
                    girafeOutput("distPlot", height=850
                                 )),
                
                
                column(6,
                       DT::dataTableOutput('map_table', height = 800)))
              
              )
      
      ),
      
      
      # National network tab conent - domestic corporate ownership
      tabItem(tabName = "domestic_network",
              
              fluidPage(
                
                fluidRow(
                
                box(width=3,
                    selectInput(inputId="country5", label="Choose a country",  choices=unique(edge_list$Country_Name), selected="United States",multiple=FALSE)),
                
                box(width=2,
                    pickerInput(inputId="industry5", label="Choose which sectors to include",   choices=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","individual","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"), selected=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","individual","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"),options = list(`actions-box` = TRUE),multiple = T)),
                
                box(width=2,
                    pickerInput(inputId="internal1", label="Do you want to include external and/or internal edges (self-loops)?",  choices=unique(edge_list$internal), selected=c("External","Internal (self-loops)"),multiple=TRUE)),
                
                box(width=3,
                    sliderInput(inputId="edge_weight0", label="Choose equity value for individual edges (th USD)", min=0, max=5000000000,
                                value = c(0, 5000000000))),
                
                box(width=1,
                    actionButton("reset_input6", "Reset all inputs"))),
                
                fluidRow(
                
                column(6,
                       h1("Domestic investments by sector: A chord diagram view")),
                
                column(6,
                       h1("Domestic investments by sector: A network view"))),
                
                fluidRow(
                
                column(6,
                       chorddiagOutput("distPlot2", height = 1000)),
                
                column(6,
                       visNetworkOutput("domestic_network",height = 800)))
                
                #column(6,DT::dataTableOutput('domestic_network_table', height = 800))
                
            
              )
  
              
      ),
     
      
      # National network tab conent - comparing domestic corporate ownership
      tabItem(tabName = "domestic_network2",
              
              fluidPage(
                
                fluidRow(
                box(width=2,
                    selectInput(inputId="country8a", label="Choose Country #1",  choices=unique(edge_list$Country_Name), selected="United States",multiple=FALSE)),
                
                box(width=2,
                    selectInput(inputId="country8b", label="Choose Country #2",  choices=unique(edge_list$Country_Name), selected="United Kingdom",multiple=FALSE)),
                
                
                box(width=1,
                    pickerInput(inputId="industry8", label="Choose which sectors to include",  choices=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","individual","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"), selected=c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","individual","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"),options = list(`actions-box` = TRUE),multiple = T)),
                
                box(width=2,
                    pickerInput(inputId="internal1a", label="Do you want to include external and/or internal edges (self-loops)?",  choices=unique(edge_list$internal), selected=c("External","Internal (self-loops)"),multiple=TRUE)),
                
                box(width=3,
                    sliderInput(inputId="edge_weight0a", label="Choose equity value for individual edges (th USD)", min=0, max=5000000000,
                                value = c(0, 5000000000))),
                
                box(width=1,
                    actionButton("reset_input7", "Reset all inputs"))),
                
                fluidRow(
                
                column(6,
                       h1("Country #1: A chord diagram of domestic investments by sector")),
                
                column(6,
                       h1("Country #2: A chord diagram of domestic investments by sector"))),
                
                
                fluidRow(
                column(6,
                       chorddiagOutput("distPlot2a", height = 1000)),
                
                column(6,
                       chorddiagOutput("distPlot2b", height = 1000)))
                
                #column(6,visNetworkOutput("domestic_network_facet1",height = 800)),
                
                #column(6,visNetworkOutput('domestic_network_facet2',height = 800))
                
              )
              
              
              
      ), 

      
      # National network tab conent - foreign corporate ownership
      tabItem(tabName = "foreign_network",
              
              fluidPage(
                
                
                fluidRow(box(width=2,
                    selectInput(inputId="country7", label="Choose a country",  choices=unique(edge_listf$Country_Name), selected="United States",multiple=FALSE)),
                
                box(width=3,
                    sliderInput(inputId="country_degree", label="Select countries based on equity owned (th USD)", min=0, max=20000000000,
                                value = c(50000000, 20000000000))),  
                
                box(width=2,
                    selectInput(inputId="incoming", label="Choose whether to include incoming or outgoing investments",  choices=c("Incoming","Outgoing"), selected=c("Incoming","Outgoing"),multiple=TRUE)),
                
                
                box(width=3,
                    sliderInput(inputId="edge_weight", label="Choose equity value for individual edges (th USD)", min=0, max=2000000000,
                                value = c(1000000, 2000000000))),
                
                box(width=1,
                    actionButton("reset_input8", "Reset all inputs"))),
                
                
                column(12,
                       h1("Network of incoming and outgoing FDI")),
                
                fluidRow(
                column(6,
                       visNetworkOutput("FDInetwork",height = 800)),
                
                column(6,
                       DT::dataTableOutput('FDI_network_table', height = 800)))
                
                
              )
      ),
      
      
      
      # National network tab conent - comparing foreign corporate ownership
      tabItem(tabName = "foreign_network2",
              
              
              fluidPage(
                
                fluidRow(
                
                box(width=2,
                    selectInput(inputId="country7a", label="Choose Country #1",  choices=unique(edge_listf$Country_Name), selected="United States",multiple=FALSE)),
                
                box(width=2,
                    selectInput(inputId="country7b", label="Choose Country #2",  choices=unique(edge_listf$Country_Name), selected="United Kingdom",multiple=FALSE)),
                
                box(width=3,
                    selectInput(inputId="incominga", label="Incoming or outgoing investments",  choices=c("Incoming","Outgoing"), selected=c("Incoming","Outgoing"),multiple=TRUE)),
                
                box(width=2,
                    sliderInput(inputId="country_degreea", label="Select countries based on owned equity(th USD)", min=0, max=20000000000,
                                value = c(50000000, 20000000000))),  
                
                box(width=2,
                    sliderInput(inputId="edge_weighta", label="Choose equity value for edges(th USD)", min=0, max=2000000000,
                                value = c(1000000, 2000000000))),
                
                box(width=1,
                    actionButton("reset_input9", "Reset all inputs"))),
                
                fluidRow(
                  column(6,
                       h1("Country #1: Network of incoming and outgoing FDI")),
                
                column(6,
                       h1("Country #2: Network of incoming and outgoing FDI"))),
                
                
                fluidRow(column(6,
                       visNetworkOutput("FDInetwork1",height = 800)),
                
                column(6,
                       visNetworkOutput('FDInetwork2',height = 800)))
              
      )), 
      
      
      
      # Global network tab conent - corporate ownership between countries
      tabItem(tabName = "country_network",
              
              fluidPage(
                
                fluidRow(
                box(width=1,
                    pickerInput(inputId="country6", label="Choose a Country",  choices=unique(country_industry$Country_Name),selected = country_industry$Country_Name,options = list(`actions-box` = TRUE),multiple = T)),
                
                
                box(width=1,
                    pickerInput(inputId="continent6", label="Choose Continent",  choices=c("Africa","Asia","Europe","North America","Oceania","South America"),selected = c("Africa","Asia","Europe","North America","Oceania","South America"),options = list(`actions-box` = TRUE),multiple = T)),
                
                
                box(width=3,
                    sliderInput(inputId="country_degree1", label="Select countries based on equity owned (th USD)", min=1000, max=20000000000,
                                value = c(100000000, 20000000000))),  
                
                box(width=2,
                    selectInput(inputId="domestic_foreign", label="Choose whether to include both international and domestic ties",  choices=c("Domestic","International"), selected="International",multiple=TRUE)),
                
                box(width=3,
                    sliderInput(inputId="edge_weight1", label="Choose equity value for individual edges (th USD)", min=0, max=20000000000,
                                value = c(0, 20000000000))),
                
                box(width=1,
                    actionButton("reset_input10", "Reset all inputs")))),
              
              fluidRow(
                
              column(6,
                     h1("Global investments by countries: A chord diagram view")),
              
              column(6,
                     h1("Global investments by countries: A network view"))),
              
              fluidRow(
              
              column(6,
                       chorddiagOutput("distPlot3", height = 1000)),
                
              column(6,
                       visNetworkOutput("globalnetwork",height = 800)))
              
         ),

    
      
      
      # US centric tab conent - domestic ownership
      #tabItem(tabName = "domestic_ownership"
              
              
              
      #),
      
      
      # US centric tab conent - domestic ownership
      #tabItem(tabName = "foreign_ownership"
              
              
              
      #),
      
      
      # US centric tab conent - domestic ownership
      #tabItem(tabName = "global_ownership"
              
              
              
      #),
      
      
      # Author tab conent
      tabItem(tabName = "author"
              
              
              
      ),
      
      
      
      
      
      # Resources tab conent
      tabItem(tabName = "resources"
              
              
              
      )
      
      )
    )
  )




### Server portion
server <- function(input, output,session) {
  
  output$picture1 <-
    renderText({
      c(
        '<img src="',
        "https://github.com/albina23/Data-analysis/blob/main/images/domestic.png?raw=true",
        '">'
      )
    })
  
  
  output$picture2 <-
    renderText({
      c(
        '<img src="',
        "https://github.com/albina23/Data-analysis/blob/main/images/finance.png?raw=true",
        '">'
      )
    })
  
  
  
  output$picture3 <-
    renderText({
      c(
        '<img src="',
        "https://github.com/albina23/Data-analysis/blob/main/images/domestic_shareholders.png?raw=true",
        '">'
      )
    })
  
  
  
  
  output$picture4 <-
    renderText({
      c(
        '<img src="',
        "https://github.com/albina23/Data-analysis/blob/main/images/map.png?raw=true",
        '">'
      )
    })
  
  
  output$picture5 <-
    renderText({
      c(
        '<img src="',
        "https://github.com/albina23/Data-analysis/blob/main/images/foreign.png?raw=true",
        '">'
      )
    })
  
  output$picture6 <-
    renderText({
      c(
        '<img src="',
        "https://github.com/albina23/Data-analysis/blob/main/images/scatterplot.png?raw=true",
        '">'
      )
    })
  
  output$picture7 <-
    renderText({
      c(
        '<img src="',
        "https://github.com/albina23/Data-analysis/blob/main/images/domestic_investments.png?raw=true",
        '">'
      )
    })
  
  output$picture8 <-
    renderText({
      c(
        '<img src="',
        "https://github.com/albina23/Data-analysis/blob/main/images/FDI2.png?raw=true",
        '">'
      )
    })
  
  output$picture9 <-
    renderText({
      c(
        '<img src="',
        "https://github.com/albina23/Data-analysis/blob/main/images/global_investments.png?raw=true",
        '">'
      )
    })
  
  output$chart_0 <- renderPlot({
    
    foreign_domestic <- country_foreign %>%
      filter(NACEDesc!="NA") %>% 
      filter(Country!="NA") %>%
      filter(Continent!="NA") %>%
      filter(domestic=="Domestic") %>%
      filter(Country_Name %in% input$country0) %>%
      filter(Continent %in% input$continent0) %>%
      filter(NACEDesc %in% input$industry0) %>%
      filter(EquityCountry<input$equity0[2]) %>%
      filter(EquityCountry>input$equity0[1]) %>%
      ggplot(aes(fill=fct_rev(NACEDesc), x = fct_rev(NACEDesc), y=EquityShare)) +
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~Country_Name, ncol = input$cols0) +
      geom_text(aes(label= scales::percent(round(EquityShare,2),accuracy = 1L),y=EquityShare), position = position_stack(vjust = 0.5), size=4*input$font_num0) +
      coord_flip()+
      guides(fill = guide_legend(reverse=TRUE))+
      labs(
        #title="Domestic owners of capital in a given industry in a given country",
        x="Economic sectors", y="Share of capital owned by domestic shareholders",fill="Economic sectors")+
      theme(title=element_text(size=20,color="black"),
            axis.text=element_text(size=20,color="black"),
            axis.title.y=element_text(size=20,color="black"),
            axis.title.x=element_text(size=20,color="black"),
            axis.text.y.left=element_text(size=18*input$font_sector0,color="black"),
            strip.text=element_text(size=18,color="black"),
            legend.text = element_text(size=20,color="black"),
            legend.title = element_text(size=20,color="black"))
  
  foreign_domestic
  
  },height = function(){input$height0*540})
  
  observe({
    input$reset_input
    updateTextInput(session, "country0", value = c("United States","United Kingdom","China"))
    updateTextInput(session, "continent0", value = c("Africa","Asia","Europe","North America","Oceania","South America"))
    updateTextInput(session, "industry0", value = c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"))
    updateNumericInput(session, "equity0", value = c(2000000000, 20000000000))
    updateNumericInput(session, "height0", value = 1)
    updateNumericInput(session, "cols0", value = 3)
    updateNumericInput(session, "font_sector0", value = 1)
    updateNumericInput(session, "font_num0", value = 1)
    
  })
  
  
  

  output$chart_1 <- renderPlot({
        financialization <- country_finance %>%
          filter(NACEDesc!="NA") %>%
          filter(Country!="NA") %>%
          filter(Continent!="NA") %>%
          filter(Country_Name %in% input$country1) %>%
          filter(Continent %in% input$continent1) %>%
          filter(NACEDesc %in% input$industry1) %>%
          filter(EquityCountry<input$equity1[2]) %>%
          filter(EquityCountry>input$equity1[1]) %>%
          ggplot(aes(fill=fct_rev(NACEDesc), x = fct_rev(NACEDesc))) +
          geom_bar(aes(y=EquityShare),stat = "identity")+
          facet_wrap(~Country_Name, ncol = input$cols1) +
          geom_text(aes(label= scales::percent(EquityShare,accuracy = 1L),y=EquityShare), position = position_stack(vjust = 0.5), size=4*input$font_num1) +
          coord_flip()+
          guides(fill = guide_legend(reverse=TRUE))+
          labs(
            #title="Share of capital in a given industry in a given country owned by domestic financial sector",
            x="Economic sectors", y="Share of capital owned by domestic finance",fill="Economic sectors")+
          theme(title=element_text(size=20,color="black"),
                axis.text=element_text(size=20,color="black"),
                axis.title.y=element_text(size=20,color="black"),
                axis.title.x=element_text(size=20,color="black"),
                axis.text.y.left=element_text(size=18*input$font_sector1,color="black"),
                strip.text=element_text(size=18,color="black"),
                legend.text = element_text(size=20,color="black"),
                legend.title = element_text(size=20,color="black"))
        
    financialization
      },height = function(){input$height1*540})

  
  observe({
    input$reset_input1
    updateTextInput(session, "country1", value = c("United States","United Kingdom","China"))
    updateTextInput(session, "continent1", value = c("Africa","Asia","Europe","North America","Oceania","South America"))
    updateTextInput(session, "industry1", value = c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"))
    updateNumericInput(session, "equity1", value = c(2000000000, 20000000000))
    updateNumericInput(session, "height1", value = 1)
    updateNumericInput(session, "cols1", value = 3)
    updateNumericInput(session, "font_sector1", value = 1)
    updateNumericInput(session, "font_num1", value = 1)
    
  })
  
  
 
  output$chart_2 <- renderPlot({
    domestic_ownership <- country_industry %>%
      filter(NACEDesc!="NA") %>% 
      filter(domestic=="Domestic") %>%
      filter(EquityShare>0.01) %>% 
      filter(Country_Name %in% input$country2) %>%
      filter(Continent %in% input$continent2) %>%
      filter(NACEDesc %in% input$industry2) %>%
      filter(Sh_NACEDesc %in% input$sh_industry2) %>%
      filter(EquityCountry<input$equity2[2]) %>%
      filter(EquityCountry>input$equity2[1]) %>%
      ggplot(aes(fill=Sh_NACEDesc, x = fct_rev(NACEDesc), y=EquityShare)) +
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~Country_Name, ncol = input$cols2) +
      geom_col() + 
      scale_fill_industry()+
      geom_text(aes(label= scales::percent(EquityShare,accuracy = 1L),y=EquityShare), position = position_stack(vjust = 0.5), size=4*input$font_num2) +
      coord_flip()+
      labs(
        #title="Share of capital in a given industry in a given country owned by type of domestic shareholder",
        x="Economic sectors", y="Share of capital owned by particular economic sectors",fill="Shareholder industry")+
      theme(title=element_text(size=20,color="black"),
            axis.text=element_text(size=20,color="black"),
            axis.title.y=element_text(size=20,color="black"),
            axis.title.x=element_text(size=20,color="black"),
            axis.text.y.left=element_text(size=18*input$font_sector2,color="black"),
            strip.text=element_text(size=18,color="black"),
            legend.text = element_text(size=20,color="black"),
            legend.title = element_text(size=20,color="black"))
    
    domestic_ownership
    
      },height = function(){input$height2*540})
  

  observe({
    input$reset_input2
    updateTextInput(session, "country2", value = c("United States","United Kingdom","China"))
    updateTextInput(session, "continent2", value = c("Africa","Asia","Europe","North America","Oceania","South America"))
    updateTextInput(session, "industry2", value = c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"))
    updateTextInput(session, "sh_industry2", value = c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","individual","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"))
    updateNumericInput(session, "equity2", value = c(2000000000, 20000000000))
    updateNumericInput(session, "height2", value = 1)
    updateNumericInput(session, "cols2", value = 3)
    updateNumericInput(session, "font_sector2", value = 1)
    updateNumericInput(session, "font_num2", value = 1)
    
  })
  
  
 
  output$chart_3 <- renderPlot({
    foreignowners <- country_foreign2 %>%
      filter(NACEDesc!="NA") %>%
      filter(ShareholderCountry!="") %>% 
      filter(domestic=="International") %>%
      filter(EquityForeignShare>0.001) %>%
      filter(Country_Name==input$country3) %>%
      filter(Sh_Country_Name %in% input$sh_country3) %>%
      filter(NACEDesc %in% input$industry3) %>%
      filter(Sh_Continent %in% input$continent3) %>%
      distinct() %>% 
      ggplot(aes(fill=fct_rev(Sh_Country_Name), x =fct_rev(Sh_Country_Name), y=EquityForeignShare)) +
      geom_bar(position="stack", stat="identity")+
      facet_wrap(~NACEDesc, ncol = input$cols3) +
      geom_text(aes(label= scales::percent(EquityForeignShare,accuracy = 1L),y=EquityForeignShare), position = position_stack(vjust = 0.5), size=5*input$font_num3) +
      coord_flip()+
      guides(fill = guide_legend(reverse=TRUE))+
      labs(
        #title="Share of capital in a given industry in a given country owned by foreign investors (showing investments above 0.1%)",
        x="Economic sectors",y="Share of capital owned by foreign investors", fill="Shareholder country")+
      theme(title=element_text(size=20,color="black"),
            axis.text=element_text(size=20,color="black"),
            axis.title.y=element_text(size=20,color="black"),
            axis.title.x=element_text(size=20,color="black"),
            axis.text.y.left=element_text(size=18*input$font_sector3,color="black"),
            strip.text=element_text(size=18,color="black"),
            legend.text = element_text(size=20,color="black"))
    
    foreignowners
    
  },height = function(){input$height3*760})

  
  observe({
    input$reset_input3
    updateTextInput(session, "country3", value = c("United States"))
    updateTextInput(session, "continent3", value = c("Africa","Asia","Europe","North America","Oceania","South America"))
    updateTextInput(session, "industry3", value = c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"))
    updateTextInput(session, "sh_country3", value = country_foreign2$Country_Name)
    updateNumericInput(session, "height3", value = 1)
    updateNumericInput(session, "cols3", value = 9)
    updateNumericInput(session, "font_sector3", value = 1)
    updateNumericInput(session, "font_num3", value = 1)
    
  })
  
  

  
  output$chart_4 <- renderPlotly({
    scatterplot <- country_scatterplot %>%
      arrange(desc(EquityCountry)) %>%
      filter(Country_Name %in% input$country4) %>%
      filter(Continent %in% input$continent4) %>%
      filter(domestic==input$domestic4) %>%
      filter(Sh_NACEDesc==input$shareholder4) %>%
      filter(EquityCountry<input$equity4[2]) %>%
      filter(EquityCountry>input$equity4[1]) %>%
      ggplot(aes_string(x=input$x, y=input$y)) +
      geom_point(aes(label=Country_Name, fill=Continent, size=EquityCountry),pch=21,color="black",stroke=0.15) +
      scale_size_continuous(guide = FALSE,range = c(1,30)) +
      geom_text(aes(label=Country),check_overlap = T)+
      labs(
        #title="The share of capital owned by a selected shareholder in chosen industries", 
        fill="Continent")+
      theme(title=element_text(size=20,color="black"),
            axis.text=element_text(size=20,color="black"),
            axis.title.y=element_text(size=20,color="black"),
            axis.title.x=element_text(size=20,color="black"),
            axis.text.y.left=element_text(size=18,color="black"),
            legend.text = element_text(size=16,color="black"))
    
    scatterplot
    
  })
  
  observe({
    input$reset_input4
    updateTextInput(session, "country4", value = unique(country_scatterplot$Country_Name))
    updateTextInput(session, "continent4", value = c("Africa","Asia","Europe","North America","Oceania","South America"))
    updateTextInput(session, "shareholder4", value = c("finance"))
    updateNumericInput(session, "equity4", value = c(50000000, 20000000000))
    updateTextInput(session, "domestic4", value = "Domestic")
    updateTextInput(session, "y", value = "finance")
    updateTextInput(session, "x", value = "nonfinance")

  })
  
  
  
  
  output$distPlot <- renderGirafe({
    ggiraph(code = print(worldMaps(df, world_data, input$NACEDesc, input$Sh_NACEDesc, input$domestic)))
  })
  
  
  map_table1 <- reactive({
    country_scatterplot_ %>% 
      filter(NACEDesc==input$NACEDesc) %>% 
      filter(Sh_NACEDesc==input$Sh_NACEDesc) %>%
      filter(domestic==input$domestic) %>% 
      rename(Capital_owned=EquityForeignShare) %>%
      rename(Industry=NACEDesc) %>%
      rename(Shareholder_industry=Sh_NACEDesc) %>%
      rename(Shareholder_type=domestic) %>% 
      ungroup() %>% 
      select(Country_Name,Industry,Shareholder_industry,Shareholder_type,Capital_owned)
      })
  
  
  observe({
    input$reset_input5
    updateTextInput(session, "domestic", value = "Global")
    updateTextInput(session, "Sh_NACEDesc", value = "finance")
    updateTextInput(session, "NACEDesc", value = "finance")
    
  })
  
  
  output$map_table = DT::renderDataTable(DT::datatable(map_table1(), options = list(pageLength = 15)))
  
  
  output$distPlot2 <- renderChorddiag({
    
    
    
    edges1 <- edge_list %>% 
      filter(domestic=="Domestic") %>% 
      filter(Sh_Country_Name==input$country5|Country_Name==input$country5) %>% 
      filter(internal %in% input$internal1)%>% 
      filter(width<input$edge_weight0[2]*0.00000002) %>%
      filter(width>input$edge_weight0[1]*0.00000002) %>% 
      filter(Sh_NACEDesc %in% input$industry5) %>% 
      filter(NACEDesc %in% input$industry5)%>%
      ungroup() %>%
      arrange(Sh_NACEDesc,NACEDesc) %>% 
      select(Sh_NACEDesc,NACEDesc,weight) %>% 
      rename(from=Sh_NACEDesc,to=NACEDesc)
    
    
    matrix <- as.matrix(get.adjacency(graph.data.frame(edges1),attr='weight'))
    
    matrix <- matrix[order(rownames(matrix)), order(colnames(matrix))]
    
    nodes <- as.data.frame(rownames(matrix))
    
    colnames(nodes)[1] <- "NACEDesc"
    
    nodes <- nodes %>% 
      mutate(color="") %>% 
      mutate(color=replace(color,NACEDesc=="accomodation food","#F8766D")) %>% 
      mutate(color=replace(color,NACEDesc== "admin","#E9842C" )) %>% 
      mutate(color=replace(color,NACEDesc== "agriculture", "#D69100")) %>%
      mutate(color=replace(color,NACEDesc== "arts entertainment","#BC9D00" )) %>% 
      mutate(color=replace(color,NACEDesc== "construction","#9CA700")) %>%
      mutate(color=replace(color,NACEDesc== "education", "#6FB000")) %>% 
      mutate(color=replace(color,NACEDesc== "electricity water waste", "#00B813")) %>%
      mutate(color=replace(color,NACEDesc== "finance","#00BD61" )) %>%  # usually #000000
      mutate(color=replace(color,NACEDesc== "government","#00BD61" )) %>%
      mutate(color=replace(color,NACEDesc== "health social work","#00C0B4" )) %>% 
      mutate(color=replace(color,NACEDesc== "individual", "#00BDD4")) %>%
      mutate(color=replace(color,NACEDesc== "information", "#00B5EE")) %>% 
      mutate(color=replace(color,NACEDesc== "manufacturing", "#00A7FF")) %>%  
      mutate(color=replace(color,NACEDesc== "mining", "#00A7FF")) %>% 
      mutate(color=replace(color,NACEDesc==  "other", "#BC81FF")) %>%
      mutate(color=replace(color,NACEDesc== "professional", "#E26EF7")) %>% 
      mutate(color=replace(color,NACEDesc==  "real estate", "#F863DF")) %>%  
      mutate(color=replace(color,NACEDesc==  "transport storage", "#FF62BF")) %>% 
      mutate(color=replace(color,NACEDesc==  "wholesale and retail","#FF6A9A" )) %>% 
      mutate(color=replace(color,NACEDesc=="NA","#D3D3D3" ))
    
    
    colors <- nodes %>% 
      select(color) %>% 
      ungroup()
    
    colors <- dplyr::pull(colors, color)
    
    chorddiag(matrix/1000000,  showGroupnames = T, type = "directional", showTicks = F, groupColors = colors, groupnameFontsize = 20, groupnamePadding = 2, margin = 170,
              tooltipGroupConnector = " invests in ",tooltipUnit = " billion USD",precision = 2,tooltipFontsize=20)
    
  })
  
  
  output$domestic_network <- renderVisNetwork({
    
    nodes <- node_file %>% 
      filter(Country_Name==input$country5) %>% 
      filter(NACEDesc %in% input$industry5) %>% 
      mutate(color="") %>% 
      mutate(color=replace(color,NACEDesc=="accomodation food","#F8766D")) %>% 
      mutate(color=replace(color,NACEDesc== "admin","#E9842C" )) %>% 
      mutate(color=replace(color,NACEDesc== "agriculture", "#D69100")) %>%
      mutate(color=replace(color,NACEDesc== "arts entertainment","#BC9D00" )) %>% 
      mutate(color=replace(color,NACEDesc== "construction","#9CA700")) %>%
      mutate(color=replace(color,NACEDesc== "education", "#6FB000")) %>% 
      mutate(color=replace(color,NACEDesc== "electricity water waste", "#00B813")) %>%
      mutate(color=replace(color,NACEDesc== "finance","#00BD61" )) %>%  # usually #000000
      mutate(color=replace(color,NACEDesc== "government","#00BD61" )) %>%
      mutate(color=replace(color,NACEDesc== "health social work","#00C0B4" )) %>% 
      mutate(color=replace(color,NACEDesc== "individual", "#00BDD4")) %>%
      mutate(color=replace(color,NACEDesc== "information", "#00B5EE")) %>% 
      mutate(color=replace(color,NACEDesc== "manufacturing", "#00A7FF")) %>%  
      mutate(color=replace(color,NACEDesc== "mining", "#00A7FF")) %>% 
      mutate(color=replace(color,NACEDesc==  "other", "#BC81FF")) %>%
      mutate(color=replace(color,NACEDesc== "professional", "#E26EF7")) %>% 
      mutate(color=replace(color,NACEDesc==  "real estate", "#F863DF")) %>%  
      mutate(color=replace(color,NACEDesc==  "transport storage", "#FF62BF")) %>% 
      mutate(color=replace(color,NACEDesc==  "wholesale and retail","#FF6A9A" )) %>% 
      mutate(color=replace(color,NACEDesc=="NA","#D3D3D3" ))
    
    
    edges <- edge_list %>% 
      filter(domestic=="Domestic") %>% 
      filter(Sh_Country_Name==input$country5|Country_Name==input$country5) %>% 
      filter(internal %in% input$internal1)%>% 
      mutate(color=ifelse(Sh_NACEDesc=="finance","#00BD61","#84C1F7")) %>% 
      filter(width<input$edge_weight0[2]*0.00000002) %>%
      filter(width>input$edge_weight0[1]*0.00000002) %>% 
      filter(Sh_NACEDesc %in% input$industry5) %>% 
      filter(NACEDesc %in% input$industry5) %>% 
      arrange(Sh_NACEDesc)
    
    visNetwork(nodes, edges, 
               #main = "A domestic corporate ownership network", 
               width = "100%") %>%
      visNodes(shape = "dot",
               color = list(color=nodes$color, 
                            #background = "lightblue", 
                            border = "black",
                            highlight = "black"),
               scaling = list(min=15, max=80, label = list(enabled = T, min=20, max=25,font.color="black"))
      ) %>%
      visIgraphLayout(layout = "layout_in_circle") %>%
      visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor =1)), 
               color = list(color=edges$color, highlight = "yellow"),smooth = TRUE) %>% 
      visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE,selectedBy = "group") %>%
      visPhysics(solver = "forceAtlas2Based", timestep = 0.5,forceAtlas2Based = list(gravitationalConstant = -10,avoidOverlap = 0.5)) %>% 
      visInteraction(hover = TRUE) %>% 
      visEvents(hoverNode = "function(nodes) {Shiny.onInputChange('current_node_id', nodes);;}") %>% 
      visConfigure(enabled=F)
    
  })
  
  
  observe({
    input$reset_input6
    updateTextInput(session, "country5", value = "United States")
    updateTextInput(session, "industry5", value = c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","individual","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"))
    updateTextInput(session, "internal1", value = c("External","Internal (self-loops)"))
    updateNumericInput(session, "edge_weight0", value = c(0, 5000000000))
  })
  
  

  
  output$domestic_network_table = DT::renderDataTable(DT::datatable(edges_table1(), options = list(pageLength = 20)))
  
  
  output$distPlot2a <- renderChorddiag({
    
    
    edges1a <- edge_list %>% 
      filter(domestic=="Domestic") %>% 
      filter(Sh_Country_Name==input$country8a|Country_Name==input$country8a) %>% 
      filter(internal %in% input$internal1a)%>% 
      filter(width<input$edge_weight0a[2]*0.00000002) %>%
      filter(width>input$edge_weight0a[1]*0.00000002) %>% 
      filter(Sh_NACEDesc %in% input$industry8) %>% 
      filter(NACEDesc %in% input$industry8) %>% 
      ungroup() %>%
      arrange(Sh_NACEDesc,NACEDesc) %>% 
      select(Sh_NACEDesc,NACEDesc,weight) %>% 
      rename(from=Sh_NACEDesc,to=NACEDesc)

    
    matrixa <- as.matrix(get.adjacency(graph.data.frame(edges1a),attr='weight'))
    
    matrixa <- matrixa[order(rownames(matrixa)), order(colnames(matrixa))]
    
    nodesa <- as.data.frame(rownames(matrixa))
    
    colnames(nodesa)[1] <- "NACEDesc"
    
    nodesa <- nodesa %>% 
      mutate(color="") %>% 
      mutate(color=replace(color,NACEDesc=="accomodation food","#F8766D")) %>% 
      mutate(color=replace(color,NACEDesc== "admin","#E9842C" )) %>% 
      mutate(color=replace(color,NACEDesc== "agriculture", "#D69100")) %>%
      mutate(color=replace(color,NACEDesc== "arts entertainment","#BC9D00" )) %>% 
      mutate(color=replace(color,NACEDesc== "construction","#9CA700")) %>%
      mutate(color=replace(color,NACEDesc== "education", "#6FB000")) %>% 
      mutate(color=replace(color,NACEDesc== "electricity water waste", "#00B813")) %>%
      mutate(color=replace(color,NACEDesc== "finance","#00BD61" )) %>%  # usually #000000
      mutate(color=replace(color,NACEDesc== "government","#00BD61" )) %>%
      mutate(color=replace(color,NACEDesc== "health social work","#00C0B4" )) %>% 
      mutate(color=replace(color,NACEDesc== "individual", "#00BDD4")) %>%
      mutate(color=replace(color,NACEDesc== "information", "#00B5EE")) %>% 
      mutate(color=replace(color,NACEDesc== "manufacturing", "#00A7FF")) %>%  
      mutate(color=replace(color,NACEDesc== "mining", "#00A7FF")) %>% 
      mutate(color=replace(color,NACEDesc==  "other", "#BC81FF")) %>%
      mutate(color=replace(color,NACEDesc== "professional", "#E26EF7")) %>% 
      mutate(color=replace(color,NACEDesc==  "real estate", "#F863DF")) %>%  
      mutate(color=replace(color,NACEDesc==  "transport storage", "#FF62BF")) %>% 
      mutate(color=replace(color,NACEDesc==  "wholesale and retail","#FF6A9A" )) %>% 
      mutate(color=replace(color,NACEDesc=="NA","#D3D3D3" ))
    
    
    colorsa <- nodesa %>% 
      select(color) %>% 
      ungroup()
    
    colorsa <- dplyr::pull(colorsa, color)
    
    chorddiag(matrixa/1000000,  showGroupnames = T, type = "directional", showTicks = F, groupColors = colorsa, groupnameFontsize = 20, groupnamePadding = 2, margin = 170,
              tooltipGroupConnector = " invests in ",tooltipUnit = " billion USD",precision = 2,tooltipFontsize=20)
    
  })
  
  
  output$distPlot2b <- renderChorddiag({
    
    
    
    edges1b <- edge_list %>% 
      filter(domestic=="Domestic") %>% 
      filter(Sh_Country_Name==input$country8b|Country_Name==input$country8b) %>% 
      filter(internal %in% input$internal1a)%>% 
      filter(width<input$edge_weight0a[2]*0.00000002) %>%
      filter(width>input$edge_weight0a[1]*0.00000002) %>% 
      filter(Sh_NACEDesc %in% input$industry8) %>% 
      filter(NACEDesc %in% input$industry8)%>%
      ungroup() %>%
      arrange(Sh_NACEDesc,NACEDesc) %>% 
      select(Sh_NACEDesc,NACEDesc,weight) %>% 
      rename(from=Sh_NACEDesc,to=NACEDesc)
    
    
    
    matrixb <- as.matrix(get.adjacency(graph.data.frame(edges1b),attr='weight'))
    
    matrixb <- matrixb[order(rownames(matrixb)), order(colnames(matrixb))]
    
    nodesb <- as.data.frame(rownames(matrixb))
    
    colnames(nodesb)[1] <- "NACEDesc"
    
    nodesb <- nodesb %>% 
      mutate(color="") %>% 
      mutate(color=replace(color,NACEDesc=="accomodation food","#F8766D")) %>% 
      mutate(color=replace(color,NACEDesc== "admin","#E9842C" )) %>% 
      mutate(color=replace(color,NACEDesc== "agriculture", "#D69100")) %>%
      mutate(color=replace(color,NACEDesc== "arts entertainment","#BC9D00" )) %>% 
      mutate(color=replace(color,NACEDesc== "construction","#9CA700")) %>%
      mutate(color=replace(color,NACEDesc== "education", "#6FB000")) %>% 
      mutate(color=replace(color,NACEDesc== "electricity water waste", "#00B813")) %>%
      mutate(color=replace(color,NACEDesc== "finance","#00BD61" )) %>%  # usually #000000
      mutate(color=replace(color,NACEDesc== "government","#00BD61" )) %>%
      mutate(color=replace(color,NACEDesc== "health social work","#00C0B4" )) %>% 
      mutate(color=replace(color,NACEDesc== "individual", "#00BDD4")) %>%
      mutate(color=replace(color,NACEDesc== "information", "#00B5EE")) %>% 
      mutate(color=replace(color,NACEDesc== "manufacturing", "#00A7FF")) %>%  
      mutate(color=replace(color,NACEDesc== "mining", "#00A7FF")) %>% 
      mutate(color=replace(color,NACEDesc==  "other", "#BC81FF")) %>%
      mutate(color=replace(color,NACEDesc== "professional", "#E26EF7")) %>% 
      mutate(color=replace(color,NACEDesc==  "real estate", "#F863DF")) %>%  
      mutate(color=replace(color,NACEDesc==  "transport storage", "#FF62BF")) %>% 
      mutate(color=replace(color,NACEDesc==  "wholesale and retail","#FF6A9A" )) %>% 
      mutate(color=replace(color,NACEDesc=="NA","#D3D3D3" ))
    
    
    colorsb <- nodesb %>% 
      select(color) %>% 
      ungroup()
    
    colorsb <- dplyr::pull(colorsb, color)
    
    chorddiag(matrixb/1000000,  showGroupnames = T, type = "directional", showTicks = F, groupColors = colorsb, groupnameFontsize = 20, groupnamePadding = 2, margin = 170,
              tooltipGroupConnector = " invests in ",tooltipUnit = " billion USD",precision = 2,tooltipFontsize=20)
    
  })
  
  
  
  
  
  
  observe({
    input$reset_input7
    updateTextInput(session, "country8a", value = "United States")
    updateTextInput(session, "country8b", value = "United Kingdom")
    updateTextInput(session, "industry8", value = c("accomodation food","admin","agriculture","arts entertainment","construction","education","electricity water waste","finance","government","health social work","individual","information","manufacturing","mining","other","professional","real estate","transport storage","wholesale and retail"))
    updateTextInput(session, "internal1a", value = c("External","Internal (self-loops)"))
    updateNumericInput(session, "edge_weight0a", value = c(0, 5000000000))
  })
  

  
  output$FDInetwork <- renderVisNetwork({
    
    
    edges <- edge_listf %>% 
      filter(domestic=="International") %>% 
      filter(width<input$edge_weight[2]*0.00000002) %>%
      filter(width>input$edge_weight[1]*0.00000002) %>% 
      filter(Country_Name==input$country7|Sh_Country_Name==input$country7) %>% 
      mutate(incoming=ifelse(Country_Name==input$country7,"Incoming","Outgoing")) %>% 
      mutate(color=ifelse(incoming=="Incoming","red","blue")) %>% 
      filter(incoming %in% input$incoming)
    
    
    nodes_ <- edges %>%
      ungroup() %>%
      select(from, to) %>%
      rename(id1=from,id2=to)
    
    nodes_ <- stack(nodes_)
    
    nodes_ <- nodes_ %>% 
      distinct() %>% 
      mutate(check=1) %>% 
      rename(id=values) %>%
      select(id,check) %>% 
      distinct()
    
    node_filef <- left_join(node_filef, nodes_, by = "id")
    
    nodes <- node_filef %>%
      filter(check==1) %>% 
      filter(value<input$country_degree[2]) %>%
      filter(value>input$country_degree[1]) %>% 
      mutate(group = replace(group,is.na(group),"Unknown")) %>% 
      mutate(title = replace(title,is.na(title),"Unknown")) %>% 
      mutate(label = replace(label,title=="Unknown","??"))
    
    
    edges <- edges %>% 
      filter(to %in% nodes$id) %>% 
      filter(from %in% nodes$id) 
    
    
    visNetwork(nodes, edges, 
               #main = "Network of incoming and outgoing FDI", 
               width = "100%") %>%
      visNodes(shape = "circle",
               color = list(background = "lightblue", 
                            border = "darkblue",
                            highlight = "yellow"),
               scaling = list(min=50, max=150, label = list(enabled = T, font.size=nodes$value, min=50, max=150))) %>%
      #visIgraphLayout(layout = "layout_nicely") %>%
      visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor =1)), 
               color = list(color=edges$color, highlight = "red"),
               smooth = list(roundness = 0.5)) %>% 
      visLayout(randomSeed = 123) %>% 
      visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE,selectedBy = "group") %>%
      visPhysics(solver = "forceAtlas2Based", timestep = 0.2,
                 forceAtlas2Based = list(gravitationalConstant = -100,springConstant=0.08, springLength=120,damping=0.4, avoidOverlap = 0.3)) %>% 
      visInteraction(hover = TRUE) %>%
      visEvents(hoverNode = "function(nodes) {Shiny.onInputChange('current_node_id', nodes);;}") %>% 
      visConfigure(enabled=F)
    
  })
  
  
  observe({
    input$reset_input8
    updateTextInput(session, "country7", value = "United States")
    updateNumericInput(session, "country_degree", value = c(50000000, 20000000000))
    updateTextInput(session, "incoming", value = c("Incoming","Outgoing"))
    updateNumericInput(session, "edge_weight", value = c(1000000, 2000000000))

  })
  
  
  edges_table2 <- reactive({
    
    edge_listf %>% 
      filter(domestic=="International") %>% 
      filter(width<input$edge_weight[2]*0.00000002) %>%
      filter(width>input$edge_weight[1]*0.00000002) %>% 
      filter(Country_Name==input$country7|Sh_Country_Name==input$country7) %>%
      mutate(incoming=ifelse(Country_Name==input$country7,"Incoming","Outgoing")) %>% 
      filter(incoming %in% input$incoming) %>% 
      mutate(Investment_mil_USD=weight/1000) %>%
      rename(Incoming_Outgoing_FDI=incoming) %>%
      rename(Shareholder_Country=Sh_Country_Name) %>%
      mutate(Shareholder_Country=replace(Shareholder_Country,is.na(Shareholder_Country),"Unknown")) %>%
      mutate(Investment_mil_USD=round(Investment_mil_USD,2)) %>% 
      ungroup() %>% 
      select(Country_Name, Shareholder_Country,Incoming_Outgoing_FDI,Investment_mil_USD) 
  })
  
  
  output$FDI_network_table = DT::renderDataTable(DT::datatable(edges_table2(), options = list(pageLength = 20)))
  
  
  output$FDInetwork1 <- renderVisNetwork({
    
    edges <- edge_listf %>% 
      filter(domestic=="International") %>% 
      filter(width<input$edge_weighta[2]*0.00000002) %>%
      filter(width>input$edge_weighta[1]*0.00000002) %>% 
      filter(Country_Name==input$country7a|Sh_Country_Name==input$country7a) %>% 
      mutate(incoming=ifelse(Country_Name==input$country7a,"Incoming","Outgoing")) %>% 
      mutate(color=ifelse(incoming=="Incoming","red","blue")) %>% 
      filter(incoming %in% input$incominga)
    
    
    
    nodes_ <- edges %>%
      ungroup() %>%
      select(from, to) %>%
      rename(id1=from,id2=to)
    
    nodes_ <- stack(nodes_)
    
    nodes_ <- nodes_ %>% 
      distinct() %>% 
      mutate(check=1) %>% 
      rename(id=values) %>%
      select(id,check) %>% 
      distinct()
    
    node_filef <- left_join(node_filef, nodes_, by = "id")
    
    nodes <- node_filef %>%
      filter(check==1) %>% 
      filter(value<input$country_degreea[2]) %>%
      filter(value>input$country_degreea[1]) %>% 
      mutate(group = replace(group,is.na(group),"Unknown")) %>% 
      mutate(title = replace(title,is.na(title),"Unknown")) %>% 
      mutate(label = replace(label,title=="Unknown","??"))
    
    
    edges <- edges %>% 
      filter(to %in% nodes$id) %>% 
      filter(from %in% nodes$id) 
    
    
    visNetwork(nodes, edges, 
               #main = "Country #1: Network of incoming and outgoing FDI", 
               width = "100%") %>%
      visNodes(shape = "circle",
               color = list(background = "lightblue", 
                            border = "darkblue",
                            highlight = "yellow"),
               scaling = list(min=50, max=150, label = list(enabled = T, font.size=nodes$value, min=50, max=150))) %>%
      #visIgraphLayout(layout = "layout_nicely") %>%
      visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor =1)), 
               color = list(color=edges$color, highlight = "red"),
               smooth = list(roundness = 0.5)) %>% 
      visLayout(randomSeed = 123) %>% 
      visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE,selectedBy = "group") %>%
      visPhysics(solver = "forceAtlas2Based", timestep = 0.2,
                 forceAtlas2Based = list(gravitationalConstant = -100,springConstant=0.08, springLength=120,damping=0.4, avoidOverlap = 0.3)) %>% 
      visInteraction(hover = TRUE) %>%
      visEvents(hoverNode = "function(nodes) {Shiny.onInputChange('current_node_id', nodes);;}") %>% 
      visConfigure(enabled=F)
    
  })
  
  
  output$FDInetwork2 <- renderVisNetwork({
    
    edges <- edge_listf %>% 
      filter(domestic=="International") %>% 
      filter(width<input$edge_weighta[2]*0.00000002) %>%
      filter(width>input$edge_weighta[1]*0.00000002) %>% 
      filter(Country_Name==input$country7b|Sh_Country_Name==input$country7b) %>% 
      mutate(incoming=ifelse(Country_Name==input$country7b,"Incoming","Outgoing")) %>% 
      mutate(color=ifelse(incoming=="Incoming","red","blue")) %>% 
      filter(incoming %in% input$incominga)
    
    
    
    nodes_ <- edges %>%
      ungroup() %>%
      select(from, to) %>%
      rename(id1=from,id2=to)
    
    nodes_ <- stack(nodes_)
    
    nodes_ <- nodes_ %>% 
      distinct() %>% 
      mutate(check=1) %>% 
      rename(id=values) %>%
      select(id,check) %>% 
      distinct()
    
    node_filef <- left_join(node_filef, nodes_, by = "id")
    
    nodes <- node_filef %>%
      filter(check==1) %>% 
      filter(value<input$country_degreea[2]) %>%
      filter(value>input$country_degreea[1]) %>% 
      mutate(group = replace(group,is.na(group),"Unknown")) %>% 
      mutate(title = replace(title,is.na(title),"Unknown")) %>% 
      mutate(label = replace(label,title=="Unknown","??"))
    
    
    edges <- edges %>% 
      filter(to %in% nodes$id) %>% 
      filter(from %in% nodes$id) 
    
    
    visNetwork(nodes, edges, 
               #main = "Country #2: Network of incoming and outgoing FDI", 
               width = "100%") %>%
      visNodes(shape = "circle",
               color = list(background = "lightblue", 
                            border = "darkblue",
                            highlight = "yellow"),
               scaling = list(min=50, max=150, label = list(enabled = T, font.size=nodes$value, min=50, max=150))) %>%
      #visIgraphLayout(layout = "layout_nicely") %>%
      visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor =1)), 
               color = list(color=edges$color, highlight = "red"),
               smooth = list(roundness = 0.5)) %>% 
      visLayout(randomSeed = 123) %>% 
      visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE,selectedBy = "group") %>%
      visPhysics(solver = "forceAtlas2Based", timestep = 0.2,
                 forceAtlas2Based = list(gravitationalConstant = -100,springConstant=0.08, springLength=120,damping=0.4, avoidOverlap = 0.3)) %>% 
      visInteraction(hover = TRUE) %>%
      visEvents(hoverNode = "function(nodes) {Shiny.onInputChange('current_node_id', nodes);;}") %>% 
      visConfigure(enabled=F)
    
  })
  
  
  observe({
    input$reset_input9
    updateTextInput(session, "country7a", value = "United States")
    updateTextInput(session, "country7b", value = "United Kingdom")
    updateNumericInput(session, "country_degreea", value = c(50000000, 20000000000))
    updateTextInput(session, "incominga", value = c("Incoming","Outgoing"))
    updateNumericInput(session, "edge_weighta", value = c(1000000, 2000000000))
    
  })
  
 
  output$globalnetwork <- renderVisNetwork({
    
    nodes <- node_filef %>% 
      filter(title %in% input$country6) %>%
      filter(group %in% input$continent6) %>% 
      filter(value<input$country_degree1[2]) %>%
      filter(value>input$country_degree1[1]) %>% 
      mutate(group = replace(group,is.na(group),"Unknown")) %>% 
      mutate(title = replace(title,is.na(title),"Unknown")) %>% 
      mutate(label = replace(label,title=="Unknown","??"))
    
    edges <- edge_listf %>% 
      filter(domestic!="NA") %>% 
      filter(domestic %in% input$domestic_foreign) %>% 
      filter(width<input$edge_weight1[2]*0.00000002) %>%
      filter(width>input$edge_weight1[1]*0.00000002)%>% 
      filter(to %in% nodes$id) %>% 
      filter(from %in% nodes$id)
    
    
    
    graph <- graph_from_data_frame(d=edges, vertices=nodes, directed=T)

    visNetwork(nodes, edges, 
               #main = "Global corporate ownership network", 
               width = "100%") %>%
      visNodes(shape = "circle",
               color = list(background = "lightblue", 
                            border = "darkblue",
                            highlight = "yellow"),
               scaling = list(min=20, max=130, label = list(enabled = T, font.size=nodes$value, min=20, max=130))) %>%
      visIgraphLayout(layout="layout_with_lgl") %>% 
      visEdges(arrows =list(to = list(enabled = TRUE, scaleFactor =1)), 
               color = list(color = "#aebbff", highlight = "red"),smooth = TRUE) %>%
      visOptions(highlightNearest = TRUE,nodesIdSelection = TRUE,selectedBy = "group") %>%
      visPhysics(solver = "forceAtlas2Based", timestep = 0.1,
                 forceAtlas2Based = list(gravitationalConstant = -40,avoidOverlap = 0.8)) %>% 
      visInteraction(hover = TRUE) %>%
      visEvents(hoverNode = "function(nodes) {
        Shiny.onInputChange('current_node_id', nodes);
      ;}") %>% 
      visConfigure(enabled=F)
    
  })
  
  observe({
    input$reset_input10
    updateTextInput(session, "country6", value = country_industry$Country_Name)
    updateTextInput(session, "continent6", value = c("Africa","Asia","Europe","North America","Oceania","South America"))
    updateNumericInput(session, "country_degree1", value = c(100000000, 20000000000))
    updateTextInput(session, "domestic_foreign", value = "International")
    updateNumericInput(session, "edge_weight1", value = c(0, 20000000000))
    
  })
  
  

  output$distPlot3 <- renderChorddiag({
    
    nodes3 <- node_filef %>% 
      filter(title %in% input$country6) %>%
      filter(group %in% input$continent6) %>% 
      filter(value<input$country_degree1[2]) %>%
      filter(value>input$country_degree1[1])
    
    edges3 <- edge_listf %>% 
      filter(domestic!="NA") %>% 
      filter(domestic %in% input$domestic_foreign) %>% 
      filter(width<input$edge_weight1[2]*0.00000002) %>%
      filter(width>input$edge_weight1[1]*0.00000002)%>% 
      filter(to %in% nodes3$id) %>% 
      filter(from %in% nodes3$id) %>% 
      ungroup() %>%
      select(Sh_Country_Name,Country_Name,weight) %>% 
      rename(from=Sh_Country_Name,to=Country_Name)

    matrix3 <- as.matrix(get.adjacency(graph.data.frame(edges3),attr='weight'))
    
    chorddiag(matrix3/1000000,  showGroupnames = T, type = "directional", showTicks = F, groupnameFontsize = 20, groupnamePadding = 2, margin = 170,
              tooltipGroupConnector = " invests in ",tooltipUnit = " billion USD",precision = 2,tooltipFontsize=20)
    
  })
  
  
  

}

# Run the app
shinyApp(ui, server)


