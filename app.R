# GLOBAL
{
library(shiny)
library(tidyverse)
library(magrittr)
library(plotly)
library(ggrepel)
library(gganimate)
library(shinythemes)
library(shinydashboard)
library(fmsb)
library(colormap)
library(leaflet)
library(geojsonio)
library(ggstance)
library(gifski)
library(png)

resol <- read_csv("data/resolutions.csv")
votes <- read_csv("data/votes.csv")

# Pre-processing
resol_processed <- resol %>% 
    mutate(year = as.integer(assembly_session + 1945)) %>% 
    gather(issue,value,colonization,human_rights,israel_palestine,disarmament,nuclear_weapons,economic_development) %>% 
    filter(value == 1) %>% 
    arrange(vote_id)

votes_processed <- votes %>% 
    filter(vote <= 3) %>% 
    mutate(year = as.integer(assembly_session + 1945)) %>% 
    left_join(x=.,y=resol_processed,by = c("vote_id")) %>% 
    select(state_name,vote,year.x,issue) %>% 
    drop_na() %>% 
    rename(year = year.x)

votes_processed %<>% 
    group_by(state_name,year,issue) %>% 
    summarise(mean_vote = mean(vote)) %>%
    spread(issue,mean_vote) %>% 
    replace(is.na(.),2) # NA with abstain

votes_scale <- as.data.frame(scale(votes_processed[3:8]))

# Clustering and PCA
set.seed(212)
km <- kmeans(votes_scale,4,nstart = 20)
pc <- prcomp(votes_scale[,1:6],center = T)

# Combine to dataframe
vot <- data.frame(pc$x, 
                  clust = factor(km$cluster), 
                  country = votes_processed$state_name, 
                  year = votes_processed$year,
                  colonization = votes_processed$colonization,
                  disarmament = votes_processed$disarmament,
                  economic_development = votes_processed$economic_development,
                  human_rights = votes_processed$human_rights,
                  israel_palestine = votes_processed$israel_palestine,
                  nuclear_weapons = votes_processed$nuclear_weapons)

# Radar Plot
data_radar <- vot %>% 
    group_by(clust) %>% 
    summarise_at(vars(human_rights,disarmament,economic_development,colonization,israel_palestine,nuclear_weapons),.funs = mean) %>% 
    arrange(clust) %>% 
    select(-clust) 
data_radar <-rbind(rep(2,10) , rep(0,10) , data_radar)
colnames(data_radar) <- c("Human Rights", "Disarmament", "Colonization", "Economic Development", "Israel-Palestine","Nuclear Weapon")
title_radar <- c("Group 1", "Group 2", "Group 3", "Group 4")

colors_border <- colormap(colormap=colormaps$viridis, nshades=4, alpha=0.1)
colors_in <- colormap(colormap=colormaps$viridis, nshades=4, alpha=0.3)

# Tendency Map
world_country <-geojson_read("countries.geo.json", what = "sp")

data_tendency <- vot %>% 
    group_by(country) %>% 
    summarise(clust = round(mean(as.numeric(clust)),0)) %>% 
    filter(country %in% world_country$name) 
map_tendency <- world_country[world_country$name %in% data_tendency$country, ]

data_tendency <- merge(data_tendency, as.data.frame(world_country), by.x = "country", by.y = "name", all.x = TRUE)
data_tendency %<>% arrange(id)

pal_tendency <- colorFactor(colors_border, domain = data_tendency$clus)

unique_country <- unique(vot$country)

# US Tendency
us <- vot %>% 
    filter(country == "United States of America") %>% 
    mutate(us_colonization = colonization,
           us_disarmament = disarmament,
           us_economic = economic_development,
           us_human = human_rights,
           us_israel = israel_palestine,
           us_nuclear = nuclear_weapons) %>% 
    select(year,us_colonization,us_disarmament,us_economic, us_human,us_israel,us_nuclear)

us_combine <- vot %>% 
    group_by(year,country) %>% 
    left_join(.,us,by = "year") %>% 
    select(-c(1:7))

# us_cor <- tibble(country = unique(us_combine$country),cor=NA)
# 
# for(i in unique(us_combine$country)){
#     x <- us_combine %>%
#         filter(country == i)
#     z <- c(cor(x$colonization,x$us_colonization),
#            cor(x$disarmament,x$us_disarmament),
#            cor(x$economic_development,x$us_economic),
#            cor(x$human_rights,x$us_human),
#            cor(x$israel_palestine,x$us_israel),
#            cor(x$nuclear_weapons,x$us_nuclear))
#     us_cor[which(us_cor$country == i),"cor"] <- mean(z,na.rm = T)
# }

us_cor1 <- readRDS("us_cor")

us_cor1 <- us_cor1 %>% filter(country %in% world_country$name) 
us_cor1 <- merge(us_cor1, as.data.frame(world_country), by.x = "country", by.y = "name", all.x = TRUE)
us_cor1 %<>% arrange(id)

pal_us <- colorBin("Blues", domain = us_cor1$cor,4)

map_us <- world_country[world_country$name %in% us_cor1$country, ]

# China Tendency
ch <- vot %>% 
    filter(country == "China") %>% 
    mutate(ch_colonization = colonization,
           ch_disarmament = disarmament,
           ch_economic = economic_development,
           ch_human = human_rights,
           ch_israel = israel_palestine,
           ch_nuclear = nuclear_weapons) %>% 
    select(year,ch_colonization,ch_disarmament,ch_economic, ch_human,ch_israel,ch_nuclear)

ch_combine <- vot %>% 
    group_by(year,country) %>% 
    filter(year >="1971") %>% 
    left_join(.,ch,by = "year") %>% 
    select(-c(1:7))

# ch_cor <- tibble(country = unique(ch_combine$country),cor=NA)
# 
# for(i in unique(ch_combine$country)){
#     x <- ch_combine %>%
#         filter(country == i)
#     z <- c(cor(x$colonization,x$ch_colonization),
#            cor(x$disarmament,x$ch_disarmament),
#            cor(x$economic_development,x$ch_economic),
#            cor(x$human_rights,x$ch_human),
#            cor(x$israel_palestine,x$ch_israel),
#            cor(x$nuclear_weapons,x$ch_nuclear))
#     ch_cor[which(ch_cor$country == i),"cor"] <- mean(z,na.rm = T)
# }
# 
# ch_cor %<>% filter(country != "Taiwan")

ch_cor1 <- readRDS("ch_cor")

ch_cor1 <- ch_cor1 %>% filter(country %in% world_country$name) 
ch_cor1 <- merge(ch_cor1, as.data.frame(world_country), by.x = "country", by.y = "name", all.x = TRUE)
ch_cor1 %<>% arrange(id)

pal_ch <- colorBin("Reds", domain = ch_cor1$cor,bins = 4)

map_ch <- world_country[world_country$name %in% ch_cor1$country, ]

}

# UI
ui = fluidPage(theme = shinytheme("yeti"), 
               # tags$style("*{font-family: Helvetica Neue !important;}"), 
                {navbarPage(title = "UN General Assembly Clustering",
                            tabPanel(strong("Introduction"),
                                     fluidRow(column(7,tags$img(src='un.png',width = 600)),
                                              column(5,h1("The purpose of the U.N. General Assembly is to discuss, debate, and create recommendations on 
                                                          subjects regarding global development and international peace.",align = "left"))),
                                     hr(),
                                     ("The General Assembly conducts voting on resolutions of matters recommended by country members. If the voting results are approved by 
                                     2/3 of respondents, the U.N. will adopt these resolutions within a considerable timeframe. Each country member will only have one vote for each resolution.
                                     I used UN General Assembly voting data from 1946-2015 to perform this analysis."),
                                     br(),br(),
                                     ("I am performing country clustering to see the group of country that has similar attitude tendency towards the global issues. Besides, I also did correlation 
                                     analysis to see how current superpower countries (US and China) in correlation with others. Voting Resolutions are already divided into 6 categories by UN 
                                     (Israel-Palestine, Colonization, Economic Development, Human Rights, Nuclear Weapon, and Disarmament). "),
                                     br(),br(),
                                     h4(strong(em("Each country has 3 choices; yes, no, or abstain. Agreeing to the resolution, can be reflected as a form of support/approval from a country, so that the 
                                            resolution can be resolved at the global level immediately.")),align="center"),
                                     br(),hr(),
                                     a(href="https://www.kaggle.com/unitednations/general-assembly", "Dataset Source"),br()),
                            tabPanel(strong("Clusters"),
                                     h3(strong("Group Characteristics"),align = "center"),hr(),
                                     plotOutput("attrPlot1"),
                                     fluidRow(
                                     (column(3,"Tendency to abstain or disagree on every resolutions.",align = "center")),
                                     (column(3,"Tendency to abstain or disagree about Israel-Palestine, Colonization, and Economic Development.",align = "center")),
                                     (column(3,"Tendency to abstain or disagree about Human Rights, Nuclear Weapons dan Disarmament.",align = "center")),
                                     (column(3,"Tendency to agree on every resolutions.",align = "center"))),br(),
                                     h3(strong("Tendency Map"),align = "center"),hr(),
                                     leafletOutput("mapPlot",height = "550px"),br(),
                                     fluidRow(column(12,"The map shows us each country tendency towards one group on 1946-2015 period.",align = "center")),br()),
                            tabPanel(strong("US-China"),
                                     fluidRow(
                                         column(6,h3(strong("United States Correlation"),align = "center"),hr(),
                                                leafletOutput("usTendency",height = "300px",width = "550px")),
                                         column(6,h3(strong("China Correlation"),align = "center"), hr(),
                                                leafletOutput("chTendency",height = "300px",width = "550px"))),br(),
                                     fluidRow(
                                         column(6,"Each country votes correlations with United States on 1946-2015 period.",align="center"),
                                         column(6,"Each country votes correlations with China on 1971-2015 period.",align="center"))),
                            # tabPanel("Veto",
                            #          h3(strong("Veto Country Group Over Years"),align = "center"),hr(),
                            #          imageOutput("vetoPlot"),br(),br(),br(),br(),br(),
                            #          fluidRow(column(12,h5(em("Shows how group in Veto Country changes on 1946-2015 period.")),align="center")),br()),
                            # tabPanel(strong("Indonesia"),
                            #          h3(strong("Indonesia Group Over Years"),align = "center"),hr(),
                            #          imageOutput("indoPlot"),br(),br(),br(),br(),br(),
                            #          fluidRow(column(12,h5(em("Shows how group in Indonesia changes on 1950-2015 period.")),align="center")),br()),
                            tabPanel(strong("Choose Country"),
                                     verticalLayout(
                                         h3(strong("Choose Your Own Countries"),align = "center"),hr(),
                                         fluidRow(column(12,align="center",selectInput("country_opt", 
                                                    label = h4("Choose your own countries to see how group changes on 1945-2015 period:"), 
                                                    choices = unique_country, 
                                                    selected = "United Kingdom",
                                                    multiple = T, 
                                                    width = '500px'))),
                                         plotlyOutput("choPlot")))
                    )})

# SERVER
server = function(input, output) {

    output$attrPlot1 <- renderPlot({
            # Split 
            par(mar=c(0, 0.8, 2.5, 0.8))
            par(mfrow=c(1,4))
            
            # Loop for each plot
            for(i in 1:4){
                radarchart( data_radar[c(1,2,i+2),], axistype = 4, 
                            pcol=colors_border[i] , pfcol=colors_in[i] , plwd=0.1, 
                            cglcol="gray", cglty=1, axislabcol="gray", caxislabels=seq(0,4,1), cglwd=0.7,
                            vlcex=1.15,calcex = 1,pty = 20,
                            title=title_radar[i],cex.main = 3)}
    })
    
    output$mapPlot <- renderLeaflet({
        leaflet(map_tendency) %>% 
            setView(lat = 30,lng = 0,zoom = 1.5) %>% 
            addTiles() %>% 
            addPolygons(weight = 1,
                        fillColor= ~pal_tendency(data_tendency$clust),
                        color = "grey",
                        label = ~paste0(data_tendency$country,"-",data_tendency$clust)) %>% 
            addLegend(pal = pal_tendency, values = ~data_tendency$clust, opacity = 0.4,title = "Groups",position = "bottomleft")
    })
    
    output$usTendency <- renderLeaflet({
        leaflet(map_us) %>%
            setView(lat = 30,lng = 0,zoom = 1) %>% 
            addTiles() %>% 
            addPolygons(weight = 1,
                        fillColor= ~pal_us(us_cor1$cor),
                        fillOpacity = 1,
                        color = "grey",
                        label = ~paste0(us_cor1$country,", ",round(us_cor1$cor,2))) %>%
            addLegend(pal = pal_us, values = ~us_cor1$cor, opacity = 0.8,title = "Correlation",position = "bottomleft")
    })
    
    output$chTendency <- renderLeaflet({
        leaflet(map_ch) %>% 
            setView(lat = 30,lng = 0,zoom = 1) %>% 
            addTiles() %>% 
            addPolygons(weight = 1,
                        fillColor= ~pal_ch(ch_cor1$cor),
                        fillOpacity = 1,
                        color = "grey",
                        label = ~paste0(ch_cor1$country,", ",round(ch_cor1$cor,2))) %>% 
            addLegend(pal = pal_ch, values = ~ch_cor1$cor, opacity = 0.8,title = "Correlation",position = "bottomleft")
    })
    
    output$vetoPlot <- renderImage({
        
        outfile <- tempfile(fileext='.gif')
        
        p <- ggplot(vot %>% filter(country %in% c("China","France","Russia","United Kingdom","United States of America")), aes(x=year, y=clust,col = country,group = country)) +
            geom_line(position=position_dodgev(height=0.3),size = 2, alpha = 1) +
            transition_reveal(year) +
            facet_wrap(~country,ncol = 1)+
            theme_minimal(base_family = "Helvetica Neue")+
            theme(legend.position = "none",axis.title.y = element_text(size = 22,margin = unit(c(1.5,1.5,1.5,1.5),"cm")),axis.text.x = element_text(size = 17),axis.text.y = element_text(size = 12),
                  strip.text.x = element_text(size = 20)) +
            scale_color_brewer(palette = "Pastel1") +
            scale_x_continuous(breaks=seq(1940, 2020, by = 10))+
            labs(color = NULL,x = NULL, y = "Group")
            # geom_text(aes(x=2010,y=4.32,label = year), 
            #           size = 14,
            #           color = "gray70", 
            #           alpha = 0.1,
            #           fontface = "italic",
            #           family = "Avenir") +

            # guides(colour = guide_legend(override.aes = list(size=4)))+
        
        anim_save("outfile.gif", animate(p,fps = 2,nframes = 70,width = 1000, height=500))
        list(src = "outfile.gif",
             contentType = 'image/gif'
        )}, deleteFile = TRUE)
    
    output$indoPlot <- renderImage({
        
        outfile <- tempfile(fileext='.gif')
        
        p <- ggplot(vot %>% filter(country == "Indonesia"), aes(x=year, y=clust,col = country,group = country)) +
            geom_line(size = 2, alpha = 1) +
            transition_reveal(year) +
            geom_text(aes(x=2010,y=4.32,label = year), 
                      size = 17,
                      color = "gray70", 
                      alpha = 0.7,
                      fontface = "italic",
                      family = "Avenir") +
            theme_minimal(base_family = "Helvetica Neue")+
            scale_x_continuous(breaks=seq(1940, 2020, by = 10))+
            theme(legend.position = "none",axis.title.y = element_text(size = 22,margin = unit(c(1.5,1.5,1.5,1.5),"cm")),axis.text.x = element_text(size = 17),axis.text.y = element_text(size = 14)) +
            labs(color = "Cluster",x = NULL, y = "Group")
        
        anim_save("outfile.gif", animate(p,fps = 2,nframes = 64,width = 1000, height=500))
        list(src = "outfile.gif",
             contentType = 'image/gif')
        }, deleteFile = TRUE)
    
    output$choPlot <- renderPlotly({
        votc <- vot %>% 
            filter(country  %in% input$country_opt) %>% 
            mutate(text = paste(year,"-",country))
        
       p <-  ggplot(votc, aes(x=year, y=clust,col = country,group = country,text=text)) +
            geom_line(position=position_dodgev(height=0.3),size = 1, alpha = 1) +
            theme_minimal(base_family = "Helvetica Neue")+
            theme(legend.position = "none",axis.title.y = element_text(size = 15,margin = unit(c(1.5,1.5,1.5,1.5),"cm")),axis.text.x = element_text(size = 13),axis.text.y = element_text(size = 11)) +
            scale_color_brewer(palette = "Pastel1")+
            scale_x_continuous(breaks=seq(1940, 2020, by = 10))+
            labs(color = "Cluster",subtitle = "1946-2015",x = NULL, y = "Group")+
            scale_y_discrete(limits = 1:4)
       
       ggplotly(p,tooltip = "text")
    })
        
}

shinyApp(ui = ui,server = server)