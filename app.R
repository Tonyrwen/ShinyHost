
require(tidyverse)
require(ggplot2); require(ggpubr)
require(qacBase); require(qacDR)
require(Rtsne); require(cluster)

require(ggalluvial)
require(igraph);require(networkD3) # (interactive) network

require(kableExtra)

require(fmsb) # radar plot

require(shiny)

# load in collected data
player_stats = read.csv("winnestTeam_stats.csv")
lineup_stats = read.csv("lineup_stats.csv")
assist_net = read.csv("AssistNetwork.csv")

#try to replace NAs with 0s
# and get rid of time-dependent fouls
player_stats %>% replace(is.na(.), 0) %>% 
  select(-contains("Period")) -> player_stats_mod


# FA for a factor of 4
player_stats_model = player_stats_mod  %>% 
                      .[11:ncol(player_stats_mod)] %>% 
                      FA(nfactors = 4, rotate = "promax")

player_stats_model$Structure %>% add_rownames(var = "From") %>%
  mutate(prime_factor = case_when(pmax(abs(F1), abs(F2), abs(F3), abs(F4)) == abs(F1) ~ "F1",
                                  pmax(abs(F1), abs(F2), abs(F3), abs(F4)) == abs(F2) ~ "F2",
                                  pmax(abs(F1), abs(F2), abs(F3), abs(F4)) == abs(F3) ~ "F3",
                                  pmax(abs(F1), abs(F2), abs(F3), abs(F4)) == abs(F4) ~ "F4")) %>%
  gather("To", "Weight", 2:5)%>%
  #reorder
  select(From, To, Weight, prime_factor) %>%
  mutate(edgeCol = case_when(To == "F1" ~ "red",
                             To == "F2" ~ "orange",
                             To == "F3" ~ "green",
                             To == "F4" ~ "blue"))-> pstats_from_to

pstats_from_to %>% select("From") %>% unique() %>%
  left_join(pstats_from_to, by = "From") %>%
  mutate(color = case_when(prime_factor == "F1" ~ "red",
                           prime_factor == "F2" ~ "orange",
                           prime_factor == "F3" ~ "green",
                           prime_factor == "F4" ~ "blue"),
         size = ifelse(prime_factor %in% c("F1", "F2", "F3", "F4"), 10, 5)) %>%
  select(From, color, size, prime_factor)%>% 
  rbind(tibble(From = c("F1", "F2", "F3", "F4"),
               color = c("red", "orange", "green", "blue"),
               size = c(10, 10, 10, 10),
               prime_factor = c("F1", "F2", "F3", "F4"))) %>% unique() -> pstats_node 

#build a shiny app for all three visual breakdown of the factors
ui = fluidPage(
  titlePanel(textOutput("title")),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "FactorInvolved",
        label = "Select the Factors Involved (Best for Network)",
        choices = c("F1", "F2", "F3", "F4"),
        selected = "F1"
      ),
      radioButtons(
        inputId = "Polarity",
        label = "Select the Direction of the Correlation (Alluvial Only)",
        choices = c("Positive", "Negative"),
        selected = "Positive"
      )
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Network", plotOutput(outputId = "network")),
        tabPanel("Alluvial", plotOutput(outputId = "alluvial")),
        tabPanel("Table", tableOutput(outputId = "FactorComp"))
      )
    )
  )
)


server = function(input, output){
  
  output$title = renderText("Factor Breakdown")
  
  output$FactorComp = function()(
    {
      player_stats_model$Structure %>% 
        mutate(prime_factor = case_when(pmax(abs(F1), abs(F2), abs(F3), abs(F4)) == abs(F1) ~ "F1",
                                        pmax(abs(F1), abs(F2), abs(F3), abs(F4)) == abs(F2) ~ "F2",
                                        pmax(abs(F1), abs(F2), abs(F3), abs(F4)) == abs(F3) ~ "F3",
                                        pmax(abs(F1), abs(F2), abs(F3), abs(F4)) == abs(F4) ~ "F4")) %>% 
        filter(prime_factor %in% input$FactorInvolved) %>% arrange(desc(input$FactorInvolved[1])) -> result
      
      result %>% kable("html", align = "ccccc") %>%
        row_spec(seq(1,nrow(result), 2), 
                 background = "gray",
                 color = "white") %>%
        kable_paper("hover") %>%
        scroll_box(height = "500px")
    }
  )
  
  output$network = renderPlot(
    {
      plot(graph_from_data_frame(pstats_from_to %>% filter(prime_factor %in% input$FactorInvolved),
                                 directed = F,
                                 pstats_node %>% filter(prime_factor %in% input$FactorInvolved|From %in% c("F1", "F2", "F3", "F4"))),
           edge.width = (pstats_from_to %>% filter(prime_factor %in% input$FactorInvolved))$Weight,
           edge.color = (pstats_from_to %>% filter(prime_factor %in% input$FactorInvolved))$edgeCol)
    }, height = 1000, width = 1000
  )
  
  output$alluvial = renderPlot(
    {
      if (input$Polarity == "Positive"){
        pstats_from_to %>% filter(prime_factor %in% input$FactorInvolved, 
                                  Weight > 0) %>%
          ggplot(aes(axis1 = From, axis2 = To, y = Weight))+
          geom_flow(aes(fill = To))+
          geom_stratum()+
          geom_text(stat = "stratum", infer.label = T)+
          scale_x_discrete(limits = c("Stats", "Factors"))}
      else{
        pstats_from_to %>% filter(prime_factor %in% input$FactorInvolved, 
                                  Weight < 0) %>%
          ggplot(aes(axis1 = From, axis2 = To, y = Weight))+
          geom_flow(aes(fill = To))+
          geom_stratum()+
          geom_text(stat = "stratum", infer.label = T)+
          scale_x_discrete(limits = c("Stats", "Factors"))
      }
    }, height = 800, width = 1000
  )
}

shinyApp(ui, server, options = list(height = 1000))
