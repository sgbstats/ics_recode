library(rlang)
library(shiny)
library(tidyverse)
library(shinyWidgets)
#library(lubridate)
#library(readxl)
library(ggplot2)
#library(ggsankey)
library(htmltools)
#library(flextable)
library(htmlwidgets)
library(shinyjs)
load("Data/aggregated_results.Rda")
load("Data/imp_aggregated_results.Rda")
study_names=names(results_imp[1:22])
load("Data/vars.Rda")
`%notin%`=Negate(`%in%`)
unpack=function(list)
{
  for(i in names(list))
  {
    assign(i, list[[i]], envir = globalenv())
  }
}
unpack(vars)
outcomes=outcomes_list$outcome
interaction=interactions$name
# as=vars$analysis_sets
names(outcomes)=outcomes_list$label
names(interaction)=interactions$label

interaction2=interactions$name[-1]
names(interaction2)=interactions$label[-1]
#study_names=names(results)[names(results) %notin% c( "aggregated" ,  "aggregated_marginal" ,"meta"  ,   "meta_marginal"  ,"meta_diagnostics", "study_diagnostics"  )]
# names(as)=c("All Included participants", "No Asthma", "Triple Thearpy Trials", "ICS at baseline", "No ICS at baseline")
# 
# outcomes_skip=c(3:15, 16:29, 31:34)
# interactions_skip=c(3:9, 11:12)
# analysis_sets_skip=2:5
# outcomes_list3=outcomes_list %>% filter(i %notin% outcomes_skip)
# interactions3 =interactions %>% filter(j %notin% interactions_skip)

# outcomes3=outcomes_list3$outcome
# interaction3=interactions3$name
# as=vars$analysis_sets
# names(outcomes3)=outcomes_list3$label
# names(interaction3)=interactions3$label

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-validation {
        color: #ff0000;
        font-weight: bold;
      }
    "))
  ),
  tags$head(tags$script('
                                var dimension = [0, 0];
                                $(document).on("shiny:connected", function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                                $(window).resize(function(e) {
                                    dimension[0] = window.innerWidth;
                                    dimension[1] = window.innerHeight;
                                    Shiny.onInputChange("dimension", dimension);
                                });
                            ')),
  
  tabsetPanel(
    tabPanel("Meta Analysis", fluid=T,
             shinyjs::useShinyjs(),
             sidebarLayout(
               sidebarPanel(
                 shinyjs::useShinyjs(),
                 id = "side-panel",
                 #radioButtons("data", label="Data", choices = list("Imputed dataset"=T,"Complete Case"=F), selected = T),
                 selectInput("outcome", "Outcome", choices= outcomes),
                 selectInput("interaction", "Interaction", choices = interaction),
                 selectInput("set", "Analysis Set", choices=analysis_sets, selected = "included_main"),
                 pickerInput("studies", "Included Studies", choices = study_names, selected = study_names, multiple = T ),
                 sliderInput("xwidth", label = "X width", min = 2, 
                             max = 10, value = 4),
                 actionButton("reset_input", "Reset inputs"),
                 width=4
               ),
               mainPanel(
                 plotOutput("forest", width = "100%", height = "500px")
               )
             )
    ),
    tabPanel("Marginal Effects", fluid=T,
             shinyjs::useShinyjs(),
             sidebarLayout(
               sidebarPanel(
                 #radioButtons("data2", label="Data", choices = list("Imputed dataset"=T,"Complete Case"=F), selected = T),
                 selectInput("outcome2", "Outcome", choices= outcomes),
                 selectInput("interaction2", "Interaction", choices = interaction2),
                 selectInput("set2", "Analysis Set", choices=analysis_sets),
                 # pickerInput("studies2", "Included Studies", choices = study_names, selected = study_names, multiple = T ),
                 actionButton("reset_input2", "Reset inputs"),
                 width=4
               ),
               mainPanel(
                 plotOutput("marginal", width = "100%")
               )
             )
    )#,
    
    # tabPanel("Unadjusted vs Adjusted Effects", fluid=T,
    #          shinyjs::useShinyjs(),
    #          sidebarLayout(
    #            sidebarPanel(
    #              #radioButtons("data3", label="Data", choices = list("Imputed dataset"=T,"Complete Case"=F), selected = T),
    #              selectInput("outcome3", "Outcome", choices= outcomes),
    #              selectInput("interaction3", "Interaction", choices = interaction),
    #              # selectInput("set2", "Analysis Set", choices=analysis_sets),
    #              pickerInput("studies3", "Included Studies", choices =  study_names, selected =study_names, multiple = T ),
    #              actionButton("reset_input3", "Reset inputs"),
    #              width=4
    #            ),
    #            mainPanel(
    #              plotOutput("adj", width = "100%", height = "1200")
    #            )
    #          )
    # ),
    # tabPanel("Metabias", fluid=T,
    #          shinyjs::useShinyjs(),
    #          sidebarLayout(
    #            sidebarPanel(
    #              #radioButtons("data4", label="Data", choices = list("Imputed dataset"=T,"Complete Case"=F), selected = T),
    #              selectInput("outcome4", "Outcome", choices= outcomes),
    #              selectInput("interaction4", "Interaction", choices = interaction),
    #              selectInput("set4", "Analysis Set", choices=analysis_sets),
    #              pickerInput("studies4", "Included Studies", choices =  study_names, selected =study_names, multiple = T ),
    #              actionButton("reset_input4", "Reset inputs"),
    #              width=4
    #            ),
    #            mainPanel(
    #              plotOutput("bias", width = "100%")
    #            )
    #          )
    # )
  )
)