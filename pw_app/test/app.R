library(rlang)
library(shiny)
library(tidyverse)
library(shinyWidgets)
#library(lubridate)
#library(readxl)
library(ggplot2)
#library(ggsankey)
library(htmltools)
#ibrary(flextable)
library(htmlwidgets)
library(meta)
library(shinyjs)
library(googledrive)
load("Data/imp_aggregated_results.RDa")
load("Data/vars.RDa")
load("Data/study_names.RDa")


unpack=function(list)
{
  for(i in names(list))
  {
    assign(i, list[[i]], envir = globalenv())
  }
}
results_shiny=results_imp
study_names=names(results_imp[1:22])
`%notin%`=Negate(`%in%`)

unpack(vars)
outcomes=outcomes_list$outcome
interaction=interactions$name
# as=vars$analysis_sets
names(outcomes)=outcomes_list$label
names(interaction)=interactions$label

interaction2=interactions$name[-1]
names(interaction2)=interactions$label[-1]


# Define UI
ui <- fluidPage(
  uiOutput("page") # Dynamic UI that changes after login
)

# Define server
server <- function(input, output, session) {

 
  # Set a correct password
  correct_password <- "uhsm"
  
  # Reactive value to track login state
  user_authenticated <- reactiveVal(FALSE)
  
  # UI changes based on login state
  output$page <- renderUI({
    if (!user_authenticated()) {
      # Landing Page with Password Input
      tagList(
        fluidRow(
          column(4, offset = 4,
                 wellPanel(
                   h3("Welcome! Please enter the password:"),
                   passwordInput("password", "Password"),
                   actionButton("login_btn", "Login"),
                   textOutput("login_message")
                 )
          )
        )
      )
    } else {
      # Main App with Tabs after Successful Login
      fluidPage(
        titlePanel("ICS-RECODE"),
        
        sidebarLayout(
          sidebarPanel(
            # Different inputs for each plot
            shinyjs::useShinyjs(),
            conditionalPanel(
              condition = "input.tabselected == 'forest'",
              selectInput("outcome", "Outcome", choices= outcomes),
              selectInput("interaction", "Interaction", choices = interaction),
              selectInput("set", "Analysis Set", choices=analysis_sets, selected = "included_main"),
              pickerInput("studies", "Included Studies", choices = study_names, selected = study_names, multiple = T ),
              sliderInput("xwidth", label = "X width", min = 2, 
                          max = 10, value = 4),
              actionButton("reset_input", "Reset inputs"),
            ),
            conditionalPanel(
              condition = "input.tabselected == 'marginal'",
              selectInput("outcome2", "Outcome", choices= outcomes),
              selectInput("interaction2", "Interaction", choices = interaction2),
              selectInput("set2", "Analysis Set", choices=analysis_sets),
              # pickerInput("studies2", "Included Studies", choices = study_names, selected = study_names, multiple = T ),
              actionButton("reset_input2", "Reset inputs")
            ),
            actionButton("logout_btn", "Logout", class = "btn-danger")
          ),
          
          # Main Panel with Tabs
          mainPanel(
            tabsetPanel(
              id = "tabselected",
              tabPanel("Forest Plot", value = "forest", plotOutput("forest")),
              tabPanel("Marginal Plot", value = "marginal", plotOutput("marginal"))
            )
          )
        )
      )
    }
  })
  
  # Observe login attempt
  observeEvent(input$login_btn, {
    if (input$password == correct_password) {
      user_authenticated(TRUE)
    } else {
      output$login_message <- renderText("❌ Incorrect password. Try again.")
    }
  })
  
  # Logout functionality
  observeEvent(input$logout_btn, {
    user_authenticated(FALSE)
  })
  
  observeEvent(input$reset_input, {
    shinyjs::reset("outcome")
    shinyjs::reset("set")
    shinyjs::reset("studies")
    shinyjs::reset("interaction")
    shinyjs::reset("data")
  })
  observeEvent(input$reset_input2, {
    shinyjs::reset("outcome2")
    shinyjs::reset("set2")
    shinyjs::reset("studies2")
    shinyjs::reset("interaction2")
    shinyjs::reset("data2")
  })
  
  output$forest=renderPlot({
    
    
    if(input$interaction=="main")
    {
      label.left="Favours ICS"
      label.right="Favours No ICS"
    }else 
    {
      label.left=paste0((interactions %>% filter(name==input$interaction))$interpretation,"\nFavours ICS")
      label.right=paste0((interactions %>% filter(name==input$interaction))$interpretation,"\nFavours No ICS")
      
      
    }
    
    outcome=outcomes_list %>% filter(outcome==input$outcome)
    stat=case_when(outcome$class=="count"~"log(RR)",
                   outcome$class=="tte"~"log(HR)",
                   outcome$class=="lm"~"Diff",
                   T~"")
    
    
    
    sm=if_else(grepl("FEV|SGRQ|fvc", outcome$outcome, ignore.case=T), "MD","RR")
    agg_res=results_shiny[["aggregated"]][[input$outcome]][[input$interaction]][[input$set]] %>% filter(studLab %in% input$studies) %>% 
      filter(seTE<if_else(sm=="MD", 10,3))
    m=meta::metagen(data = agg_res, 
                    TE=TE, seTE=seTE, studlab = studLab, n.c = n.c, n.e=n.e, sm=sm,
                    method.tau = "REML", random=T, common=F, control=list(maxiter=1000, verbose=F, step=0.5))
    
    if(stat !="Diff")
    {
      
      forest(m,
             leftcols=c("studLab", "TE", "seTE","event.e", "n.e","event.c", "n.c" ),
             leftlabs=c("Study", "log(RR)", "SE", "ICS\nEvents", "ICS\nN", "No ICS\nEvents", "No ICS\nN"),
             just="c",  width = 9, colgap = "0.4cm",
             label.left = label.left, label.right = label.right,
             # label.e = "ICS", label.c = "No ICS",
             # label.e.attach = "event.e", label.c.attach = "event.c",
             col.square = "#FF0000",
             col.diamond = "#888888",
             lwd=2,
             xlim=c(1/input$xwidth, input$xwidth)
      ) 
    }else{
      forest(m,
             leftcols=c("studLab", "TE", "seTE","n.e", "n.c" ),
             leftlabs=c("Study", stat, "SE", "ICS\nN", "No ICS\nN"),
             just="c",  width = 13, colgap = "0.4cm",
             label.left = label.right, label.right = label.left,
             # label.e = "ICS", label.c = "No ICS",
             # label.e.attach = "event.e", label.c.attach = "event.c",
             col.square = "#FF0000",
             col.diamond = "#888888",
             lwd=2
             # xlim=c(0.1,10)
      )
    }
  })
  
  output$marginal=renderPlot({
    # if(input$data2)
    # {
    results_shiny=results_imp
    # }else
    # {
    #   results_shiny=results
    # }
    interaction=interactions %>% filter(name==input$interaction2)
    outcome=outcomes_list %>% filter(outcome==input$outcome2)
    analysis_set=analysis_sets[analysis_sets==input$set2]
    
    
    margin=case_when(interaction$value=="eos_v"~0.03,
                     interaction$value=="eos_pc_v"~0.3,
                     interaction$value=="change_v"~0.015,
                     interaction$value=="percentage_v"~1.5,
                     interaction$value=="logical_v"~0.3)
    
    
    # png(file = paste0("results/marginal_plots/",outcomes_list$outcome[i],"_",interactions$name[j],"_",analysis_sets[k], "_", "forest_plot.png"), width = 3600, height = 1800, res = 300)
    data=results_shiny[["meta_marginal"]][[outcome$outcome]][[interaction$name]][[analysis_set]] %>%
      mutate(status=case_when(upperrr<1~"Favours ICS",
                              lowerrr>1~"Favours no ICS",
                              T~"No significance"))
    # 
    yaxis=unique(data$value)
    #names(yaxis)=yaxis
    if(input$interaction2=="smoking_bl")
    {
      names(yaxis)=c("Current smoker", "Former smoker")
    }else if(input$interaction2=="ics_bl")
    {
      names(yaxis)=c("ICS", "No ICS")
    }else if(input$interaction2=="laba_bl")
    {
      names(yaxis)=c("LABA", "No LABA")
    }else
    {
      names(yaxis)=yaxis
    }
    
    xlab=if_else(outcome$class=="count", "Risk ratio", "Hazard ratio")
    ylab=if_else(input$interaction2 %in% c("ics_bl", "laba_bl", "smoking_bl"), interaction$label,paste0(interaction$label, " (", interaction$unit, ")"))
    
    p=data %>% ggplot(aes(x=rr, y=value, colour=status, xmin=lowerrr, xmax=upperrr))+
      geom_point(size=5)+
      geom_errorbarh(linewidth=1)+
      geom_vline(xintercept = 1, linewidth=1)+
      labs(x=xlab,
           y=ylab,
           colour="Interpretation")+
      theme_bw()+
      scale_colour_manual(values=c("Favours ICS"="#4DAF4A",
                                   "Favours no ICS"="#E41A1C",
                                   "No significance"="#007AC0"))+
      scale_x_log10()
    if(interaction$value=="logical_v")
    {
      p+scale_y_discrete(breaks = yaxis, labels=names(yaxis))
    }else
    {
      p+scale_y_continuous(breaks = yaxis)
    }
    
    
    
  })
  observeEvent(list(input$interaction, input$outcome, input$set),{
    req(input$interaction, input$outcome, input$set)
    studies=(results_shiny[["aggregated"]][[input$outcome]][[input$interaction]][[input$set]])$studLab

    updatePickerInput(session, "studies",
                      choices = studies,
                      selected = studies)


  })
}

# Run the app
shinyApp(ui, server)
