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
library(flextable)
library(bslib)
# library(httr)
# library(jsonlite)
# library(googledrive)
load("Data/imp_aggregated_results.RDa")
load("Data/vars.RDa")
load("Data/study_names.RDa")
# load("Data/test_list.RDa")
# load("Data/submodels_pneum.RDa")
# load("Data/submodels_modsev.RDa")
# load("Data/submodels_sev.RDa")
load("Data/submodels_pneum_smol.RDa")
load("Data/submodels_modsev_smol.RDa")
load("Data/submodels_sev_smol.RDa")
source("helper_scripts/prediction.R")
# plan(multisession)
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
  theme = bs_theme(preset = "pulse"),
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
            conditionalPanel(condition = "input.tabselected == 'pred'",
                             checkboxGroupInput("vars_to_use", label="Vars to use", c("Eos", "Age", "Exacerbations", "FEV1", "Sex", "Smoking"),
                                                selected = c("Eos", "Age", "Exacerbations", "FEV1", "Sex", "Smoking"), inline=T),
                             conditionalPanel("input.vars_to_use.includes('Eos')",
                                              numericInput("eos_bl", label = "Eosinophils (10^6/L)", value = 200, min=0) ),

                             conditionalPanel("input.vars_to_use.includes('Age')",
                                              numericInput("age_imp", label = "Age", value = 65, min=30, max=100)),

                             conditionalPanel("input.vars_to_use.includes('Exacerbations')",
                                              numericInput("exac_bl", label = "Exacberbations in 12m", value = 1, min=0, max=10)),

                             conditionalPanel("input.vars_to_use.includes('FEV1')",
                                              numericInput("fev1_bl", label = "FEV1", value = 0.8, min=0)),

                             conditionalPanel("input.vars_to_use.includes('Sex')",
                                              radioButtons("sex", label = "Sex", c("Male"="M", "Female"="F"), selected = "M", inline=T)),

                             conditionalPanel("input.vars_to_use.includes('Smoking')",
                                              radioButtons("smoking_bl","Smoking history", c("Current"="Current", "Former"="Former"), selected = "Former", inline=T)),

                             numericInput("trt_dur", label = "Time horizon", value = 1, min=0.25, max=2),
                             actionButton("go", "Go"),
                             actionButton("reset_input5", "Reset input"),
            ),
            br(),
            actionButton("logout_btn", "Logout", class = "btn-danger")
          ),
          
          # Main Panel with Tabs
          mainPanel(
            tabsetPanel(
              id = "tabselected",
              tabPanel("Forest Plot", value = "forest", plotOutput("forest")),
              tabPanel("Marginal Plot", value = "marginal", plotOutput("marginal")),
              tabPanel("Prediction", value = "pred",
                       conditionalPanel(
                         condition = "!output.has_prediction_data",
                         HTML("<b>Please press go to see the predictions</b>")
                       ),
                       conditionalPanel(
                         condition = "output.has_prediction_data",
                         navset_card_underline(
                           nav_panel("Plot", plotOutput("waffle1")),
                           nav_panel("Table", uiOutput("table1")),
                           #nav_panel("Coefficients", uiOutput("coefs2"))
                         )
                       )
              ),
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
  observeEvent(input$reset_input5, {
    shinyjs::reset("eos_bl")
    shinyjs::reset("age_imp")
    shinyjs::reset("fev1_bl")
    shinyjs::reset("exac_bl")
    shinyjs::reset("sex")
    shinyjs::reset("smoking_bl")
    shinyjs::reset("trt_dur")
  })
  
  output$has_prediction_data <- reactive({
    # Check if pred() has been run and returned valid data
    pred_data <- try(pred(), silent = TRUE)
    
    # Return TRUE if we have valid prediction data
    return(!is.null(pred_data) && 
             !inherits(pred_data, "try-error") && 
             !is.null(pred_data$modsev) && 
             !is.null(pred_data$modsev$fit) && 
             length(pred_data$modsev$fit) >= 2)
  })
  
  # Make sure this reactive value is available to the UI
  outputOptions(output, "has_prediction_data", suspendWhenHidden = FALSE)
  
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
    xaxis=unique(data$value)
    #names(yaxis)=yaxis
    if(input$interaction2=="smoking_bl"){
      names(xaxis)=c("Current smoker", "Former smoker")
    }else if(input$interaction2=="ics_bl"){
      names(xaxis)=c("ICS", "No ICS")
    }else if(input$interaction2=="laba_bl"){
      names(xaxis)=c("LABA", "No LABA")
    }else{
      names(xaxis)=xaxis
    }
    
    # xlab=case_when(outcome$class=="count"~"Risk ratio",
    #                outcome$class=="tte"~"Hazard ratio",
    #                outcome$class=="lm"~"Mean difference",
    #                outcome$class=="logisitc"~"Odds ratio")
    
    ylab=if_else(input$interaction2 %in% c("ics_bl", "laba_bl", "smoking_bl"), interaction$label,paste0(interaction$label, " (", interaction$unit, ")"))
    no_effect=if_else(outcome$class=="lm", 0,1)
    
    if(interaction$value!="logical_v")  {
      p1=data %>% ggplot(aes(y=rr, x=value, ymin=lowerrr, ymax=upperrr))+
        geom_line(linewidth=1, col="#007AC0")+
        geom_ribbon(alpha=0.3, col="#007AC0", fill="#007AC0")+
        geom_hline(yintercept = no_effect, linewidth=1)+
        theme_classic()+
        theme(legend.position="top",
              plot.margin = margin(5.5, 5.5, 5.5, 5.5))+
        scale_y_log10()+
        coord_cartesian(clip="off")+
        scale_x_continuous(breaks = xaxis)+
        labs(y=case_when(outcome$class=="count"~ "Risk ratio",
                         outcome$class=="tte"~"Hazard ratio",
                         outcome$class=="logistic"~"Odds Ratio",
                         grepl("sgrq",outcome$outcome)~"Mean difference",
                         outcome$class=="lm"~"Mean difference (L)"),
             x=paste0(interaction$label, " (", interaction$unit, ")"))
    }else {
      p1=data %>% ggplot(aes(y=rr, x=value, ymin=lowerrr, ymax=upperrr))+
        geom_point(size=5, ,col="#007AC0")+
        geom_errorbar(linewidth=1,col="#007AC0")+
        geom_hline(yintercept = no_effect, linewidth=1)+
        theme_classic()+
        theme(legend.position="top",
              plot.margin = margin(5.5, 5.5, 5.5, 5.5))+
        scale_y_log10()+
        coord_cartesian(clip="off")+
        scale_x_discrete(labels = names(xaxis))+
        labs(y=case_when(outcome$class=="count"~ "Risk ratio",
                         outcome$class=="tte"~"Hazard ratio",
                         outcome$class=="logistic"~"Odds Ratio",
                         grepl("sgrq",outcome$outcome)~"Mean difference",
                         outcome$class=="lm"~"Mean difference (L)"),
             x=""
        )
    }
    
    p1
    
  })
  
  observeEvent(list(input$interaction, input$outcome, input$set),{
    req(input$interaction, input$outcome, input$set)
    studies=(results_shiny[["aggregated"]][[input$outcome]][[input$interaction]][[input$set]])$studLab
    
    updatePickerInput(session, "studies",
                      choices = studies,
                      selected = studies)
    
    
  })
  #original method
  pred=eventReactive(input$go,{

    eos=if_else("Eos" %in% input$vars_to_use,input$eos_bl/1000, NA_integer_)
    age=if_else("Age" %in% input$vars_to_use,input$age_imp, NA_integer_)
    exac=if_else("Exacerbations" %in% input$vars_to_use,input$exac_bl, NA_integer_)
    sex=if_else("Sex" %in% input$vars_to_use,input$sex, NA_character_)
    fev1=if_else("FEV1" %in% input$vars_to_use,input$fev1_bl, NA_integer_)
    smoking=if_else("Smoking" %in% input$vars_to_use,input$smoking_bl, NA_character_)

    test3=tribble(~"arm_ipd", ~"eos_bl",~"age_imp",~"exac_bl",~"sex",~"fev1_bl",~"smoking_bl",~"trt_dur",
                  "ICS", eos, age, exac, sex,fev1, smoking, input$trt_dur,
                  "Control", eos, age, exac, sex,fev1, smoking, input$trt_dur,)

    # exac_modsev=predict.submodels(test3, submodels.object_modsev, type="response")
    # exac_sev=predict.submodels(test3, submodels.object_sev, type="response")
    # exac_pneum=predict.submodels(test3, submodels.object_pneum, type="response")
    
    exac_modsev=predict.glm.nb.submodel(test3, submodels.object_modsev_smol, type="response")
    exac_sev=predict.glm.nb.submodel(test3, submodels.object_sev_smol, type="response")
    exac_pneum=predict.glm.nb.submodel(test3, submodels.object_pneum_smol, type="response")
    
    list("modsev"=exac_modsev,
         "sev"=exac_sev,
         "pneum"=exac_pneum,
         "data"=test3)

  })
# 
#   
# # API
#   pred <- eventReactive(input$go, {
#     eos <- if_else("Eos" %in% input$vars_to_use, input$eos_bl / 1000, NA_integer_)
#     age <- if_else("Age" %in% input$vars_to_use, input$age_imp, NA_integer_)
#     exac <- if_else("Exacerbations" %in% input$vars_to_use, input$exac_bl, NA_integer_)
#     sex <- if_else("Sex" %in% input$vars_to_use, input$sex, NA_character_)
#     fev1 <- if_else("FEV1" %in% input$vars_to_use, input$fev1_bl, NA_integer_)
#     smoking <- if_else("Smoking" %in% input$vars_to_use, input$smoking_bl, NA_character_)
# 
#     data_to_send <- tribble(
#       ~"arm_ipd", ~"eos_bl", ~"age_imp", ~"exac_bl", ~"sex", ~"fev1_bl", ~"smoking_bl", ~"trt_dur",
#       "ICS", eos, age, exac, sex, fev1, smoking, input$trt_dur,
#       "Control", eos, age, exac, sex, fev1, smoking, input$trt_dur
#     )
# 
#     print("Triggering API call")
#     print(data_to_send) # Log what you're sending
# 
#     tryCatch({
#       api_url <- "http://127.0.0.1:8000/predict2"
# 
#       # Use better error handling for the API call
#       response <- httr::POST(
#         url = api_url,
#         body = list(newdata = as.list(data_to_send)),
#         encode = "json",
#         httr::timeout(10) # Add timeout to avoid hanging
#       )
# 
#       # Log HTTP status and response
#       print(paste("HTTP Status:", httr::http_status(response)$status))
# 
#       if (httr::http_error(response)) {
#         error_message <- paste("API request failed with status:", httr::http_status(response)$message)
#         print(error_message)
#         stop(error_message)
#       }
# 
#       content <- httr::content(response, "text")
#       print(paste("API Response:", substr(content, 1, 100), "...")) # Preview response
#       content <- jsonlite::fromJSON(content)
# 
#       list(
#         modsev = content$modsev,
#         sev = content$sev,
#         pneum = content$pneum,
#         coefs = content$coefs
#       )
#     }, error = function(e) {
#       print(paste("Error in API call:", e$message))
#       stop(e)
#     })
#   })
# 
#   # pred <- eventReactive(input$go, {
#   #   eos <- if_else("Eos" %in% input$vars_to_use, input$eos_bl / 1000, NA_integer_)
#   #   age <- if_else("Age" %in% input$vars_to_use, as.numeric(input$age_imp), NA_integer_)
#   #   exac <- if_else("Exacerbations" %in% input$vars_to_use, as.numeric(input$exac_bl), NA_integer_)
#   #   sex <- if_else("Sex" %in% input$vars_to_use, input$sex, NA_character_)
#   #   fev1 <- if_else("FEV1" %in% input$vars_to_use, input$fev1_bl, NA_integer_)
#   #   smoking <- if_else("Smoking" %in% input$vars_to_use, input$smoking_bl, NA_character_)
#   #   
#   #   data_to_send <- tribble(
#   #     ~"arm_ipd", ~"eos_bl", ~"age_imp", ~"exac_bl", ~"sex", ~"fev1_bl", ~"smoking_bl", ~"trt_dur",
#   #     "ICS", eos, age, exac, sex, fev1, smoking, 1,
#   #     "Control", eos, age, exac, sex, fev1, smoking, 1
#   #   )
#   #   
#   #   matched_test=match_elements(original_list = original_list,
#   #                              data_to_send %>% dplyr::select(names(original_list)) %>% 
#   #                                mutate(sex=as.character(sex),
#   #                                       arm_ipd=as.character(arm_ipd),
#   #                                       smoking_bl=as.character(smoking_bl)) %>% 
#   #                                as.list())
#   #   predictions=get_data_from_nested_list(matched_test, test_list)
#   #   predictions <- predictions %>%
#   #     mutate(across(contains("fit"), ~ . * imput$trt_dur))
#   #   list("modsev"=predictions$ms_fit,
#   #               "sev"=predictions$s_fit,
#   #               "pneum"=predictions$p_fit)
#   #   
#   # })
#   
# 
# 
#   
  output$table1=renderUI({
    exac_modsev=pred()$modsev
    exac_sev=pred()$sev
    exac_pneum=pred()$pneum

    # req(result_reactive())
    # exac_modsev <- result_reactive()$modsev
    # exac_sev <- result_reactive()$sev
    # exac_pneum <- result_reactive()$pneum

    tribble(~"outcome",~"trt", ~"events",
            "Mod/Sev Exac", "ICS", sprintf("%.1f",exac_modsev$fit[1]),
            "Mod/Sev Exac", "No ICS", sprintf("%.1f",exac_modsev$fit[2]),
            "Mod/Sev Exac", "Absolute difference", sprintf("%.1f",exac_modsev$fit[1]-exac_modsev$fit[2]),
            "Mod/Sev Exac", "Relative difference", paste0(sprintf("%.1f",100*(exac_modsev$fit[1]-exac_modsev$fit[2])/exac_modsev$fit[2]), "%"),
            "Severe Exac", "ICS", sprintf("%.1f",exac_sev$fit[1]),
            "Severe Exac", "No ICS", sprintf("%.1f",exac_sev$fit[2]),
            "Severe Exac", "Absolute difference", sprintf("%.1f",exac_sev$fit[1]-exac_sev$fit[2]),
            "Severe Exac", "Relative difference", paste0(sprintf("%.1f",100*(exac_sev$fit[1]-exac_sev$fit[2])/exac_sev$fit[2]), "%"),
            "Pneumonias", "ICS", sprintf("%.1f",exac_pneum$fit[1]),
            "Pneumonias", "No ICS", sprintf("%.1f",exac_pneum$fit[2]),
            "Pneumonias", "Absolute difference", sprintf("%.1f",exac_pneum$fit[1]-exac_pneum$fit[2]),
            "Pneumonias", "Relative difference", paste0(sprintf("%.1f",100*(exac_pneum$fit[1]-exac_pneum$fit[2])/exac_pneum$fit[2]), "%")) %>%
      flextable::flextable() %>%
      set_header_labels(outcome="Outcome",
                        trt="",
                        events="Predicted") %>%
      align(align="center", part="header") %>%
      align(j=3,align="center", part="body") %>%
      valign(valign="bottom", part = "header")%>%
      valign( valign="top", part = "body")%>%
      merge_v(j=1:2)%>%
      autofit() %>%
      font(fontname = "Arial", part="all") %>%
      htmltools_value()

  })
#   
#   # output$coefs=renderUI({
#   #   
#   #   
#   #   
#   #   unpack(missingness_pattern(pred()$data))
#   #   
#   #   coef_modsev=submodel.print(submodels.object_modsev)
#   #   coef_sev=submodel.print(submodels.object_sev)
#   #   coef_pneum=submodel.print(submodels.object_pneum)
#   #   
#   #   interpretation=data.frame("var"=c("(Intercept)","age_imp","arm_ipdICS", "arm_ipdICS:eos_bl","eos_bl","exac_bl","fev1_bl","sexM","smoking_blCurrent"),
#   #                             "interpretation"=c("Intercept", "Age", "ICS-Yes", "Eos Interaction", "Eos/10^9", "Per Exac", "FEV1/L", "Sex-Male", "Smoking-Current"))
#   #   
#   #   coef_modsev[["coefficients"]] %>% rename("modsev"="coef") %>% 
#   #     merge(coef_sev[["coefficients"]] %>% rename("sev"="coef") ) %>%
#   #     merge(coef_pneum[["coefficients"]] %>% rename("pneum"="coef") ) %>%
#   #     mutate(pattern=as.character(pattern)) %>%
#   #     filter(pattern==as.character(tmp.pattern[1])) %>%
#   #     merge(interpretation) %>%
#   #     dplyr::select(interpretation, modsev, sev, pneum) %>%
#   #     mutate(modsev=sprintf("%.3f", modsev),
#   #            sev=sprintf("%.3f", sev),
#   #            pneum=sprintf("%.3f", pneum)) %>% 
#   #     flextable::flextable() %>% 
#   #     set_header_labels(interpretation="Variable",
#   #                       modsev="Mod/Sev",
#   #                       sev="Sev",
#   #                       pneum="Pneum") %>%
#   #     align(align="center", part="header") %>%
#   #     align(j=2:4,align="center", part="body") %>%
#   #     valign(valign="bottom", part = "header")%>%
#   #     valign( valign="top", part = "body")%>%
#   #     # merge_v(j=1:2)%>%
#   #     autofit() %>%
#   #     font(fontname = "Arial", part="all") %>% 
#   #     htmltools_value()
#   # })
#   
#   output$coefs2=renderUI({
# 
# 
# 
#     pred()$coefs %>%
#     # result_reactive()$coefs
#       flextable::flextable() %>%
#       set_header_labels(interpretation="Variable",
#                         modsev="Mod/Sev",
#                         sev="Sev",
#                         pneum="Pneum") %>%
#       align(align="center", part="header") %>%
#       align(j=2:4,align="center", part="body") %>%
#       valign(valign="bottom", part = "header")%>%
#       valign( valign="top", part = "body")%>%
#       # merge_v(j=1:2)%>%
#       autofit() %>%
#       font(fontname = "Arial", part="all") %>%
#       htmltools_value()
#   })
#   

  output$waffle1=renderPlot({
    exac_modsev=pred()$modsev
    exac_sev=pred()$sev
    exac_pneum=pred()$pneum

    print(exac_modsev)

    # req(result_reactive())
    # exac_modsev <- result_reactive()$modsev
    # exac_sev <- result_reactive()$sev
    # exac_pneum <- result_reactive()$pneum
    # #
    ms_with_ics=100*exac_modsev$fit[1]
    ms_saved=100*(exac_modsev$fit[2]-exac_modsev$fit[1])
    s_with_ics=100*exac_sev$fit[1]
    s_saved=100*(exac_sev$fit[2]-exac_sev$fit[1])
    p_with_ics=100*exac_pneum$fit[1]
    p_saved=100*(exac_pneum$fit[2]-exac_pneum$fit[1])

    data <- tibble(
      group = factor(c(rep(c("Events avoided with ICS", "Expected events"),3)), levels = c("Events avoided with ICS", "Expected events", "Events avoided by not using ICS"), ordered = T),
      value = c(ms_saved, ms_with_ics, s_saved, s_with_ics, p_saved, p_with_ics),
      cat=factor(rep(c("Mod/sev\nexacacerbations",  "Severe\nexacerbations",  "Pneumonias"), each=2),levels = c("Mod/sev\nexacacerbations",  "Severe\nexacerbations",  "Pneumonias"), ordered = T),
    ) %>%
      mutate(group=case_when(group=="Expected events"~"Expected events",
                             value<0~"Events avoided by not using ICS",
                             T~"Events avoided with ICS"))

    ggplot(data, aes(x=cat,y = value, fill = group)) +
      geom_col(position = "stack") +
      geom_text(aes(label = sprintf("%.0f", abs(value))), position = position_stack(vjust = 0.5)) +
      labs(x = "", y = "Events/100 people", fill = "") +
      theme_minimal()+
      scale_fill_manual(values=c("Events avoided with ICS"="#4DAF4A",
                                 "Expected events"="#007AC0",
                                 "Events avoided by not using ICS"="#E41A1C"))
  })
}

# Run the app
shinyApp(ui, server)
