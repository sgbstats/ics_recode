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


# temp <- tempfile(fileext = ".zip")
# dl <- drive_download(
#   "https://drive.google.com/file/d/134g7LCeV8rs75ibU4lSVYtuRJQOqfs4y/view?usp=sharing", path = paste0(getwd(),"\\Data\\data.zip"), overwrite = TRUE)
# out <- unzip(paste0(getwd(),"\\Data\\data.zip"), exdir=paste0(getwd(),"\\Data"))
# load(out[1])
# load(out[2])
# load(out[3])

# drive_deauth()
# dl=drive_download("https://drive.google.com/file/d/1pWO06CLeEvbSeTt-Pe-fQ7jBD9wXUd4E/view?usp=sharing", path = "Data\\imp_aggregated_results.RDa", overwrite = T)
# dl2=drive_download("https://drive.google.com/file/d/1J7e6skxB7KzMzigEdwDdGI5iOLjKhAh3/view?usp=drive_link", path = "Data\\vars.RDa", overwrite = T)


load("Data/imp_aggregated_results.RDa")
load("Data/vars.RDa")


results_shiny=results_imp
study_names=names(results_imp[1:22])
`%notin%`=Negate(`%in%`)
unpack=function(list)
{
  for(i in names(list))
  {
    assign(i, list[[i]], envir = globalenv())
  }
}

function(input, output, session) {
  
  observe({
    studies=(results_shiny[["aggregated"]][[input$outcome]][[input$interaction]][[input$set]])$studLab
    
    updatePickerInput(session, "studies",
                      choices = studies,
                      selected = studies)
    
    
  })
  observe({
    studies2=unique((results_shiny[["aggregated_marginal"]][[input$outcome2]][[input$interaction2]][["included_main"]])$studLab)
    
    updatePickerInput(session, "studies2",
                      choices = studies2,
                      selected = studies2)
  })
  # 
  # observe({
  #   studies3=unique((results_shiny[["aggregated_marginal"]][[input$outcome3]][[input$interaction3]][["included_main"]])$studLab)
  #   
  #   updatePickerInput(session, "studies3",
  #                     choices = studies3,
  #                     selected = studies3)
  # })
  # observe({
  #   studies4=unique((results_shiny[["aggregated_marginal"]][[input$outcome4]][[input$interaction4]][[input$set4]])$studLab)
  #   
  #   updatePickerInput(session, "studies4",
  #                     choices = studies4,
  #                     selected = studies4)
  # })
  # 
  `%notin%`=Negate(`%in%`)
  unpack=function(list)
  {
    for(i in names(list))
    {
      assign(i, list[[i]], envir = globalenv())
    }
  }
  unpack(vars)
  
  
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
  
  # output$adj=renderPlot({
  #   if(input$data3)
  #   {
  #     results_shiny=results_imp
  #   }else
  #   {
  #     results_shiny=results
  #   }
  #   r=results_shiny[["aggregated"]][[input$outcome3]][[input$interaction3]][["included_main"]] %>% mutate(model="Adjusted") %>% 
  #     rbind.data.frame(results_shiny[["aggregated"]][[input$outcome3]][[input$interaction3]][["unadjusted"]] %>% mutate(model="Unadjusted")) %>% 
  #     mutate(studLab2=paste(studLab, model, sep="-")) %>% 
  #     filter(studLab %in% input$studies3)
  #   
  #   m=metagen(data = r, subgroup = studLab,
  #             TE=TE, seTE=seTE, studlab = studLab2,  sm="RR", method.tau = "REML", random=F, fixed=F)
  #   if(input$interaction3=="main")
  #   {
  #     label.left="Favours ICS"
  #     label.right="Favours No ICS"
  #   }else 
  #   {
  #     label.left=paste0((interactions %>% filter(name==input$interaction3))$interpretation,"\nFavours ICS")
  #     label.right=paste0((interactions %>% filter(name==input$interaction3))$interpretation,"\nFavours No ICS")
  #     
  #     
  #   }
  #   forest(m,
  #          leftcols=c("model", "TE", "seTE" ),
  #          leftlabs=c("Model", "log(RR)", "SE"),
  #          just="c",  width = 9, colgap = "0.4cm",
  #          label.left = label.left, label.right = label.right,
  #          # label.e = "ICS", label.c = "No ICS",
  #          # label.e.attach = "event.e", label.c.attach = "event.c",
  #          col.square = "#FF0000",
  #          col.diamond = "#888888",
  #          subgroup.name = "",
  #          fontsize = 14
  #   ) 
  # })
  # 
  # output$bias=renderPlot({
  #   if(input$data4)
  #   {
  #     results_shiny=results_imp
  #   }else
  #   {
  #     results_shiny=results
  #   }
  #   agg_res=results_shiny[["aggregated"]][[input$outcome4]][[input$interaction4]][[input$set4]] %>% filter(studLab %in% input$studies4)
  #   if(input$interaction=="main")
  #   {
  #     label.left="Favours ICS"
  #     label.right="Favours No ICS"
  #   }else 
  #   {
  #     label.left=paste0((interactions %>% filter(name==input$interaction))$interpretation,"\nFavours ICS")
  #     label.right=paste0((interactions %>% filter(name==input$interaction))$interpretation,"\nFavours No ICS")
  #     
  #     
  #   }
  #   m=metagen(data = agg_res, 
  #             TE=TE, seTE=seTE, studlab = studLab, n.c = n.c, n.e=n.e, sm="RR", method.tau = "REML", comb.random=T, comb.fixed=F)
  #   funnel(m, studlab = T)
  # })
  
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
  
  # observeEvent(input$reset_input3, {
  #   shinyjs::reset("outcome3")
  #   shinyjs::reset("studies3")
  #   shinyjs::reset("interaction3")
  #   shinyjs::reset("data3")
  # })
  # observeEvent(input$reset_input4, {
  #   shinyjs::reset("outcome4")
  #   shinyjs::reset("set4")
  #   shinyjs::reset("studies4")
  #   shinyjs::reset("interaction4")
  #   shinyjs::reset("data4")
  # })
}
