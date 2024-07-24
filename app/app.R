#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(bslib)
library(tidyr)
library(tidyverse)
library(epiR)
library(ggplot2)
library(pwr)

pair_p <- function(myn, b, a, mean, sd) power.t.test(n = myn, delta = mean/sd, power = NULL, sig.level = 1-a/2)$power
pair_n <- function(myn, b, a, mean, sd) power.t.test(n = NULL, delta = mean/sd, power = b, sig.level = 1-a/2)$n

prop_p <- function(myn, b, a, prop1, prop2) power.prop.test(n = myn, p1 = prop1, p2 = prop2, power = NULL, sig.level = 1-a/2)$power
prop_n <- function(myn, b, a, prop1, prop2) power.prop.test(n = NULL, p1 = prop1, p2 = prop2, power = b, sig.level = 1-a/2)$n

cohort_p <- function(myn, b, a, ccr, ep, rr) epi.sscohortc(irexp0 = ep, irexp1 = ep*rr, r = 1/ccr, n = myn*(ccr+1), power = NA, conf.level = a)$power
cohort_n <- function(myn, b, a, ccr, ep, rr) epi.sscohortc(irexp0 = ep, irexp1 = ep*rr, r = 1/ccr, n = NA, power = b, conf.level = a)$n.exp1

case_p <- function(myn, b, a, ccr, ep, rr) epi.sscc(p0 = ep, p1 = ep*rr, OR = rr, r = ccr, n = myn*(ccr+1), power = NA, conf.level = a)$power
case_n <- function(myn, b, a, ccr, ep, rr) epi.sscc(p0 = ep, p1 = ep*rr, OR = rr, r = ccr, n = NA, power = b, conf.level = a)$n.case

adj_table <- function(df){
  adj <- df %>% mutate(max = as.numeric(rownames(df))) %>% pivot_longer(cols = -max, names_to = "variable", values_to = "val")
  return(as.data.frame(adj))
}

choices1 <- c("I want to determine power", "I want to determine sample size")
choices2 <- c("continuous data (ex. height, body temperature).", "discrete data (ex. TRUE/FALSE, exposed/susceptible).")
choices3 <- c("Paired (Related)", "Independent")
choices4 <- c("I have the proportion of a population", "I want to determine cohort-power", "I want to determine case-control power")

# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "Multi-Purpose Power and Sample Size Calculator",
  # SETUP
  sidebar = sidebar(
    radioButtons("type1", "Which of the following describes your objective:", choices1),
    radioButtons("type2", "for...", choices2),
    uiOutput("select3")
  ),
  # DATA ENTRY
  card(uiOutput("requirements")),
  # RESULTS
  navset_card_tab(
    nav_panel("Result",
              uiOutput("computed")
              ),
    nav_panel("Table",
              tableOutput(output = 'mytable')
              ),
    nav_panel("Graph",
              plotOutput(output = 'myplot')
              )
  )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # MAKE SELECTIONS TO SETUP TOOL
  output$select3 <- renderUI({
    c2 <- input$type2
    if (c2 == choices2[1]){
      radioButtons("type3", "My data is:", choices3)
    }
    else if (c2 == choices2[2]){
      radioButtons("type4", "Select one of the following:", choices4)
    }
  })
  
  # ENTER REQUIRED DATA FOR INPUT
  output$requirements <- renderUI({
    c1 <- input$type1
    c2 <- input$type2
    c3 <- input$type3
    c4 <- input$type4

    # Applicable for All
    selectors <- tagList(p("Enter what you know about your data or desired output:"))
    
    # PAIRED DATA
    if (c2 == choices2[1] && c3 == choices3[1]){
      selectors <- tagAppendChild(selectors, tagList(
        fluidRow(
          column(width = 6, numericInput(inputId = "mChange", "Mean change:", value = 4.0, min = 0)),
          column(width = 6, numericInput(inputId = "sdChange", "Std. Deviation of change:", value = 4.3, min = 0))
        )))
      # COMPUTE POWER
      if (c1 == choices1[1]){
        selectors <- tagAppendChild(selectors, tagList(
          fluidRow(
            column(width = 6, sliderInput("alpha", "Confidence level \n (recommended: 0.95):", min = 0, max = 1, value = 0.95)),
            column(width = 6, numericInput("myNo", "Size of Control and Treatment Groups (Equal):", min = 0, value = 50))
          )
        ))
      }
      # COMPUTE SIZE
      else if (c1 == choices1[2]){
        selectors <- tagAppendChild(selectors, tagList(
          fluidRow(
            column(width = 6, sliderInput("alpha", "Confidence level \n (recommended: 0.95):", min = 0, max = 1, value = 0.95)),
            column(width = 6, sliderInput("beta", "Power value \n (recommended: 0.8):", min = 0, max = 1, value = 0.8))
          )
        ))
      }
    }
    # INDEPENDENT DATA
    else if (c2 == choices2[1] && c3 == choices3[2]){
      selectors <- tagAppendChild(selectors, tagList(
        fluidRow(
          column(width = 6, numericInput(inputId = "mChange1", "Mean change seen in group A:", value = 4.0, min = 0)),
          column(width = 6, numericInput(inputId = "sdChange1", "Std. Deviation seen in group A:", value = 4.3, min = 0))
        ),
        fluidRow(
          column(width = 6, numericInput(inputId = "mChange2", "Mean change seen in group B:", value = 4.0, min = 0)),
          column(width = 6, numericInput(inputId = "sdChange2", "Std. Deviation seen in group B:", value = 4.3, min = 0))
        )
        ))
      # COMPUTE POWER
      if (c1 == choices1[1]){
        selectors <- tagAppendChild(selectors, tagList(
          fluidRow(
            column(width = 6, sliderInput("alpha", "Confidence level \n (recommended: 0.95):", min = 0, max = 1, value = 0.95)),
            column(width = 6, numericInput("myNo", "Size of Control and Treatment Groups (Equal):", min = 0, value = 50))
          )
        ))
      }
      # COMPUTE SIZE
      else if (c1 == choices1[2]){
        selectors <- tagAppendChild(selectors, tagList(
          fluidRow(
            column(width = 6, sliderInput("alpha", "Confidence level \n (recommended: 0.95):", min = 0, max = 1, value = 0.95)),
            column(width = 6, sliderInput("beta", "Power value \n (recommended: 0.8):", min = 0, max = 1, value = 0.8))
          )
        ))
      }
    }
    # CTRL/TREATMENT
    else if (c2 == choices2[2] && c4 == choices4[1]){
      selectors <- tagAppendChild(selectors, tagList(
        fluidRow(
          column(width = 6, numericInput(inputId = "rrControl", "Response rate in control group", value = 0.2, min = 0, max = 1)),
          column(width = 6, numericInput(inputId = "rrTreatment", "Response rate in treatment group", value = 0.25, min = 0, max = 1))
        )))
      # COMPUTE POWER
      if (c1 == choices1[1]){
        selectors <- tagAppendChild(selectors, tagList(
          fluidRow(
              column(width = 6, sliderInput("alpha", "Confidence level \n (recommended: 0.95):", min = 0, max = 1, value = 0.95)),
              column(width = 6, numericInput("myNo", "Size of Control and Treatment Groups (Equal):", min = 0, value = 50))
          )
          ))
      }
      # COMPUTE SIZE
      else if (c1 == choices1[2]){
        selectors <- tagAppendChild(selectors, tagList(
          fluidRow(
            column(width = 6, sliderInput("alpha", "Confidence level \n (recommended: 0.95):", min = 0, max = 1, value = 0.95)),
            column(width = 6, sliderInput("beta", "Power value \n (recommended: 0.8):", min = 0, max = 1, value = 0.8))
          )
          ))
      }
    }
    # COHORT POWER
    else if (c2 == choices2[2] && c4 == choices4[2]){
      selectors <- tagAppendChild(selectors, tagList(
        fluidRow(
          column(width = 6, numericInput("ueRatio", "Unexposed:Exposed Ratio:", min = 0, value = 2)),
          column(width = 6, numericInput("riskUe", "Risk of Exposure (in Unexposed Population):", min = 0, max = 1, value = 0.001))
        ),
        fluidRow(
          column(width = 6, numericInput("RR", "Relative Risk Effect (RR) Value:", min = 0, value = 1.5)),
          column(width = 6, sliderInput("alpha", "Confidence level \n (recommended: 0.95):", min = 0, max = 1, value = 0.95))
        )
      ))
      # COMPUTE POWER
      if (c1 == choices1[1]){
        selectors <- tagAppendChild(selectors, tagList(
          fluidRow(
            column(width = 12, numericInput("myNo", "Size of Exposed Population:", min = 0, value = 1000)),
          )
        ))
      }
      # COMPUTE SAMPLE SIZE
      else if(c1 == choices1[2]){
        selectors <- tagAppendChild(selectors, tagList(
          fluidRow(
            column(width = 12, sliderInput("beta", "Power value \n (recommended: 0.8):", min = 0, max = 1, value = 0.8))
        )))      
      }
    }
    # CASE-CONTROL POWER
    else if (c2 == choices2[2] && c4 == choices4[3]){
      selectors <- tagAppendChild(selectors, tagList(
        fluidRow(
          column(width = 6, numericInput("ccRatio", "Control:Case Ratio:", min = 0, value = 4)),
          column(width = 6, numericInput("riskUe", "Prevalence of Exposure (in Control Population):", min = 0, max = 1, value = 0.25))
        ),
        fluidRow(
          column(width = 6, numericInput("OR", "Odds Ratio (OR) Value:", min = 0, value = 1.5)),
          column(width = 6, sliderInput("alpha", "Confidence level \n (recommended: 0.95):", min = 0, max = 1, value = 0.95))
        )
      ))
      # COMPUTE POWER
      if (c1 == choices1[1]){
        selectors <- tagAppendChild(selectors, tagList(
          fluidRow(
            column(width = 12, numericInput("myNo", "# of Cases:", min = 0, value = 100)),
          )
        ))
      }
      # COMPUTE SAMPLE SIZE
      else if(c1 == choices1[2]){
        selectors <- tagAppendChild(selectors, tagList(
          fluidRow(
            column(width = 12, sliderInput("beta", "Power value \n (recommended: 0.8):", min = 0, max = 1, value = 0.8))
          )
        ))      
      }
    }
    
    selectors
  })
  
  # TABLE OUTPUT
  outtable <- reactive({
    c1 <- input$type1
    c2 <- input$type2
    c3 <- input$type3
    c4 <- input$type4
    
    # PAIRED SAMPLES
    if (c2 == choices2[1] && c3 == choices3[1]){
      m <- input$mChange
      sd <- input$sdChange
      a <- input$alpha
      # TABLE FOR POWER
      if (c1 == choices1[1]){
        myn <- input$myNo
        
        cases <- c(seq(0, myn/2, myn/4), seq(myn, 4*myn, myn/2))
        trts <- c(m/2, seq(m, 2*m, m/2))
        ccdf <- data.frame(matrix(ncol = length(trts), nrow = length(cases)))
        colnames(ccdf) <- trts
        rownames(ccdf) <- cases
        
        c_index <- 1
        for (i in trts){
          r_index <- 1
          for (j in cases){
            ccdf[r_index, c_index] <- pair_p(j, NA, a, i, sd)
            r_index <- r_index + 1
          }
          c_index <- c_index + 1
        }
        
        return(ccdf)
      }
      # FOR SAMPLE SIZE
      if (c1 == choices1[2]){
        b <- input$beta
        
        betas <- seq(0.5, 0.95, 0.05)
        trts <- c(m/2, seq(m, 2*m, m/2))
        ccdf <- data.frame(matrix(ncol = length(trts), nrow = length(betas)))
        colnames(ccdf) <- trts
        rownames(ccdf) <- betas

        c_index <- 1
        for (i in trts){
          r_index <- 1
          for (j in betas){
            #print(c(j, a, ctrl, i))
            #print(prop_n(NULL, j, a, ctrl, i))
            ccdf[r_index, c_index] <- pair_n(NA, j, a, i, sd)
            r_index <- r_index + 1
          }
          c_index <- c_index + 1
        }
        
        return(ccdf)
      }
    }
    # INDEPENDENT SAMPLES
    else if (c2 == choices2[1] && c3 == choices3[2]){
      m1 <- input$mChange1
      sd1 <- input$sdChange1
      m2 <- input$mChange2
      sd2 <- input$sdChange2
      a <- input$alpha
      # TABLE FOR POWER
      if (c1 == choices1[1]){
        
      }
      # FOR SAMPLE SIZE
      if (c1 == choices1[2]){
        
      }
    }
    # PROPORTION INFO
    else if (c2 == choices2[2] && c4 == choices4[1]){
      ctrl <- input$rrControl
      trt <- input$rrTreatment
      a <- input$alpha
      # TABLE FOR POWER
      if (c1 == choices1[1]){
        myn <- input$myNo
        
        cases <- c(seq(0, myn/2, myn/4), seq(myn, 4*myn, myn/2))
        trts <- c(trt/2, seq(trt, 2*trt, trt/2))
        ccdf <- data.frame(matrix(ncol = length(trts), nrow = length(cases)))
        colnames(ccdf) <- trts
        rownames(ccdf) <- cases
        
        c_index <- 1
        for (i in trts){
          r_index <- 1
          for (j in cases){
            ccdf[r_index, c_index] <- prop_p(j, NA, a, ctrl, i)
            r_index <- r_index + 1
          }
          c_index <- c_index + 1
        }
        
        return(ccdf)
      }
      # TABLE FOR SAMPLE SIZE
      else if (c1 == choices1[2]){
        p <- input$beta

        betas <- seq(0.5, 0.95, 0.05)
        trts <- c(trt/2, seq(trt, 2*trt, trt/2))
        ccdf <- data.frame(matrix(ncol = length(trts), nrow = length(betas)))
        colnames(ccdf) <- trts
        rownames(ccdf) <- betas
        
        c_index <- 1
        for (i in trts){
          r_index <- 1
          for (j in betas){
            #print(c(j, a, ctrl, i))
            #print(prop_n(NULL, j, a, ctrl, i))
            ccdf[r_index, c_index] <- prop_n(NA, j, a, ctrl, i)
            r_index <- r_index + 1
          }
          c_index <- c_index + 1
        }
        
        return(ccdf)
      }
    }
    # COHORT
    else if (c2 == choices2[2] && c4 == choices4[2]){
      a <- input$alpha
      uer <- input$ueRatio
      riu <- input$riskUe
      myrr <- input$RR
      
      # FOR POWER
      if (c1 == choices1[1]){
        myn <- input$myNo
          
        cases <- c(seq(0, myn/2, myn/4), seq(myn, 4*myn, myn/2))
        rrs <- c(myrr/2, seq(myrr, 2*myrr, myrr/2))
        ccdf <- data.frame(matrix(ncol = length(rrs), nrow = length(cases)))
        colnames(ccdf) <- rrs
        rownames(ccdf) <- cases
          
        c_index <- 1
        for (i in rrs){
          r_index <- 1
          for (j in cases){
            ccdf[r_index, c_index] <- cohort_p(j, NA, a, uer, riu, i)
            r_index <- r_index + 1
          }
          c_index <- c_index + 1
        }
        return(ccdf)
      }
      # FOR SAMPLE SIZE
      else if (c1 == choices1[2]){
        p <- input$beta
        
        betas <- seq(0.5, 0.95, 0.05)
        rrs <- c(myrr/2, seq(myrr, 2*myrr, myrr/2))
        ccdf <- data.frame(matrix(ncol = length(rrs), nrow = length(betas)))
        colnames(ccdf) <- rrs
        rownames(ccdf) <- betas
        
        c_index <- 1
        for (i in rrs){
          r_index <- 1
          for (j in betas){
            ccdf[r_index, c_index] <- cohort_n(NA, j, a, uer, riu, i)
            r_index <- r_index + 1
          }
          c_index <- c_index + 1
        }
        
        return(ccdf)
      }
    }
    # CASE-CONTROL
    else if (c2 == choices2[2] && c4 == choices4[3]){
      a <- input$alpha
      uer <- input$ccRatio
      riu <- input$riskUe
      myrr <- input$OR
      
      if (c1 == choices1[1]){
        myn <- input$myNo
        
        cases <- c(seq(0, myn/2, myn/4), seq(myn, 4*myn, myn/2))
        rrs <- c(myrr/2, seq(myrr, 2*myrr, myrr/2))
        ccdf <- data.frame(matrix(ncol = length(rrs), nrow = length(cases)))
        colnames(ccdf) <- rrs
        rownames(ccdf) <- cases
        
        c_index <- 1
        for (i in rrs){
          r_index <- 1
          for (j in cases){
            ccdf[r_index, c_index] <- case_p(j, NA, a, uer, riu, i)
            r_index <- r_index + 1
          }
          c_index <- c_index + 1
        }
        return(ccdf)
      }
      else if (c1 == choices1[2]){
        p <- input$beta
        
        betas <- seq(0.5, 0.95, 0.05)
        rrs <- c(myrr/2, seq(myrr, 2*myrr, myrr/2))
        ccdf <- data.frame(matrix(ncol = length(rrs), nrow = length(betas)))
        colnames(ccdf) <- rrs
        rownames(ccdf) <- betas
        
        c_index <- 1
        for (i in rrs){
          r_index <- 1
          for (j in betas){
            ccdf[r_index, c_index] <- case_n(NA, j, a, uer, riu, i)
            r_index <- r_index + 1
          }
          c_index <- c_index + 1
        }
        
        return(ccdf)
      }
    }
    else{ccdf <- matrix(nrow = NULL, ncol = NULL)}
    ccdf
  })
  
  # GET TABLE
  output$mytable <- renderTable({
    outtable()
  }, rownames = TRUE)
  
  # PLOT OUTPUT
  output$myplot <- renderPlot({
    c1 <- input$type1
    c2 <- input$type2
    c3 <- input$type3
    c4 <- input$type4
    
    if (c1 == choices1[1]){
      plotdata <- adj_table(outtable())
      #print(plotdata)
      outplot <- ggplot(plotdata, aes(y = val, x = max, group = variable, colour = variable)) + geom_vline(xintercept = input$myNo, colour = "black")
      outplot <- outplot + geom_line() + geom_point() + theme_minimal()
      return(outplot)
    }
    
    else if (c1 == choices1[2]){
      plotdata <- adj_table(outtable())
      #print(plotdata)
      outplot <- ggplot(plotdata, aes(x = val, y = max, group = variable, colour = variable)) + geom_hline(yintercept = input$beta, colour = "black")
      outplot <- outplot + geom_line() + geom_point() + theme_minimal()
      return(outplot)
    }
    
  })
  
  # TEXT OUTPUT
  output$computed <- renderUI({
    c1 <- input$type1
    c2 <- input$type2
    c3 <- input$type3
    c4 <- input$type4
    
    # PAIRED DATA
    if (c2 == choices2[1] && c3 == choices3[1]){
      m <- input$mChange
      sd <- input$sdChange
      a <- input$alpha
      # TABLE FOR POWER
      if (c1 == choices1[1]){
        myn <- input$myNo
        pval <- pair_p(myn, NA, a, m, sd)
        return(p(paste("The power of this test is", pval)))
      }
      # FOR SAMPLE SIZE
      if (c1 == choices1[2]){
        b <- input$beta
        ncase <- pair_n(NA, b, a, m, sd)
        return(p(paste("At least",ceiling(ncase),"cases are required to satisfy the chosen inputs.")))
      }
    }
    # INDEPENDENT DATA
    else if (c2 == choices2[1] && c3 == choices3[2]){
      m1 <- as.numeric(input$mChange1)
      sd1 <- as.numeric(input$sdChange1)
      m2 <- as.numeric(input$mChange2)
      sd2 <- as.numeric(input$sdChange2)
      
      sd <- round(mean(c(sd1, sd2)), 1)
      m <- round(mean(c(m1, m2)), 1)
      
      a <- input$alpha
      
      # TABLE FOR POWER
      if (c1 == choices1[1]){
        myn <- input$myNo
        pval <- pair_p(myn, NA, a, m, sd)
        return(p(paste("The power of this test is", pval)))
      }
      # FOR SAMPLE SIZE
      if (c1 == choices1[2]){
        b <- input$beta
        ncase <- pair_n(NA, b, a, m, sd)
        return(p(paste("At least",ceiling(ncase),"cases are required to satisfy the chosen inputs.")))
      }
    }
    # PROPORTION DATA
    else if (c2 == choices2[2] && c4 == choices4[1]){
      a <- input$alpha
      ctrl <- input$rrControl
      trt <- input$rrTreatment
      # FOR POWER
      if (c1 == choices1[1]){
        myn <- input$myNo
        pval <- prop_p(myn, NA, a, ctrl, trt)
        return(p(paste("The power of this test is", pval)))
      }
      # FOR SS
      else if (c1 == choices1[2]){
        p <- input$beta
        ncase <- prop_n(NA, p, a, ctrl, trt)
        return(p(paste("At least",ceiling(ncase),"subjects in the control and treatment groups are required to satisfy the chosen inputs.")))
      }
    }
    # COHORT DATA
    else if (c2 == choices2[2] && c4 == choices4[2]){
      a <- input$alpha
      uer <- input$ueRatio
      riu <- input$riskUe
      myrr <- input$RR
      if (c1 == choices1[1]){
        myn <- input$myNo
        pval <- cohort_p(myn, NA, a, uer, riu, myrr)
        return(p(paste("The power of this test is", pval)))
      }
      else if (c1 == choices1[2]){
        p <- input$beta
        ncase <- cohort_n(NA, p, a, uer, riu, myrr)
        return(p(paste("At least",ceiling(ncase),"exposed subjects are required to satisfy the chosen inputs.")))
      }
    }
    # CASE-CONTROL DATA
    else if (c2 == choices2[2] && c4 == choices4[3]){
      a <- input$alpha
      uer <- input$ccRatio
      riu <- input$riskUe
      myrr <- input$OR
      # FOR POWER
      if (c1 == choices1[1]){
        myn <- input$myNo
        pval <- case_p(myn, NA, a, uer, riu, myrr)
        return(p(paste("The power of this test is", pval)))
      }
      # FOR SAMPLE
      else if (c1 == choices1[2]){
        p <- input$beta
        ncase <- case_n(NA, p, a, uer, riu, myrr)
        return(p(paste("At least",ceiling(ncase),"cases are required to satisfy the chosen inputs.")))
      }
    }
  })
}

# Run the application 

shinyApp(ui = ui, server = server)
