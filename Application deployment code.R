library(shiny)
library(DT)
library(shinydashboard)
library(ggplot2)
library(caret)
library(dplyr)
library(mlr3)
library(mlr3verse)
library(mlr3learners)
library(tidyverse)
library(kernelshap) 
library(shapviz)

ui <- dashboardPage(
  dashboardHeader(title = "Cardiometabolic diseases prediction based on eGFR trajectory", titleWidth = 600),
  dashboardSidebar(
    width = 150, 
    sidebarMenu(
      menuItem("Batch prediction", tabName = "Batch_prediction"), 
      menuItem("Case prediction", tabName = "Case_prediction")
    )
  ),
  dashboardBody(
    tabItems(
      tabItem(tabName = "Batch_prediction",
              fluidRow(
                box(
                  title = "Select trajectory type", 
                  width = 12, 
                  height = "85px", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  radioButtons(
                    inputId = "trajectory_type1",
                    label = NULL,
                    choices = c("Low decline", "Unstable"),
                    inline = TRUE
                  ),
                  tags$div(style = "font-size: 20px;", textOutput("trajectory_type1"))
                )
              ),
              box(
                title = "Batch prediction",
                width = 13,
                status = "primary",
                collapsible = TRUE,
                solidHeader = TRUE,
                conditionalPanel(
                  condition = "input.trajectory_type1 == 'Low decline'",
                  downloadButton("downloadTemplate1", 
                                 "Download data template (Low decline)")
                ),
                conditionalPanel(
                  condition = "input.trajectory_type1 == 'Unstable'",
                  downloadButton("downloadTemplate2", 
                                 "Download data template (Unstable)")
                ),
                fileInput("batch_data", "Please upload a .csv file.", accept = ".csv"),
                fluidRow(
                  box(
                    title = "Prediction results",
                    width = 12, 
                    height = "580px",
                    status = "primary",
                    collapsible = FALSE,
                    solidHeader = T,
                    DTOutput("batch_predict_results"),
                    downloadButton("download_results", "Download results")
                  )
                )
                
              )
      ),
      tabItem(tabName = "Case_prediction",
              fluidRow(
                box(
                  title = "Select trajectory type", 
                  width = 12, 
                  height = "85px", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  radioButtons(
                    inputId = "trajectory_type2",
                    label = NULL,
                    choices = c("Low decline", "Unstable"),
                    inline = TRUE
                  ),
                  tags$div(style = "font-size: 20px;", textOutput("trajectory_type2"))
                )
              ),
              fluidRow(
                box(
                  title = "Input variables", 
                  width = 12,
                  height = "175px",
                  status = "primary",
                  collapsible = TRUE,
                  solidHeader = TRUE,
                  
                  ## -------- Low decline --------
                  conditionalPanel(
                    condition = "input.trajectory_type2 == 'Low decline'",
                    
                    fluidRow(
                      column(2, numericInput("LH_Age", "Age (years)", 50)),
                      column(3, numericInput("LH_Waist_circumference", "Waist circumference (cm)", 80)),
                      column(2, numericInput("LH_FBG", "FBG (mmol/L)", 5.1)),
                      column(2, numericInput("LH_UA", "UA (¦Ìmol/L)", 350)),
                      column(2, numericInput("LH_WBC", "WBC (¡Á10^9/L)", 6.5))
                    )
                  ),
                  
                  ## -------- Unstable --------
                  conditionalPanel(
                    condition = "input.trajectory_type2 == 'Unstable'",
                    
                    fluidRow(
                      column(2, numericInput("UH_Age", "Age (years)", 50)),
                      column(2, numericInput("UH_HDL_C", "HDL-C (mmol/L)", 1.2)),
                      column(2, numericInput("UH_LDL_C", "LDL-C (mmol/L)", 2.3)),
                      column(2, numericInput("UH_FBG", "FBG (mmol/L)", 5.1)),
                      column(2, numericInput("UH_WBC", "WBC (¡Á10^9/L)", 6.5))
                    )
                  ),
                  fluidRow(
                    column(width = 3, 
                           actionButton("Predict", label = "Predict", icon = icon("play")))
                  )
                )
              ),
              fluidRow(
                box(
                  title = "Prediction result", 
                  width = 12, 
                  height = "100px", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  tags$div(style = "font-size: 20px;", textOutput("prediction_result"))
                )
              ),
              fluidRow(
                box(
                  title = "SHAP force plot", 
                  width = 12, 
                  height = "370px", 
                  status = "primary", 
                  solidHeader = TRUE, 
                  div(
                    style = "text-align: center;",
                    plotOutput("shap_force_plot")
                  )
                )
              )
      )
    )
  )
)

server <- function(input, output) {
  
  load("data.RData")
  
  data_dict <- data.frame(rbind(
    c("Age", "Age"), 
    c("Waist_circumference", "Waist circumference"), 
    c("FBG", "FBG"), 
    c("UA", "UA"), 
    c("WBC", "WBC"), 
    c("HDL_C", "HDL-C"), 
    c("LDL_C", "LDL-C")
  ))
  names(data_dict) <- c("short", "long")
  
  
  data_template1 <- data.frame(
    ID = c("001", "002"),
    Age = c(50, 60),
    Waist_circumference = c(80, 90),
    FBG = c(5.1, 5.2),
    UA = c(350, 360),
    WBC = c(6.5, 6.6)
  )
  
  data_template2 <- data.frame(
    ID = c("001", "002"),
    Age = c(50, 60),
    HDL_C = c(1.2, 1.3),
    LDL_C = c(2.3, 2.4),
    FBG = c(5.1, 5.2),
    WBC = c(6.5, 6.6)
  )  
  
  output$downloadTemplate1 <- downloadHandler(
    filename = function() {
      paste("data template", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_template1, file, row.names = FALSE)
    }
  )
  
  output$downloadTemplate2 <- downloadHandler(
    filename = function() {
      paste("data template", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data_template2, file, row.names = FALSE)
    }
  )
  
  data_LH_train5 <- data_LH_train[, c("Age", "Waist_circumference", "FBG", "UA", "WBC")]
  standardized_para_LH5 <- preProcess(data_LH_train5, method = c("center", "scale"))
  data_LH_train6 <- data_LH_train_std_num[, c("Age", "Waist_circumference", "FBG", "UA", "WBC", "CMDs")]
  
  data_UH_train5 <- data_UH_train[, c("Age", "HDL_C", "LDL_C", "FBG", "WBC")]
  standardized_para_UH5 <- preProcess(data_UH_train5, method = c("center", "scale"))
  data_UH_train6 <- data_UH_train_std_num[, c("Age", "HDL_C", "LDL_C", "FBG", "WBC", "CMDs")]
  
  
  batch_result <- reactive({
    req(input$batch_data)
    
    # Read uploaded files
    ext <- tools::file_ext(input$batch_data$name)
    input_file <- switch(ext,
                         csv = vroom::vroom(input$batch_data$datapath, delim = ","),
                         validate("Invalid file; Please upload a .csv file."))
    
    # Extract ID and delete the first column
    ID <- input_file$ID
    input_file <- input_file[, -1]
    
    # Convert all variables to numeric values
    input_file[] <- lapply(input_file, function(x) as.numeric(as.character(x)))
    
    # Select the model based on the trajectory type chosen by the user (Low decline or Unstable)
    if (input$trajectory_type1 == "Low decline") {
      # Use the LH model
      input_file_std <- predict(standardized_para_LH5, newdata = input_file)
      input_file_pred <- LH_round_outputs[[7]][["xgb_learner"]]$predict_newdata(input_file_std)
      
      # Set the Low decline threshold
      prob_threshold <- 0.087
    } else if (input$trajectory_type1 == "Unstable") {
      
      # Use the UH model
      input_file_std <- predict(standardized_para_UH5, newdata = input_file)
      input_file_pred <- UH_round_outputs[[7]][["xgb_learner"]]$predict_newdata(input_file_std)
      
      # Set the Unstable threshold
      prob_threshold <- 0.082
    }
    
    # Calculate the probability of prediction
    input_file_prob <- data.frame(
      ID = ID,
      No_probability = round(input_file_pred$prob[, "No"] * 100, 2),
      Yes_probability = round(input_file_pred$prob[, "Yes"] * 100, 2),
      Prob_Yes_num = input_file_pred$prob[, "Yes"]
    )
    
    # Set different thresholds for response classification based on the selected trajectory type
    input_file_prob$CMDs_prediction_class <- ifelse(input_file_prob$Prob_Yes_num >= prob_threshold, "Yes", "No")
    input_file_prob <- input_file_prob[, -4]
    data_dict2 <- data.frame(rbind(
      c("ID", "ID"), 
      c("No_probability", "No probability (%)"), 
      c("Yes_probability", "Yes probability (%)"), 
      c("CMDs_prediction_class", "CMDs prediction class")
    ))
    names(data_dict2) <- c("short", "long")
    colnames(input_file_prob) <- data_dict2$long[match(names(input_file_prob), data_dict2$short)]
    
    input_file_prob
  })
  
  output$batch_predict_results <- renderDT({
    datatable(
      batch_result(),
      rownames = FALSE
    )
  })
  
  output$download_results <- downloadHandler(
    filename = function() {
      paste("Batch prediction results", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(batch_result(), file, row.names = FALSE)
    }
  )
  
  
  results <- eventReactive(input$Predict, {
    # Select the model based on the trajectory type chosen by the user (Low decline or Unstable)
    if (input$trajectory_type2 == "Low decline") {
      input_data <- data.frame(
        Age = input$LH_Age,
        Waist_circumference = input$LH_Waist_circumference, 
        FBG = input$LH_FBG,
        UA = input$LH_UA,
        WBC = input$LH_WBC
      )
      
      # Use the LH model
      input_data_std <- predict(standardized_para_LH5, newdata = input_data)
      input_data_pred <- LH_round_outputs[[7]][["xgb_learner"]]$predict_newdata(input_data_std)
      
      prob <- input_data_pred$prob[, "Yes"]
      
      train_task <- as_task_classif(data_LH_train6, target = "CMDs")
      train_x <- train_task$data(cols = train_task$feature_names)
      
      set.seed(123)
      train_x_200 <- train_x %>% 
        slice_sample(n = 200)
      
      input_SHAP_value <- kernelshap(LH_round_outputs[[7]][["xgb_learner"]], 
                                     X = input_data_std, 
                                     bg_X = train_x_200, predict_type = "prob", verbose = F)
      
    } else if (input$trajectory_type2 == "Unstable") {
      input_data <- data.frame(
        Age = input$UH_Age,
        HDL_C = input$UH_HDL_C,
        LDL_C = input$UH_LDL_C,
        FBG = input$UH_FBG,
        WBC = input$UH_WBC
      )
      
      # Use the UH model
      input_data_std <- predict(standardized_para_UH5, newdata = input_data)
      input_data_pred <- UH_round_outputs[[7]][["xgb_learner"]]$predict_newdata(input_data_std)
      prob <- input_data_pred$prob[, "Yes"]
      
      train_task <- as_task_classif(data_UH_train6, target = "CMDs")
      train_x <- train_task$data(cols = train_task$feature_names)
      
      set.seed(123)
      train_x_200 <- train_x %>% 
        slice_sample(n = 200)
      
      input_SHAP_value <- kernelshap(UH_round_outputs[[7]][["xgb_learner"]], 
                                     X = input_data_std, 
                                     bg_X = train_x_200, predict_type = "prob", verbose = F)
    }
    
    input_sv_xgb <- shapviz(input_SHAP_value, which_class = 2)
    
    # Data dictionary maps variable names
    input_shap_values_df <- as.data.frame(input_sv_xgb$S)
    colnames(input_shap_values_df) <- data_dict$long[match(names(input_shap_values_df), data_dict$short)]
    input_sv_xgb$S <- as.matrix(input_shap_values_df)
    colnames(input_sv_xgb$X) <- data_dict$long[match(names(input_sv_xgb$X), data_dict$short)]
    
    input_force_plot <- sv_force(input_sv_xgb, row_id = 1, max_display = Inf, size = 8,
                                 fill_colors = c("#48EDFE", "#6601F7")) +
      labs(x = "SHAP value") +
      theme(panel.grid = element_blank())
    
    force_plot <- ggsave(plot = input_force_plot, 
                         filename = "input_force_plot.png", 
                         width = 20, 
                         height = 6,
                         units = "cm", 
                         dpi = 600)
    
    return(list(prob = prob, force_plot = force_plot))
  })
  
  output$prediction_result <- renderText({
    paste0(round(results()$prob * 100, 2), "%")
  })
  
  output$shap_force_plot <- renderImage({
    list(
      src = results()$force_plot,
      height = "77.5%"
    )
  }, deleteFile = T)
}

shinyApp(ui, server)