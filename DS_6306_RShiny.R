

# loading the required packages
library(shiny)
library(shinyFiles)
library(aws.s3)

# Set AWS credentials
Sys.setenv("AWS_ACCESS_KEY_ID" = "AKIASXWFQWBV2AEG7EEN",
           "AWS_SECRET_ACCESS_KEY" = "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",
           "AWS_DEFAULT_REGION" = "us-east-2")

# Define UI
ui <- fluidPage(
  
  titlePanel("DS 6306 FINAL PROJECT"),
  
  sidebarLayout(
    sidebarPanel(
      #shinyFilesButton("file", "Select CSV File", "Upload", multiple = FALSE),
      checkboxGroupInput("variables", "Select Variables for the Prediction Regression Model:",
                         choices = c("JobLevel", "JobRole", "TotalWorkingYears"),
                         selected = c("JobLevel", "JobRole", "TotalWorkingYears")),
      actionButton("runModel", "Run Model")
    ),
    
    mainPanel(
      textOutput("rmseOutput")
    )
  )
)


# Define server logic to plot various variables against mpg ----
server <- function(input, output, session) {
  
  observeEvent(input$runModel, {
    # Load data from S3
    bucket <- "msdsds6306"
    file_path <- "CaseStudy2-data.csv" # Specify the path to your CSV file in the S3 bucket
    
    data <- aws.s3::get_object(object = file_path, bucket = bucket)
    data <- read.csv(text = rawToChar(data))
    
    #setting the seed to have a constant resullt
    set.seed(123)
    
    # Subset data
    trainIndices <- sample(seq_len(nrow(data)), round(0.7 * nrow(data)))
    train <- data[trainIndices, ]
    test <- data[-trainIndices, ]
    
    # Fit the linear regression model
    model <- lm(MonthlyIncome ~ ., data = train[, c("MonthlyIncome", input$variables)])
    
    # Make predictions
    predictions <- predict(model, newdata = test)
    
    # Calculate residuals
    residuals <- predictions - test$MonthlyIncome
    
    # Compute RMSE
    rmse <- sqrt(mean(residuals^2))
    
    # Output RMSE
    output$rmseOutput <- renderText({
      paste("Root Mean Squared Error (RMSE):", round(rmse, 2))
    })
  })
}

# Run the application
shinyApp(ui, server)

