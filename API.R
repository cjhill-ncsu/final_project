
library(tidymodels)
library(ggplot2)


data <- readRDS("processed_data.rds")
final_rf_model <- readRDS("final_rf_model.rds")


#* Predict diabetes status
#* @param BMI Numeric. Body Mass Index. Default: 28
#* @param Sex Factor. Default: "Female"
#* @param Age Factor. Default: "70-74"
#* @param Income Factor. Default: "$10,000 to $15,000"
#* @param HeartDiseaseorAttack Factor. Default: "No" 
#* @post /predict
function(BMI = 28, 
         Sex = "Female", 
         Age = "70-74", 
         Income = "$10,000 to $15,000", 
         HeartDiseaseorAttack = "No") {
  
  input_data <- data.frame(
    BMI = as.numeric(BMI),
    Sex = factor(Sex, 
                 levels = levels(data$Sex)),
    Age = factor(Age, 
                 levels = levels(data$Age), 
                 ordered = is.ordered(data$Age)),  # Ensure ordered factor
    Income = factor(Income, 
                    levels = levels(data$Income),
                    ordered = is.ordered(data$Income)),
    HeartDiseaseorAttack = factor(HeartDiseaseorAttack, 
                                  levels = levels(data$HeartDiseaseorAttack))
  )
  
  predict(final_rf_model, new_data = input_data, type = "prob")
}


#* Plot the confusion matrix
#* @serializer png
#* @get /confusion
function() {
  # Generate predictions
  predictions <- predict(final_rf_model, new_data = data, type = "class")
  
  # Ensure predictions are added as a column
  cm_data <- data.frame(
    truth = factor(data$Diabetes_binary, levels = levels(data$Diabetes_binary)),
    prediction = factor(predictions$.pred_class, levels = levels(data$Diabetes_binary))
  )
  
  # Compute the confusion matrix
  cm <- conf_mat(data = cm_data, truth = truth, estimate = prediction)
  
  print(cm)
  
  # Use autoplot to create a simple visualization
  autoplot(cm, type = "heatmap")
}


#* Information about the API
#* @get /info
function() {
  list(
    name = "Chris Hill",
    github_pages_url = "https://cjhill-ncsu.github.io/final_project/"
  )
}

# Start the Plumber API
# plumb("API.R")$run(port = 8000)

