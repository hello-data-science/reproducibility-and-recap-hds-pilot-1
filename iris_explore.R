# explore iris dataset!

# plot petal length by petal width, colour species
library(ggplot2)
library(plotly)

# Load the iris dataset
data(iris)

# Create the plot
ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species)) +
  geom_point() +
  labs(title = "Petal Length vs Petal Width by Species",
       x = "Petal Length",
       y = "Petal Width") +
  theme_minimal()

# logistic regression model to discriminate between virginica species and non-virginica
iris$IsVirginica <- ifelse(iris$Species == "virginica", 1, 0)
logreg_model <- glm(IsVirginica ~ Petal.Length + Petal.Width, data = iris, family = binomial)

# Calculate predicted probabilities and confidence intervals
iris$PredictedProb <- predict(logreg_model, type = "response")
conf_int <- predict(logreg_model, type = "link", se.fit = TRUE)
iris$LowerCI <- logreg_model$family$linkinv(conf_int$fit - 1.96 * conf_int$se.fit)
iris$UpperCI <- logreg_model$family$linkinv(conf_int$fit + 1.96 * conf_int$se.fit)

# Calculate the boundaries for 5% and 95% predicted probabilities
boundary_5 <- (log(0.05 / 0.95) - coef(logreg_model)[1]) / coef(logreg_model)[3]
boundary_95 <- (log(0.95 / 0.05) - coef(logreg_model)[1]) / coef(logreg_model)[3]

# Re-do earlier plot, now with linear discrimination line from logreg model and 5%/95% probability boundaries
p <- ggplot(iris, aes(x = Petal.Length, y = Petal.Width, color = Species, text = paste("Predicted Probability: ", round(PredictedProb, 2)))) +
  geom_point() +
  labs(title = "Petal Length vs Petal Width by Species",
       x = "Petal Length",
       y = "Petal Width") +
  theme_minimal() +
  geom_abline(intercept = -coef(logreg_model)[1] / coef(logreg_model)[3], 
              slope = -coef(logreg_model)[2] / coef(logreg_model)[3], 
              color = "black", linetype = "dashed") +
  geom_abline(intercept = boundary_5, 
              slope = -coef(logreg_model)[2] / coef(logreg_model)[3], 
              color = "black", linetype = "dotted") +
  geom_abline(intercept = boundary_95, 
              slope = -coef(logreg_model)[2] / coef(logreg_model)[3], 
              color = "black", linetype = "dotted")

# Convert to plotly
ggplotly(p, tooltip = "text")
