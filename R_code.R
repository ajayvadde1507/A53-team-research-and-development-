############################################################
# MEMBER 4 (Siddu): Model Validation & Interpretation
############################################################

# Null model
null_model <- multinom(Class ~ 1, data = mod_df, trace = FALSE)

# Log-likelihoods
ll_full <- logLik(multinom_mod)
ll_null <- logLik(null_model)

# McFadden Pseudo R-squared
pseudo_r2 <- 1 - (ll_full / ll_null)
cat("\nMcFadden Pseudo R-squared:", round(pseudo_r2, 4), "\n")

# Prediction & Confusion Matrix
predicted_class <- predict(multinom_mod, mod_df)
confusion_matrix <- table(
  Predicted = predicted_class,
  Actual = mod_df$Class
)

print(confusion_matrix)

# Save for appendix
write.csv(confusion_matrix, "confusion_matrix_multinomial.csv")
cat("Confusion matrix saved as 'confusion_matrix_multinomial.csv'\n")