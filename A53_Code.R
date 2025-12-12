########################################################################
# Analysis.R
# xAPI-Edu-Data analysis for 7COM1079 final report
# Research Question:
#   Is there a relationship between students' online activity
#   (raisedhands, VisITedResources, AnnouncementsView, Discussion)
#   and their final performance class (High / Medium / Low)?
#
# This script:
#  - loads and inspects the data
#  - produces plots (from R output, not screenshots)
#  - runs statistical tests (ANOVA or Kruskal-Wallis as appropriate)
#  - runs a multinomial logistic regression
#  - saves plot images and prints results for the report appendices
########################################################################

# ---- 0. Setup ----
packages <- c("tidyverse", "ggplot2", "car", "nnet", "broom", "rstatix", "ggpubr")
installed <- rownames(installed.packages())
for(p in packages){
  if(!(p %in% installed)) install.packages(p, repos = "https://cloud.r-project.org")
}

library(tidyverse)
library(ggplot2)
library(car)
library(nnet)
library(broom)
library(rstatix)
library(ggpubr)

# ---- 1. Load data ----
data_path <- "C:/Dataset/xAPI-Edu-Data.csv"
df <- read.csv(data_path, stringsAsFactors = FALSE)

# Quick structure check
str(df)
summary(df)

# ---- 2. Basic cleaning & factor conversions ----
# Trim whitespace from all character columns
df <- df %>%
  mutate(across(where(is.character), ~ trimws(.)))

# Convert variables used in analysis to appropriate types
df <- df %>%
  mutate(
    gender = factor(gender),
    StageID = factor(StageID),
    GradeID = factor(GradeID),
    SectionID = factor(SectionID),
    Topic = factor(Topic),
    Semester = factor(Semester),
    Relation = factor(Relation)
  )

# Fix Class: map "L" -> "Low", "M" -> "Medium", "H" -> "High" safely
df$Class <- toupper(df$Class)
df$Class <- ifelse(df$Class == "L", "Low",
                   ifelse(df$Class == "M", "Medium",
                          ifelse(df$Class == "H", "High", NA)))
df$Class <- factor(df$Class, levels = c("Low","Medium","High"))

# Numeric variables for activity
activity_vars <- c("raisedhands","VisITedResources","AnnouncementsView","Discussion")
df[activity_vars] <- lapply(df[activity_vars], function(x) as.numeric(as.character(x)))

# ---- 3. Descriptive statistics ----
cat("Total rows:", nrow(df), "\n")
cat("Class distribution:\n")
print(table(df$Class))

desc_by_class <- df %>%
  group_by(Class) %>%
  summarize(
    n = n(),
    mean_raisedhands = mean(raisedhands, na.rm = TRUE),
    sd_raisedhands = sd(raisedhands, na.rm = TRUE),
    mean_resources = mean(VisITedResources, na.rm = TRUE),
    sd_resources = sd(VisITedResources, na.rm = TRUE),
    mean_announcements = mean(AnnouncementsView, na.rm = TRUE),
    sd_announcements = sd(AnnouncementsView, na.rm = TRUE),
    mean_discussion = mean(Discussion, na.rm = TRUE),
    sd_discussion = sd(Discussion, na.rm = TRUE)
  )

cat("\n=== Descriptive statistics by Class ===\n")
print(desc_by_class)
write.csv(desc_by_class, "descriptive_by_class.csv", row.names = FALSE)
cat("Descriptive statistics saved as 'descriptive_by_class.csv'\n")

# ---- 4. Visualisations ----
p_list <- lapply(activity_vars, function(var){
  ggplot(df, aes_string(x = "Class", y = var)) +
    geom_boxplot() +
    stat_summary(fun = median, geom = "point", shape = 20, size = 2, color = "black") +
    labs(title = paste(var, "by Class"),
         x = "Performance Class",
         y = var) +
    theme_minimal()
})

main_plot <- ggarrange(plotlist = p_list, ncol = 2, nrow = 2, labels = "AUTO")
print(main_plot)   # Display in RStudio Plots pane
ggsave("boxplots_activity_by_class.png", main_plot, width = 10, height = 8)

hist_plot <- ggplot(df, aes(x = raisedhands)) +
  geom_histogram(binwidth = 1, closed = "left") +
  labs(title = "Histogram of Raised Hands",
       x = "Raised Hands (count)",
       y = "Frequency") +
  theme_minimal()
print(hist_plot)   # Display in Plots pane
ggsave("hist_raisedhands.png", hist_plot, width = 7, height = 5)

tab_gender_class <- table(df$gender, df$Class)
cat("\n=== Gender vs Class table ===\n")
print(as.data.frame(tab_gender_class))
write.csv(as.data.frame(tab_gender_class), "gender_class_table.csv", row.names = FALSE)
cat("Gender-Class table saved as 'gender_class_table.csv'\n")

bar_gender_class <- as.data.frame(tab_gender_class) %>%
  ggplot(aes(x = Var2, y = Freq, fill = Var1)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Count by Class and Gender", x = "Class", y = "Count", fill = "Gender") +
  theme_minimal()
print(bar_gender_class)   # Display in Plots pane
ggsave("bar_gender_class.png", bar_gender_class, width = 7, height = 5)

# ---- 5. Statistical testing ----
test_results <- list()

for(var in activity_vars){
  cat("\n\n=== Analysis for:", var, "===\n")
  
  if(length(levels(df$Class)[!is.na(levels(df$Class))]) < 2){
    cat("Skipping", var, "- Class factor has less than 2 levels.\n")
    next
  }
  
  shapiro_by_class <- df %>%
    group_by(Class) %>%
    summarize(
      n = sum(!is.na(.data[[var]])),
      shapiro_p = ifelse(n >= 3, shapiro.test(.data[[var]])$p.value, NA_real_)
    )
  print(shapiro_by_class)
  
  lev <- leveneTest(as.formula(paste(var, "~ Class")), data = df)
  print(lev)
  
  shapiro_ok <- all(na.omit(shapiro_by_class$shapiro_p) > 0.05)
  levene_p <- lev$`Pr(>F)`[1]
  
  if(!is.na(shapiro_ok) && shapiro_ok && levene_p > 0.05){
    cat("Using one-way ANOVA for", var, "\n")
    aov_mod <- aov(as.formula(paste(var, "~ Class")), data = df)
    print(summary(aov_mod))
    tuk <- TukeyHSD(aov_mod)
    print(tuk)
    test_results[[var]] <- list(method = "ANOVA", model = aov_mod, posthoc = tuk)
  } else {
    cat("Using Kruskal-Wallis for", var, "\n")
    kw <- kruskal.test(as.formula(paste(var, "~ Class")), data = df)
    print(kw)
    dunn <- dunn_test(df, as.formula(paste(var, "~ Class")), p.adjust.method = "bonferroni")
    print(dunn)
    test_results[[var]] <- list(method = "Kruskal-Wallis", test = kw, posthoc = dunn)
  }
}

# ---- 6. Chi-square test ----
cat("\n\n=== Chi-square test: Gender vs Class ===\n")
if(length(levels(df$Class)[!is.na(levels(df$Class))]) < 2){
  cat("Cannot perform Chi-square: Class factor has less than 2 levels.\n")
} else {
  chi <- chisq.test(tab_gender_class)
  print(chi)
  if(any(chi$expected < 5)){
    cat("Some expected counts < 5; running Fisher's Exact Test:\n")
    fisher_res <- fisher.test(tab_gender_class, simulate.p.value = TRUE)
    print(fisher_res)
  }
}

# ---- 7. Multinomial logistic regression ----
mod_df <- df %>%
  select(Class, all_of(activity_vars), gender) %>%
  drop_na()

if(length(levels(mod_df$Class)[!is.na(levels(mod_df$Class))]) >= 2){
  multinom_mod <- multinom(Class ~ raisedhands + VisITedResources + AnnouncementsView + Discussion + gender,
                           data = mod_df, trace = FALSE)
  
  cat("\n\n=== Multinomial logistic regression tidy results ===\n")
  tidy_multinom <- broom::tidy(multinom_mod, exponentiate = FALSE, conf.int = FALSE)
  print(tidy_multinom)
  write.csv(tidy_multinom, "multinom_model_results.csv", row.names = FALSE)
  cat("Multinomial regression results saved as 'multinom_model_results.csv'\n")
  
  # Odds ratios
  or <- exp(coef(multinom_mod))
  cat("\nOdds ratios:\n")
  print(or)
  
} else {
  cat("Cannot fit multinomial regression: Class factor has less than 2 levels.\n")
}

# ---- 8. Summary / Quick Interpretations ----
cat("\n\n=== Summary for report ===\n")
for(var in activity_vars){
  res <- test_results[[var]]
  if(!is.null(res)){
    if(res$method == "ANOVA"){
      p_anova <- summary(res$model)[[1]][["Pr(>F)"]][1]
      cat(sprintf("%s: ANOVA p = %.4g\n", var, p_anova))
    } else {
      p_kw <- res$test$p.value
      cat(sprintf("%s: Kruskal-Wallis p = %.4g\n", var, p_kw))
    }
  }
}
if(exists("chi")) cat("\nChi-square Gender vs Class p =", chi$p.value, "\n")

cat("\nAll plots displayed in Plots pane. Descriptive tables and model outputs saved as CSV.\n")
