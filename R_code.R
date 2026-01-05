
########################################################################
# Member 3: Statistical Testing
# Contributor: Uday Kiran (uj24aag@herts.ac.uk)
# Role: Chi-square and Kruskal-Wallis testing
########################################################################

# --- Chi-square test: Gender vs Class ---
chi_member3 <- chisq.test(tab_gender_class)
print(chi_member3)

# --- Kruskal-Wallis tests for activity variables ---
for (var in activity_vars) {
  cat("\nMember 3 – Kruskal-Wallis for:", var, "\n")
  kw <- kruskal.test(as.formula(paste(var, "~ Class")), data = df)
  print(kw)
} 
