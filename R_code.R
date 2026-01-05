############################################################
# MEMBER 2 (Sadhik): Enhanced Visualisation Contribution
############################################################

# Violin plot: Raised Hands by Class
p_violin <- ggplot(df, aes(x = Class, y = raisedhands, fill = Class)) +
  geom_violin(trim = FALSE, alpha = 0.6) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  labs(
    title = "Distribution of Raised Hands by Performance Class",
    x = "Performance Class",
    y = "Raised Hands"
  ) +
  theme_minimal()

print(p_violin)
ggsave("violin_raisedhands_by_class.png", p_violin, width = 7, height = 5)


# Scatter plot: VisITedResources vs Discussion
p_scatter <- ggplot(df, aes(x = VisITedResources, y = Discussion, color = Class)) +
  geom_point(alpha = 0.6) +
  labs(
    title = "Visited Resources vs Discussion Activity",
    x = "Visited Resources",
    y = "Discussion Posts"
  ) +
  theme_minimal()

print(p_scatter)
ggsave("scatter_resources_discussion.png", p_scatter, width = 7, height = 5)
