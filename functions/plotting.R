
#function for plotting bad figure 
plot_bad_figure <- function(penguins_clean){
  penguins_clean %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm, color = species, shape = island)) +
  geom_point(size = 10, alpha = 0.7) +
  labs(title = "Penguin Body Mass vs. Flipper Length",
       x = "body_mass_g",
       y = "flipper_length_mm",
       color = "Species",
       shape = "Island") +
  theme_dark()+
  theme(legend.position = "bottom", legend.title = element_blank(),
        panel.grid = element_blank(), axis.line = element_line(color = "black"),
        plot.title = element_text(hjust = 0.5, size = 20, face = "bold", color = "red"),
        axis.title = element_text(size = 12, face = "italic", color = "purple"),
        legend.text = element_text(size = 8, face = "bold", color = "blue"))
}

#function for plotting exploratory figure 
plot_flipper_figure <- function(flipper_data){
  flipper_data %>% 
    ggplot(aes(x = species, y = flipper_length_mm)) +
    geom_boxplot(aes(color = species), width = 0.3, show.legend = FALSE) +
    geom_jitter(aes(color = species), alpha = 0.3, show.legend = FALSE, position = position_jitter(width = 0.2, seed = 0)) +
    scale_color_manual(values = c("darkorange","purple","cyan4")) +
    scale_x_discrete(labels=c("Adelie","Chinstrap","Gentoo")) +
    labs(x = "Penguin Species",
         y = "Flipper length (mm)") +
    theme_bw()
}


#function for plotting exploratory plot 
plot_explor <- function(penguins_clean){
  penguins_clean %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm , color = species)) +
  geom_point() +
  labs(title = "Exploratory Plot - Penguin Body Mass vs. Flipper Length",
       x = "Body Mass (g)",
       y = "Flipper Length (mm)")
}

#function for saving vector image 
save_exploratory_plot_svg <- function(penguins_clean, 
                                  filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  exploratory_plot <- plot_explor(penguins_clean)
  print(exploratory_plot)
  dev.off()
}

#function for saving explanatory figure 
save_explanatory_plot_svg <- function(penguins_clean, 
                                      filename, size, scaling){
  size_inches = size/2.54
  svglite(filename, width   = size_inches, 
          height  = size_inches, 
          scaling = scaling)
  explanatory_plot <- plot_explan(penguins_clean)
  print(explanatory_plot)
  dev.off()
}

#function for plotting explanatory figure 
plot_explan <- function(penguins_clean){
  penguins_clean %>%
  ggplot(aes(x = body_mass_g, y = flipper_length_mm , color = species)) +
geom_point() +
  facet_wrap(~species)+
    geom_smooth(method = "lm", se = FALSE, color = "black")+
    stat_cor(method = "pearson", size = 4) +
  labs(title = "Penguin Body Mass vs. Flipper Length Across Species",
       x = "Body Mass (g)",
       y = "Flipper Length (mm)")+
    theme_minimal()+
    theme(axis.title.x = element_text(margin = margin(t = 10)),
          axis.title.y = element_text(margin = margin(r = 10)),
          plot.title = element_text(hjust = 0.5))
}









