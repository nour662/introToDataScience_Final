## Importing other libraries
library(patchwork)
library(ggplot2)
library(cowplot)
library(gridExtra)
library(png)

#############################################
## Importing enegry and wind datasets 
energy_data = read.csv("datasets/energy.csv")
wind_data = read.csv("datasets/wind.csv")

## Merge the two datasets by state t_state 

merged_df = merge(energy_data, wind_data, by = "t_state")

#############################################
## Summary of full dataset
summary(merged_df)

## Summary of Turbine attributes 
summary(merged_df$Net_generation) ## Generation of energy by wind turbine 
summary(merged_df$t_hh) ## Turbine hub height 
summary(merged_df$t_rd) ## Turbine root diameter 
summary(merged_df$t_rsa) ## Turbine swept area
summary(merged_df$t_ttlh) ## Turbine height from the ground to its apex

#############################################
## Plot Frequency of wind turbines by state 
png(filename = "generated_graphs/wind_turbine_frequency_by_state_barplot.png", width = 800, height = 600) ## Saving png
barplot(table(merged_df$t_state), 
        main = "Frequency of Wind Turbines by State",
        xlab = "State",
        ylab = "Number of Turbines",
        col = "blue",
        border = "black",
        names.arg = names(table(merged_df$t_state)),  # Use state names as x-axis labels
        las = 2,  # Rotate labels vertically
        cex.names = 0.7  # Adjust font size for x-axis labels (0.7 for smaller)
)
dev.off()
#############################################
## Histogram plot to visualize the distribution of wind turbines by year
png(filename = "generated_graphs/wind_turbine_frequency_by_year_histogram.png", width = 800, height = 600) ## Saving png
ggplot(data = merged_df) +
  geom_histogram(binwidth = 1, 
                 fill = "blue",  ## Set the filling color of bins to Magenta
                 color = "black",   ## Set the outline color of bins to black
                 aes(p_year), 
                 na.rm = TRUE) +  ## Remove NA values from the plot
  xlab("Year") + 
  ylab("Number of Wind Turbines") +
  ggtitle("Number of Wind Turbines by Year")+
  theme(panel.grid = element_blank())
dev.off()


#############################################
## Looking into project capacity and turbine capacity over the years 

# Create an aggregated dataset to summarize the maximum turbine rated capacity (kW) for each year
max_turbine_capacity_data <- aggregate(t_cap ~ p_year, 
                                       data = merged_df, 
                                       FUN = max)
# Scatter plot of highest turbine capacity
highest_turbine_capacity_plot <- ggplot(data = max_turbine_capacity_data, 
                                        aes(x = p_year, y = t_cap)) +
  geom_point(color = "blue", ##Represent data points as black open circles
             pch = 20) +
  geom_smooth(method = "loess", se = FALSE, color='gray') +  ## Add a loess smoother to visualize trends, without displaying the confidence interval
  xlab("Year") +
  ylab("Turbine Capacity (KW)") +
  ggtitle("Highest Turbine Capacity (kW) Over the Years")+
  theme_bw()

# Create an aggregated dataset to summarize the maximum turbine rated capacity (kW) for each year
max_project_capacity_data <- aggregate(p_cap ~ p_year, 
                                       data = merged_df, 
                                       FUN = max)
# Scatter plot of highest project capacity
highest_project_capacity_plot <- ggplot(data = max_project_capacity_data, 
                                        aes(x = p_year, y = p_cap)) +
  geom_point(color = "blue", ##Represent data points as black open circles
             pch = 20) +
  geom_smooth(method = "loess", se = FALSE, color='gray') +  ## Add a loess smoother to visualize trends, without displaying the confidence interval
  xlab("Year") +
  ylab("Project Capacity (MW)") +
  ggtitle("Highest Project Capacity (MW) Over the Years")+
  theme_bw()

png(filename = "generated_graphs/combined_capacity_scatterplots.png", width = 800, height = 600) ## Saving png
combined_plots <- grid.arrange(highest_turbine_capacity_plot, highest_project_capacity_plot, ncol=2) ## combining all graphs together
dev.off()

#############################################
## Looking into turbine attributes over time
## Rotor Diameter of Turbines Over Time 
rd_over_time <- ggplot(data = merged_df, aes(x = p_year, y = t_rd)) +
  geom_point(pch = 21, fill = "blue", color = "black", size = 1) +
  labs(
    x = "Year",
    y = "Rotor Diameter (m)",
    title = "Rotor Diameter of Turbines Over Time"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5)
    
  )

## Total Height of Turbines Over Time 
ttlh_over_time <- ggplot(data = merged_df, aes(x = p_year, y = t_ttlh)) +
  geom_point(pch = 21, fill = "red", color = "black", size = 1) +
  labs(
    x = "Year",
    y = "Total Height(m)",
    title = "Total Height of Turbines Over Time"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5)
    
  )


## Swept Area of Turbines Over Time 
rsa_over_time <- ggplot(data = merged_df, aes(x = p_year, y = t_rsa)) +
  geom_point(pch = 21, fill = "green", color = "black", size = 1) +
  labs(
    x = "Year",
    y = "Swept Area (m^2)",
    title = "Swept Area of Turbines Over Time"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5)
    
  )

## Hub Height of Turbines Over Time 
hh_over_time <- ggplot(data = merged_df, aes(x = p_year, y = t_hh)) +
  geom_point(pch = 21, fill = "yellow", color = "black", size = 1) +
  labs(
    x = "Year",
    y = "Hub Height (m)",
    title = "Hub Height of Turbines Over Time"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5)
    
  )

png(filename = "generated_graphs//combined_wind_turbine_attributes_scatterplots.png", width = 800, height = 600) ## Saving png
combined_plots <- grid.arrange(hh_over_time,rd_over_time ,ttlh_over_time, rsa_over_time, ncol=2) ## combining all graphs together
dev.off()