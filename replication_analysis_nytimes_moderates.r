# ==============================================================================
# SECTION 1: SETUP (LIBRARIES)
# ==============================================================================

# Load all necessary libraries once at the top
library(tidyverse) 
library(fixest)    
library(broom)     
library(scales)    

# ==============================================================================
# SECTION 2: DATA LOADING AND PREPARATION
# ==============================================================================

# Load the dataset from the local file
# Original URL: "https://www.dropbox.com/scl/fi/dpfinq0nbog5955y3xvc8/cong_gen_election_and_ideology_dataset_master.csv?rlkey=7kpxrzeml102flz3tinodvz73&dl=1"
#https://github.com/abonica/nyt_moderates_analysis/blob/134d5a79aa23e2c7f561ed90eb179f5a0e8c7776/cong_gen_election_and_ideology_dataset_master.csv'

wdd <- read.csv("cong_gen_election_and_ideology_dataset_master.csv")

# Use one 'mutate' call to create all new variables
wdd <- wdd %>%
  mutate(
    dem.fundraising = log1p(dem_total_receipts),
    rep.fundraising = log1p(rep_total_receipts),
    rep_diff = dem_diff * -1,
    dem_no_pac_contribs = as.numeric(dem_total_pac_contribs < 1000),
    rep_no_pac_contribs = as.numeric(rep_total_pac_contribs < 1000)
  )

# ==============================================================================
# SECTION 3: EXPLORATORY ANALYSIS & PRELIMINARY MODELS
# ==============================================================================

summary(lm(dem_diff ~ dem_no_pac_contribs, data = wdd, subset = (Year == 2024)))
summary(lm(rep_diff ~ rep_no_pac_contribs, data = wdd, subset = (Year == 2024)))

# Create a filtered dataset for these models
wd_models_base <- wdd %>% filter(Year == 2024 & dem_total_pac_contribs > 1000)

# 1. Democratic Models
dem.diff.model.no.controls <- feols(dem_diff ~ dem_pac_mod_nyt, data = wd_models_base)
dem.diff.model <- feols(dem_diff ~ dem_pac_mod_nyt + dem.fundraising + rep.fundraising + incumbency_status, data = wd_models_base)

print("--- Democratic Models (Filtered: dem_total_pac_contribs > 1000) ---")
etable(list(PVS = dem.diff.model.no.controls,
            PVS = dem.diff.model))

# 2. Republican Models
rep.diff.model.no.controls <- feols(rep_diff ~ as.numeric(rep_pac_mod_nyt == 'mod'), data = wd_models_base)
rep.diff.model <- feols(rep_diff ~ as.numeric(rep_pac_mod_nyt == 'mod') + dem.fundraising + rep.fundraising + incumbency_status, data = wd_models_base)

print("--- Republican Models (Filtered: dem_total_pac_contribs > 1000) ---")
etable(list(PVS = rep.diff.model.no.controls,
            PVS = rep.diff.model))



##Breaking out non-moderates and progressives shows that PAC-funded progressives perform at the same rates and moderates. +1.2 for progs versus +1.3 for mods.
wd_models_comp <- wdd %>% filter(Year == 2024 )
wd_models_comp$comp <- ifelse(wd_models_comp$dem_total_pac_contribs <= 1000,'No PAC funding',
                                ifelse(wd_models_comp$dem_pac_mod_nyt =='mod','Moderate',
                                ifelse(wd_models_comp$dem_pac_prog_nyt =='prog','Progressive','Non-Moderate')))                                
summary(feols(dem_diff ~ -1 + comp , data = wd_models_comp))


# ==============================================================================
# SECTION 4: PLOT 1 (THREE-WAY CATEGORY BAR CHART)
# ==============================================================================

get_3way_plot <- function(summary_3way,use.bracket=TRUE,new.title ='',new.subtitle='') {
  
  # Define the X coordinate for the bracket and text
  max_bar_value <- median(summary_3way$avg_overperformance)
  bracket_x <- max_bar_value + 0.005 # Place bracket just to the right
  text_x <- bracket_x + 0.001       # Place text just to the right of the bracket
  
    if(new.title != ''){
      g.title <- new.title
      g.subtitle <- new.subtitle
    }else{
      g.title = "The NYT's 'Non-Moderate' Group Is a Biased Comparison"
      g.subtitle = "It conflates serious, funded 'Non-Moderates' with non-competitive 'No PAC Funding' candidates, skewing the results."
    } 
  plot_3way <- ggplot(summary_3way, aes(x = avg_overperformance, y = fct_rev(Category), fill = avg_overperformance > 0)) +
    geom_col() +
    # Add the value labels to the bars
    geom_text(aes(label = Label, x = avg_overperformance + 0.001 * sign(avg_overperformance)),
              hjust = ifelse(summary_3way$avg_overperformance > 0, 0, 1),
              size = 4.5,
              fontface = "bold") +
    scale_fill_manual(values = c("TRUE" = "#A066D3", "FALSE" = "#E59F00"), guide = "none") +
    
   
    # --- Update titles and axis labels ---
    labs(
      title = g.title,
      subtitle = g.subtitle,
      x = "Percentage Point Overperformance vs. Presidential Vote",
      y = ""
    ) +
    
    # Set x-axis limits (in share, not percentage points) and scale
    scale_x_continuous(labels = scales::percent_format(scale = 100, accuracy = 1)) +
    
    # Expand x-axis to make room for the bracket annotation
    coord_cartesian(xlim = c(-0.02, 0.02), clip = "off") +
    
    theme_minimal(base_family = "Helvetica") +
    
    # --- Theme adjustments for left-alignment ---
    theme(
      panel.grid.major.y = element_blank(),
      panel.grid.minor.x = element_blank(),
      
      # Left-align title and subtitle
      plot.title.position = "plot",
      plot.title = element_text(hjust = 0, face = "bold", size = 18, margin = margin(b = 5)),
      plot.subtitle = element_text(hjust = 0, size = 12, margin = margin(b = 15), color = "gray30"),
      
      plot.margin = margin(10, 40, 10, 10), # Add right margin for annotation
      
      axis.text.y = element_text(size = 13, face = "bold"),
      axis.text.x = element_text(size = 11),
      axis.title.x = element_text(size = 12, margin = margin(t = 10))
    )

    if(use.bracket){
      plot_3way <- plot_3way +  
      # Add the vertical line of the bracket
      annotate("segment",
              x = bracket_x, xend = bracket_x,
              y = 0.8, yend = 2.2, # Spans from y=1 to y=2
              size = 0.5, color = "gray20") +
      # Add the top tick of the bracket
      annotate("segment",
              x = bracket_x, xend = bracket_x - 0.0005,
              y = 2.2, yend = 2.2,
              size = 0.5, color = "gray20") +
      # Add the bottom tick of the bracket
      annotate("segment",
              x = bracket_x, xend = bracket_x - 0.0005,
              y = 0.8, yend = 0.8,
              size = 0.5, color = "gray20") +
      # Add the text label for the bracket
      annotate("text",
              x = text_x, y = 1.5, # y=1.5 is the midpoint of the bracket
              label = "NYT 'Non-Moderate' Group\n(Combines these two categories)",
              hjust = 0, size = 3,
              fontface = "italic", color = "gray20") 
    }  
  
  return(plot_3way)
}

# --- Data Prep & Save: Democratic 3-Way Plot ---
wdd_2024 <- wdd %>% filter(Year == 2024)

# Prepare summary data for the Dem plot
summary_3way_dem <- wdd_2024 %>%
  mutate(
    dem_overperformance = dem_diff,
    # Create the new three-level category
    Category = case_when(
      dem_total_pac_contribs < 10000 ~ "No PAC Funding",
      dem_pac_mod_nyt == 'mod' ~ "Moderate (PAC Funded)",
      # dem_pac_prog_nyt_24 =='prog' ~ "Progressive (PAC Funded)", # This was commented out in original
      TRUE ~ "Non-Moderate (PAC Funded)"
    )
  ) %>%
  group_by(Category) %>%
  summarise(avg_overperformance = mean(dem_overperformance, na.rm = TRUE)) %>%
  mutate(
    Label = sprintf("%+.1f", avg_overperformance * 100),
    # Ensure the order is logical for plotting
    Category = factor(Category, levels = c("Moderate (PAC Funded)", "Non-Moderate (PAC Funded)",
                                           # "Progressive (PAC Funded)",
                                           "No PAC Funding"))
  )

# Generate and save the plot
plot_3way_dem <- get_3way_plot(summary_3way_dem)
ggsave("nyt_style_barchart_3way_dem.png", plot_3way_dem, width = 12, height = 4, dpi = 300)


# Prepare summary data for the Rep plot (using the same wdd_2024 base)
summary_3way_rep <- wdd_2024 %>%
  mutate(
    rep_overperformance = rep_diff, # Use rep_diff created in Section 2
    # Create the new three-level category
    Category = case_when(
      rep_total_pac_contribs < 10000 ~ "No PAC Funding",
      rep_pac_mod_nyt == 'mod' ~ "Moderate (PAC Funded)",
      TRUE ~ "Non-Moderate (PAC Funded)"
    )
  ) %>%
  group_by(Category) %>%
  summarise(avg_overperformance = mean(rep_overperformance, na.rm = TRUE)) %>%
  mutate(
    Label = sprintf("%+.1f", avg_overperformance * 100),
    # Ensure the order is logical for plotting
    Category = factor(Category, levels = c("Moderate (PAC Funded)", "Non-Moderate (PAC Funded)", "No PAC Funding"))
  )

# Generate and save the plot
plot_3way_rep <- get_3way_plot(summary_3way_rep)
ggsave("nyt_style_barchart_3way_rep.png", plot_3way_rep, width = 12, height = 4, dpi = 300)

print("Plot 1 (3-way comparison) saved:")
print("- nyt_style_barchart_3way_dem.png")
print("- nyt_style_barchart_3way_rep.png")


# ==============================================================================
#   Flipped analysis to progressive versus non-progressive 
# ==============================================================================

wd_models_comp <- wdd %>% filter(Year == 2024)
wd_models_comp$mod <- ifelse(wd_models_comp$dem_pac_mod_nyt =='mod','mod','nonmod')
wd_models_comp$prog <- ifelse(wd_models_comp$dem_pac_prog_nyt =='prog','prog','nonprog')

summary(feols(dem_diff ~ -1 + prog , data = wd_models_comp))
summary(feols(dem_diff ~ -1 + mod , data = wd_models_comp))


summary_2way_prog <- wd_models_comp %>%
  mutate(
    rep_overperformance = dem_diff, # Use rep_diff created in Section 2
    # Create the new three-level category
    Category = case_when(
      prog == 'prog' ~ "Progressive",
      TRUE ~ "Non-Progressive"
    )
  ) %>%
  group_by(Category) %>%
  summarise(avg_overperformance = mean(rep_overperformance, na.rm = TRUE)) %>%
  mutate(
    Label = sprintf("%+.1f", avg_overperformance * 100),
    # Ensure the order is logical for plotting
    Category = factor(Category, levels = c("Progressive", "Non-Progressive"))
  )

# Generate and save the plot
plot_2way_prog <- get_3way_plot(summary_2way_prog,
                                use.bracket=FALSE,
                                new.title = "The 'Progressive Advantage' Using the Same Method",
                                new.subtitle='Using the same method, "progressives" outperform and "non-progressives" underperform because the candidate with no PAC support are lumped in with the latter.')

ggsave("nyt_style_barchart_prog_nonprog.png", plot_2way_prog, width = 12, height = 4, dpi = 300,bg='white')



summary_2way_mod <- wd_models_comp %>%
  mutate(
    rep_overperformance = dem_diff, # Use rep_diff created in Section 2
    # Create the new three-level category
    Category = case_when(
      mod == 'mod' ~ "Moderate",
      TRUE ~ "Non-Moderate"
    )
  ) %>%
  group_by(Category) %>%
  summarise(avg_overperformance = mean(rep_overperformance, na.rm = TRUE)) %>%
  mutate(
    Label = sprintf("%+.1f", avg_overperformance * 100),
    # Ensure the order is logical for plotting
    Category = factor(Category, levels = c("Moderate", "Non-Moderate"))
  )

# Generate and save the plot
plot_2way_mod <- get_3way_plot(summary_2way_mod,use.bracket=FALSE)
ggsave("nyt_style_barchart_mod_non.png", plot_2way_mod, width = 12, height = 4, dpi = 300,bg='white')


# ==============================================================================
# (DISAPPEARING EFFECT - 3 BARS, WITH NYT CLAIM)
# ==============================================================================

# Filter for Democratic candidates with *very significant* PAC contributions (>= 10000)
  wd_plot3_filtered <- wdd_2024 %>%
    mutate(
      dem_pac_mod_nyt_numeric = as.numeric(dem_pac_mod_nyt == 'mod' & dem_pac_prog_nyt !='prog')
    ) %>%
    filter(dem_total_pac_contribs >1000 )

  # 1. Unadjusted (Simple) Model
  dem.model.simple.p3 <- feols(dem_diff ~ dem_pac_mod_nyt_numeric, data = wd_plot3_filtered)

  # 2. Adjusted (Controlled) Model
  dem.model.controlled.p3 <- feols(dem_diff ~ dem_pac_mod_nyt_numeric + dem.fundraising + rep.fundraising + incumbency_status, data = wd_plot3_filtered)


  # Tidy the models to get coefficients
  tidy_simple_p3 <- tidy(dem.model.simple.p3) %>% filter(term == "dem_pac_mod_nyt_numeric")
  tidy_controlled_p3 <- tidy(dem.model.controlled.p3) %>% filter(term == "dem_pac_mod_nyt_numeric")

  # Create a data frame for our two models
  plot_data_p3_models <- rbind(tidy_simple_p3, tidy_controlled_p3)
  plot_data_p3_models$Model <- c("Unadjusted", "Adjusted")

  nyt_claim <- data.frame(
    term = "dem_pac_mod_nyt_numeric",
    estimate = 0.02, # This is 2.0 percentage points
    std.error = NA,
    statistic = NA,
    p.value = NA, # No p-value for the claim itself
    Model = "NYT Claim"
  )

  # Combine all three rows
  plot_data_p3_final <- rbind(nyt_claim, plot_data_p3_models)
  plot_data_p3_final
# --- Create the "Sideways" Bar Chart ---
disappearing_effect_plot_3 <- plot_data_p3_final %>%
  mutate(
    # Convert share (e.g., 0.02) to percentage points (e.g., 2.0)
    estimate_pp = estimate * 100,
    
    # Create main label (e.g., "+0.88 pts.")
    # The original code had an empty '' for the star, replicating that.
    label_text = sprintf("%+.2f pts.%s",
                         estimate_pp, ''),
    
    # Create significance label
    sig_label = ifelse(!is.na(p.value) & p.value >= 0.05,
                       "(Statistically Insignificant)",
                       "")
  ) %>%
  ggplot(aes(x = estimate_pp,
             # Order bars from largest to smallest (top to bottom)
             y = fct_reorder(Model, estimate_pp),
             fill = Model)) +
  geom_col() +
  
  # Add the main text label (the number)
  geom_text(aes(label = label_text),
            hjust = 0,
            nudge_x = 0.05, # Place text just to the right of bar
            size = 5,
            fontface = "bold") +
  
  # Add the statistical insignificance label
  geom_text(aes(label = sig_label),
            hjust = 0,
            nudge_x = 0.05, # Place label in same spot
            vjust = 2.5,  # Place it just below the main label
            size = 4,
            fontface = "italic",
            color = "gray30") +
  
  # Add a vertical line at zero
  geom_vline(xintercept = 0, linetype = "solid", color = "gray20") +
  
  scale_fill_manual(values = c("NYT Claim" = "#E59F00",
                               "Unadjusted" = "#A066D3",
                               "Adjusted" = "#A066D3"),
                    guide = "none") +
  
  labs(
    title = "The Disappearing 'Moderation Advantage'",
    subtitle = "The NYT's claimed effect for Democrats shrinks to zero after adding basic controls.",
    x = "Estimated Vote Share Advantage (Percentage Points)",
    y = ""
  ) +
  
  # Set x-axis limits
  coord_cartesian(xlim = c(0, 2.5), clip = "off") + # Widen for labels
  
  theme_minimal(base_family = "Helvetica") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_line(linetype = "dashed", color = "gray80"),
    
    # Left-align title and subtitle
    plot.title.position = "plot",
    plot.title = element_text(hjust = 0, face = "bold", size = 18, margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0, size = 12, margin = margin(b = 15), color = "gray30"),
    
    plot.margin = margin(10, 20, 10, 10), # Add right margin for labels
    
    axis.text.y = element_text(size = 13, face = "bold"),
    axis.text.x = element_text(size = 11),
    axis.title.x = element_text(size = 12, margin = margin(t = 10))
  )

# --- Save the Plot ---
ggsave("disappearing_effect_bar_chart.png", disappearing_effect_plot_3, width = 10, height = 5, dpi = 300,bg='white')

print("Plot 3 (3-bar disappearing effect) saved:")
print("- disappearing_effect_bar_chart.png")

print("--- Analysis Complete ---")
