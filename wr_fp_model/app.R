#### Libraries ####
library(tidyverse)
library(nflreadr)
library(randomForest)
library(shiny)
library(DT)
#### Load Data ####
player_stats <- nflreadr::load_player_stats(seasons = 2022, stat_type = "offense")
ff_opportunity <- nflreadr::load_ff_opportunity(seasons = 2022, stat_type = "weekly")

##### WR Data #####

wr_stats <- player_stats |> 
  filter(position == "WR") |> 
  select(player_id, player_display_name, recent_team, week, racr, wopr, receiving_epa, target_share, air_yards_share) |> 
  rename(
    team = recent_team,
    full_name = player_display_name,
  )

wr_opportunity <- ff_opportunity |> 
  filter(position== "WR") |> 
  rename(
    team = posteam,
  )

wr_combined <- wr_opportunity |> 
  inner_join(wr_stats) |> 
  select(
    season, team, week, game_id, player_id, full_name, rec_attempt, receptions, receptions_exp, receptions_diff,
    racr, wopr, receiving_epa, target_share, air_yards_share, rec_fantasy_points, rec_fantasy_points_exp, rec_fantasy_points_diff
  ) |> 
  drop_na() |> 
  rename(targets = rec_attempt) |> 
  group_by(week) |> 
  mutate(week_rank = dense_rank(desc(rec_fantasy_points_exp))) |> 
  mutate(rank_group = case_when(
    week_rank %in% c(1:15) ~ "WR1",
    week_rank %in% c(16:40) ~ "WR2",
    week_rank >= 41 ~ "WR3"
  )) |> 
  mutate_if(is.numeric, round, digits = 2)

wr_combined$rank_group <- as.factor(wr_combined$rank_group)

wr_combined

##### Summary Data for app #####

wr_season_averages <- wr_combined |> 
  group_by(player_id, full_name, team) |> 
  summarise(
    targets = mean(targets, na.rm = TRUE),
    receptions_exp = mean(receptions_exp, na.rm = TRUE),
    racr = mean(racr, na.rm = TRUE),
    wopr = mean(wopr, na.rm = TRUE),
    receiving_epa = mean(receiving_epa, na.rm = TRUE),
    target_share = mean(target_share, na.rm = TRUE),
    air_yards_share = mean(air_yards_share, na.rm = TRUE)
  )

#### Random Forest Model ####

# classification
fit_rf_wr <- randomForest(
  rank_group ~ targets + receptions_exp + wopr,
  data = wr_combined
)

fit_rf_wr

# regression
fit_rf_reg_wr <- randomForest(
  rec_fantasy_points_exp ~ targets + receptions_exp + racr + wopr + receiving_epa + target_share + air_yards_share,
  data = wr_combined, importance = TRUE
)

##### RF Interpretation #####
wr_variable_importance <- as.data.frame(importance(fit_rf_reg_wr))

wr_variable_importance$Var.Names <- row.names(wr_variable_importance)

wr_variable_importance |> 
  ggplot(aes(x = Var.Names, y = `%IncMSE`)) +
  geom_segment(aes(x = Var.Names, xend = Var.Names, y = 0, yend = `%IncMSE`), color = "skyblue") +
  geom_point(aes(size = IncNodePurity), color = "blue", alpha = 0.6) +
  theme_light() +
  coord_flip() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank()
  )
##### Sample New Data #####

new_data_example <- data.frame(
  targets = 8,
  receptions_exp = 5.86,
  wopr = 0.62
)

new_data_example

pred_fp_example <- predict(fit_rf_wr, newdata = new_data_example, type = "prob")



#### Shiny UI ####
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("WR Fantasy Point Predictor"),
    
    fluidRow(
      column(4,
             sliderInput(
               inputId = "targets",
               label = "Targets",
               min = min(wr_combined$targets),
               max = max(wr_combined$targets),
               value = median(wr_combined$targets)
             )),
      column(4,
             sliderInput(
               inputId = "receptions_exp",
               label = "Receptions Expected",
               min = min(wr_combined$receptions_exp),
               max = max(wr_combined$receptions_exp),
               value = median(wr_combined$receptions_exp)
             )),
      column(4,
             sliderInput(
               inputId = "wopr",
               label = "WOPR",
               min = min(wr_combined$wopr),
               max = max(wr_combined$wopr),
               value = median(wr_combined$wopr)
             )),
    ),

        # Show a plot of the generated distribution
        mainPanel(
          width = 12,
          div( style = "margin:auto;text-align: center; ",
               plotOutput(
                 outputId = "bar_plot", inline = TRUE
               ))
        )
    
)


#### Shiny Server ####
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  dat <- reactive({
    
    new_df <- data.frame(
      targets = input$targets,
      receptions_exp = input$receptions_exp,
      wopr = input$wopr
    )
    
    pred_rank <- predict(fit_rf_wr, newdata = new_df, type = "prob")
    
    pred_rank <- pred_rank |> 
      data.frame() |> 
      pivot_longer(
        cols = everything(),
        names_to = "wr_rank",
        values_to = "probability"
      )
    
    pred_rank
    
  })
  
  output$bar_plot <- renderPlot({
    
    dat() |> 
      ggplot(aes(x = probability, y = wr_rank)) +
      geom_col(
        fill = "blue",
        alpha = 0.7,
        color = "black"
      ) +
      geom_label(aes(label = scales::percent(probability)), size = 5) +
      scale_x_continuous(labels = scales::percent) +
      theme_bw() +
      labs(
        x = "Probability",
        y = "WR Rank"
      ) +
      theme(
        axis.text = element_text(size = 14, face = "bold"),
        axis.title = element_text(size = 17, face = "bold")
      )
    
  }, height = 600, width = 800)
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
