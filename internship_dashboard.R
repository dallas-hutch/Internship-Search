oldw <- getOption("warn")
options(warn = -1)

# Attach necessary libraries
library(dplyr)
library(plyr)
library(ggplot2)
library(readxl)
library(shiny)
library(shinythemes)
library(plotly)
library(scales)
library(shinydashboard)
library(fresh)
library(ggtext)
library(ggdark)

options(warn = oldw)

# Read in internship search data
df <- as.data.frame(readxl::read_xlsx("internships.xlsx"))
#head(df)
#summary(df)
#nrow(df) # 134 rows or internship applications

### Data Cleaning
# Change 'app_date' and 'response date' to date type instead of datetime
df$app_date <- as.Date(df$app_date)
df$response_date <- as.Date(df$response_date)

# Fill NAs with 0s in the 'rejection notice' column
df$rejection_notice[is.na(df$rejection_notice)] <- 0

# Change resume, response, and rejection notice to factors
df$resume <- as.factor(df$resume)
df$rejection_notice <- as.factor(df$rejection_notice)
df$response <- as.factor(df$response)

# # Flip curve direction
# directions <- c(1, -1)
# df$direction <- rep(directions, length.out=length(df$app_date))

# Separate into only rows where company responded
responded <- df[!is.na(df$response_date),]
responded$no_int <- ifelse(is.na(responded$interview1), 1, -1) # company responded with straight rejection

### Create dataframes for month and year to establish timeline
# Month dataframe for text description
month_date_range <- seq(min(df$app_date) - 5, max(df$app_date) + 110, by='month')
month_format <- format(month_date_range, '%b')
month_df <- data.frame(month_date_range, month_format)

# Year dataframe for text description
year_date_range <- c(min(df$app_date), max(df$app_date)-55)
# year_date_range <- as.Date(
#   intersect(
#     floor_date(year_date_range, unit="year"),
#     floor_date(year_date_range, unit="year")
#   ),  origin = "1970-01-01"
# )
year_format <- format(year_date_range, '%Y')
year_df <- data.frame(year_date_range, year_format)

### Create metrics and visuals to use in dashboard
# Find "hit rate" by resume type
df$hit <- ifelse(!is.na(df$interview1), 1, 0)
df %>% group_by(resume) %>% 
  dplyr::summarise(sent=n(), hit=sum(hit)) %>% 
  mutate(hit_rate = round(hit / sent, 3)*100) %>% 
  arrange(desc(hit_rate))

rates <- df %>% group_by(resume) %>% 
  dplyr::summarise(sent=n(), hit=sum(hit)) %>% 
  mutate(hit_rate = round(hit / sent, 3)*100)

# Num internships applied to
num_apps <- nrow(df)

# "Hit" rate among all applications
overall_hit_rate <- round((sum(df$hit) / nrow(df))*100,1)

# Calculate time difference in days between when applications were sent and first 
# response (rejection or interview process), find average length of these differences
responded$days_to_respond <- as.numeric(difftime(responded$response_date, responded$app_date, units = "days"))
avg_days_to_respond <- round(mean(responded$days_to_respond),0)

# Set levels and colors for different resumes used
status_levels <- c("First", "Second", "Third", "Fourth", "Fifth")
status_colors <- c("#00ffff", "#24b0d8", "#605ca8", "#bf0243", "#ff5e22")
#"#621e74" old purple

#responded$color <- if_else(responded$resume == 1, "#00ffff", if_else(
#  responded$resume == 2, "#24b0d8", if_else(
#    responded$resume == 3, "#605ca8", if_else(
#      responded$resume == 4, "#bf0243", "#ff5e22"
#    )
#  )
#))
#col <- as.character(responded$color)
#names(col) <- as.character(responded$resume)

### Timeline plot

timeline_plot <- ggplot(responded, aes(x=app_date, y=0)) + 
                  geom_curve(data=filter(responded, no_int==-1), aes(x=app_date, 
                                                                     xend=response_date, 
                                                                     yend=0.001, 
                                                                     colour=resume),
                              size=2.5, curvature=-1, ncp=100, angle=90)  +
                  geom_curve(data=filter(responded, no_int==1), aes(x=app_date, 
                                                                    xend=response_date,
                                                                    yend=0.001, 
                                                                    colour=resume),
                              size=1.10, curvature=-1, alpha=0.5, ncp=100, angle=90) +
                  dark_theme_gray() + 
                  ylim(-0.14, 1) +
                  geom_text(data=month_df, aes(x=month_date_range,
                                               y=-0.07,
                                               label=month_format, 
                                               fontface="bold"),
                            size=4.5,vjust=0.5, color='white', angle=90) +
                  geom_text(data=year_df, aes(x=year_date_range,
                                              y=-0.14,
                                              label=year_format, 
                                              fontface="bold"),
                            size=4.5, color='white') +
                  scale_color_manual(values=status_colors, labels=status_levels, drop = FALSE) +
                  ggtitle("Time to First Response from Employer") +
                  labs(subtitle = "Color = Resume used, Highlight = 'Hit'") +
                  theme(
                    plot.title = element_text(size = 16, hjust = 0.5),
                    plot.subtitle = element_text(size = 12, hjust = 0.5),
                    panel.background = element_rect(fill = "black"),
                    plot.background = element_rect(fill = "black"),
                    legend.background = element_rect(fill = "black"),
                    axis.line.y = element_blank(),
                    axis.line.x = element_blank(),
                    axis.text.y = element_blank(),
                    axis.text.x = element_blank(),
                    axis.title.x = element_blank(),
                    axis.title.y = element_blank(),
                    axis.ticks.x = element_blank(),
                    axis.ticks.y = element_blank(),
                    legend.title = element_blank(),
                    legend.text = element_text(size = 12),
                    legend.position = c(0.05,0.6)) +
                  guides(color = guide_legend(override.aes = list(size = 5))) +
                  geom_hline(yintercept=0, color = "white", size=2) #+
                  #layout(height = 1200)

#timeline_plot



### Testing out ggplotly options

#p <- ggplot(subset(responded, resume %in% c(1, 2, 3, 4, 5)), 
#aes(x = days_to_respond, fill = resume, colour = resume)) + 
#  geom_density(alpha = 0.4) + scale_color_manual(values = col, labels = col)
#ggplotly(p)

#h <- ggplot(responded, aes(days_to_respond)) + 
#geom_histogram(aes(y = ..density..), alpha = 0.7, fill = '#333333', binwidth = 15) +
#  geom_density(fill = '#ff4d4d', alpha = 0.4) +
#  theme(panel.background = element_rect(fill = '#ffffff')) + 
#  ggtitle("Density with Histogram overlay")
#ggplotly(h)



### Below is the code for the shiny theme, UI, server, and Shiny App
# Define theme for UI
my_theme <- create_theme(
  adminlte_color(
    light_blue = "#434C5E"
  ),
  adminlte_sidebar(
    width = "300px",
    dark_bg = "#DFDFDF",
    dark_hover_bg = "#81A1C1",
    dark_color = "#2E3440"
  ),
  adminlte_global(
    content_bg = "#000000",
    box_bg = "#000000", 
    info_box_bg = "#000000"
  )
)

# Define UI for app ----
sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem("Summary", tabName = "summary"),
    menuItem("Resume Breakdown", tabName = "breakdown")
  )
)
  
body <- dashboardBody(
  
  use_theme(my_theme),
  
  tabItems(
    ## Aggregate metrics body
    tabItem(tabName = "summary",
            fluidRow(
              valueBox(num_apps, "Positions Applied For", 
                       color = "purple", 
                       icon("list-alt", 
                            lib = "glyphicon")),
              valueBox(paste0(overall_hit_rate,"%"), 'Overall "Hit" Rate', 
                       color = "purple", 
                       icon("screenshot", 
                            lib = "glyphicon")),
              valueBox(paste0(avg_days_to_respond," days"), "Average Time to Respond", 
                       color = "purple", 
                       icon("time", 
                            lib = "glyphicon"))
            ),
            fluidRow(
              box(#height = 12,
                  width = 12,
                  solidHeader = TRUE,
                  plotOutput("curvedPlot", height = 530))
            )
    ),
    
    ## Resume breakdown body
    tabItem(tabName = "breakdown",
            fluidRow(
              column(
                #div(dataTableOutput('table1'), style = "font-size:60%"),
                selectInput(inputId = 'resin1', label = 'Select a resume:',
                            choices = unique(df$resume), selectize = FALSE),
                selectInput(inputId = 'resin2', label = 'Select a different resume:',
                            choices = unique(df$resume), selected = "2", selectize = FALSE),
                span(textOutput("description1"), style="color:teal"),
                span(textOutput("description2"), style="color:maroon"),
                width = 4,
                height = 8,
              ),
              column(width = 8,
                     height = 8,
                     plotlyOutput("density"),
              ),
            fluidRow(
              box(width = 8,
                  solidHeader = TRUE,
                  plotlyOutput("hitrate_bar"))
            )
            )
    )
  )
)

ui <- dashboardPage(skin = "black",
  dashboardHeader(title= "Road to an Internship", titleWidth = 320),
  sidebar,
  body
)


# Define server logic required to draw a histogram ----
server <- function(input, output) {

  #  Render the timeline-resume plot
  output$curvedPlot <- renderPlot({
    
    timeline_plot
    
  })
  
  # Filter df down to the two resume formats selected
  filtered_df <- reactive({
    responded %>% filter(resume %in% input$resin1)
  })
  #resume_two
  output$hitrate_bar <- renderPlotly({
    rates <- df %>% group_by(resume) %>% 
              dplyr::summarise(sent=n(), hit=sum(hit)) %>% 
              mutate(hit_rate = round(hit / sent, 3)*100)
    rates_subset <- subset(rates, resume %in% c(input$resin1, input$resin2))
    hitrate_bar <- ggplot(rates_subset, aes(y = resume,
                                            color = resume,
                                            fill = resume,
                                            text = paste0("Applications sent: ", sent,
                                                          "\n", "Number 'hit': ", hit,
                                                          "\n", "'Hit' rate: ", hit_rate, "%"))) +
      geom_bar(aes(weight = hit_rate)) +
      dark_theme_gray() +
      scale_color_manual(values = c("#00ffff", "#bf0243")) +
      scale_fill_manual(values = c("#00ffff", "#bf0243")) +
      ggtitle("How often did each resume get to the first stage of interviews?") +
      labs(x = "Hit Rate (%)", y = "Resume Used") +
      theme(
        plot.title = element_text(size = 12, hjust = 0.5),
        legend.position = "none",
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        legend.background = element_rect(fill = "black")
      )
    ggplotly(hitrate_bar, tooltip = "text") %>% layout(height = 246)
  })
  
  output$density <- renderPlotly({
    # filter to 2 selected inputs and create mean by group
    data <- subset(responded, resume %in% c(input$resin1, input$resin2))
    mu <- ddply(data, "resume", summarise, mean_value=mean(days_to_respond))
    
    # plot density distribution
    density <- ggplot(data=data, aes(x = days_to_respond, fill = resume, color = resume)) + 
      geom_density(alpha = 0.6) +
      dark_theme_gray() +
      geom_vline(data = mu, aes(xintercept = mean_value, color = resume), linetype = "dashed", show.legend = FALSE) +
      scale_fill_manual(values = c("#00ffff", "#bf0243")) +
      scale_color_manual(values = c("#00ffff", "#bf0243")) +
      scale_x_continuous(expand = c(0, 0)) +
      scale_y_continuous(expand = c(0, 0)) +
      ggtitle("Performance of each Resume: Days to First Response") +
      labs(subtitle = "Dashed line = Average", x = "Days to Response", y = "Density", caption = "caption") +
      theme(
        plot.title = element_text(size = 14, hjust = 0.5),
        plot.subtitle = element_text(size = 12, hjust = 0.5, color="white"),
        panel.background = element_rect(fill = "black"),
        plot.background = element_rect(fill = "black"),
        legend.position = "none",
        legend.background = element_rect(fill = "black")
      )

      
  })
  
  # Render text description
  output$description1 <- renderText({ 
    paste("Lower numbered choice will output in teal.") 
    })
  
  output$description2 <- renderText({ 
    paste("Higher numbered choice will output in red.") 
    })
  
  output$table1 <- renderDataTable(sample_n(responded, 5), options = list(scrollX = TRUE))
}




# Create Shiny app ----
shinyApp(ui = ui, server = server)

