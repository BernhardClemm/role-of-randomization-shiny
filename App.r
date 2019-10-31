library(markdown)
library(shiny)
library(magrittr)
library(tidyr)
library(dplyr)
library(stringr)
library(ggplot2)
library(gridExtra)

# Read in British Election Study 2017 Version 1.3 
## (https://www.britishelectionstudy.com/data-object/2017-face-to-face/)
## Selected variables: Age, gender, ethnicity, religion, leftright, presence of respondent's partner during interview y48_1

data <- read.csv("./bes.csv")

n <- nrow(data)

## Describe complete sample for table

### Continuous and binary variables

sample_age_gender <- data %>%
  summarize("Age (mean)" = mean(Age, na.rm = T),
            "Gender (proportion female)" = mean(Gender, na.rm = T)) %>%
  round(2) %>%
  as.numeric(.[1, ])

sample_ideology_partner <- data %>%
  summarize("Ideology (mean)" = mean(Ideology, na.rm = T),
            "Partner present (proportion)" = mean(Partner, na.rm = T)) %>%
  round(2) %>%
  as.numeric(.[1, ])

### Categorical variables

sample_ethn <- round(prop.table(table(data$Ethnicity)), 3)
sample_edu <- round(prop.table(table(data$Education)), 3)

### Combine in one vector, including NAs for empty rows in table 

sample <- c(sample_age_gender, NA, sample_ethn, NA, sample_edu, sample_ideology_partner)

# UI

ui <- fluidPage(
  
  titlePanel("The role of randomization"),
  
  sidebarLayout(
    fluid=TRUE,
    
    sidebarPanel(
      numericInput("treatments", "Pick number of treatments", min = 2, max = 4, value = 2),
      actionButton("assign", "Assign treatments!")
    ),
    mainPanel(
      
      tabsetPanel(
        tabPanel("One assignment", tableOutput("table")),
        tabPanel("One hundred assignments", plotOutput(outputId = "plot"))),
      tabsetPanel(
        tabPanel("Instructions", includeMarkdown("instruction.md")),
        tabPanel("About", includeMarkdown("about.md"))
      )
    )
  ))

# Server

server <- function(input, output) {
    
    data_assigned_once <- eventReactive(input$assign,{
      
      ## Assign random treatment variables according to number of treatments

      n_treatments <- input$treatments
      data$treatment <- sample(1:n_treatments, n, replace = T)
      
      ## Summarize variables per treatment group
      
      ### Continuous and binary variables: treatment means

      age_gender <- data %>%
        group_by(treatment) %>%
        summarize("Age (mean)" = mean(Age, na.rm = T),
                  "Gender (proportion female)" = mean(Gender, na.rm = T)) %>%
        mutate_if(is.numeric, round, 2) %>%
        t() %>% as.data.frame() %>%
        setNames(gsub("V","Treatment",names(.))) %>%
        tibble::rownames_to_column(., "Variable") %>%
        filter(Variable != "treatment")
      
      ideology_partner <- data %>%
        group_by(treatment) %>%
        summarize("Ideology (mean)" = mean(Ideology, na.rm = T),
                  "Partner present (proportion)" = mean(Partner, na.rm = T)) %>%
        mutate_if(is.numeric, round, 2) %>%
        t() %>% as.data.frame() %>%
        setNames(gsub("V","Treatment",names(.))) %>%
        tibble::rownames_to_column(., "Variable") %>%
        filter(Variable != "treatment")
      
      ### Categorical non-binary variables: chi-squared statistic
      
      ethnicity <- data %>%
        gather(variable, value, Ethnicity) %>%
        group_by(treatment, variable, value) %>%
        summarise (n = n()) %>%
        mutate(freq = n / sum(n)) %>%
        ungroup() %>%
        select(-c(n)) %>%
        filter(!is.na(value)) %>%
        spread(value, freq) %>% 
        mutate_if(is.numeric, round, 3) %>%
        t() %>% as.data.frame() %>%
        setNames(gsub("V","Treatment",names(.))) %>%
        tibble::rownames_to_column(., "Variable") %>%
        filter(Variable != "variable")
      ethnicity[1,] <- NA
      ethnicity[1,1] <- "Ethnicity (proportions)"
      ethnicity[2,1] <- "<i>African</i>"
      ethnicity[3,1] <- "<i>Asian</i>"
      ethnicity[4,1] <- "<i>Mixed/Other</i>"
      ethnicity[5,1] <- "<i>White</i>"
      
      education <- data %>%
        gather(variable, value, Education) %>%
        group_by(treatment, variable, value) %>%
        summarise (n = n()) %>%
        mutate(freq = n / sum(n)) %>%
        ungroup() %>%
        select(-c(n)) %>%
        filter(!is.na(value)) %>%
        spread(value, freq) %>% 
        mutate_if(is.numeric, round, 3) %>%
        t() %>% as.data.frame() %>%
        setNames(gsub("V","Treatment",names(.))) %>%
        tibble::rownames_to_column(., "Variable") %>%
        filter(Variable != "variable")
      education[1,] <- NA
      education[1,1] <- "Education (proportions)"
      education[2,1] <- "<i>First degree</i>"
      education[3,1] <- "<i>Lower degree</i>"
      education[4,1] <- "<i>No qualification</i>"
      education[5,1] <- "<i>Postgraduate degree</i>"
        
      summary <- rbind(age_gender, ethnicity, education, ideology_partner)

      summary["All"] <- sample

      return(summary)
      return(n_treatments)

    })
    
    data_assigned_100 <- eventReactive(input$assign,{
      
      ## Re-sample assignments a hundred times and bind them to data 
      
      n_treatments <- input$treatments
      treatments_100 <- matrix(sample(1:n_treatments, n*100, replace = T),
                               nrow = n, ncol = 100)
      treatments_100 %<>%
        as.data.frame(treatments_100) %>%
        setNames(gsub("V","Assignment_",names(.)))
      data <- cbind(data, treatments_100)
      assignments <- data %>% select(starts_with("Assignment_")) %>% colnames()

      ## Create histograms of statistics: 
      ### Two treatments with binary or continouous variable: mean difference
      ### Two treatments with categorical non-binary variable: chi2 statistic
      ### More than two treatments with continouous variable: F statistic
      ### More than two treatments with categorical variable: chi2 statistic

      age_stats <- c()
      gender_stats <- c()
      ethnicity_stats <- c()
      education_stats <- c()
      ideology_stats <- c()
      partner_stats <- c()
      
      ### Two treatments

      if (n_treatments == 2) {
        
        for (assignment in assignments) {
          
          age_means <- aggregate(data$Age, data[assignment], mean, na.rm = T)
          age_diffs <- age_means[2,2] - age_means[1,2]
          age_stats <- c(age_stats, age_diffs)
        
          gender_means <- aggregate(data$Gender, data[assignment], mean, na.rm = T)
          gender_diffs <- gender_means[2,2] - gender_means[1,2]
          gender_stats <- c(gender_stats, gender_diffs)
          
          ethnicity_tbl <- table(data[["Ethnicity"]], data[[assignment]])
          ethnicity_chi2s <- chisq.test(ethnicity_tbl)[["statistic"]]
          ethnicity_stats <- c(ethnicity_stats, ethnicity_chi2s)
          
          education_tbl <- table(data[["Education"]], data[[assignment]])
          education_chi2s <- chisq.test(education_tbl)[["statistic"]]
          education_stats <- c(education_stats, education_chi2s)
          
          ideology_means <- aggregate(data$Ideology, data[assignment], mean, na.rm = T)
          ideology_diffs <- ideology_means[2,2] - ideology_means[1,2]
          ideology_stats <- c(ideology_stats, ideology_diffs)
          
          partner_means <- aggregate(data$Partner, data[assignment], mean, na.rm = T)
          partner_diffs <- partner_means[2,2] - partner_means[1,2]
          partner_stats <- c(partner_stats, partner_diffs)
          
        }
      }
      
      ### More than two treatments
      
      if (n_treatments > 2) {
        
        for (assignment in assignments) {
          
          age_form <- formula(paste("Age ~ ", assignment))
          age_anova <- aov(age_form, data = data)
          age_fs <- summary(age_anova)[[1]]["F value"][1,1]
          age_stats <- c(age_stats, age_fs)
          
          gender_tbl <- table(data[["Gender"]], data[[assignment]])
          gender_chi2s <- chisq.test(gender_tbl)[["statistic"]]
          gender_stats <- c(gender_stats, gender_chi2s)
          
          ethnicity_tbl <- table(data[["Ethnicity"]], data[[assignment]])
          ethnicity_chi2s <- chisq.test(ethnicity_tbl)[["statistic"]]
          ethnicity_stats <- c(ethnicity_stats, ethnicity_chi2s)
          
          education_tbl <- table(data[["Education"]], data[[assignment]])
          education_chi2s <- chisq.test(education_tbl)[["statistic"]]
          education_stats <- c(education_stats, education_chi2s)
        
          ideology_form <- formula(paste("Ideology ~ ", assignment))
          ideology_anova <- aov(ideology_form, data = data)
          ideology_fs <- summary(ideology_anova)[[1]]["F value"][1,1]
          ideology_stats <- c(ideology_stats, ideology_fs)
          
          partner_tbl <- table(data[["Partner"]], data[[assignment]])
          partner_chi2s <- chisq.test(partner_tbl)[["statistic"]]
          partner_stats <- c(partner_stats, partner_chi2s)
          
        }
      }

      statistics_df <- data.frame(age_stats, 
                                  gender_stats, 
                                  ethnicity_stats,
                                  education_stats, 
                                  ideology_stats,
                                  partner_stats)

      return(statistics_df)

    })
    
    n_treatments <- eventReactive(input$assign,{
      
      n_treatments <- input$treatments
      return(n_treatments)
      
    })
    
    ## Table for one assignment
      
    output$table <- renderTable({
      
      my_table <- data_assigned_once()
      my_table
      
    }, na = "", sanitize.text.function = function(x) x);
    
    ## Six plots for hundred assignments
    
    output$plot <- renderPlot({
  
      n_treatments <- n_treatments()
      my_table <- data_assigned_100()
      
      ### Different statistics plotted depending on treatment number
      
      if (n_treatments == 2) {
        
        age_plot <- ggplot(my_table, aes(x = age_stats)) + 
        geom_histogram() +
        theme_linedraw() +
        ggtitle("Age") +
        labs(y = "Frequency", 
             x = "Treatment mean diff.s") +
        xlim(-5, 5)
        
      gender_plot <- ggplot(my_table, aes(x = gender_stats)) + 
        geom_histogram() +
        theme_linedraw() +
        ggtitle("Gender") +
        labs(y = "Frequency", 
             x = "Treatment proportion diff.s") +
        xlim(-0.1, 0.1)
      
      ethnicity_plot <- ggplot(my_table, aes(x = ethnicity_stats)) + 
        geom_histogram()  +
        theme_linedraw() +
        ggtitle("Ethnicity") +
        labs(y = "Frequency", 
             x = "Chi-squared stat.s")
      
      education_plot <- ggplot(my_table, aes(x = education_stats)) + 
        geom_histogram()  +
        theme_linedraw() +
        ggtitle("Education") +
        labs(y = "Frequency", 
             x = "Chi-squared stat.s")
      
      ideology_plot <- ggplot(my_table, aes(x = ideology_stats)) + 
        geom_histogram()  +
        theme_linedraw() +
        ggtitle("Ideology") +
        labs(y = "Frequency", 
             x = "Treatment mean diff.s") +
        xlim(-0.5, 0.5)
      
      partner_plot <- ggplot(my_table, aes(x = partner_stats)) + 
        geom_histogram()  +
        theme_linedraw() +
        ggtitle("Partner presence") +
        labs(y = "Frequency", 
             x = "Treatment proportion diff.s") +
        xlim(-0.1, 0.1)
      
      }
      
      if (n_treatments > 2) {
        
        age_plot <- ggplot(my_table, aes(x = age_stats)) + 
          geom_histogram() +
          theme_linedraw() +
          ggtitle("Age") +
          labs(y = "Frequency", 
               x = "F stat.s")

        gender_plot <- ggplot(my_table, aes(x = gender_stats)) + 
          geom_histogram() +
          theme_linedraw() +
          ggtitle("Gender") +
          labs(y = "Frequency", 
               x = "Chi-squared stat.s")

        ethnicity_plot <- ggplot(my_table, aes(x = ethnicity_stats)) + 
          geom_histogram()  +
          theme_linedraw() +
          ggtitle("Ethnicity") +
          labs(y = "Frequency", 
               x = "Chi-squared stat.s")
        
        education_plot <- ggplot(my_table, aes(x = education_stats)) + 
          geom_histogram()  +
          theme_linedraw() +
          ggtitle("Education") +
          labs(y = "Frequency", 
               x = "Chi-squared stat.s")
        
        ideology_plot <- ggplot(my_table, aes(x = ideology_stats)) + 
          geom_histogram()  +
          theme_linedraw() +
          ggtitle("Ideology") +
          labs(y = "Frequency", 
               x = "F stat.s")

        partner_plot <- ggplot(my_table, aes(x = partner_stats)) + 
          geom_histogram()  +
          theme_linedraw() +
          ggtitle("Partner presence") +
          labs(y = "Frequency", 
               x = "Chi-squared stat.s")

      }
      
      grid.arrange(age_plot, gender_plot, ethnicity_plot, 
                   education_plot, ideology_plot, partner_plot,
                   ncol = 3)

    });
    
  }

shinyApp(ui, server)
