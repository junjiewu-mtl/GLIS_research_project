## Junjie Wu  
## cn_preliminary_analysis Shiny App

rm(list = ls())

#setwd('~/Desktop/GLIS Research/GLIS_research_project/script/cn_preliminary_analysis')

library(rsconnect)
library(data.table)
library(shiny)
library(rpart)
library(rpart.plot)
library(plotly)
library(arsenal)
library(ggplot2)
library(rattle)
library(RColorBrewer)
library(statsr)
library(tidyverse)
library(arules)
library(arulesViz)



# Define UI for application 
ui <-
    navbarPage("cn_preliminary_analysis",
               tabPanel("H1 Decision Tree: Individual Response",
                        mainPanel(
                            plotOutput("h1_ind",
                                       width = "100%", height = "600px")
                        )
               ),
               tabPanel("H1 Bayes Inference",
                        sidebarLayout(
                            sidebarPanel(
                                # Input: Selector for choosing dataset ----
                                selectInput(inputId = "gender",
                                            label = "Predicted sum of aesthetic and sustainable values:",
                                            choices = c("Male",
                                                        "Female")),
                            ),
                            
                            mainPanel(
                                verbatimTextOutput('h1_bae'),
                                verbatimTextOutput("h1_pred"),
                                #width = "100%", height = "600px")
                            )
                        )
               ),
               tabPanel("H1 Apriori",
                        mainPanel(width = 10,
                            verbatimTextOutput("h1_ar")
                        )
               ),
               tabPanel("H1 Decision Tree: aes and sus score",
                        mainPanel(
                            plotOutput("h1_sum",
                                       width = "100%", height = "600px")
                        )
               ),
               tabPanel("H2 Linear Regression",
                        mainPanel(
                            plotlyOutput("h2",
                                         width = "100%", height = "600px")
                        )
               ),
               tabPanel("H3 Linear Regression",
                        mainPanel(
                            plotlyOutput("h3",
                                         width = "100%", height = "600px")
                        )
               ),
               tabPanel('Descriptive Analysis: Q31 & Q38',
                        verbatimTextOutput('summary'))
    )




# Define server logic required to draw a histogram
server <- function(input, output) {
    
    ################### scatter plot function: ggplotRegression ################### 
    ggplotRegression <- function (fit, correlation_accuracy) {
        
        require(ggplot2)
        
        ggplot(fit$model, aes_string(x = names(fit$model)[2], y = names(fit$model)[1])) + 
            geom_point() +
            stat_smooth(method = "lm", col = "red") +
            labs(title = paste("Adj R2 = ",signif(summary(fit)$adj.r.squared, 5),
                               "Intercept =",signif(fit$coef[[1]],5 ),
                               " Slope =",signif(fit$coef[[2]], 5),
                               " Accuarcy rate=", round(correlation_accuracy, digits = 2)))
    }
    
    output$h1_ind <- renderPlot({
        dt_a <- fread('cn_h1_ind.csv')
        ## 75% of the sample size
        smp_size <- floor(0.75 * nrow(dt_a))
        ## set the seed to make your partition reproducible
        set.seed(123)
        train_ind <- sample(seq_len(nrow(dt_a)), size = smp_size)
        
        train <- dt_a[train_ind, ]
        test <- dt_a[-train_ind, ]
        
        # regression tree
        rtree.dt_a = rpart(Q41.Sex ~ ., data=train, method = 'class',   minsplit = 2, 
                           minbucket = 1, 
                           cp = 0.008)
        # Plot the tree using prp command defined in rpart.plot package
        t_pred = predict(rtree.dt_a,test,type="class")
        fancyRpartPlot(rtree.dt_a, main = c('accuracy_rate',round(mean(test$Q41.Sex == t_pred), digits = 2)))
    })
    
    output$h1_sum <- renderPlot({
        dt_a <- fread('cn_h1_sum.csv')
        ## 75% of the sample size
        smp_size <- floor(0.75 * nrow(dt_a))
        ## set the seed to make your partition reproducible
        set.seed(123)
        train_ind <- sample(seq_len(nrow(dt_a)), size = smp_size)
        
        train <- dt_a[train_ind, ]
        test <- dt_a[-train_ind, ]
        
        # regression tree
        rtree.dt_a = rpart(Q41.Sex ~ ., data=train, method = 'class', parms = list(split = "information"))
        # Plot the tree using prp command defined in rpart.plot package
        t_pred = predict(rtree.dt_a,test,type="class")
        mean(test$Q41.Sex == t_pred) 
        fancyRpartPlot(rtree.dt_a, main = c('accuracy_rate',round(mean(test$Q41.Sex == t_pred), digits = 2)))
    })
    

    output$h1_bae <- renderPrint({
        
        dt_a_s <- fread('cn_h1_bae.csv')
        
        bf <- bayes_inference(y = score_sum, x = Q41.Sex, data = dt_a_s, 
                              statistic = "mean", 
                              type = "ht", alternative = "twosided", null = 0, 
                              prior = "JZS", rscale = 1, 
                              method = "theoretical", show_plot = FALSE)
    })
    
    genderInput <- reactive({
        switch(input$gender,
               "Male" = 1,
               "Female" = 2)
    })
    
    output$h1_pred <- renderPrint({
        dt_a_s <- fread('cn_h1_bae.csv')
        #predict mean sum of male
        gender <- genderInput()
        
        dt_male <- dt_a_s[Q41.Sex == gender,]
        
        male_post <- bayes_inference(y = score_sum, data = dt_male, 
                                     statistic = "mean", 
                                     type = "ci", mu_0 = 43.9, 
                                     prior = "JZS", rscale = 1, 
                                     method = "simulation")
        
        m_samples = as.data.frame(male_post$samples)
        m_nsim = nrow(m_samples)
        m_samples = mutate(m_samples, y_pred = rnorm(m_nsim, mu, sqrt(sig2)))
        
        ggplot(data = m_samples, aes(x = y_pred)) + 
            geom_histogram(aes(y = ..density..), bins = 100) +
            geom_density() + 
            xlab(expression(y[new]))
        
        dplyr::select(m_samples, mu, y_pred) %>%
            map(quantile, probs=c(0.025, 0.50, 0.975))
    })
    
    output$h1_ar <- renderPrint({
        dt_aes_sus <- fread('cn_h1_sum.csv')
        
        dt_aes_sus[,dt_aes_val:= cut(aes_val,4,include.lowest=TRUE,
                                     labels=c("Low", "Lower Middle", "Upper Middle","High"))]
        dt_aes_sus[,dt_sus_val:= cut(sus_val,4,include.lowest=TRUE,
                                     labels=c("Low", "Lower Middle", "Upper Middle","High"))]
        
        df <- dt_aes_sus %>% select(Q41.Sex, dt_sus_val, dt_aes_val)
        
        # association rule without split gender
        rules.male <- apriori(df, control = list(verbose=F),
                              parameter = list(supp = 0.1, conf = 0.01),
                              appearance = list(default="lhs", rhs = c("Q41.Sex=Male")))                           
        
        rules.female <- apriori(df, control = list(verbose=F),
                                parameter = list(supp = 0.1, conf = 0.01),
                                appearance = list(default="lhs", rhs = c("Q41.Sex=Female")))
        
        
        inspect(head(sort(rules.male, by = "confidence")))
        inspect(head(sort(rules.female, by = "confidence")))
        
    })
    
    
    output$h2 <- renderPlotly({
        dt_2 <- fread('cn_h2.csv')
        
        ## 75% of the sample size
        smp_size <- floor(0.75 * nrow(dt_2))
        
        ## set the seed to make your partition reproducible
        set.seed(123)
        train_ind <- sample(seq_len(nrow(dt_2)), size = smp_size)
        
        train <- dt_2[train_ind, ]
        test <- dt_2[-train_ind, ]
        
        linear_2 <- lm(con_inno ~ sus_aes, data = train)
        
        p_lm <- predict(linear_2, test)
        actuals_preds <- data.frame(cbind(actuals=test$con_inno, predicteds=p_lm))
        correlation_accuracy <- cor(actuals_preds)[1,2] 
        
        ggplotRegression(linear_2, correlation_accuracy)
    })
    
    output$h3 <- renderPlotly({
        dt_3 <- fread('cn_h3.csv')
        
        ## 75% of the sample size
        smp_size <- floor(0.75 * nrow(dt_3))
        
        ## set the seed to make your partition reproducible
        set.seed(123)
        train_ind <- sample(seq_len(nrow(dt_3)), size = smp_size)
        
        train <- dt_3[train_ind, ]
        test <- dt_3[-train_ind, ]
        
        linear_3 <- lm(green_con ~ sus_val, data = train)
        
        p_lm <- predict(linear_3, test)
        actuals_preds <- data.frame(cbind(actuals=test$green_con, predicteds=p_lm))
        correlation_accuracy <- cor(actuals_preds)[1,2] 
        
        ggplotRegression(linear_3, correlation_accuracy)
    })
    
    output$summary <- renderPrint({
        dt <- fread('knn_imputed_cn_data.csv')[,-1]
        dt <- dt[,37:56]
        table_one <- arsenal::tableby(~.,dt)
        summary(dt)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

