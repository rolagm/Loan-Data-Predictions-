#Case Study Scenario 1: Predicting Classification
  #Sample the data 
      set.seed(252)   
      sample<- sample(nrow(loan_data), 1000 )
      view(sample)
      df <- loan_data[sample, ]
      view(df)
      summary(df)
      install.packages('vtable')
      library(vtable)
      vtable(df)
  # dependent variable = credit.policy
      df$credit.policy <- as.factor(df$credit.policy)
  # independent variable = fico, dti
      # Replace Values Based on fico condition
        df2 <- df
        vtable(df2)
        df2$fico[df2$fico <= 669] <- 'F'
        df2$fico[df2$fico >= 670 & df2$fico <= 739] <- 'G'
        df2$fico[df2$fico >= 740 & df2$fico <= 799] <- 'VG'
        df2$fico[df2$fico >= 800 & df2$fico <= 822]  <- 'E'
        
        print(df)
        df2$fico <- as.factor(df2$fico)
        vtable(df2)
      # Replace Values Based on dti condition
        df2$dti[df2$dti <= 15] <- 'I'
        df2$dti[df2$dti > 15 & df2$dti <= 29.6] <- 'NI'
        df2$dti <- as.factor(df2$dti)
        view(df2)
        vtable(df2)
      #Summary of df2
        summary(df2)
    #Graphs Credit.Policy Facet Wrap
        ggplot(df2, aes(x=credit.policy, fill= fico)) + 
          geom_bar() + 
          geom_text(aes(label= after_stat(count)), 
                    stat='count', 
                    position = position_stack(vjust = 0.5)) +
          labs(title = 'Denied and Accept Loans By DTI',
               subtitle = 'Separated by FICO'  )+
          facet_wrap(~dti) 
          
        # confirm this:
         df2 %>%
            group_by(fico, dti, credit.policy ) %>%
            dplyr::summarise(freq = n()) %>%
            pivot_wider(names_from = credit.policy, values_from = 'freq') %>% 
            group_by(fico)
        # Fico and dti to annual.inc
          #change log.annual.inc to annual.inc
            df2$annual.inc <-  exp(df2$log.annual.inc)
            df2$annual.inc <- as.numeric(df2$annual.inc)
            view(df2)
          #Boxplot fico to annual.inc
            ggplot(data =df2) +
              geom_boxplot(aes(x= fico, y=log.annual.inc, fill= fico))
            ggplot(data =df2) +
              geom_boxplot(aes(x= dti, y=log.annual.inc, fill= dti))
        #Method 1: Linear Regression Formula Credit.Policy = FICO + DTI
            df2$credit.policy<-factor(df2$credit.policy, c(0,1), labels=c('Denied', 'Accepted'))
            df2$credit.policy <-as.character(df$credit.policy)
            str(df2)
            o1 <- lm(credit.policy ~ fico + dti + log.annual.inc , data = df2)
            summary(o1)
            o2 <- lm(credit.policy ~ fico, data = df2)
            summary(o2)
         # Doing a Linear Regression is not accurate enough
            ## r-sq = 0.1159 and p-value is <2.2e-16
        #Method 2: Decision Tree
            install.packages('FSelector')
            install.packages('rpart')
            install.packages('caret', dependencies = TRUE)
            install.packages('rpart.plot')
            install.packages('data.tree')
            install.packages('xlsm')
            install.packages('caTools')
            library(FSelector)
            library(rpart)
            library(caret)
            library(rpart.plot)
            library(data.tree)
            library(caTools)
            set.seed(123)
            df3<-df
            df3<- select(df3, credit.policy, dti, fico, log.annual.inc)
            str(df3)
            df3<- mutate(df3, credit.policy = as.factor(credit.policy))
            sample1 = sample.split(df3$credit.policy, SplitRatio = .70)
            train = subset(df3, sample1 == TRUE)
            test = subset(df3, sample1 == FALSE)
            
            #Training the Tree
            tree <- rpart(credit.policy ~., data = train)
            #Predictions 
            tree.credit.policy.pred <- predict(tree, test, type = 'class')
            #Evaluate using Confusion Matrix 
            confusionMatrix(tree.credit.policy.pred, test$credit.policy)
          ##Decision Tree is more accurate than Regression ie a better model to use
              #Visual the tree 
            prp(tree)