library(dplyr)

monitoring_data <- Modified_Data_New

# Group by subpopulation and calculate the average of first.num.stems
average_first_num_stems <- monitoring_data %>%
  group_by(subpop) %>%
  summarise(avg_first_num_stems = mean(first.num.stems, na.rm = TRUE))

# Group by subpopulation, remove NAs from r.t., and create a list of non-NA LRR values
list_of_LRR <- monitoring_data %>%
  group_by(subpop) %>%
  summarise(list_r_t = list(na.omit(as.numeric(`r.t.`))))  # Convert to numeric here

#Assuming I have LRR values, initial population size (N0), and the number of years to project (50 years)
LRR <- list_of_LRR$list_r_t[19] #change this number for each subpopulation

LRR_vector <- unlist(LRR)

num_iterations <- 50 ##interested in 50 year time horizon
projected_population <- numeric(num_iterations)
sims <- 1000 ##perform 1000 simulations

#initialize the results vector
results <- numeric(sims)

for(a in 1:sims){
  NO <- average_first_num_stems$avg_first_num_stems[19] ##change this number for each subpop. starting pop size.
  for (i in 1:num_iterations) {
    # Randomly choose a different LRR value from the observed data
    random_LRR <- sample(LRR_vector, size = 1, replace = TRUE)
    
    # Stochastic population projection formula
    projected_population[i] <- NO * exp(random_LRR)
    
    # Update N0 for the next iteration
    NO <- projected_population[i]
  }
  results[a] <- NO #results is a vector with ending population for each sim
}

count <- sum(results < 10) #number of simulations that result in less than 10 individuals

#end population should be ending population size
#need to incorporate K, carrying capacity somehow?? not sure if I have the data

endpop <- mean(results)

print(endpop) ##ending population size
print(count) ##number of sims that result in <10 individuals
print(count/1000) ##calculate extinction risk by dividing by 1000


###########################################


# INCORPORATING WOODY STUFF (DON'T EDIT ABOVE THIS LINE!!!)


# Create subsets of data for each woody category
woody_none <- monitoring_data %>% filter(woody == "none")
woody_high <- monitoring_data %>% filter(woody == "high")
woody_low <- monitoring_data %>% filter(woody == "low")

#START RUNNING HERE:
#Run new simulations with random LRRs from each category to see how this affects population sizes:

LRR_values <- na.omit(woody_none$r.t.) #change woody category depending on simulation

#Assuming I have LRR values, initial population size (N0), and the number of years to project (e.g., 50 years)

num_iterations <- 50 ##interested in 50 year time horizon
projected_population <- numeric(num_iterations)
sims <- 1000 ##perform 1000 simulations

#initialize the results vector
results <- numeric(sims)

for(a in 1:sims){
  NO <- average_first_num_stems$avg_first_num_stems[19] ##change this number for each subpop. starting pop size.
  for (i in 1:num_iterations) {
    # Randomly choose a different LRR value from the observed data
    random_LRR <- sample(LRR_values, size = 1, replace = TRUE)
    
    # Stochastic population projection formula
    projected_population[i] <- NO * exp(random_LRR)
    
    # Update N0 for the next iteration
    NO <- projected_population[i]
  }
  results[a] <- NO #results is a vector with ending population for each sim
}

count <- sum(results < 10) #number of simulations that result in less than 10 individuals

print(count)
print(count/1000) ##calculate extinction risk by dividing by 1000

###########################################

#Make sure to import excel file "R_Data_for_ANOVA")
#Run Anova to see how Extinction Risk is affected by woody encroachment:

library(car)

# Fit ANOVA model
model <- aov(ext_risk ~ woody, data = R_Data_for_ANOVA)

# Check homogeneity of variances
leveneTest(ext_risk ~ woody, data = R_Data_for_ANOVA)

# Check normality
shapiro.test(residuals(model))

#perform ANOVA
summary(model)

# If ANOVA is significant, conduct post-hoc tests
posthoc <- TukeyHSD(model)
print(posthoc)

# Box Plot:

library(ggplot2)
library(scales) # for percent formatting

ggplot(R_Data_for_ANOVA, aes(woody, ext_risk)) +
  geom_boxplot() +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(x = "Woody Encroachment", y = "Extinction Risk")


#ANOVA to see how Woody encroachment affects LRR Directly:

model2 <- aov(r.t. ~ woody, data = monitoring_data)

#perform ANOVA
summary(model2)


library(ggplot2)
library(scales) # for percent formatting

ggplot(monitoring_data, aes(woody, r.t.)) +
  geom_boxplot() +
  labs(x = "Woody Encroachment", y = "LRR")

