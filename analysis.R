## analysis.R
## Analyze the data frame output by cleaning_script.R

library(stargazer)
library(ggplot2)
source("cl.R")

############################################################################################################
## Read in data
############################################################################################################
## Read in and prepare data
data <- read.csv("mail_data_clean_experiment.csv")

mail_final <- data[,c("received_response","treat","is_sf","persona","beds","baths","sqft","price","npics",
                      "days_since_posting","posting_id")]
mail_final <- mail_final[complete.cases(mail_final),]
mail_final$posting_id <- factor(as.character(mail_final$posting_id))

############################################################################################################
## Test Randomization
############################################################################################################
tim <- subset(data,treat == 1)
pat <- subset(data,treat == 0)

t.test(tim$beds, pat$beds)
t.test(tim$baths, pat$baths)
t.test(tim$sqft, pat$sqft)
t.test(tim$price, pat$price)
t.test(tim$npics, pat$npics)
t.test(tim$days_since_posting, pat$days_since_posting)

############################################################################################################
## Regression Output
############################################################################################################

## Simplest regression
model1 <- lm(received_response ~ treat + treat*is_sf + is_sf + persona, mail_final)        
model1_cl <- cl(lm(received_response ~ treat + treat*is_sf + is_sf + persona, mail_final), mail_final$posting_id)

## Regression with controls
model2 <- lm(received_response ~ treat + is_sf + treat*is_sf + persona + beds + baths + sqft + price + npics + days_since_posting, mail_final)
model2_cl <- cl(lm(received_response ~ treat + is_sf + treat*is_sf + persona + 
              beds + baths + sqft + price + npics + days_since_posting, mail_final), mail_final$posting_id)

## Output results
stargazer(model1, model2, title="Results", align=TRUE, report = "vc*sp", type = 'html', out="results.htm")

############################################################################################################
## Other Graphics
############################################################################################################

## Function to make graphs
get_prop_graph <- function(x, fill = "received_response", df = data, binsize = NA){
        df <- as.data.frame(table(df[,x], df[,fill]))
        names(df) <- c(x, fill, "Frequency")
        ggplot(data = df, aes_string(x = x, y = "Frequency", fill = fill)) +
                geom_histogram(stat = "identity", position = position_dodge()) + facet_grid(reformulate(".",fill)) 
        }

## Covariate Checks
ggsave(filename="persona_treat.jpg", plot=get_prop_graph(x = "persona", fill = "treat"))
ggsave(filename="npics_treat.jpg", get_prop_graph(x = "npics", fill="treat"))
ggsave(filename="beds_treat.jpg", get_prop_graph(x = "beds", fill = "treat"))
ggsave(filename="is_sf_treat.jpg", get_prop_graph(x = "is_sf", fill = "treat"))
ggsave(filename="baths_treat.jpg", get_prop_graph("baths", fill = "treat"))
ggsave(filename="days_since_posting_treat.jpg", get_prop_graph("days_since_posting", fill = "treat"))

## Response Splits
ggsave(filename="response_treat.jpg", plot =get_prop_graph(x = "received_response", fill = "treat"))

ggsave(filename="persona_response.jpg", plot=get_prop_graph(x = "persona"))
ggsave(filename="beds_response.jpg", plot=get_prop_graph(x = "beds"))
ggsave(filename="is_sf_response.jpg", plot=get_prop_graph(x = "is_sf"))
ggsave(filename="baths_response.jpg", plot=get_prop_graph(x = "baths"))
ggsave(filename="days_since_posting_response.jpg", plot=get_prop_graph(x = "days_since_posting"))
ggsave(filename="npics_response.jpg", plot=get_prop_graph(x = "npics"))

## Duration by Treatment
data2 <- data
data2$ln_time_before_response <- log(data2$duration)
mean1 <- mean(subset(data2, treat == 1)$ln_time_before_response,na.rm = T)
mean2 <- mean(subset(data2, treat == 0)$ln_time_before_response,na.rm = T)

means <- data.frame(the_mean = c(mean1, mean2), treat = c(1,0))

data2$treat <- factor(data2$treat)

time_before_response <- ggplot(data = data2, aes_string(x = "ln_time_before_response", fill = "treat")) + 
        geom_histogram(position = position_dodge()) + facet_grid(reformulate(".","treat")) + 
        geom_vline(data=means, aes(xintercept = the_mean), colour="black", size = 1) + 
        xlab("Log of Time Before Response in Minutes") + labs(fill="Is Tom")
ggsave(filename="time_before_response_treat.jpg", plot = time_before_response)


## Tables
table(data$treat, data$persona)

table(data$treat, data$persona, data$is_sf)


