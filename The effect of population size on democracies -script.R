library(tidyverse)
library(ggthemes)

#setwd("C:/Users/luisn/OneDrive - Universität St.Gallen/FS22/Democratic Choice and Welfare/Relevant Litterature")

------------------------------------------------------------------------------------------
  ## 
  # code for Figure 1: The size of parliaments (log-log plot)
  
# information on the data:
# The list_of_democracies comes from the economists democracy index ranking of 2021
  #https://www.eiu.com/n/campaigns/democracy-index-2021/
# The data for size_of_parliament comes from a wikipedia-table.
  #https://en.wikipedia.org/wiki/List_of_legislatures_by_number_of_members
# The data was gathered and cleaned using excel and saved as a csv file. 
#The respective files can be downloaded from the repository.
 
# load data from files
d_index <- read.csv2("list_of_democracies.csv",header = TRUE)
parl_size <- read.csv2("size_of_parliament .csv",header = TRUE)

# rename columns
colnames(parl_size) <- c("country","size_parlament","population")
colnames(d_index) <- c("country","democracy_index")

# individually fix non-matching country names
parl_size[25,1] <- "United States of America"
parl_size[50,1] <- "South Korea"
parl_size[153,1] <- "Cabo Verde"
parl_size[168,1] <- "Bosnia and Hercegovina"
parl_size[174,1] <- "Gambia"

# merge to generate the relevant data frame
data <- left_join(d_index,parl_size, by=c("country"="country"))

# convert the data to numeric values
data[,2]<- as.numeric(gsub(",", "", data[,2]))
data[,3]<- as.numeric(gsub(",", "", data[,3]))                  
data[,4]<- as.numeric(gsub(",", "", data[,4]))  

# apply log for log-log plot
size_parlament <- log(data$size_parlament)
population <- log(data$population)
# create data frame for log-log plot
df <- data.frame(logPopulation=population,logParlamentSize=size_parlament)
# estimate a linear model
reg <- lm(size_parlament~population,data = df)
# add fitted values to data frame
df <- data.frame(logPopulation=population,logParlamentSize=size_parlament,fit=reg$fitted.values)
# look at the estimated parameters for the model: P = a*N^theta
a <- exp(reg$coefficients[1])
a
theta <- reg$coefficients[2]
theta

# plot the log-log plot
ggplot(data=df, aes(x=logPopulation))+
  geom_point(aes(y=logParlamentSize), color="blue")+
  geom_line(aes(y=fit),color="red")+
  labs(y = "number of MPs", x = "population size")+
  ggtitle(" The size of parliaments (log-log plot)")+
  theme_economist() +
  scale_colour_economist()


------------------------------------------------------------------------------------------
  ## 
  # code for Figure 2: Condorcet's Jury Theorem visualized
 
# we write a function to calculate the probability Pn that the majority vote 
# chooses the correct alternative.
  
Condrcet_jury <- function(n,p) {
  
  Pn <- 0
  voters <- c(seq(1,n,1))
  voter_to_win <- voters[((length(voters)+1)/2):length(voters)]
  for(i in voter_to_win){
    
    x <- choose(n,i)*(p)^(i)*(1-p)^(n-i)
    Pn <- Pn +x
    
  }
  return(Pn)
}
# define the parameters:
n <- c(seq(1,1000,2)) # the size of the jury

p1 <- 0.51 # different levels of competence
p2 <- 0.53
p3 <- 0.55

# create a data frame
Condorcets_Jury_Teorem <- data.frame(number_of_voters=n,Pn_0.51=NA, Pn_0.55=NA, Pn_0.6=NA)

# and run the calculation
counter <- 1
for (i in n){
  Condorcets_Jury_Teorem$Pn_0.51[counter] <- Condrcet_jury(i,p1)
  Condorcets_Jury_Teorem$Pn_0.55[counter] <- Condrcet_jury(i,p2)
  Condorcets_Jury_Teorem$Pn_0.6[counter] <- Condrcet_jury(i,p3)
  counter <- counter + 1
}

# plot the results
ggplot(data = Condorcets_Jury_Teorem, aes(x = number_of_voters))+
  geom_line(aes(y=Pn_0.51,color="0.51"))+
  geom_line(aes(y=Pn_0.55,color="0.55"))+
  geom_line(aes(y=Pn_0.6,color="0.6"))+
  labs(y = "Pn", x = "n")+
  labs(color = "Competence:")+
  ggtitle("Condorcet's Jury Theorem visualized")+
  theme_economist() +
  scale_colour_economist()



