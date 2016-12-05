library(tidyverse)
library(apaTables)


bfi_data <- psych::bfi
# View(bfi_data)

categorical_variables <-select(bfi_data, gender, education, age)

categorical_variables$gender <- as.factor(categorical_variables$gender)
levels(categorical_variables$gender) <- list("Male"=1,"female"=2)

#str(categorical_variables)

agreeableness_items <- select (bfi_data, A1, A2, A3, A4, A5)
extraversion_items <-select (bfi_data, E1, E2, E3, E4, E5)
neuroticism_items <- select (bfi_data, N1, N2, N3, N4, N5)

#agreeableness
agreeableness_items <- mutate(agreeableness_items,A1=7-A1)
str(agreeableness_items)

#extraversion
extraversion_items <- mutate(extraversion_items,E1=7-E1)
extraversion_items <- mutate(extraversion_items,E2=7-E2)
str(extraversion_items)

#neuroticism - no reverse

#turn qs into 1 numerical value
extraversion <- psych::alpha(as.data.frame(extraversion_items), check.keys=FALSE)$scores
agreeableness <- psych::alpha(as.data.frame(agreeableness_items), check.keys=FALSE)$scores
neuroticism <- psych::alpha(as.data.frame(neuroticism_items),check.keys=FALSE)$scores

#create analytic data
analytic_data <- cbind(agreeableness, extraversion, neuroticism, categorical_variables)

#View(analytic_data)

write_csv(analytic_data,path="analytic_data.csv")


#Run analyses

#cor table 1
analytic_data2 <- select(analytic_data, agreeableness, extraversion, neuroticism, education, age)
#View(analytic_data2)
apa.cor.table(analytic_data2,filename="Table_1.doc",table.number=1)

#men over 40
analytic_data_male <- filter(analytic_data, gender=="Male")
analytic_data_male <- select(analytic_data_male, -gender)
analytic_data_male <- filter(analytic_data_male, age>40)
#View(analytic_data_male)
apa.cor.table(analytic_data_male,filename="Table_2.doc",table.number=2)

write_csv(analytic_data_male,path="analytic_data_male.csv")

#scatter plot
my.scatter <- qplot(x=agreeableness,y=extraversion,data=analytic_data_male)
my.scatter <- my.scatter + geom_smooth(method = "lm", se = FALSE, color='black')
my.scatter <- my.scatter + labs(title="Relation between agreeableness and extraversion for men over 40",
                                x="Agreeableness",y="Extraversion")
my.scatter <- my.scatter + theme_classic()
my.scatter <- my.scatter + theme(axis.line.x = element_line(colour = 'black',size=0.5, linetype='solid'),
                                 axis.line.y = element_line(colour = 'black', size=0.5, linetype='solid'))

print(my.scatter)
ggsave(filename="Figure1.pdf", plot=my.scatter, width=6,height=6, units="in")
