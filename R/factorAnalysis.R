library(readstata13) # to load in .dta file
library(dplyr) # data manipulation
library(tidyr) # data manipulation
library(paran) # parallel analysis
library(psych) # factor / pPCA analysis
library(ordinal) # ordered probit mixed effects
library(MASS) # ordered probit
library(systemfit) # for seemingly unrelated regression 
library(texreg) # for tables
library(coefplot) # for coef plot
library(ggplot2) # for figures

# load data set
df <- readstata13::read.dta13("data/cocops_cg_stata13.dta", convert.factors = F) 
df$rowname <- rownames(df)

# Test for Missingness Patterns
varsCovered <- rowSums(!is.na(df))
prcntCovered <- varsCovered/(ncol(df)-1)

countryCoverage <- data.frame(df, prcntCovered, varsCovered)
countryCoverage <- countryCoverage %>%
  dplyr::select(rowname, country, varsCovered, prcntCovered, everything()) %>%
  tidyr::gather(varname, value, -c(1:4)) %>%
  dplyr::group_by(country, varname) %>%
  dplyr::summarize(varsMissing = sum(is.na(value))) %>%
  tidyr::spread(key = varname, value= varsMissing) %>%
  dplyr::ungroup() %>%
  dplyr::mutate(varsMissing = rowSums(.[,-1])) %>%
  dplyr::select(country,  varsMissing, everything())

# Serbia has 27618 missing responses, twice as many as the rest, so it will be removed
#df<- df %>% dplyr::filter(!country==8)

# Table 1: Performance Information use ---------------------------------------------

# select q9_1 - q9_8
piUse <- df %>% dplyr::select(contains("q9")) %>% 
  dplyr::filter(complete.cases(.))

# parallel analysis 
paran::paran(piUse, iterations = 1000, centile = 0, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 1234)
# --> 3 factors to retain 

# factor analysis
piFact <- factanal(~q9_1+ q9_2+ q9_3+ q9_4+ q9_5+ q9_6+ q9_7+ q9_8, factors=2, 
                   data=df, rotation="promax", scores = "regression", na.action=na.exclude)
piFact 
# --> q9_1 - q9_5 load on the first factor, q9_6 - q9_8 load on the second factor

# save scores
piScores <- data.frame(piFact$scores)
piScores$rowname <- rownames(piScores)

# Check if it holds for every country
piFactList <- list()
for(i in unique(df$country)) {
  piFactList[[i]] <- df %>% dplyr::filter(country==i) %>% 
    factanal(~q9_1+ q9_2+ q9_3+ q9_4+ q9_5+ q9_6+ q9_7+ q9_8, factors=2, 
             data=., rotation="promax", scores = "regression", na.action=na.exclude)
}

# does not hold for Spain, Italy, Portugal

# merge with main data set
df2 <- df %>% 
  dplyr::full_join(., piScores, by = "rowname") %>% 
  dplyr::select(rowname, Internal = Factor1, External = Factor2, everything()) 

# Table 2: Role-identities ---------------------------------------------------------

# select q5_1 - q5_8
ri <- df %>% dplyr::select(contains("q5")) %>% 
  dplyr::filter(complete.cases(.))

# parallel analysis 
paran::paran(ri, iterations = 1000, centile = 0, quietly = FALSE, 
      status = TRUE, all = TRUE, cfa = TRUE, graph = TRUE, color = TRUE, 
      col = c("black", "red", "blue"), lty = c(1, 2, 3), lwd = 1, legend = TRUE, 
      file = "", width = 640, height = 640, grdevice = "png", seed = 1234)
# --> 3 factors to retain, one is quite small

# factor analysis
riFact <- factanal(~q5_1+ q5_2+ q5_3+ q5_4+ q5_5+ q5_6+ q5_7+ q5_8, factors=3, 
                   data=df, rotation="promax", scores = "regression", na.action=na.exclude)
riFact
# --> q5_4, q5_5, q5_7 load on the first factor, q5_3, q5_6, q5_8 load on the second factor,
# q5_1 and q5_2 load on the third factor

# save scores
riScores <- data.frame(riFact$scores)
riScores$rowname <- rownames(riScores)

# merge with main data set
df2 <- df2 %>% 
  dplyr::full_join(., riScores, by = "rowname") %>% 
  dplyr::select(rowname, Internal, External, Manager = Factor2, Networker = Factor1,
                Bureaucrat = Factor3, everything()) 

# Table 3: Ordered probit regressions
roles <- df2 %>% dplyr::select(Manager,Networker,Bureaucrat)
policy <- df2 %>% dplyr::select(contains("q2_"))
country <- df2 %>% dplyr::select(country)

# Success depends on ability
m1 <- ordinal::clmm(as.factor(q25_1) ~ 
       Manager+Networker+Bureaucrat + (1|country), data=df3, link = "probit")

summary(m1)
# regForm <- paste("as.factor(q25_1) ~",paste(paste(names(roles),collapse="+"),
#                                             "factor(country)",
#                                             paste(names(policy),collapse="+"), sep = "+"))
# m1 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
# summary(m1)
# ctableM1 <- coef(summary(m1))
# p <- pnorm(abs(ctableM1[, "t value"]), lower.tail = FALSE) * 2
# ctableM1 <- cbind(ctableM1, "p value" = p)
# ctableM1[1:3,]

# -->  Manager strongest relation to Success depends on ability

# Thinking up new ideas are important
m2 <- ordinal::clmm(as.factor(q25_4) ~ 
                      Manager+Networker+Bureaucrat + (1|country), data=df3, link = "probit")

summary(m2)

# regForm <- paste("as.factor(q25_4) ~",paste(paste(names(roles),collapse="+"),
#                                              "factor(country)",
#                                              paste(names(policy),collapse="+"), sep = "+"))
# m2 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
# summary(m2)
# ctableM2 <- coef(summary(m2))
# p <- pnorm(abs(ctableM2[, "t value"]), lower.tail = FALSE) * 2
# ctableM2 <- cbind(ctableM2, "p value" = p)
# ctableM2[1:3,]

# -->  Networker strongest relation to Thinking up new ideas are important

# Avoid upset the status quo
m3 <- ordinal::clmm(as.factor(q25_5) ~ 
                      Manager+Networker+Bureaucrat + (1|country), data=df3, link = "probit")

summary(m3)

# regForm <- paste("as.factor(q25_5) ~",paste(paste(names(roles),collapse="+"),
#                                             "factor(country)",
#                                             paste(names(policy),collapse="+"), sep = "+"))
# m3 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
# summary(m3)
# ctablem3 <- coef(summary(m3))
# p <- pnorm(abs(ctablem3[, "t value"]), lower.tail = FALSE) * 2
# ctablem3 <- cbind(ctablem3, "p value" = p)
# ctablem3[1:3,]

# -->  Manager strongest relation to Avoid upset the status quo

# Motivation: High Income
m4 <- ordinal::clmm(as.factor(q24_2) ~ 
                      Manager+Networker+Bureaucrat + (1|country), data=df3, link = "probit")

summary(m4)

# regForm <- paste("as.factor(q24_2) ~",paste(paste(names(roles),collapse="+"),
#                                             "factor(country)",
#                                             paste(names(policy),collapse="+"), sep = "+"))
# m4 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
# summary(m4)
# ctablem4 <- coef(summary(m4))
# p <- pnorm(abs(ctablem4[, "t value"]), lower.tail = FALSE) * 2
# ctablem4 <- cbind(ctablem4, "p value" = p)
# ctablem4[1:3,]

# -->  Manager strongest relation to Motivation: High Income

# Motivation: Opportunities to help others
m5 <- ordinal::clmm(as.factor(q24_3) ~ 
                      Manager+Networker+Bureaucrat + (1|country), data=df3, link = "probit")

summary(m5)

# regForm <- paste("as.factor(q24_3) ~",paste(paste(names(roles),collapse="+"),
#                                             "factor(country)",
#                                             paste(names(policy),collapse="+"), sep = "+"))
# m5 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
# summary(m5)
# ctablem5 <- coef(summary(m5))
# p <- pnorm(abs(ctablem5[, "t value"]), lower.tail = FALSE) * 2
# ctablem5 <- cbind(ctablem5, "p value" = p)
# ctablem5[1:3,]
# -->  Networker and Bureaucrat strongest relation to Motivation: Opportunities to help others

# Motivation: Job security
m6 <- ordinal::clmm(as.factor(q24_4) ~ 
                      Manager+Networker+Bureaucrat + (1|country), data=df3, link = "probit")

summary(m6)

# regForm <- paste("as.factor(q24_4) ~",paste(paste(names(roles),collapse="+"),
#                                             "factor(country)",
#                                             paste(names(policy),collapse="+"), sep = "+"))
# m6 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
# summary(m6)
# ctablem6 <- coef(summary(m6))
# p <- pnorm(abs(ctablem6[, "t value"]), lower.tail = FALSE) * 2
# ctablem6 <- cbind(ctablem6, "p value" = p)
# ctablem6[1:3,]

# -->  Bureaucrat strongest relation to Motivation: Job security

# Motivation: Room to make decisions
m7 <- ordinal::clmm(as.factor(q24_5) ~ 
                      Manager+Networker+Bureaucrat + (1|country), data=df3, link = "probit")

summary(m7)

# regForm <- paste("as.factor(q24_5) ~",paste(paste(names(roles),collapse="+"),
#                                             "factor(country)",
#                                             paste(names(policy),collapse="+"), sep = "+"))
# m7 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
# summary(m7)
# ctablem7 <- coef(summary(m7))
# p <- pnorm(abs(ctablem7[, "t value"]), lower.tail = FALSE) * 2
# ctablem7 <- cbind(ctablem7, "p value" = p)
# ctablem7[1:3,]

# -->  Manager and Bureaucrat strongest relation to Motivation: Room to make decisions

# Motivation: Useful for society
m8 <- ordinal::clmm(as.factor(q24_7) ~ 
                      Manager+Networker+Bureaucrat + (1|country), data=df3, link = "probit")

summary(m8)

# regForm <- paste("as.factor(q24_7) ~",paste(paste(names(roles),collapse="+"),
#                                             "factor(country)",
#                                             paste(names(policy),collapse="+"), sep = "+"))
# m8 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
# summary(m8)
# ctablem8 <- coef(summary(m8))
# p <- pnorm(abs(ctablem8[, "t value"]), lower.tail = FALSE) * 2
# ctablem8 <- cbind(ctablem8, "p value" = p)
# ctablem8[1:3,]

#-->  Networker strongest relation to Motivation: Useful for society

# Socio-demographic factors -----------------------------------------------

# Hierarchal position
df2 <- df2 %>% dplyr::mutate(hierach = replace(q4, q4 %in% 2:3, 0))

# Educational attainment
df2 <- df2 %>% dplyr::mutate(educat = replace(edu, edu==1, 0)) %>% 
  dplyr::mutate(educat = replace(educat, educat %in% 2:3, 1))

# Seniority
df2 <- df2 %>% dplyr::mutate(senior = replace(q30_1, q30_1 == 1, 0)) %>% 
  dplyr::mutate(senior = replace(senior, senior %in% 2:3, 1)) %>% 
  dplyr::mutate(senior = replace(senior, senior %in% 4:5, 2)) %>% 
  dplyr::mutate(senior = as.factor(senior))

# Private sector job experience
df2 <- df2 %>% dplyr::mutate(private = replace(q31_1, q31_1 %in%  1:2, 0)) %>% 
  dplyr::mutate(private = replace(private, private %in% 3:4, 1)) %>% 
  dplyr::mutate(private = replace(private, private %in% 5:6, 2)) %>% 
  dplyr::mutate(private = as.factor(private))


# Organizational and Contextual factors -----------------------------------------------

# Organization size
df2 <- df2 %>% dplyr::mutate(size = replace(q3, q3 %in% 1:2, 0)) %>% 
  dplyr::mutate(size = replace(size, size %in% 3:4, 1)) %>% 
  dplyr::mutate(size = replace(size, size %in% 5:6, 2)) %>% 
  dplyr::mutate(size = as.factor(size))

# Organization type
df2 <- df2 %>% dplyr::mutate(agency = replace(q1, q1 %in% c(1,3), 0)) %>% 
  dplyr::mutate(agency = replace(agency, agency == 2, 1))

# Goal clarity
goals <- df2 %>% dplyr::select(q8_1, q8_2) 

# pPCA
goalsPca <- psych::polychoric(goals)
goalsPca <- psych::principal(goalsPca$rho)
# --> PC1 accounts for 92% of the variance

# polychoric factor analysis
goalsFaPoly <- psych::fa.poly(goals)
goalsFaPoly
# --> MR1 accounts for 83% of the variance

goalClarity <- data.frame(goalsFaPoly$scores$scores)
goalClarity$rowname <- rownames(goalClarity)
colnames(goalClarity) <- c("goalClarity", "rowname")

# Financial distress
cutback <- df2 %>% dplyr::select(contains("q21_")) 

#pPCA 
distressPca <- psych::polychoric(cutback)
distressPca <- psych::principal(distressPca$rho)
# --> PC1 accounts for 41% of the variance

# polychoric factor analysis
distressFaPoly <- psych::fa.poly(cutback)
distressFaPoly
# --> MR1 accounts for 34% of the variance

distress <- data.frame(distressFaPoly$scores$scores)
distress$rowname <- rownames(distress)
colnames(distress) <- c("distress", "rowname")

# Networking complexity
interaction <- df2 %>% dplyr::select(contains("q10_")) %>% 
  dplyr::select(-q10_1, -q10_3, -q10_4) %>% 
  dplyr::mutate(q10_10 = replace(q10_10, q10_10 == 7, NA))

#pPCA 
interactionPca <- psych::polychoric(interaction)
interactionPca <- psych::principal(interactionPca$rho)
interactionPca
# --> PC1 accounts for 33% of the variance

# polychoric factor analysis
interactionFaPoly <- psych::fa.poly(interaction)
interactionFaPoly
# --> MR1 accounts for 27% of the variance

networkComplex <- data.frame(interactionFaPoly$scores$scores)
networkComplex$rowname <- rownames(networkComplex)
colnames(networkComplex) <- c("networkComplex", "rowname")


# merge all with df2
df2 <- df2 %>%dplyr::full_join(., goalClarity) %>% 
  dplyr::full_join(., distress) %>% 
  full_join(., networkComplex)

# Table 3: Seemingly unrelated regression  --------------------------------
# dftest <- df2 %>% 
#   dplyr::select(rowname, contains("q2_")) %>% 
#   tidyr::gather(type, value, q2_1:q2_o) %>%
#   tidyr::separate(type, c("variable", "policy")) %>%
#   tidyr::spread(variable, value, convert = TRUE) %>% 
#   dplyr::filter(!policy=="o") %>% 
#   dplyr::filter(q2==2)

# Group center variables
df2 <- df2 %>% 
  dplyr::select(rowname, country, Internal, External, 
                Bureaucrat, Manager, Networker, private, agency, size, 
                #educat, hierach, senior, distress, networkComplex
                goalClarity) %>% 
  dplyr::group_by(country) %>% 
  dplyr::mutate_each(funs(center = scale(.) %>% as.vector), -rowname, 
                     #-educat, -hierach, -senior, 
                     -private, -agency, -size) %>% 
  dplyr::ungroup()

# Percentage of Missing variables
df2 %>% 
  dplyr::summarise_each(., funs(sum(!is.na(.))/length(.))) %>% t() %>% View()

# Model Equations

r1 <- Internal_center~Bureaucrat_center + Manager_center + Networker_center + 
  factor(private) + agency + factor(size) + goalClarity_center
r2 <- External_center~Bureaucrat_center + Manager_center + Networker_center + 
  factor(private) + agency + factor(size) + goalClarity_center


# Model Estimation
system <- list(internal = r1,
                       external = r2)
fitsur <- systemfit::systemfit(system, data=df2, method="SUR")


## Postestimation

# Waldt Tests
restriction1 <- "external_Bureaucrat_center - external_Networker_center"
linearHypothesis(fitsur, restriction1, test = "Chisq")

restriction2 <- "internal_goalClarity_center - external_goalClarity_center"
linearHypothesis(fitsur, restriction2, test = "Chisq")

# save data frame
foreign::write.dta(df2, file ="data.dta")

# Figures
coefFigure <- coefplot::multiplot(fitsur$eq[[1]], fitsur$eq[[2]],
                    single = F, title = "",
                    xlab = "", ylab = "", 
                    decreasing = T,
                    legend.position = "none", 
                    names = c("Internal", "External"),
                    newNames = c(`(Intercept)`="Constant", 
                                 Bureaucrat_center="Role: Bureaucrat",
                                 Manager_center="Role: Manager",
                                 Networker_center="Role: Networker",
                                 `factor(private)1`="Private sector exp.: 1-10 years",
                                 `factor(private)2`="Private sector exp.: 10+ years",
                                 agency = "Agency",
                                 `factor(size)1`="Size: 100-999",
                                 `factor(size)2`="Size: Over 1000",
                                 goalClarity_center="Goal clarity"))
coefFigure <- coefFigure + 
  ggplot2::scale_color_manual(values=c("black","black"))
save(coefFigure, file = "coefFigure.RData")
ggsave(coefFigure, filename = "coefFigure.png", 
       width = 10, height = 6)

# Tables
texreg::htmlreg(list(fitsur),
                beside = T, digits = 3,
                custom.coef.names = c("Constant", 
                                      "Role: Bureaucrat",
                                      "Role: Manager",
                                      "Role: Networker",
                                      #"Education: MA or higher",
                                      #"Top hierachical level",
                                      #"Seniority: 1-10 years",
                                      #"Seniority: 10+ years",
                                      "Private sector exp.: 1-10 years",
                                      "Private sector exp.: 10+ years",
                                      "Agency",
                                      "Size: 100-999",
                                      "Size: Over 1000",
                                      "Goal clarity"
                                      #,"Financial distress",
                                      #"Network complexity"
                                      ),
                file = "regressionTable.doc")
