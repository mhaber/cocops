library(readstata13) # to load in .dta file
library(dplyr) # data manipulation
library(paran) # parallel analysis
library(psych) # factor / pPCA analysis
library(MASS) # ordered probit
library(systemfit) # for seemingly unrelated regression 

# load data set
df <- readstata13::read.dta13("data/cocops_cg_stata13.dta", convert.factors = F) 
df$rowname <- rownames(df)

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
regForm <- paste("as.factor(q25_1) ~",paste(paste(names(roles),collapse="+"),
                                            "factor(country)",
                                            paste(names(policy),collapse="+"), sep = "+"))
m1 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
summary(m1)
ctableM1 <- coef(summary(m1))
p <- pnorm(abs(ctableM1[, "t value"]), lower.tail = FALSE) * 2
ctableM1 <- cbind(ctableM1, "p value" = p)
ctableM1[1:3,]
# -->  Manager strongest relation to Success depends on ability 

# Thinking up new ideas are important
regForm <- paste("as.factor(q25_4) ~",paste(paste(names(roles),collapse="+"),
                                             "factor(country)",
                                             paste(names(policy),collapse="+"), sep = "+"))
m2 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
summary(m2)
ctableM2 <- coef(summary(m2))
p <- pnorm(abs(ctableM2[, "t value"]), lower.tail = FALSE) * 2
ctableM2 <- cbind(ctableM2, "p value" = p)
ctableM2[1:3,] 
# -->  Networker strongest relation to Thinking up new ideas are important

# Avoid upset the status quo
regForm <- paste("as.factor(q25_5) ~",paste(paste(names(roles),collapse="+"),
                                            "factor(country)",
                                            paste(names(policy),collapse="+"), sep = "+"))
m3 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
summary(m3)
ctablem3 <- coef(summary(m3))
p <- pnorm(abs(ctablem3[, "t value"]), lower.tail = FALSE) * 2
ctablem3 <- cbind(ctablem3, "p value" = p)
ctablem3[1:3,] 
# -->  Manager strongest relation to Avoid upset the status quo

# Motivation: High Income
regForm <- paste("as.factor(q24_2) ~",paste(paste(names(roles),collapse="+"),
                                            "factor(country)",
                                            paste(names(policy),collapse="+"), sep = "+"))
m4 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
summary(m4)
ctablem4 <- coef(summary(m4))
p <- pnorm(abs(ctablem4[, "t value"]), lower.tail = FALSE) * 2
ctablem4 <- cbind(ctablem4, "p value" = p)
ctablem4[1:3,] 
# -->  Manager strongest relation to Motivation: High Income

# Motivation: Opportunities to help others
regForm <- paste("as.factor(q24_3) ~",paste(paste(names(roles),collapse="+"),
                                            "factor(country)",
                                            paste(names(policy),collapse="+"), sep = "+"))
m5 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
summary(m5)
ctablem5 <- coef(summary(m5))
p <- pnorm(abs(ctablem5[, "t value"]), lower.tail = FALSE) * 2
ctablem5 <- cbind(ctablem5, "p value" = p)
ctablem5[1:3,] 
# -->  Networker and Bureaucrat strongest relation to Motivation: Opportunities to help others

# Motivation: Job security
regForm <- paste("as.factor(q24_4) ~",paste(paste(names(roles),collapse="+"),
                                            "factor(country)",
                                            paste(names(policy),collapse="+"), sep = "+"))
m6 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
summary(m6)
ctablem6 <- coef(summary(m6))
p <- pnorm(abs(ctablem6[, "t value"]), lower.tail = FALSE) * 2
ctablem6 <- cbind(ctablem6, "p value" = p)
ctablem6[1:3,] 
# -->  Bureaucrat strongest relation to Motivation: Job security

# Motivation: Room to make decisions
regForm <- paste("as.factor(q24_5) ~",paste(paste(names(roles),collapse="+"),
                                            "factor(country)",
                                            paste(names(policy),collapse="+"), sep = "+"))
m7 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
summary(m7)
ctablem7 <- coef(summary(m7))
p <- pnorm(abs(ctablem7[, "t value"]), lower.tail = FALSE) * 2
ctablem7 <- cbind(ctablem7, "p value" = p)
ctablem7[1:3,] 
# -->  Manager and Bureaucrat strongest relation to Motivation: Room to make decisions

# Motivation: Useful for society
regForm <- paste("as.factor(q24_7) ~",paste(paste(names(roles),collapse="+"),
                                            "factor(country)",
                                            paste(names(policy),collapse="+"), sep = "+"))
m8 <- MASS::polr(as.formula(regForm), data = df2, method = "probit", Hess = T)
summary(m8)
ctablem8 <- coef(summary(m8))
p <- pnorm(abs(ctablem8[, "t value"]), lower.tail = FALSE) * 2
ctablem8 <- cbind(ctablem8, "p value" = p)
ctablem8[1:3,] 
# -->  Networker strongest relation to Motivation: Useful for society

# Socio-demographic factors -----------------------------------------------

# Hierarchal position
df2 <- df2 %>% dplyr::mutate(hierach = replace(q4, q4 %in% 2:3, 0))

# Educational attainment
df2 <- df2 %>% dplyr::mutate(educat = replace(edu, edu==1, 0)) %>% 
  dplyr::mutate(educat = replace(educat, educat %in% 2:3, 1))

# Seniority
df2 <- df2 %>% dplyr::mutate(senior = replace(q30_1, q30_1 == 1, 0)) %>% 
  dplyr::mutate(senior = replace(senior, senior %in% 2:3, 1)) %>% 
  dplyr::mutate(senior = replace(senior, senior %in% 4:5, 2))

# Private sector job experience
df2 <- df2 %>% dplyr::mutate(private = replace(q31_1, q31_1 %in%  1:2, 0)) %>% 
  dplyr::mutate(private = replace(private, private %in% 3:4, 1)) %>% 
  dplyr::mutate(private = replace(private, private %in% 5:6, 2))


# Organizational and Contextual factors -----------------------------------------------

# Organization size
df2 <- df2 %>% dplyr::mutate(size = replace(q3, q3 %in% 1:2, 0)) %>% 
  dplyr::mutate(size = replace(size, size %in% 3:4, 1)) %>% 
  dplyr::mutate(size = replace(size, size %in% 5:6, 2))

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
r1 <- Internal~Bureaucrat + Manager + Networker + educat + hierach + factor(senior) +
  factor(private) + agency + factor(size) + goalClarity + distress +  networkComplex +
  q2_1 + q2_2 + q2_3 + q2_4 + q2_5 + q2_6 + q2_7 + q2_8 + q2_9 + q2_10 +
  q2_11 + q2_12 + q2_13 + q2_o
r2 <- External~Bureaucrat + Manager + Networker + educat + hierach + factor(senior) +
  factor(private) + agency + factor(size) + goalClarity + distress +  networkComplex +
  q2_1 + q2_2 + q2_3 + q2_4 + q2_5 + q2_6 + q2_7 + q2_8 + q2_9 + q2_10 +
  q2_11 + q2_12 + q2_13 + q2_o

system <- list(internal = r1, external = r2)
fitsur <- systemfit::systemfit(system, data=df2)
summary(fitsur)
