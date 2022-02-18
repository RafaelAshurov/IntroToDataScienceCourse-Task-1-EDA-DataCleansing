source("mechkar.R")

library(readr)
if (!require("dplyr")) {install.packages("dplyr");library("dplyr")}
if (!require("png")) {install.packages("png");library("pnf")}
if (!require("car")) {install.packages("car");library("car")}
if (!require("ggplot2")) {install.packages("ggplot2");library("ggplot2")}

hos <- read.csv("hospitalizations.csv")
hos <- data.frame(hos)

# GENDER lang marital ethnic religion INSURANCE admit_location death

## transform categorical variables to factors
hos$GENDER <- factor(hos$GENDER)
hos$lang <- factor(hos$lang)
hos$marital <- factor(hos$marital)
hos$ethnic <- factor(hos$ethnic)
hos$religion <- factor(hos$religion)
hos$INSURANCE <- factor(hos$INSURANCE)
hos$admit_location <- factor(hos$admit_location)
hos$HOSP_WEEKDAY <- factor(hos$HOSP_WEEKDAY)
hos$HOSP_HOUR <- factor(hos$HOSP_HOUR)
hos$HOSP_SEASON <- factor(hos$HOSP_SEASON)
hos$death <- factor(ifelse(hos$death=="Death",1,0))

summary(hos)

############################
####     EDA
############################

###########################
### Univariable Analysis
###########################
exploreData(data=hos, y="death")

## names of categorical variables
catvars <- c("GENDER","lang","marital","ethnic","religion","INSURANCE",
             "admit_location","HOSP_WEEKDAY","HOSP_HOUR","HOSP_SEASON","death")

## names of numeric variables
numvars <- c("SUBJECT_ID","HADM_ID","AGE","Hematocrit","Potassium","Creatinine",
             "Urea","Chloride","Hemoglobin","Sodium","Bicarbonate","Platelet",
             "Anion_Gap","WBC","MCHC","RBC","MCV","MCH","RDW","Glucose",
             "Magnesium","LOS")

## create an output PDF file to save the graphs on it.
pdf(file = "EDA_full_data.pdf", height=11, width=8.8)

## create graphs containing 3 columns and 4 rows
par(mfrow=c(4,3))

## begining with scatter plots for each numerical variables
x = 3
y = 3
repeat {
  if (y >= length(numvars)) {
    x = x + 1
    if (x == length(numvars)) {
      break
    }
    y = x + 1
  } else {
    y = y + 1
  }
  var1 <- numvars[x]
  var2 <- numvars[y]
  print(paste(x,y,var1,"-",var2))
  title <- paste(var1, "vs.",var2)
  cor1 <- cor.test(hos[[var1]],hos[[var2]],method="spearman")
  col1 <- ifelse(cor1$p.value < 0.05,"red","blue")
  txt1 <- paste('correlation:', round(cor1$estimate,3),"| p-value:",round(cor1$p.value,3))
  plot(hos[[var2]] ~ hos[[var1]],main=title,ylab=var2,xlab=var1,sub=txt1,col.sub=col1)
}



## now create boxplots between numerical variables and categorical
for (x in numvars) {
  for (y in catvars) {
    if(length(levels(hos[[y]])) == 2) {
      test1 <- wilcox.test(hos[[x]] ~ hos[[y]],alternative="two.sided")
      txt1 <- paste('Mann-Whitney:', round(test1$statistic,3),"| p-value:",round(test1$p.value,3))
    } else {
      test1 <- kruskal.test(hos[[x]], hos[[y]])
      txt1 <- paste('Kruskal-Wallis:', round(test1$statistic,3),"| df:",test1$parameter,"| p-value:",round(test1$p.value,3))
    }
    col1 <- ifelse(test1$p.value < 0.05,"red","blue")
    boxplot(hos[[x]] ~ hos[[y]], main=paste(x,"vs.",y),xlab=y,ylab=x,
            sub=txt1,col.sub=col1)
  }
}


## plot graphs for comparing categorical variables 

x = 3
y = 3
repeat {
  if (y >= length(catvars)) {
    x = x + 1
    if (x == length(catvars)) {
      break
    }
    y = x + 1
  } else {
    y = y + 1
  }
  var1 <- catvars[x]
  var2 <- catvars[y]
  print(paste(x,y,var1,"-",var2))
  title <- paste(var1, "vs.",var2)
  test1 <- chisq.test(hos[[var1]],hos[[var2]])
  txt1 <- paste('Chi-square:', round(test1$statistic,3),"| df:",test1$parameter,"| p-value:",round(test1$p.value,3))
  col1 <- ifelse(test1$p.value < 0.05,"red","blue")
  plot(hos[[var2]] ~ hos[[var1]],main=title,ylab=var2,xlab=var1)
  mtext(txt1,col=col1,cex=0.6)
}

par(mfrow=c(1,1))

library(corrgram)
corrgram(hos, order=NULL, lower.panel=panel.shade,
         upper.panel=panel.ellipse, text.panel=panel.txt,
         main="Hospitalization Data (unsorted)")


dev.off()
tab1 <- Table1(data=hos, y="death", categorize = TRUE)

#####################
#### check missing
#####################

getMissingness(hos)


##################################################
######    OUTLIERS
##################################################

outlierMatrix <- function(data,threshold=1.5) {
  vn <- names(data)
  outdata <- data.frame(row1=1:nrow(data))
  for(v in vn) {
    if(is.numeric(data[[v]])) {
      outlow <- quantile(data[[v]],probs = 0.25,na.rm = T) 
      outhigh <- quantile(data[[v]],probs = 0.75, na.rm = T)
      irq_level <- (outhigh - outlow) * threshold
      outlow <- outlow - irq_level
      outhigh <- outhigh +  irq_level
      mv <- ifelse(data[[v]] < outlow | data[[v]] > outhigh, 1, 0)
      outdata[v] <- mv
    } else {
      mv <- rep(0,nrow(data))
    }
  }
  outdata$row1 <- NULL
  return(outdata)
}

hosOut <- outlierMatrix(hos)

summary(hosOut)

heatmap(as.matrix(hosOut),Rowv = NA, 
        col=c(1,2),
        Colv = NA, main="Outliers Patterns")

getMissingness(hosOut)

########################################################################
### Desiding which values could be droped and which not.
### Checking for differences in the distribution We will compare if 
### there are a difference between the distribution of the variable 
### when having the outliers and when the outliers are removed. We 
### do so using the Kolmogorov–Smirnov statistic. This is a 
### non-parametric statistic that can be used on variables without a 
### normal distribution, and is specially useful for testing if two 
### distributions differ.
###
### We will iterate on each variable with a loop, and will save the 
### results in a data frame. If the difference (p-value) is significant 
### we will add a plus, while when the p-value is not significant, we 
### will add a minus.
#########################################################################

res1 <- NULL
for (n in numvars) {
  out <- hos[[n]]
  non <- hos[[n]][which(hosOut[[n]]==0)]
  outnum <- length(out) - length(non)
  pval <- suppressWarnings(ks.test(out, non)$p.value)
  res1 <- rbind(res1, cbind(var=n, outliers_cnt=outnum, distribution_changed=ifelse(pval<0.05,"+","-")))
}

res1

#############################################################################
### The second test we have to make with the outliers is to check if they 
### change the correlation between the variable and the outcome variable. 
### A statistical method for checking if two correlation are significantly 
### different was developed by Diedenhofen and Musch[1]. The method was 
### implemented in an R package called corcor .
### -------
### [1] Diedenhofen B, Musch J (2015) cocor: A Comprehensive Solution for 
### the Statistical Comparison of Correlations. PLoS ONE 10(4): e0121945. 
### https://doi.org/10.1371/journal.pone.0121945)
#############################################################################

library(cocor)
res2 <- NULL
outcome <- "death"
for (n in res1[,1]) {
  out <- hos[[n]]
  non <- hos[[n]][which(hosOut[[n]]==0)]
  outcome_out <- ifelse(hos[[outcome]]==1,1,0)
  outcome_non <- ifelse(outcome_out[which(hosOut[[n]]==0)]==1,1,0)
  outdf <- data.frame(x_out=out,y_out=outcome_out)
  nondf <- data.frame(x_non=non,y_non=outcome_non)
  tryCatch({
    cr <- cocor(~ x_out + y_out | x_non + y_non, data=list(outdf,nondf))
    pval <- cr@fisher1925$p.value
    res2 <- rbind(res2, cbind(var=n, correlation_changed=ifelse(pval<0.05,"+","-")))
  }, error = function(e) {
    cr <- 0
    res2 <- rbind(res2, cbind(var=n, correlation_changed="-"))
  })
}
res2

res <- left_join(data.frame(res1),data.frame(res2),by="var")
res$drop <- ifelse(res$distribution_changed=="+" & res$correlation_changed == "+","No","Yes")
res <- data.frame(res)
res <- res %>% filter(outliers_cnt!=0)
View(res)

#### variables to with outliers to be converted to NA:
vars <- res$var[which(res$drop=="Yes")]
vars 

### Deletion of the values
for (v in vars) {
  hos[[v]] <- ifelse(hosOut[[v]]==1,NA,hos[[v]])
}




##############################
#### Missing evaluation
##############################

getMissingness(hos)

hosmiss <- as.matrix(is.na(hos))*1
hos_miss <- data.frame(is.na(hos))
summary(hosmiss)
heatmap(hosmiss,Rowv = NA, Colv = NA, main="Missingness Patterns")

### Missingness desicion:
### missingness mechanism using differences in the distribution (as done with outliers)

mm <- getMissingness(hos,getRows = TRUE)

res2 <- NULL
for (m in mm$missingness$var) {
  p <- list()
  for (n in numvars) {
    if (n != m) {
      miss <- hos[[n]]
      non <- hos[which(hos_miss[[m]]==0),n]
      missnum <- length(miss) - length(non)
      pval <- suppressWarnings(ks.test(miss, non)$p.value)
      res2 <- rbind(res2, cbind(var=n,missing=m, missing_cnt=missnum, distribution_changed=ifelse(pval<0.05,"+","-")))
    }
  }
}
res2


#############################################
#### Missing values treatment
#############################################

################
# Imputations
################

## model substitution
if (!require("mice")) {install.packages("mice");library("mice")}
mimp <- mice(hos, method = "cart",seed=1)

summary(mimp$imp$Urea)

plot(mimp$imp$Urea$`1` ~ mimp$imp$Urea$`2`, col=hos_miss$Urea+1)
plot(mimp$imp$Urea$`1` ~ mimp$imp$Urea$`5`, col=hos_miss$Urea+1)
summary(mimp$imp$Urea)

hos_imp <- complete(mimp,"long",inc=FALSE)
getMissingness(hos_imp)

write.csv(hos_imp, file="hos_cleaned.csv",row.names = FALSE)

hos <- hos_imp

############################
####     EDA on cleaned data
############################

###########################
### Univariable Analysis
###########################

exploreData(data=hos, y="death")


## names of categorical variables
catvars <- c("GENDER","lang","marital","ethnic","religion","INSURANCE",
             "admit_location","HOSP_WEEKDAY","HOSP_HOUR","HOSP_SEASON","death")

## names of numeric variables
numvars <- c("SUBJECT_ID","HADM_ID","AGE","Hematocrit","Potassium","Creatinine",
             "Urea","Chloride","Hemoglobin","Sodium","Bicarbonate","Platelet",
             "Anion_Gap","WBC","MCHC","RBC","MCV","MCH","RDW","Glucose",
             "Magnesium","LOS")

pdf(file = "EDA_full_data_Cleaned.pdf", height=11, width=8.8)

par(mfrow=c(4,3))

##Multivariable Analysis
## begining with scatter plots for each numerical variables
x = 3
y = 3
repeat {
  if (y >= length(numvars)) {
    x = x + 1
    if (x == length(numvars)) {
      break
    }
    y = x + 1
  } else {
    y = y + 1
  }
  var1 <- numvars[x]
  var2 <- numvars[y]
  print(paste(x,y,var1,"-",var2))
  title <- paste(var1, "vs.",var2)
  cor1 <- cor.test(hos[[var1]],hos[[var2]],method="spearman")
  col1 <- ifelse(cor1$p.value < 0.05,"red","blue")
  txt1 <- paste('correlation:', round(cor1$estimate,3),"| p-value:",round(cor1$p.value,3))
  plot(hos[[var2]] ~ hos[[var1]],main=title,ylab=var2,xlab=var1,sub=txt1,col.sub=col1)
}



## now create boxplots between numerical variables and categorical
for (x in numvars) {
  for (y in catvars) {
    if(length(levels(hos[[y]])) == 2) {
      test1 <- wilcox.test(hos[[x]] ~ hos[[y]],alternative="two.sided")
      txt1 <- paste('Mann-Whitney:', round(test1$statistic,3),"| p-value:",round(test1$p.value,3))
    } else {
      test1 <- kruskal.test(hos[[x]], hos[[y]])
      txt1 <- paste('Kruskal-Wallis:', round(test1$statistic,3),"| df:",test1$parameter,"| p-value:",round(test1$p.value,3))
    }
    col1 <- ifelse(test1$p.value < 0.05,"red","blue")
    boxplot(hos[[x]] ~ hos[[y]], main=paste(x,"vs.",y),xlab=y,ylab=x,
            sub=txt1,col.sub=col1)
  }
}


## plot graphs for comparing categorical variables 

x = 3
y = 3
repeat {
  if (y >= length(catvars)) {
    x = x + 1
    if (x == length(catvars)) {
      break
    }
    y = x + 1
  } else {
    y = y + 1
  }
  var1 <- catvars[x]
  var2 <- catvars[y]
  print(paste(x,y,var1,"-",var2))
  title <- paste(var1, "vs.",var2)
  test1 <- chisq.test(hos[[var1]],hos[[var2]])
  txt1 <- paste('Chi-square:', round(test1$statistic,3),"| df:",test1$parameter,"| p-value:",round(test1$p.value,3))
  col1 <- ifelse(test1$p.value < 0.05,"red","blue")
  plot(hos[[var2]] ~ hos[[var1]],main=title,ylab=var2,xlab=var1)
  mtext(txt1,col=col1,cex=0.6)
}

par(mfrow=c(1,1))

library(corrgram)
corrgram(hos, order=NULL, lower.panel=panel.shade,
         upper.panel=panel.ellipse, text.panel=panel.txt,
         main="Hospitalization Data (unsorted)")


dev.off()



#####################################
##  Feature Extraction
#####################################
### Anemia: low Hemoglobin (HGB) values. The cutoff values are different
### between women (< 12) and men (<14) 
hos$Anemia <- factor(
  ifelse((hos$GENDER=="M" & hos$Hemoglobin < 14) | 
           (hos$GENDER=="F" & hos$Hemoglobin < 12), 1, 0)
)
table(hos$Anemia)

### LowHCT: low Hematocrit (HCT) values. The cutoff values are different
### between women (< 36.1) and men (<40.7) 
hos$LowHCT <- factor(
  ifelse((hos$GENDER=="M" & hos$Hematocrit < 40.7) | 
           (hos$GENDER=="F" & hos$Hematocrit < 36.1), 1, 0))
table(hos$LowHCT)

### HighPotassium: high Potassium values. Higher than 5.1
hos$HighPotassium <- factor( ifelse(hos$Potassium > 5.1, 1, 0))
table(hos$HighPotassium)

### LowPotassium: low Potassium values. lower than 3.5
hos$LowPotassium <- factor( ifelse(hos$Potassium < 3.5, 1, 0))
table(hos$LowPotassium)


### KidneyProblems: High Creatinine values. he cutoff values are different
### between women (> 0.95) and men (>1.17) 
hos$KidneyProblems <- factor(
  ifelse((hos$GENDER=="M" & hos$Creatinine > 1.17) | 
           (hos$GENDER=="F" & hos$Creatinine > 0.95), 1, 0))
table(hos$KidneyProblems)


### MuscleIllness: Low Creatinine values. he cutoff values are different
### between women (< 0.51) and men (<0.67) 
hos$MuscleIllness <- factor(
  ifelse((hos$GENDER=="M" & hos$Creatinine < 0.67) | 
           (hos$GENDER=="F" & hos$Creatinine < 0.51), 1, 0))
table(hos$MuscleIllness)


################################
## Feature Engineering
################################

hist(hos$Hematocrit/hos$RBC)
hist(log(hos$Hematocrit/hos$RBC))
hist(sqrt(hos$Hematocrit/hos$RBC))
hos$HCT_RBC_ration <- hos$Hematocrit/hos$RBC

hist(hos$RBC/hos$WBC)
hist(log(hos$RBC/hos$WBC))
hist(sqrt(hos$RBC/hos$WBC))
hos$RBC_WBC_log_ratio <- log(hos$RBC/hos$WBC)

### drop the variables that are not relevant for the model generation
hos$.id <- NULL
hos$.imp <- NULL
hos$SUBJECT_ID <- NULL
hos$HADM_ID <- NULL

#####################
### Saving the Extended dataset
#####################

write.csv(hos,file="hospitalization_extended.csv",row.names = FALSE)

############################################
### Feature Selection
############################################

tab1 <- Table1(data=hos, y="death")
tab1$`p-value`

tab1$pval <- ifelse(as.character(tab1$`p-value`)=="<0.001",0.001,as.numeric(tab1$`p-value`))
tab1$pval

tab2 <- tab1 %>% 
  select(Variables,pval) %>%
  filter(pval < 0.05) 

nm <- tab2$Variables
tab2$Variables

write.csv(hos[,nm], file="Hospitalization-Data-seleted.csv",row.names = FALSE)
