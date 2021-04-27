#install.packages('corrplot')
#install.packages('ggpubr')
#install.packages('nortest')
#install.packages('ggplot2')
#install.packages('tree')
#install.packages('e1071')
#install.packages('caret')
#install.packages('FSA')
library(corrplot)
library(ggpubr)
library(nortest)
library(ggplot2)
library(tree)
library (e1071)
library(caret)
library(FSA)

data("Wages", package = "plm")

names(Wages)
head(Wages)


any(is.na(Wages))

## ΑΝΑΓΝΩΡΙΣΗ ΜΕΤΑΒΛΗΤΩΝ

str(Wages)
summary(Wages)

Wages$ind <- as.factor(Wages$ind)
levels(Wages$ind) <- c('no','yes')

Wages$ed <-ifelse(Wages$ed<12,"Low",ifelse(Wages$ed==12,"Basic","High"))
Wages$ed<-factor(Wages$ed,levels=c('Low','Basic','High'),ordered = TRUE)

attach(Wages)

##############################################################

### ΠΕΡΙΓΡΑΦΙΚΗ ΣΤΑΤΙΣΤΙΚΗ

## Ποσοτικές Μεταβλητές

 ## ΓΡΑΦΗΜΑΤΑ ΓΙΑ ΠΟΣΟΤΙΚΕΣ ΜΕΤΑΒΛΗΤΕΣ

hist(lwage,col = 'cyan4',ylim = c(0,1900))
hist(wks,xlim = c(25,55),col = 'darkred')
hist(exp,col = 'darkgreen')

## Κατηγορικές Μεταβλητές

# Πίνακες Συχνοτήτων - Σχετικών Συχνοτήτων
education_table <- table(ed)
rel_ed <- round(prop.table(education_table),2)

bluecol_table <- table(bluecol)
rel_bluecol <- round(prop.table(bluecol_table),2)

south_table <- table(south) 
rel_south <- round(prop.table(south_table),2)

smsa_table <- table(smsa) 
rel_smsa <- round(prop.table(smsa_table),2)

union_table <- table(union)
rel_union <- round(prop.table(union_table),2)

black_table <- table(black)
rel_black <- round(prop.table(black_table),2)

ind_table <- table(ind)
rel_ind <- round(prop.table(ind_table),2)

sex_table <- table(sex)
rel_sex_table <- round(prop.table(sex_table),2)

 ## ΓΡΑΦΗΜΑΤΑ ΓΙΑ ΠΟΙΟΤΙΚΕΣ ΜΕΤΑΒΛΗΤΕΣ 

# Education Level
a <- paste(100*rel_ed,"%")
pie(education_table, main = 'Education Level Distribution',labels = a, col = c(2,3,4))
legend(x='topright',
       legend = names(education_table),
       fill = c(2,3,4))

# Blue Collar
b <- paste(100*rel_bluecol,"%")
pie(bluecol_table,labels = b,main = 'Blue Collar Distribution',col = c('orange','darkgreen'))
legend(x='topright',
       legend = names(bluecol_table),
       fill = c('orange','darkgreen'))

# South
c <- paste(100*rel_south,"%")
pie(south_table,labels = c, main = 'South Residents',col = c('darkcyan','darkolivegreen2'))
legend('topright',legend = names(south_table),fill = c('darkcyan','darkolivegreen2'))

# Sex
d <- paste(100*rel_sex_table,"%")
pie(sex_table,labels = d, main = 'Gender Distribution',col = c('blue3','brown2'))
legend('topright',legend = names(sex_table),fill = c('blue3','brown2'))

# Manufacturing Industry
e <- paste(100*rel_ind,"%")
pie(ind_table,labels = e,main = 'Manufacturing Industry Distribution',col = c('darkgoldenrod2',3))
legend('topright',legend = names(ind_table),fill = c(7,3))

# Union
f <- paste(100*rel_union,"%")
pie(union_table,labels = f ,main = 'Union Contract Distribution',col = c('darkgoldenrod2','chartreuse4'))
legend('topright',legend = names(union_table),fill = c('darkgoldenrod2','chartreuse4'))


 ##  ΓΡΑΦΗΜΑΤΑ ΠΟΣΟΤΙΚΩΝ ΜΕΤΑΒΛΗΤΩΝ ΩΣ ΠΡΟΣ ΠΟΙΟΤΙΚΕΣ 

# lwage ~ ed
lw_ed <- ggplot(Wages,aes(ed,lwage,colour = ed))+
        geom_boxplot()
lw_ed

# lwage ~ sex
lw_sex_box <- ggplot(Wages,aes(sex,lwage,fill = sex))+
                      geom_boxplot()
lw_sex_box

# lwage ~ union
lw_union <- ggplot(Wages,aes(union,lwage,fill = union))+
                    geom_boxplot()
lw_union

# lwage ~ bluecol
lw_bluecol <- ggplot(Wages,aes(bluecol,lwage,fill = bluecol))+
                      geom_boxplot()
lw_bluecol

# lwage ~ south
lw_south <- ggplot(Wages,aes(south,lwage,fill = south))+
                    geom_boxplot()
lw_south

# lwage ~ ind
lw_ind <- ggplot(Wages,aes(ind,lwage,fill = ind))+
                  geom_boxplot()
lw_ind


##################################################################################
### ΣΤΑΤΙΣΤΙΚΗ ΑΝΑΛΥΣΗ

## Έλεγχοι ανεξαρησίας μεταξύ των ποιοτικών μεταβλητών

chisq.test(smsa,union) # Ανεξάρτητες

chisq.test(table(bluecol,ind))
chisq.test(table(bluecol,south))
chisq.test(table(bluecol,union))
chisq.test(table(bluecol,sex))

chisq.test(table(ind,sex))
chisq.test(table(ind,union))

# Από τους ελέγχους chi-square test παρατηρούμε οτι σχεδόν
# ολες οι κατηγορικές μεταβλητές είναι εξαρτημένες 
# μεταξύ τους.

## Έλεγχος κανονικότητας της μεταβλητής lwage

ggqqplot(lwage,conf.int = TRUE,conf.int.level = 0.95)
lwage_nortest <- lillie.test(lwage)
lwage_nortest
 
# Η μεταβλητή lwage δεν ακολουθεί κανονική κατανομή. 
 

# ΜΗ ΠΑΡΑΜΕΤΡΙΚΟΙ ΕΛΕΓΧΟΙ ΥΠΟΘΕΣΕΩΝ

# 1
wilcox.test(formula = lwage ~ bluecol,
            data = Wages,
            alternative = 'less',
            paired = FALSE,
            conf.level = 0.95)

# Οι μισθοί των ατόμων όπου bluecol = no  είναι μεγαλύτεροι.

# Συγκριτικό Θηκόγραμμα.
ggplot(Wages,aes(bluecol,lwage,fill = bluecol))+
        geom_boxplot()

# 2
wilcox.test(formula = lwage~south,
            data = Wages,
            alternative = 'two.sided',
            paired = FALSE,
            conf.level = 0.95)

# Οι μισθοί των ατόμων που ζουν στον νότο είναι
# κατά μέσο ορο χαμηλότεροι.

# Συγκριτικό Θηκόγραμμα
ggplot(Wages,aes(south,lwage,fill = south))+
        geom_boxplot()


# 3
wilcox.test(formula = lwage~union,
            data = Wages,
            alternative = 'two.sided',
            paired = FALSE,
            conf.level = 0.95)

# Οι μισθοί των ατόμων με union contract είναι ίσοι με αυτούς 
# χώρις union contract.

# Συγκριτικό Θηκόγραμμα
ggplot(Wages,aes(union,lwage,fill = union))+
        geom_boxplot()

# 4
wilcox.test(formula = lwage~ind,
            data = Wages,
            alternative = 'two.sided',
            paired = FALSE,
            conf.level = 0.95)

# Οι μεσοι μισθοι των δυο πληθυσμων ειναι ισοι

# Συγκριτικο Θηκογραμμα
ggplot(Wages,aes(ind,lwage,fill = ind))+
        geom_boxplot()

# 5
wilcox.test(formula = lwage~sex,
            data = Wages,
            alternative = 'two.sided',
            paired = FALSE,
            conf.level = 0.95)

# Ο μισθος δεν ειναι ιδιος ανα φυλο.

# Συγκριτικό Θηκόγραμμα
ggplot(Wages,aes(sex,lwage,fill = sex))+
        geom_boxplot()

# 6
wilcox.test(formula = lwage~black,
             data = Wages,
             alternative = 'two.sided',
             paired = FALSE,
             conf.level = 0.95)

# Οι μισθοι διαφερουν ανα το χρωμα επισης .

ggplot(Wages,aes(x=black,y=lwage,fill = black))+
        geom_boxplot()

# 7
wilcox.test(formula = lwage~smsa,
            data = Wages,
            alternative = 'two.sided',
            conf.level = 0.95)

ggplot(Wages,aes(smsa,lwage,fill = smsa))+
        geom_boxplot()

## Έλεγχος γραμμικής συσχέτισης μεταξύ των ποσοτικών μεταβλητών

m <- data.frame(lwage,exp,wks)

corrplot(cor(m),method = "number",cl.pos = "n",col = 'black')

plot(wks,lwage)
plot(exp,lwage)

# Απο συντελεστές Pearson και από τα διαγράμματα φαίνεται
# πως δεν υπάρχει γραμμική συσχέτιση μεταξύ των μεταβλητών.


#######################################

### ΣΤΑΤΙΣΤΙΚΗ ΜΑΘΗΣΗ

hist(lwage)
summary(lwage)

# Μετατρέπουμε σε factor την μεταβλητή lwage με levels : Low High
Wages$lwage <- ifelse(Wages$lwage<=median(lwage),"Low","High")
Wages$lwage <- as.factor(Wages$lwage)

attach(Wages)

# Χωρίζουμε το dataset σε train και test sets
set.seed(11)
train_index = sample(nrow(Wages),0.8*nrow(Wages))
train = Wages[train_index,]
test = Wages[-train_index,]
y=test[,"lwage"]

## Support Vector Machines 

# 1.Linear Kernel 

svm_fit_linear <- svm(lwage~., data = train, kernel = 'linear', cost = 10,
               scale = FALSE)
svm_fit_linear
summary(svm_fit_linear)
svm_fit_linear$index

# Cross Validation
tune.out=tune(svm ,lwage~.,data=train ,kernel ="linear",
              ranges =list(cost=c(0.001 , 0.01, 0.1, 100) ))

# Συγκριση svm μοντελων για διαφορετικες τιμες στην παραμετρο cost
summary (tune.out)

# Επιλογη του καλυτερου μοντελου(χαμηλοτερο cross validation error rate)
bestmod = tune.out$best.model
bestmod

# Προβλεψη κλασης του μισθου στο test set
lwage_pred_lin <- predict(bestmod,test)
lwage_pred_lin

# Aξιολογηση του βελτιστου μοντελου συγκρινοντας τις παραπανω
# προβλεψεις με τις πραγματικες στο test set
xtab <- table(lwage_pred_lin,y)
xtab
confusionMatrix(xtab) 
        # Accuracy: 0.6987

# 2.Radial Kernel

svm_fit_radial <- svm(lwage~.,data = train,kernel = 'radial',
                      cost = 0.1,gamma = 1)
svm_fit_radial
summary(svm_fit_radial)

# Cross Validation 
tune.out1 <- tune(svm,lwage~.,data = train,kernel = 'radial',
                  ranges = list(cost = c(0.001,0.01,0.1,10,100),
                                gamma = c(0.5,1,2,3,4)))

# Συγκριση svm μοντελων για διαφορετικες τιμες στις παραμετρους
# cost και gamma.
summary(tune.out1)


# Επιλογη του καλυτερου μοντελου(χαμηλοτερο cross validation error rate)
bestmod1 <- tune.out1$best.model
bestmod1

# Προβλεψη κλασης του μισθου στο test set
lwage_pred_rad <- predict(bestmod1,test)

# Aξιολογηση του βελτιστου μοντελου συγκρινοντας τις παραπανω
# προβλεψεις με τις πραγματικες στο test set
xtab1 <- table(lwage_pred_rad,y)
xtab1
confusionMatrix(xtab1) 
        # Accuracy    : 0.7755
        # Sensitivity : 0.7849 
        # Specificity : 0.6902 

# 3.Polynomial Kernel

svm_fit_poly <- svm(lwage~.,data = train,kernel = 'polynomial',
                      cost = 0.1,gamma = 1)
svm_fit_poly
summary(svm_fit_poly)

# Προβλεψη κλασης του μισθου στο test set
lwage_pred_poly <- predict(svm_fit_poly,test)

# Aξιολογηση του βελτιστου μοντελου συγκρινοντας τις παραπανω
# προβλεψεις με τις πραγματικες στο test set
xtab3 <- table(lwage_pred_poly,y)
xtab3
confusionMatrix(xtab3) 
        # Accuracy: 0.7395
        # Sensitivity : 0.7849         
        # Specificity : 0.6927  


## Decision Trees

wage.tree <- tree(lwage~.,data = train)
summary(wage.tree)
plot(wage.tree)
text(wage.tree)


cv.tree <- cv.tree(wage.tree)
cv.tree

plot(cv.tree$size ,cv.tree$dev ,type="b",
     ylab = "cross-validation error rate", xlab = "size")
plot(cv.tree$k ,cv.tree$dev ,type="b",
     ylab = "cost-complexity parameter k", xlab = "size")

min(cv.tree$dev)

# Απο το διαγραμμα και το cross validation παρατηρουμε οτι
# παιρνουμε το μικροτερo σφαλμα στους 8 κομβους,οσους δηλαδη εχει
# το δεντρο που φτιαξαμε.

# Επομενως δεν χρειαζεται 'κλαδεμα' στο δεντρο 

ypred <- predict(wage.tree,test,type = 'class')
ypred

# Confusion Matrix
conmat <- table(ypred,y)
conmat

confusionMatrix(conmat)
        # Accuracy    : 0.6639
        # Sensitivity : 0.7116
        # Specificity : 0.6146
