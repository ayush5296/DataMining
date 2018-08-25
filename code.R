library(corrgram)
library(data.table)
library(tidyverse)
library(lubridate)
library(scales)
library(corrplot)
library(DT)
library(ggplot2)
require(ggplot2)
library(plyr)
library(dplyr)
library(VIM)
library(mice)
library(MASS)


df  = train
#View(df)
str(df)

#knowing the data
t =sapply(df,class)
count(t)


#Internal characters for housing price
internal_chars = c('full_sq', 'life_sq', 'floor', 'max_floor', 'build_year', 'num_room', 
                   'kitch_sq', 'state', 'price_doc')

summary(df[,internal_chars])

#total missing data
data_miss = aggr(df[,internal_chars], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(internal_chars)
                 , cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
md.pattern(df[,internal_chars])

#data cleanup and fill up
k = which(complete.cases(df)==TRUE) # number of tuples with no na's
length(k)
df_complete = na.omit(df) # complete data by removing na's from each row
summary(df_complete[internal_chars])



#some of the factors are as chr, converting them to int
df$max_floor = as.factor(df$max_floor)
df$build_year = as.integer(df$build_year)
df$num_room = as.factor(df$num_room)
df$kitch_sq = as.integer(df$kitch_sq)
df$state = as.integer(df$state)

#correlation plot
corrplot(cor(df[,internal_chars], use="complete.obs"))

#df[,internal_chars]

#total missing data
data_miss = aggr(df[,internal_chars], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(internal_chars)
                 , cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
md.pattern(df[,internal_chars])


#data cleanup
#full_sq vs price
ggplot(aes(x=full_sq, y=price_doc), data=df) + geom_point(color='maroon', alpha = 0.5)+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
#removing the outlier according to price
ind = which(df$full_sq>=1000)
ind
df$full_sq[ind]
df = df[-c(ind),]
model = lm(df$price_doc~df$full_sq)
summary(model)
model$coefficients
#removing the outlier according to area
ind = which(df$full_sq<=10)
df$full_sq[ind]
#df$num_room[ind]
df$full_sq[ind] = NA


#full_sq vs life_sq
ggplot(aes(x=full_sq, y=life_sq), data=df) + geom_point(color='blue', alpha = 0.5)+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x) + 
  geom_abline(intercept = 0, slope = 1, color = "red")
#total area should be less than living area
summary(df$full_sq)
summary(df$life_sq)
model = lm(df$full_sq ~ df$life_sq, data = df)
summary(model)
df$life_sq[df$life_sq> df$full_sq] = NA
#removing values that made no sense
ind = which(df$full_sq>300 & df$life_sq<50)
df$life_sq[ind] = NA
ind = which(df$life_sq<5)
df$life_sq[ind] = NA

#kitchen_sq is less than living area and total area
ggplot(aes(x=df$kitch_sq, y=life_sq), data=df) + geom_point(color='blue', alpha = 0.5)+
  geom_abline(intercept = 0, slope = 1, color = "red")
ind = which(df$kitch_sq >= df$life_sq)
df$kitch_sq[ind] = NA

#state
table(df$state)
df$state[df$state == 33] = NA

#build_year>Timestamp
out= which(year(df$timestamp) < df$build_year)
df$build_year[out] = NA

#observing number of rooms
table(df$num_room)
ggplot(aes(x=num_room), data=df) + 
  geom_density(fill='red', alpha = 0.5) + 
  ggtitle('Distribution of room count')
df$num_room[df$num_room >=4] = "4+"
df$num_room[df$num_room<=1] = "1-"
df$num_room = as.numeric(as.factor(df$num_room))
summary(df$num_room)
ggplot(aes(x=num_room, y=price_doc), data=df)+
  geom_point(color = "cyan", alpha = 0.5)


detach(package:plyr)
#product type
ggplot(aes(x=price_doc), data=df) + geom_density(fill='red', color='red') + facet_grid(~product_type)+
  scale_x_continuous(trans='log')
df %>% group_by(product_type) %>% summarize(median(price_doc))

#built year
#summary(df$build_year)
table(df$build_year)
#df = train
ind = which(df$build_year<1691)
df$build_year[ind] = NA
ind = which(df$build_year >2018)
df$build_year[ind] = NA

ggplot(aes(x=build_year, y = price_doc), data=df) + geom_point(fill='red', color='red', alpha = 0.5)+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)

#kitch sq
table(df$kitch_sq)
ggplot(aes(x=kitch_sq), data=df) + geom_density(fill='red', color='red')+
  scale_x_continuous(trans='log')
out = which(df$kitch_sq>500)
df = df[-out,]
out = which(df$kitch_sq>= df$life_sq)
df = df[-out,]

#floor
ggplot(aes(x = floor, y = max_floor), data = df) + 
  geom_point(color = "red", alpha = 0.5)+
  geom_abline(slope = 1, intercept = 0)+
  geom_smooth(method = "lm", se=FALSE, color="black", formula = y ~ x)
df$max_floor[df$max_floor<df$floor] = NA
ind = which(df$floor==0)
df$floor = NA
ind = which(df$max_floor== 0)
ind
df$max_floor[ind] = NA
cor(df$floor,df$max_floor, method = c("pearson"), use = "complete.obs")

#Thermal_raion
count = table(df$thermal_power_plant_raion, df$sub_area)
count
barplot(count,
        xlab="subarea", col=c("darkblue","red"),
        legend = rownames(count))
ggplot(aes(x=price_doc), data=df) + geom_density(fill='red', color='red') + facet_grid(~thermal_power_plant_raion)+
  scale_x_continuous(trans='log')
df %>% 
  group_by(thermal_power_plant_raion) %>% 
  summarize(median(price_doc))
#correct

#inceneration raion
count = table(df$incineration_raion, df$sub_area)
count
barplot(count,
        xlab="subarea", col=c("darkblue","red"),
        legend = rownames(count))
ggplot(aes(x=price_doc), data=df) + geom_density(fill='red', color='red') + facet_grid(~incineration_raion)+
  scale_x_continuous(trans='log')+geom_vline(xintercept = mean(df$price_doc))
df %>% 
  group_by(incineration_raion) %>% 
  summarize(median(price_doc))
#correct

#oil_chemistry
count = table(df$oil_chemistry_raion, df$sub_area)
count
barplot(count,
        xlab="subarea", col=c("darkblue","red"),
        legend = rownames(count))
ggplot(aes(x=price_doc), data=df) + geom_density(fill='red', color='red') + facet_grid(~oil_chemistry_raion)+
  scale_x_continuous(trans='log')+geom_vline(xintercept = mean(df$price_doc))
df %>% 
  group_by(oil_chemistry_raion) %>% 
  summarize(median(price_doc))
#correct

#radiation raion
count = table(df$radiation_raion, df$sub_area)
count
barplot(count,
        xlab="subarea", col=c("darkblue","red"),
        legend = rownames(count))
ggplot(aes(x=price_doc), data=df) + geom_density(fill='red', color='red') + facet_grid(~radiation_raion)+
  scale_x_continuous(trans='log')+geom_vline(xintercept = mean(df$price_doc))
df %>% 
  group_by(radiation_raion) %>% 
  summarize(median(price_doc))
#correct

#railroad_terminal
count = table(df$railroad_terminal_raion, df$sub_area)
barplot(count,
        xlab="subarea", col=c("darkblue","red"),
        legend = rownames(count))
ggplot(aes(x=price_doc), data=df) + geom_density(fill='red', color='red') + facet_grid(~railroad_terminal_raion)+
  scale_x_continuous(trans='log')+geom_vline(xintercept = mean(df$price_doc))
df %>% 
  group_by(railroad_terminal_raion) %>% 
  summarize(median(price_doc))
#correct

#Big market raion
count = table(df$big_market_raion, df$sub_area)
barplot(count,
        xlab="subarea", col=c("darkblue","red"),
        legend = rownames(count))
ggplot(aes(x=price_doc), data=df) + geom_density(fill='red', color='red') + facet_grid(~big_market_raion)+
  scale_x_continuous(trans='log')+geom_vline(xintercept = mean(df$price_doc))
df %>% 
  group_by(big_market_raion) %>% 
  summarize(median(price_doc))
#correct

#nuclear reactor raion
count = table(df$nuclear_reactor_raion, df$sub_area)
barplot(count,
        xlab="subarea", col=c("darkblue","red"),
        legend = rownames(count))
ggplot(aes(x=price_doc), data=df) + geom_density(fill='red', color='red') + facet_grid(~nuclear_reactor_raion)+
  scale_x_continuous(trans='log')+geom_vline(xintercept = mean(df$price_doc))
df %>% 
  group_by(nuclear_reactor_raion) %>% 
  summarize(median(price_doc))
#correct

#names = levels(df$sub_area)
#flag = TRUE
#for (name in names) {
#  if (length(levels(as.factor(df$preschool_education_centers_raion[df$sub_area ==name])))>1){
#    flag = FALSE
#  }
#}
#flag

#detention facility
count = table(df$detention_facility_raion, df$sub_area)
barplot(count,
        xlab="subarea", col=c("darkblue","red"),
        legend = rownames(count))
ggplot(aes(x=price_doc), data=df) + geom_density(fill='red', color='red') + facet_grid(~detention_facility_raion)+
  scale_x_continuous(trans='log')+geom_vline(xintercept = mean(df$price_doc))
df %>% 
  group_by(detention_facility_raion) %>% 
  summarize(median(price_doc))
#correct

#population clean
#correlation plot for types of population
corrplot(cor(cbind(df[,42:68],df$price_doc), use="complete.obs"))

#all population
ind = which(df$full_all != df$female_f+df$male_f)
ind
ggplot(aes(full_all - female_f-male_f), data = df)+geom_density(fill = "red", alpha = 0.5, color = "red")
df$female_f[ind] = df$full_all[ind]-df$male_f[ind]

#young population
ind = which(df$young_all != df$young_female+df$young_male)
ind
ggplot(aes(young_all-young_female-young_male), data = df)+geom_density(fill = "red", alpha = 0.5, color = "red")
df$young_male[ind] = df$young_all[ind]-df$young_female[ind]

#working population
ind = which(df$work_all != df$work_female+df$work_male)
ind
ggplot(aes(work_all-work_female-work_male), data = df)+geom_density(fill = "red", alpha = 0.5, color = "red")
df$work_male[ind] = df$work_all[ind]-df$work_female[ind]

#ekder population
ind = which(df$ekder_all != df$ekder_female+df$ekder_male)
ind
ggplot(aes(ekder_all-ekder_female-ekder_male), data = df)+geom_density(fill = "red", alpha = 0.5, color = "red")
df$ekder_male[ind] = df$ekder_all[ind]-df$ekder_female[ind]

#View(df)
#to match raion for all attributes
h=0
sub_2 = colnames(df)
for(i in colnames(df[,sub_2[14:84]])){
  k = table(df$sub_area,df[,i])
  for (j in rownames(k)){
    h=h+1
    if(length(unique(k[j,])) > 2){
      print(c(i,j))
      #print(j)
    }
  }
}
h/71

#Material info
material = c("raion_build_count_with_material_info", "build_count_block","build_count_wood","build_count_frame",
"build_count_brick", "build_count_monolith","build_count_panel","build_count_foam",
"build_count_slag","build_count_mix", "price_doc" )
#cor(df[,material], use="complete.obs")
corrplot(cor(cbind(df[,material]), use="complete.obs"))
summary(df[, material])
ind = which(is.na(df$build_count_block))
df$sub_area[ind]

#builddate
date = c("raion_build_count_with_builddate_info",
         "build_count_before_1920","build_count_1921.1945",
         "build_count_1946.1970", "build_count_1971.1995",
         "build_count_after_1995", "price_doc")
#cor(df[,date], use="complete.obs")
corrplot(cor(cbind(df[,date]), use="complete.obs"))
summary(df[, material])

#metro
metro = c('ID_metro','metro_min_avto','metro_km_avto','metro_min_walk','metro_km_walk','price_doc')
#cor(df[,metro], use="complete.obs")
corrplot(cor(cbind(df[,metro]), use="complete.obs"))
summary(df[,metro])

#neighbourhood
nei = c('kindergarten_km', 'school_km', 'park_km', 'green_zone_km',
        'industrial_km', 'water_treatment_km', 'cemetery_km','incineration_km','price_doc')
#cor(df[,nei], use="complete.obs")
corrplot(cor(cbind(df[,nei]), use="complete.obs"))
summary(df[,nei])#No Na's

#Transport
trans = c('railroad_station_walk_km','railroad_station_walk_min',
          'ID_railroad_station_walk','railroad_station_avto_km',
          'railroad_station_avto_min','ID_railroad_station_avto',
          'public_transport_station_km','public_transport_station_min_walk',
          'price_doc')
#cor(df[,trans], use="complete.obs")
corrplot(cor(cbind(df[,trans]), use="complete.obs"))
summary(df[,trans])

#sites
#big road1_1line & railroad_1ine &,'water_1line' is categorical
site = c('water_km','mkad_km','ttk_km','sadovoe_km','bulvar_ring_km',
         'kremlin_km','big_road1_km','ID_big_road1','big_road2_km','ID_big_road2',
         'railroad_km','zd_vokzaly_avto_km','ID_railroad_terminal','bus_terminal_avto_km',
         'ID_bus_terminal', 'price_doc')
#cor(df[,site], use="complete.obs")
corrplot(cor(cbind(df[,site]), use="complete.obs"))
summary(df[,site])#No NA's

#big_road1_line
count = table(df$big_road1_1line, df$sub_area)
barplot(count,
        xlab="subarea", col=c("darkblue","red"),
        legend = rownames(count))
ggplot(aes(x=price_doc), data=df) + geom_density(fill='red', color='red') + facet_grid(~big_road1_1line)+
  scale_x_continuous(trans='log')+geom_vline(xintercept = mean(df$price_doc))
df %>% 
  group_by(big_road1_1line) %>% 
  summarize(median(price_doc))
#Not dependent on subarea

#railroad_1line
count = table(df$railroad_1line, df$sub_area)
barplot(count,
        xlab="subarea", col=c("darkblue","red"),
        legend = rownames(count))
ggplot(aes(x=price_doc), data=df) + geom_density(fill='red', color='red') + facet_grid(~railroad_1line)+
  scale_x_continuous(trans='log')+geom_vline(xintercept = mean(df$price_doc))
df %>% 
  group_by(railroad_1line) %>% 
  summarize(median(price_doc))
#not dependent on subarea

#water_1line
count = table(df$water_1line, df$sub_area)
barplot(count,
        xlab="subarea", col=c("darkblue","red"),
        legend = rownames(count))
ggplot(aes(x=price_doc), data=df) + geom_density(fill='red', color='red') + facet_grid(~water_1line)+
  scale_x_continuous(trans='log')+geom_vline(xintercept = mean(df$price_doc))
df %>% 
  group_by(water_1line) %>% 
  summarize(median(price_doc))
#independent

#neighbourhood distance from important places
neigh = c('oil_chemistry_km','nuclear_reactor_km', 'radiation_km',
          'power_transmission_line_km', 'thermal_power_plant_km',
          'ts_km', 'big_market_km', 'market_shop_km', 'fitness_km',
          'swim_pool_km', 'ice_rink_km','stadium_km','basketball_km',
          'hospice_morgue_km','detention_facility_km','public_healthcare_km',
          'university_km','workplaces_km','shopping_centers_km',
          'office_km', 'additional_education_km','preschool_km',
          'big_church_km','church_synagogue_km','mosque_km','theater_km',
          'museum_km','exhibition_km','catering_km','price_doc')
#cor(df[,neigh], use="complete.obs")
corrplot(cor(cbind(df[,neigh]), use="complete.obs"))
summary(df[,neigh])#No NA's


#ecology
count = table(df$ecology, df$sub_area)
count
barplot(count,
        xlab="subarea", col=c("darkblue","red", 'magenta', 'yellow', 'black'),
        legend = rownames(count))
ggplot(aes(x=price_doc), data=df) + geom_density(fill='red', color='red') + facet_grid(~ecology)+
  scale_x_continuous(trans='log')+geom_vline(xintercept = mean(df$price_doc))
df %>% 
  group_by(ecology) %>% 
  summarize(median(price_doc))

#median price according to subarea
df %>%
  group_by(sub_area) %>%
  summarize(n = median(price_doc)) %>%
  ggplot(aes(x = reorder(sub_area,n), y = n)) +
  geom_bar(stat = 'identity', alpha = "0.5") + 
  geom_smooth(color = "red")+
  labs(x='', y='price_doc', title='Sales price over time')

#median price according to timestamp
df %>%
  group_by(timestamp) %>%
  summarize(n = median(price_doc)) %>%
  ggplot(aes(x = timestamp, y = n)) +
  geom_bar(stat = 'identity') + 
  geom_smooth(color = "red")+
  labs(x='', y='price_doc', title='Sales price over time')

#missing data in other factors from 154:291
data_miss = aggr(df[,154:291], col=mdc(1:2), numbers=TRUE, sortVars=TRUE, labels=names(df[,154:291])
                 , cex.axis=.7, gap=3, ylab=c("Proportion of missingness","Missingness Pattern"))
md.pattern(df[,154:291])
#prom_part_5000
#cafe_sum_3000_min_price_avg
#cafe_sum_3000_max_price_avg
#cafe_avg_price_3000
#cafe_sum_2000_min_price_avg
#cafe_sum_2000_max_price_avg
#cafe_avg_price_2000
#cafe_sum_1500_min_price_avg
#cafe_sum_1500_max_price_avg
#cafe_avg_price_1500
#cafe_sum_1000_min_price_avg
#cafe_sum_1000_max_price_avg
#cafe_avg_price_1000
#cafe_sum_500_min_price_avg
#cafe_sum_500_max_price_avg
#cafe_avg_price_500

###XGBoost

test.matrix = as.matrix(Test_matrix[,-1])
train.matrix = as.matrix(Train_matrix[,-1])
mode(test.matrix) = "numeric"
mode(train.matrix) = "numeric"
y = as.matrix(as.integer(df$price_doc))

param <- list("objective" = "reg:linear",    # multiclass classification 
              "eval_metric" = "rmse",    # evaluation metric 
              "boosting" = "gbdt",
              'learning_rate' = 0.01,
              "verbose" = 0,
              "num_leaves"  = 2^5,
              "nthread" = 8,   # number of threads to be used 
              "max_depth" = 7,    # maximum depth of tree 
              "eta" = 0.03,    # step size shrinkage 
              "gamma" = 0,    # minimum loss reduction 
              "subsample" = 0.7,    # part of data instances to grow tree 
              "colsample_bytree" = 0.7,  # subsample ratio of columns when constructing each tree 
              "min_child_weight" = 6,  # minimum sum of instance weight needed in a child
              "num_rounds" = 1500,
              'bagging_fraction'= 0.95,
              'bagging_freq'= 1,
              'bagging_seed'= 1,
              'feature_fraction'= 0.7,
              'feature_fraction_seed'= 1,
              'max_bin'= 100
)

set.seed(1234)
# k-fold cross validation, with timing
nround.cv = 200
bst.cv <- xgb.cv(param=param, data=train.matrix, label=y, 
                 nfold=3, nrounds=1700, prediction=TRUE, verbose=FALSE)

set.seed(1234)
# k-fold cross validation, with timing
nround.cv = 200
bst.cv <- xgb.cv(param=param, data=train.matrix, label=y, 
                 nfold=3, nrounds=1700, prediction=TRUE, verbose=FALSE)

min.merror.idx = which.min(bst.cv$evaluation_log[, test_rmse_mean]) 
min.merror.idx 
bst.cv$evaluation_log[min.merror.idx, test_rmse_mean]
bst <- xgboost(param=param, data=train.matrix, label=y, 
               nrounds=min.merror.idx, verbose=0)
summary(bst)
pred.train.xg = predict(bst, train.matrix)
plot(y, pred.train.xg, xlab = "actual", ylab = "predicted")
pred.test.xg = predict(bst, test.matrix)
output1 = cbind(30474:38135, pred.test.xg)
write.csv(output1, file = "output1.csv")
# get the feature real names
names = dimnames(train.matrix)[[2]]
# compute feature importance matrix
importance_matrix = xgb.importance(names, model=bst)
names = importance_matrix[1:60,1]
xgb.plot.importance(importance_matrix[1:60,])

###Linear regression
##IMputing first 60
library(mice)
names = as.vector(unlist(names))
train.lm = Train_matrix[,names]
test.lm = Test_matrix[,names]
tempData <- mice(train.nnet,m=2,maxit=5,meth='cart',seed=1234)
tempData2 = mice(test.nnet,m=2,maxit=5,meth='cart',seed=1234)
train2 = complete(tempData,3)
test2 = complete(tempData2,3)
traind = cbind(train2,y)
lm.fit = lm(y~., data = traind)
lm.predict.train = predict(lm.fit, traind[,-60])
lm.predict.test = predict(lm.fit, test2)
plot(traind$y, lm.predict.train, xlab = "actual", ylab = "predicted")
ind = which(lm.predict.test <=0)
lm.predict.test[ind] = abs(lm.predict.test[ind])
summary(lm.fit)
AIC(lm.fit)
output2 = cbind(id = 30474:38135, price_doc = lm.predict.test)
write.csv(output2, file = "output2.csv")



###neuralnet
n = as.vector(unlist(names(traind)))
f <- as.formula(paste("y ~", paste(n[!n %in% "y"], collapse = " + ")))
nnet.fit = neuralnet(f, data = traind, hidden = c(50,30),linear.output = TRUE )
summary(nnet.fit)
plot(nnet.fit)
nnet.pred.train = compute(nnet.fit, traind[,-60] )
nnet.pred.test = compute(nnet.fit, test2)
plot(nnet.fit)
output3 = cbind(id = 30474:38135, price_doc = nnet.pred.test)
write.csv(output3, file = "output3.csv")


###glm
fit.glm <- glm(y~., 
               data = traind, 
               family = Gamma(link = log))
summary(fit.glm)
pred.glm.train = predict(fit.glm, trainx[,-60])
pred.glm.test = predict(fit.glm, test2)
output4 = cbind(id = 30474:38135, price_doc = pred.glm.test)
write.csv(output4, file = "output4.csv")
fit.caret.glm <- caret::train(form =y ~ ., 
                              data = trainx, 
                              trControl = trainControl(method = "none"),   
                              method = "glm",
                              family = Gamma(link = log))
pred.glm.train.caret = predict(fit.caret.glm, trainx[,-60])
pred.glm.test.caret = predict(fit.caret.glm, test2)
output5 = cbind(id = 30474:38135, price_doc= pred.glm.test.caret)
write.csv(output5, file = "output5.csv")


###h2o
library(h2o)
h2o.init(nthreads = -1)
train.h2o = as.h2o(trainx)
test.h2o = as.h2o(test2)
#View(trainx)
col = names(trainx[,-60])
fit.h2o <- h2o.glm(x = col,
                   y = "y",
                   training_frame = train.h2o,
                   family = "Gamma",
                   lambda = 0.1)
summary(fit.h2o)
pred.h20.train = h2o.performance(model = fit.h2o, newdata = train.h2o[,col])
perd.h20.test <- h2o.performance(model = fit.h2o, newdata = test.h2o)
h2o.r2(perf)


##glmnet
library(glmnet)
fit.glmnet = glmnet(trainx[,-60], trainx[,60])



fit.glmnet.cv <- cv.glmnet(x = trainx[,-60],
                           y = trainx[,60],
                           family = "Gaussian",
                           alpha = 1.0)





fit <- h2o.glm(x = col,
               y = "V1",
               training_frame = train.h2o,
               family = "poisson",
               lambda_search = TRUE,  # compute lasso path
               alpha = 1)
perf <- h2o.performance(model = fit,
                        newdata = test.h2o)
preddd = as.matrix(h2o.predict(fit, test.h2o))
out = cbind(30474:38135, preddd)