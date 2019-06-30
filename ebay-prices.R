#----Import----

## either Windows 
# file <- "C:/Google Drive/in-progress/laptop-purchase/ebay-prices.csv"
## or Ubuntu
file <- "ebay-prices.csv"

## Load data
df <- read.csv(file = file,
               colClasses = c(
                 date = 'Date',
                 model = 'factor',
                 cpu = 'factor',
                 ram = 'numeric',
                 hd = 'numeric',
                 condition = 'factor',
                 damage = 'factor',
                 #url = "NULL", # skip column
                 url = 'character',
                 sold_for = 'numeric',
                 shipping = 'numeric',
                 total = 'numeric'
               ))
summary(df)

#----EDA and Process----

par(mfrow = c(1,1))

## CPU: can describe by type and generation 
plot(df$cpu,las=2) # number of CPUs
plot(df$cpu ,df$sold_for, # Price by CPU
     xlab = 'CPU',
     ylab = 'Sell price ($)',
     las=2)

### Type (i5, i7)
df$cpu_type <- rep('',dim(df)[1])
df$cpu_type[grep('i5-',df$cpu)] <- 'i5'
df$cpu_type[grep('i7-',df$cpu)] <- 'i7'
df$cpu_type <- as.factor(df$cpu_type)
relevel(df$cpu_type, ref = 'i5')
### Generation (6th,7th)
df$cpu_gen <- rep('',dim(df)[1])
df$cpu_gen[grep('-6',df$cpu)] <- '6'
df$cpu_gen[grep('-7',df$cpu)] <- '7'
df$cpu_gen <- as.factor(df$cpu_gen)
relevel(df$cpu_gen, ref = '6')

df <- subset(df, select = -cpu)

## RAM: no need to modify
plot(df$ram ,df$sold_for, # Price by RAM
     xlab = 'RAM (GB)',
     ylab = 'Sell price ($)')

## HD: one datapoint for 1024, remove.
plot(df$hd ,df$sold_for, # Price by HD
     xlab = 'Hard drive (GB)',
     ylab = 'Sell price ($)')
df <- df[!(df$hd == 1024),] # one datapoint for 1024, remove.

## Model: Split by yearly model and slim build
plot(df$model) # number of Models
plot(df$model, df$sold_for, # Price by Model
     xlab = 'Model',
     ylab = 'Sell price ($)')
### yearly build
df$model_yearly <- rep('',dim(df)[1])
df$model_yearly[grep('T460',df$model)] <- 'T460'
df$model_yearly[grep('T470',df$model)] <- 'T470'
df$model_yearly <- as.factor(df$model_yearly)
relevel(df$model_yearly, ref = 'T460')

### slim build
# df$slim <- rep(FALSE,dim(df)[1])
# df$slim <- grepl('0s',df$model)

df <- subset(df, select = -model)

## Condition: Make 'open box' the reference
## Combine 'used' and 'seller refurbished'
plot(df$condition) # number of conditions
plot(df$condition, df$sold_for, # Price by condition
     xlab = 'Condition',
     ylab = 'Sell price ($)')
df$condition[df$condition == 'used' | 
               df$condition == 'seller refurbished'] <- 'used'
df$condition <- droplevels(df$condition)
relevel(df$condition, ref = 'open box')

## Damage: Make 'none' the reference. 
## Combine 'scratches' and 'nicks'
plot(df$damage) # number of damage
plot(df$damage, df$sold_for, # Price by damage
     xlab = 'Damage',
     ylab = 'Sell price ($)')
levels(df$damage) <- c(levels(df$damage), 'damaged')
df$damage[df$damage == 'nicks' | df$damage == 'scratches'] <- 'damaged'
df$damage <- droplevels(df$damage)
relevel(df$damage, ref = 'none')

## Combine damage and condition
df$body <- rep('',dim(df)[1])
df$body[df$condition == 'open box'] <- 'open box'
df$body[df$damage == 'wear'] <- 'wear'
df$body[df$damage == 'damaged'] <- 'damaged'
df$body[df$condition == 'used' & df$damage == 'none'] <- 'none'
df$body <- as.factor(df$body)
print(levels(df$body))
df$body <- factor(df$body, levels(df$body)[c(3,2,4,1)])
print(levels(df$body))
relevel(df$body, ref = 'open box')
plot(df$body) # number of body condition and damage
plot(df$body, df$sold_for, # Price by body condition and damage
     xlab = 'Body',
     ylab = 'Sell price ($)')

df <- subset(df, select = -c(damage,condition))

#----Fitting----
summary(df)

## First predictor
summary(lm(sold_for ~ date, data = df))# p > .05
summary(lm(sold_for ~ ram, data = df))# p = 
summary(lm(sold_for ~ hd, data = df))# p = 
summary(lm(sold_for ~ body, data = df))# p = 
summary(lm(sold_for ~ cpu_type, data = df))# p = 
summary(lm(sold_for ~ cpu_gen, data = df))# p = 
summary(lm(sold_for ~ model_yearly, data = df))# **p = 

## Second predictor
summary(lm(sold_for ~ model_yearly + date, data = df))# p > .05
summary(lm(sold_for ~ model_yearly + ram, data = df))# p = 0.00477
summary(lm(sold_for ~ model_yearly + hd, data = df))# p = 0.000104
summary(lm(sold_for ~ model_yearly + body, data = df))# p > .05
summary(lm(sold_for ~ model_yearly + cpu_type, data = df))# **p = 1.11e-06**
summary(lm(sold_for ~ model_yearly + cpu_gen, data = df))# p = 0.0278

## Third predictor
summary(lm(sold_for ~ model_yearly + cpu_type + date, data = df))# p > .05
summary(lm(sold_for ~ model_yearly + cpu_type + ram, data = df))# p > .05
summary(lm(sold_for ~ model_yearly + cpu_type + hd, data = df))# p = 0.000254
summary(lm(sold_for ~ model_yearly + cpu_type + body, data = df))# **p = 0.000129**
summary(lm(sold_for ~ model_yearly + cpu_type + cpu_gen, data = df))# p = > .05

## Fourth predictor
summary(lm(sold_for ~ model_yearly + cpu_type + body + date, data = df))# p > .05
summary(lm(sold_for ~ model_yearly + cpu_type + body + ram, data = df))# p = 0.0258
summary(lm(sold_for ~ model_yearly + cpu_type + body + hd, data = df))# **p = 1.32e-05**
summary(lm(sold_for ~ model_yearly + cpu_type + body + cpu_gen, data = df))# p > .05

## Fifth predictor
summary(lm(sold_for ~ model_yearly + cpu_type + body + hd + date, data = df))# p > .05
summary(lm(sold_for ~ model_yearly + cpu_type + body + hd + ram, data = df))# p > .05
summary(lm(sold_for ~ model_yearly + cpu_type + body + hd + cpu_gen, data = df))# p > .05

## Stopped here and added all the variables that I think are relevant.

## Linear Model
fit <- lm(sold_for ~ model_yearly + cpu_type + body + hd + ram, 
          data = df)
summary(fit)

#----Save----
save(df,fit,file = 'model.Rdata')

#----Load----
load(file = 'model.Rdata')

#----Prediction----
names(df)

## predict one
predict(object = fit,
        newdata = data.frame(
          model_yearly = 'T470', # T460 T470
          slim = TRUE, # TRUE FALSE
          cpu_type = 'i5', # i5 i7
          cpu_gen = '7', # 6 7
          ram = 8, #
          hd = 256, # 
          body = 'none' # "open box" none wear damaged
        )
)

# ThinkPad 90W Pro Dock (w/ adapter) = $25 (used), $45 (new)
# ThinkPad T460S/T470s charger 45W = $15 (new)


## predict all
df_prd <- df
df_prd$predicted <- predict(object = fit,
                            newdata = df) # Predicted prices
df_prd$predicted_percent <- 1 - (df$sold_for / df_prd$predicted) # Prediced prices as percent of actual 

hist(df_prd$predicted_percent)
par(mfrow = c(2,1))
hist(df_prd$predicted_percent[df_prd$model_yearly == 'T470'],
     xlab = '',
     main = 'T470',
     breaks = seq(-.2,.2,.02))
hist(df_prd$predicted_percent[df_prd$model_yearly == 'T460'],
     xlab = 'Discount (%)',
     main = 'T460',
     breaks = seq(-.3,.2,.02))
# Conclusion- any discount 10% or more is very good.
# 10% + shipping should be great.

df_460 <- df_prd[df_prd$model_yearly == 'T460', ]
df_470 <- df_prd[df_prd$model_yearly == 'T470', ]

par(mfrow = c(1,1))
plot(x = df_470$date,
     y = df_470$predicted_percent,
     xlab = 'Date',
     ylab = 'Predicted Price (%)',
     pch = 16,
     col = 'black')
points(x = df_460$date,
       y = df_460$predicted_percent,
       pch = 16,
       col = 'grey')

# http://psref.lenovo.com/Product/ThinkPad_T460s
# http://psref.lenovo.com/Product/ThinkPad_T470s

