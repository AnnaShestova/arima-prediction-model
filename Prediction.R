#Loading the libraries
library(ggplot2)
library(rlang)

# Importing the dataset
dataset = read.csv('Data.csv')

# Selecting Mondays
index = (dataset[,6] == 1)
dataset = dataset[which(index == TRUE)  ,]

# Ref table for A_Shifts 
ref = data.frame(
  A_Shift = c(300000,400000,500000,600000,700000,800000,900000,1000000,
              1100000,1200000,1300000,1400000,1500000,1600000,1700000,1800000,
              1900000,2000000,2100000,2200000,2300000,2400000,2500000,2600000,
              2700000,2800000,2900000 ,3000000,3100000,3200000,3300000,3400000,
              3500000,3600000,3700000,3800000,3900000,4000000,4100000,4200000,4300000
              ,4400000,4500000,4600000,4700000,4800000,4900000,5000000,5100000,5200000
              ,5300000,5400000,5500000,5600000,5700000,5800000,5900000 ,6000000,6100000,6200000,6300000,6400000),
  Hight = c(0,0,0.939999998,1.970000029,3.00999999,4.03000021 ,5.039999962,6.050000191,7.050000191,8.039999962
            ,9.020000458,9.989999771,10.94999981,11.89999962,12.84000015,13.77000046,14.69999981,15.60999966
            ,16.51000023,17.40999985,18.29999924,19.17000008,20.04000092,20.89999962,21.75,22.59000015,23.43000031
            ,24.25,25.06999969,25.87000084,26.67000008,27.45999908,28.25,29.02000046,29.79000092,30.54000092
            ,31.29000092,32.04000092,32.77000046,33.5,34.22000122,34.93000031,35.63000107,36.33000183,37.02000046
            ,37.70000076,38.36999893,39.04000092,39.70000076,40.34999847,41,41.63999939,42.27000046,42.90000153
            ,43.52000046,44.13000107,44.74000168,45.34000015,45.93999863,46.54000092,47.13999939,47.74000168),
  Normal = c(0,0,0,0,0,0,0,0.01,0.090000004,0.230000004,0.400000006,0.620000005,0.870000005,1.149999976,1.460000038
             ,1.799999952,2.160000086,2.549999952,2.960000038,3.390000105,3.849999905,4.320000172,4.820000172
             ,5.329999924,5.860000134,6.409999847,6.980000019,7.559999943,8.159999847,8.770000458,9.390000343
             ,10.02999973,10.68000031,11.34000015,12.01000023,12.69999981,13.39000034,14.09000015,14.81000042
             ,15.52999973,16.26000023,17,17.73999977,18.5,19.26000023,20.02000046,20.79000092,21.56999969
             ,22.35000038,23.13999939,23.93000031,24.71999931,25.52000046,26.30999947,27.12000084,27.92000008
             ,28.72999954,29.53000069,30.32999992,31.12999916,31.93000031,32.72999954)
  
)


# A_shifts prediction
a_shift = dataset[,1]
a_shift_round = dataset[,2]
mileage = dataset[,3]

# Predictions
n = length(dataset)
y = seq(1,n)
k = 1
model_a = predict( arima(a_shift,order=c(1,1,0), method="ML"), n.ahead = k )
model_a_round = predict(arima(a_shift_round,order=c(1,1,0), method="ML"), n.ahead = k )
a_shift_pred =  model_a$pred
a_shift_round_pred = model_a_round$pred

# Dataframe Creation
XE = a_shift[c(n-1,n)]
Xr = a_shift_round[c(n-1,n)]
Xm = mileage[c(n-1,n)]
current_mileage = Xm[2]
past_a_shift = XE[1]
current_a_shift = XE[2]
past_a_shift_round = Xr[1] 
current_a_shift_round = Xr[2]

df = data.frame(
  Count_A_Shift = c(round(past_a_shift,0), round(current_a_shift,0), round(a_shift_pred,0) ),
  Count_A_Shift_round = c(past_a_shift_round, current_a_shift_round,a_shift_round_pred ),
  Type = c("1 Last Week", "2 This Week", "3 Next Week")
)
 df$Risk_Level = unique(dataset[,8])
 
 df

  library(dplyr)
df = inner_join(df, ref, by = c('Count_A_Shift_round' = "A_Shift"), all = TRUE)
df$Risk_Value = format(ifelse(is.na(df$Risk_Level), df$Normal,df$Hight), nsmall = 2)

#Mileage left untill high risk zone
pr_Shiftelement_A_Wear = 100 * a_shift_pred  / 3500000     # SQL
pr_Shiftelement_A_Mileage_left = (100 - pr_Shiftelement_A_Wear) * a_shift_pred / pr_Shiftelement_A_Wear
Mileage_until_high_risk_zone = format(current_mileage/(current_a_shift*pr_Shiftelement_A_Mileage_left), nsmall = 2)
Mileage_until_high_risk_zone = as.data.frame(Mileage_until_high_risk_zone)



barwidth <- 0.95
fill_color <- ifelse(df$Risk_Value >= 0 | df$Risk_Value < 6, "green", 
                     ifelse(df$Risk_Value >= 6 | df$Risk_Value < 10  , "orange", "red")   )

  ggplot(df, mapping = aes(x = Type, y = Count_A_Shift) ) +
  
  geom_bar(stat = "identity", 
           fill = fill_color,
           width = barwidth,
           show.legend = TRUE) +
  geom_text(aes(label = paste( "A Shift Total =", Count_A_Shift)),
            vjust=1.6,
            size = 5,
            colour = "white") +
  geom_text(aes(label = paste(" Risk Of Failure =", Risk_Value, "%")),
              vjust=6.6,
              size = 5,
              colour = "white") +
  ggtitle('Risk of Failure Prediction') +
  xlab('Prediction in weeks') +
  ylab('Count A Shift Total') + scale_y_continuous(labels = scales::comma)

#  install.packages("gridExtra")

 library(grid)

  sample_vp <- viewport(x = 0.22, y = 0.85, 
                        just = c("left", "top"),
                        height = 0.1, width = 0.2)
  pushViewport(sample_vp)
  grid.draw(grid.table(Mileage_until_high_risk_zone[,1], rows = c("Mileage left until high risk zone"),
                       theme = ttheme_minimal(base_size = 12, base_colour = "blue", base_family = "",
                                              parse = FALSE, padding = unit(c(10, 4), "mm"))))