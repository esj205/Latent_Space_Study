raw<-read.csv('rawdata3.csv', header=TRUE)
str(raw)

##2점 스케일링
raw$BQ2 <- ifelse(raw$BQ2>1,1,0)
raw$J2_1 <- ifelse(raw$J2_1>1,1,0)
raw$J2_2 <- ifelse(raw$J2_2>1,1,0)
raw$J2_3 <- ifelse(raw$J2_3>1,1,0)
raw$J2_4 <- ifelse(raw$J2_4>1,1,0)
raw$J2_5 <- ifelse(raw$J2_5>1,1,0)
raw$J2_6 <- ifelse(raw$J2_6>1,1,0)
raw$J2_7 <- ifelse(raw$J2_7>1,1,0)
raw$J2_8 <- ifelse(raw$J2_8>1,1,0)
raw$J2_9 <- ifelse(raw$J2_9>1,1,0)
raw$J2_10 <- ifelse(raw$J2_10>1,1,0)
raw$BQ7 <- ifelse(raw$BQ7>1,1,0)


##5점 스케일링
raw$BQ3_n2 <- ifelse(raw$BQ3_n2>4,1,0)
raw$B3 <- ifelse(raw$B3>3,1,0)
raw$B5_1 <- ifelse(raw$B5_1>3,1,0)
raw$B5_2 <- ifelse(raw$B5_2>3,1,0)
raw$B5_3 <- ifelse(raw$B5_3>3,1,0)
raw$B5_4 <- ifelse(raw$B5_4>3,1,0)
raw$B5_5 <- ifelse(raw$B5_5>3,1,0)
raw$E5_1 <- ifelse(raw$E5_1>3,1,0)
raw$E5_2 <- ifelse(raw$E5_2>3,1,0)
raw$E5_3 <- ifelse(raw$E5_3>3,1,0)
raw$E5_4 <- ifelse(raw$E5_4>3,1,0)
raw$M2_1 <- ifelse(raw$M2_1>3,1,0)
raw$M2_2 <- ifelse(raw$M2_2>3,1,0)
raw$M2_3 <- ifelse(raw$M2_3>3,1,0)
raw$M2_4 <- ifelse(raw$M2_4>3,1,0)
raw$M2_5 <- ifelse(raw$M2_5>3,1,0)
raw$BQ9 <- ifelse(raw$BQ9>3,1,0)

##6점 스케일링
raw$BQ4 <- ifelse(raw$BQ4>4,1,0)

##7점 스케일링
raw$I1_1 <- ifelse(raw$I1_1>4,1,0)
raw$I1_2 <- ifelse(raw$I1_2>4,1,0)
raw$I1_3 <- ifelse(raw$I1_3>4,1,0)
raw$I1_4 <- ifelse(raw$I1_4>4,1,0)
raw$I1_5 <- ifelse(raw$I1_5>4,1,0)
raw$I1_6 <- ifelse(raw$I1_6>4,1,0)
raw$I1_7 <- ifelse(raw$I1_7>4,1,0)
raw$K1 <- ifelse(raw$K1>4,1,0)
raw$L1_1 <- ifelse(raw$L1_1>4,1,0)
raw$L1_2 <- ifelse(raw$L1_2>4,1,0)
raw$L1_3 <- ifelse(raw$L1_3>4,1,0)
raw$L1_4 <- ifelse(raw$L1_4>4,1,0)
raw$L1_5 <- ifelse(raw$L1_5>4,1,0)
raw$L1_6 <- ifelse(raw$L1_6>4,1,0)
raw$L1_7 <- ifelse(raw$L1_7>4,1,0)
raw$M1_1 <- ifelse(raw$M1_1>4,1,0)
raw$M1_2 <- ifelse(raw$M1_2>4,1,0)
raw$M1_3 <- ifelse(raw$M1_3>4,1,0)
raw$M1_4 <- ifelse(raw$M1_4>4,1,0)
raw$M1_5 <- ifelse(raw$M1_5>4,1,0)
raw$M1_6 <- ifelse(raw$M1_6>4,1,0)
raw$M1_7 <- ifelse(raw$M1_7>4,1,0)
raw$M1_8 <- ifelse(raw$M1_8>4,1,0)
raw$M1_9 <- ifelse(raw$M1_9>4,1,0)
raw$M1_10 <- ifelse(raw$M1_10>4,1,0)
raw$M1_11 <- ifelse(raw$M1_11>4,1,0)
raw$M1_12 <- ifelse(raw$M1_12>4,1,0)

##9점 스케일링
raw$BQ12_2 <- ifelse(raw$BQ12_2>5,1,0)

##11점 스케일링
raw$BQ12_1 <- ifelse(raw$BQ12_1>6,1,0)

##100점 스케일링
raw$J1_1 <- ifelse(raw$J1_1>50,1,0)
raw$J1_2 <- ifelse(raw$J1_2>50,1,0)
raw$J1_3 <- ifelse(raw$J1_3>50,1,0)
raw$J1_4 <- ifelse(raw$J1_4>50,1,0)
raw$J1_5 <- ifelse(raw$J1_5>50,1,0)
raw$J1_6 <- ifelse(raw$J1_6>50,1,0)
raw$J1_7 <- ifelse(raw$J1_7>50,1,0)

raw<-raw[,-1]
raw

help('write.csv')
write.csv(raw, 'eda.csv', row.names = FALSE)
