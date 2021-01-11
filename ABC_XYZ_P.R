



### Simulation Parameters

### Simulated Retail Inventory

data <-read_csv(Retail_Data_Set.csv)  #Provided Data Set


### ABC Analysis  arrange(desc(Revenue_2_Sales_Ratio)) %>% 

abc <- data %>% group_by(product_title) %>% 
  dplyr::summarise(Total_Quantity  = sum(ordered_item_quantity), Total_Cost =sum(total_cost), Total_Profit =sum(gross_profit)) %>% 
  dplyr::mutate(TQ_TC =Total_Quantity/Total_Cost) %>%
  dplyr::select(product_title, Total_Quantity, Total_Cost, TQ_TC, Total_Profit) %>%
  dplyr::arrange(desc(TQ_TC))

### Make Data Frame
abc <-data.frame(distinct(abc))

###  Build Filters
quantile_abc <-quantile(abc$TQ_TC, na.rm=TRUE)
C_Filter <-as.numeric(quantile_abc[1])
B_Filter <-as.numeric(quantile_abc[3])
A_Filter <-as.numeric(quantile_abc[4])

### Assign Category

abc_df <-abc %>% mutate(C =ifelse(TQ_TC < B_Filter, 1,0), B = ifelse(TQ_TC >= B_Filter & TQ_TC<= A_Filter,1,0), 
                       A= ifelse(TQ_TC > A_Filter & TQ_TC != Inf,1,0), D = ifelse(TQ_TC == Inf,1,0))


### Profit Analysis

quantile_p <-quantile(abc$Total_Profit, na.rm=TRUE)
P1_Filter <-as.numeric(quantile_p[1])
P2_Filter <-as.numeric(quantile_p[3])
P3_Filter <-as.numeric(quantile_p[4])

abc_df <- abc_df %>% mutate(P3 =ifelse(Total_Profit < P1_Filter , 1,0), P2 = ifelse(Total_Profit >= P1_Filter & Total_Profit <= P3_Filter,1,0), 
                        P1= ifelse(Total_Profit > P3_Filter,1,0)) %>% dplyr::select(product_title, Total_Quantity, Total_Cost,
                                                                                    TQ_TC, Total_Profit, A,B,C,D, P1, P2,P3)



### XYZ Analysis
xyz <-data %>% group_by(product_title) %>% 
  dplyr::mutate(STD_DEV_Order  = sd(ordered_item_quantity), Mean_Order= mean(ordered_item_quantity)) %>%
  dplyr::select(product_title, Mean_Order, STD_DEV_Order) 

xyz <-data.frame(distinct(xyz))

### Remove NAs
xyz$STD_DEV_Order[is.na(xyz$STD_DEV_Order)]<-0

### Population Mean and Standard Deviation
pop_sd <-sd(xyz$STD_DEV_Order)
pop_mean <-mean(xyz$STD_DEV_Order)

### Find Z-Score
xyz <- xyz %>% dplyr::mutate(Z_Score_STD_DEV= (STD_DEV_Order - pop_mean)/pop_sd) %>% arrange(desc(Z_Score_STD_DEV))

### Build Filters

quantile_xyz <-quantile(xyz$Z_Score_STD_DEV, na.rm=TRUE)
Z_Filter <-2
Y_Filter <-as.numeric(quantile_xyz[4])
X_Filter <-as.numeric(quantile_xyz[1])

xyz_df <-xyz %>% mutate(Z =ifelse(Z_Score_STD_DEV > Z_Filter, 1,0), Y = ifelse(Z_Score_STD_DEV >= Y_Filter & Z_Score_STD_DEV <= Z_Filter,1,0), 
                        X= ifelse(Z_Score_STD_DEV  < Y_Filter,1,0), U = ifelse(Z_Score_STD_DEV == Inf,1,0))






### Combine

abc_xyz <-abc_df %>% left_join(xyz_df, by="product_title")

abc_xyz <-data.frame(abc_xyz)

### As
abc_xyz <- abc_xyz %>% dplyr::mutate(XA =ifelse(X + A == 2, 1, 0))
abc_xyz <- abc_xyz %>% dplyr::mutate(YA =ifelse(Y + A == 2, 1, 0))
abc_xyz <- abc_xyz %>% dplyr::mutate(ZA =ifelse(Z + A == 2, 1, 0))

### Bs
abc_xyz <- abc_xyz %>% dplyr::mutate(XB =ifelse(X + B == 2, 1, 0))
abc_xyz <- abc_xyz %>% dplyr::mutate(YB =ifelse(Y + B == 2, 1, 0))
abc_xyz <- abc_xyz %>% dplyr::mutate(ZB =ifelse(Z + B == 2, 1, 0))

### Cs
abc_xyz <- abc_xyz %>% dplyr::mutate(XC =ifelse(X + C == 2, 1, 0))
abc_xyz <- abc_xyz %>% dplyr::mutate(YC =ifelse(Y + C == 2, 1, 0))
abc_xyz <- abc_xyz %>% dplyr::mutate(ZC =ifelse(Z + C == 2, 1, 0))


### Ds
abc_xyz <- abc_xyz %>% dplyr::mutate(UNK =ifelse(D == 1 | U ==1,1,0))

### Add Categorical Values

### As
abc_xyz$segment[abc_xyz$XA == 1] <- c("High_Contribution_Low_Variance")
abc_xyz$segment[abc_xyz$YA == 1] <- c("High_Contribution_Medium_Variance")
abc_xyz$segment[abc_xyz$ZA == 1] <- c("High_Contribution_High_Variance")


### Bs
abc_xyz$segment[abc_xyz$XB == 1] <- c("Medium_Contribution_Low_Variance")
abc_xyz$segment[abc_xyz$YB == 1] <- c("Medium_Contribution_Medium_Variance")
abc_xyz$segment[abc_xyz$ZB == 1] <- c("Medium_Contribution_High_Variance")

### Cs
abc_xyz$segment[abc_xyz$XC == 1] <- c("Low_Contribution_Low_Variance")
abc_xyz$segment[abc_xyz$YC == 1] <- c("Low_Contribution_Medium_Variance")
abc_xyz$segment[abc_xyz$ZC == 1] <- c("Low_Contribution_High_Variance")

### Unknowns
abc_xyz$segment[abc_xyz$D == 1] <- c("Unclassified")
abc_xyz$segment[abc_xyz$U == 1] <- c("Unclassified")
abc_xyz$segment[is.na(abc_xyz$segment)] <- c("Unclassified")


abc_xyz_df <- abc_xyz %>% dplyr::select(product_title, Total_Quantity, Total_Cost, TQ_TC,  Total_Profit, P1, P2, P3, X, Y, Z, Mean_Order, STD_DEV_Order, Z_Score_STD_DEV, segment)

abc_xyz_df %>% dplyr::filter(segment == "High_Contribution_Low_Variance") %>% top_n(50)

write.csv(abc_xyz_df, paste0("SBG_ABC_XYZ.csv"))

#####################  Tree Map for Customer Segments ################
title<-c("ABC_XYZ")

tmp <- abc_xyz_df  %>% group_by(segment) %>% summarize(n=n()) 

### Tree Map
treemap(tmp,
        index=c("segment"),
        vSize="n",
        type="index",
        palette = "Set1",                        # Select your color palette from the RColorBrewer presets or make your own.
        title="ABC_XYZ Product Inventory Analysis",                      # Customize your title
        fontsize.title=10,                       # Size of the title
        
)


#######################################   ABC-XYZ Segments ##################################

title<-paste("ABC-XYZ Segments")


tmp <- abc_xyz_df  %>% group_by(segment) %>% summarize(n=n()) 


Segment <-unique(tmp$segment)
Number <-tmp$n
tmp<-data.frame(Segment, Number)

tmp <- tmp[order(tmp$Number), ] 
tmp$Number <- factor(tmp$Number, levels = unique(tmp$Number))

ggplot(data=tmp, aes(x = Segment, y=Number, fill= Segment, label=Number)) + geom_bar(stat = "identity")+ 
ggtitle(label =title) +theme_minimal()+theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold")) + 
xlab("ABC-XYZ") + ylab("Number") + coord_flip() + 
  geom_text(aes( label =Number, y= ..prop.. ), stat= "count", hjust = -5) 

#######################################   ABC-XYZ Segments With High Profits ##################################
title<-paste("ABC-XYZ Segments With High Profitability (P1)")


tmp <- abc_xyz_df  %>%  dplyr::filter(P1 == 1)  %>% group_by(segment) %>% summarize(n=n()) 


Segment <-unique(tmp$segment)
Number <-tmp$n
tmp<-data.frame(Segment, Number)

tmp <- tmp[order(tmp$Number), ] 
tmp$Number <- factor(tmp$Number, levels = unique(tmp$Number))

ggplot(data=tmp, aes(x = Segment, y=Number, fill= Segment, label=Number)) + 
  geom_bar(stat = "identity")+ ggtitle(label =title) +theme_minimal()+theme(plot.title = element_text(hjust = 0.5, lineheight = 0.8, face = "bold")) + 
  xlab("ABC-XYZ") + ylab("Number") + coord_flip() + geom_text(aes( label =Number, y= ..prop.. ), stat= "count", hjust = -5) 

