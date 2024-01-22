third_log<-`N41AoverN41N_N40.41BTA_Ratios_viaRatios_BySample+Annotated` %>% 
  mutate(Median_A= log(MedianConditionNormN41A,base=2),
         Median_N =log(MedianConditionNormN41N,base=2)) 

third_log %>%
  filter(TotalPeptidesOverallInGene>=3) %>% 
  ggplot(aes(Median_A,Median_N))+
  geom_point(color="red",alpha=0.5)+
  #geom_point(data= temp_log %>% filter(TotalPeptidesOverallInGene<3),color="blue",alpha=0.5)#+
  theme_classic()
  
df_log<-df %>% 
    rowwise() %>% 
    mutate(Median_0= log(median(ConditionNorm_BSA_0_1,ConditionNorm_BSA_0_2, ConditionNorm_BSA_0_3,na.rm = T),base=2),
           Median_50 =log(median(ConditionNorm_BSA_50_1, ConditionNorm_BSA_50_2, ConditionNorm_BSA_50_3,na.rm = T),base=2)) 
  
  
df_log %>%
  filter(TotalPeptidesOverallInGene>=3) %>% 
  ggplot(aes(Median_0,Median_50))+
  geom_point(color="blue",alpha=0.4)+
  geom_point(aes(Median_A,Median_N),
             data=third_log %>%
               filter(TotalPeptidesOverallInGene>=3),
             color="red",alpha=0.4)+
  geom_hline(yintercept = -9) +
  geom_vline(xintercept = -9)+
  #geom_point(data= df_log %>% filter(TotalPeptidesOverallInGene<3),color="blue",alpha=0.5)#+
  theme_classic()
  
