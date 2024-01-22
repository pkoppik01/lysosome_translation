df %>% summary()

df %>% ggplot(aes(TotalPeptidesOverallInGene))+
  geom_histogram()+
  geom_vline(xintercept = 3)


df_log<-df %>% 
  rowwise() %>% 
  mutate(Median_0= log(median(ConditionNorm_BSA_0_1,ConditionNorm_BSA_0_2, ConditionNorm_BSA_0_3,na.rm = T),base=2),
         Median_50 =log(median(ConditionNorm_BSA_50_1, ConditionNorm_BSA_50_2, ConditionNorm_BSA_50_3,na.rm = T),base=2)) 


df_log %>%
  filter(TotalPeptidesOverallInGene>=3) %>% 
  ggplot(aes(Median_0,Median_50))+
  geom_point(color="red",alpha=0.5)+
  geom_point(data= df_log %>% filter(TotalPeptidesOverallInGene<3),color="blue",alpha=0.5)#+
  theme_classic()
  
  