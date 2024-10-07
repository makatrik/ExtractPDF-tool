# Probabilistic estimates for mean concentration -----------------------------------------------------------------

bts.conc_ %>% #valid for sOIM, ssOIM: compute mean of distribution of bootstrapped mean occurrence
  group_by(k, mean) %>%
  dplyr::reframe(mean(mean_conc))
bts.conc_ %>% #valid for sOIM, ssOIM: compute median of distribution of bootstrapped mean occurrence
  group_by(k, mean) %>%
  dplyr::reframe(median(mean_conc))
bts.conc_ %>% #valid for sOIM, ssOIM: compute InterQuartile Range of distribution of bootstrapped mean occurrence
  group_by(k, mean) %>%
  dplyr::reframe(IQR(mean_conc))
bts.conc_ %>% #valid for sOIM, ssOIM: compute standard deviation of distribution of bootstrapped mean occurrence
  group_by(k, mean) %>%
  dplyr::reframe(sd(mean_conc))

o<-bts.conc_ %>% #valid for swOIM: compute mean of distribution of bootstrapped mean occurrence for each set of weight
  group_by(k, mean, setii,w80006,w80038,w80039) %>%
  reframe(mean(mean_conc))%>% filter(k==1, mean=="swOIM");
print(o,n = 1000) 
rm(o)
o<-bts.conc_ %>% #valid for swOIM: compute median of distribution of bootstrapped mean occurrence for each set of weight
  group_by(k, mean, setii,w80006,w80038,w80039) %>%
  reframe(median(mean_conc))%>% filter(k==1, mean=="swOIM");
print(o,n = 1000) 
rm(o)
o<-bts.conc_ %>%#valid for swOIM: compute InterQuartile Range of distribution of bootstrapped mean occurrence for each set of weight
  group_by(k, mean, setii,w80006,w80038,w80039) %>%
  reframe(IQR(mean_conc))%>% filter(k==1, mean=="swOIM");
print(o,n = 1000) 
rm(o)
o<-bts.conc_ %>%#valid for swOIM: compute standard deviation of distribution of bootstrapped mean occurrence for each set of weight
  group_by(k, mean, setii,w80006,w80038,w80039) %>%
  reframe(sd(mean_conc))%>% filter(k==1, mean=="swOIM"); 
print(o,n = 1000) 
rm(o)
