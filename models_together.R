# model compare

all_models <- merge(rename(pov_keras1_A,poorKeras = poor), rename(pov_gbm1_A, poorGbm = poor), by = c("id","country")) %>%
  merge(rename(pov_rf1_A, poorRf = poor), by = c("id","country")) %>%
  merge(rename(pov_knn1_A, poorKnn = poor), by = c("id","country"))

all_models2 <- gather(all_models, model, value, -id,-country) %>%
  group_by(id,country) %>%
  mutate(poor_avg = mean(value)) %>%
  mutate(poor_vote = sum(value > 0.5)) %>%
  arrange(id,country)
