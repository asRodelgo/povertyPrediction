# model compare

all_models <- merge(rename(pov_keras1_A,poorKeras = poor), rename(pov_gbm1_A, poorGbm = poor), by = c("id","country")) %>%
  merge(rename(pov_rf1_A, poorRf = poor), by = c("id","country")) %>%
  merge(rename(pov_knn1_A, poorKnn = poor), by = c("id","country"))

all_modelsA <- gather(all_models, model, value, -id,-country) %>%
  group_by(id,country) %>%
  mutate(poor_avg = mean(value)) %>%
  mutate(poor_vote = sum(value > 0.5)) %>%
  arrange(id,country)

all_modelsB <- gather(all_models, model, value, -id,-country) %>%
  group_by(id,country) %>%
  mutate(poor_avg = mean(value)) %>%
  mutate(poor_vote = sum(value > 0.5)) %>%
  arrange(id,country)

all_modelsC <- gather(all_models, model, value, -id,-country) %>%
  group_by(id,country) %>%
  mutate(poor_avg = mean(value)) %>%
  mutate(poor_vote = sum(value > 0.5)) %>%
  arrange(id,country)

all_models_final <- bind_rows(all_modelsA,all_modelsB,all_modelsC)
write.csv(all_models_final, "final_models_avg_byCountry.csv", row.names = FALSE)

final_file <- distinct(all_models_final, id, country, poor_avg, poor_vote) %>%
  mutate(new_prob = ifelse(poor_avg > .7 & poor_vote > 3, .99,
                           ifelse(poor_avg < .3 & poor_vote < 1, .01, poor_avg))) %>%
  mutate(id = as.numeric(id))

final_file2 <- left_join(submission_Yukun, final_file, by="id") %>%
  mutate(poor_vote = ifelse(poor > .5, poor_vote + 1, poor_vote),
         poor_avg = (4*poor_avg + poor)/5) %>%
  mutate(new_prob = ifelse(poor_avg > .65 & poor_vote > 3, .999,
                           ifelse(poor_avg < .35 & poor_vote < 2, .001,
                                  ifelse(poor_avg > .5 & poor_vote > 3, .75,
                                         ifelse(poor_avg < .5 & poor_vote < 2, .25, poor_avg))))) %>%
  mutate(id = as.numeric(id))

submission_Alberto <- select(final_file2, id, country = country.x, poor = new_prob) %>%
  mutate(id = as.numeric(id))
#download.file(paste0("https://s3.amazonaws.com/drivendata/data/50/public/submission_format.csv"), "submission_template.csv")
#submission_template <- read.csv("submission_template.csv", stringsAsFactors = FALSE)
submission_Yukun <- read.csv("povertySubmission_Yukun.csv", stringsAsFactors = FALSE)
submission <- left_join(data.frame(id = submission_Yukun$id),submission_Alberto,by = "id")

write.csv(submission, "poverty_feb27_2018.csv", row.names = FALSE, quote = FALSE)

# all_models_final <- all_models_final %>%
#   merge(select(hhold_test, id, country), by=c("id"), all.x = TRUE) %>%
#   mutate(country.x = as.character(ifelse(is.na(country.y),country.x,as.character(country.y)))) %>%
#   mutate(country = country.x) %>%
#   select(-country.y, country.x)


