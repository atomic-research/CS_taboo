library(tidyverse)
library(lme4)


Aleks_CST_bil_allLang_n30_GRM <- rbind(Aleks_CST_bil_eng_n30_GRM,Aleks_CST_bil_spcs_n30_GRM)

Aleks_CST_bil_allLang_n30_GRM_Dom <- merge(Aleks_CST_bil_allLang_n30_GRM, Part_overview_CST_n30, by = "RECORDING_SESSION_LABEL")


#TRIMMING

Aleks_CST_bil_allLang_n30_GRM_Dom_exp <- Aleks_CST_bil_allLang_n30_GRM_Dom %>%
  filter(e_status == "e")

write.table(Aleks_CST_bil_allLang_n30_GRM_Dom_exp, "Aleks_CST_bil_allLang_n30_GRM_Dom_exp.txt", sep = "\t")

#1408 observations
Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim <- Aleks_CST_bil_allLang_n30_GRM_Dom_exp %>%
  filter(TOTAL_DURATION > 80)
#1284

Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd <- Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim %>%
  filter(GAZE_DURATION < 1000 & GAZE_DURATION > 80)
#1157

Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat <- Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd %>%
  mutate(dom_cat=ifelse(dominance<mean(dominance), "Low Sp Dom", "High Sp Dom"))

Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat_sum <- Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat %>%
  group_by(lang, taboo_status, dom_cat) %>%
  mutate(MeanTD = mean(TOTAL_DURATION, na.rm = FALSE), MeanGD = mean(GAZE_DURATION, na.rm = FALSE), MeanFFD = mean(FIRST_FIXATION_DURATION, na.rm = FALSE), sdTD = sd(TOTAL_DURATION, na.rm = FALSE), sdGD = sd(GAZE_DURATION, na.rm = FALSE), sdFFD = sd(FIRST_FIXATION_DURATION, na.rm = FALSE))

Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat_sum <- Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat_sum %>%
  mutate(seTD = sdTD/sqrt(2), seGD = sdGD/sqrt(2), seFFD = sdFFD/sqrt(2))

ggplot(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat_sum, aes(lang, MeanGD, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanGD - seGD)<0, 0, MeanGD - seGD), ymax = MeanGD + seGD), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean Gaze Duration") + facet_wrap(~dom_cat)
#+ scale_fill_manual(values=c("#FFC300", "#FF5733"))

write.csv(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat_sum, "Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat_sum.csv")

ggplot(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat_sum, aes(lang, MeanFFD, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanFFD - seFFD)<0, 0, MeanFFD - seFFD), ymax = MeanFFD + seFFD), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean Gaze Duration") + facet_wrap(~dom_cat)
#+ scale_fill_manual(values=c("#FFC300", "#FF5733"))




#MERGING TRIMMED DATA AND POSTNORMING DATA
Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat_sum_postnorm <- merge(gather_wr_eng_all_cat, Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat_sum, by = "part_item", all.y = TRUE)

write.csv(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat_sum_postnorm, "Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat_sum_postnorm.csv")


model_bil_GD <- lmer(log1p(GAZE_DURATION) ~ lang * taboo_status.y * log1p(dominance) * cword_length *use_rating*exposure_rating +(1 + lang*taboo_status.y*cword_length|RECORDING_SESSION_LABEL.y) + (1|item_num.y), Aleks_CST_bil_allLang_n30_GRM_Dom_exp_trim_gd_dom_cat_sum_postnorm)
summary(model_bil_GD)

# TRANSFORMATIONS


Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc <- Aleks_CST_bil_allLang_n30_GRM_Dom_exp %>%
  mutate(FIRST_FIXATION_DURATION_SC = scale(FIRST_FIXATION_DURATION),
         TOTAL_DURATION_SC = scale(TOTAL_DURATION),
         GAZE_DURATION_SC = scale(GAZE_DURATION),
         dominance_SC = scale(dominance))

model_bil_GD <- lmer(GAZE_DURATION_SC ~ lang * taboo_status * dominance_SC +(1|RECORDING_SESSION_LABEL), Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc)
summary(model_bil_GD)

model_mono_gd <- lmer(FIRST_FIXATION_DURATION ~ taboo_status + (1|RECORDING_SESSION_LABEL), Aleks_mono_n9_GRM)
summary(model_mono_gd)

Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat <- Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc %>%
  mutate(dom_cat=ifelse(dominance<mean(dominance), "Low Sp Dom", "High Sp Dom"))
# in spanish they looked at neutral words more than in English
#in spanish condition, they looked at the neutral words less the more dominant in spanish they were

write.table(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum, "Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum.txt", sep = "\t")

ggplot(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat, 
       aes(lang, GAZE_DURATION, fill = taboo_status)) + stat_summary(aes(),fun.y = "mean", geom = "bar", position = "dodge") + facet_wrap(~dom_cat)
#     scale_fill_manual(values=c("#999999", "#E69F00"))



Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum <- Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat %>%
  group_by(lang, taboo_status, dom_cat) %>%
  mutate(MeanTD = mean(TOTAL_DURATION, na.rm = FALSE), MeanGD = mean(GAZE_DURATION, na.rm = FALSE), MeanFFD = mean(FIRST_FIXATION_DURATION, na.rm = FALSE), sdTD = sd(TOTAL_DURATION, na.rm = FALSE), sdGD = sd(GAZE_DURATION, na.rm = FALSE), sdFFD = sd(FIRST_FIXATION_DURATION, na.rm = FALSE))

Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum <- Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum %>%
  mutate(seTD = sdTD/sqrt(2), seGD = sdGD/sqrt(2), seFFD = sdFFD/sqrt(2))

ggplot(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum, aes(lang, MeanGD, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanGD - seGD)<0, 0, MeanGD - seGD), ymax = MeanGD + seGD), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean Gaze Duration") + facet_wrap(~dom_cat)
#+ scale_fill_manual(values=c("#FFC300", "#FF5733"))

ggplot(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum, aes(lang, MeanTD, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanTD - seTD)<0, 0, MeanTD - seTD), ymax = MeanTD + seTD), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean Total Duration") + facet_wrap(~dom_cat) 
#+ scale_fill_manual(values=c("#33C4FF", "#581845"))

theme_update(text = element_text(size=15))

ggplot(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum, aes(lang, MeanFFD, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanFFD - seFFD)<0, 0, MeanFFD - seFFD), ymax = MeanFFD + seFFD), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean 1st Fixation Duration") + facet_wrap(~dom_cat) #+ scale_fill_manual(values=c("#FFC300", "#900C3F"))


ggplot(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum, aes(lang, GAZE_DURATION, fill = taboo_status)) +
  geom_violin(na.rm = TRUE, alpha=3/4, aes(fill = taboo_status), draw_quantiles = c(0.25, 0.5, 0.75)) +
  facet_wrap(~dom_cat) + stat_summary(fun.y=mean, geom="point", shape=23, size=3) + scale_fill_manual(values=c("#FFC300", "#FF5733"))


ggplot(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum, aes(lang, TOTAL_DURATION, fill = taboo_status)) +
  geom_violin(na.rm = TRUE, alpha=3/4, aes(fill = taboo_status), draw_quantiles = c(0.25, 0.5, 0.75)) +
  facet_wrap(~dom_cat) + stat_summary(fun.y=mean, geom="point", shape=23, size=3) 
#+ scale_fill_manual(values=c("#33C4FF", "#581845"))


Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum <- merge(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum, Part_overview_CST_n30)

model_bil_GD <- lmer(TOTAL_DURATION_SC ~ lang * taboo_status * dominance +(1|RECORDING_SESSION_LABEL), Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum)
summary(model_bil_GD)


Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum <- Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum %>%
  mutate(cs_use_exp_cat=ifelse(cs_use_exp<mean(cs_use_exp), "Low CS", "High CS"))
# in spanish they looked at neutral words more than in English
#in spanish condition, they looked at the neutral words less the more dominant in spanish they were

write.table(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum, "Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum.txt", sep = "\t")


Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum_grByCS <- Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum %>%
  group_by(lang, taboo_status, cs_use_exp_cat) %>%
  mutate(MeanTD_cs = mean(TOTAL_DURATION, na.rm = TRUE), MeanGD_cs = mean(GAZE_DURATION, na.rm = TRUE), MeanFFD_cs = mean(FIRST_FIXATION_DURATION, na.rm = TRUE), sdTD_cs = sd(TOTAL_DURATION, na.rm = TRUE), sdGD_cs = sd(GAZE_DURATION, na.rm = TRUE), sdFFD_cs = sd(FIRST_FIXATION_DURATION, na.rm = TRUE))

Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum_grByCS <- Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum_grByCS %>%
  mutate(seTD_cs = sdTD_cs/sqrt(2), seGD_cs = sdGD_cs/sqrt(2), seFFD_cs = sdFFD_cs/sqrt(2))

ggplot(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum_grByCS, aes(lang, MeanGD_cs, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanGD_cs - seGD_cs)<0, 0, MeanGD_cs - seGD_cs), ymax = MeanGD_cs + seGD_cs), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean Gaze Duration") + facet_wrap(~cs_use_exp_cat) + scale_fill_manual(values=c("#FFC300", "#FF5733"))

ggplot(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum, aes(lang, MeanTD, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanTD - seTD)<0, 0, MeanTD - seTD), ymax = MeanTD + seTD), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean Total Duration") + facet_wrap(~dom_cat)

ggplot(Aleks_CST_bil_allLang_n30_GRM_Dom_exp_sc_domcat_sum, aes(lang, MeanFFD, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanFFD - seFFD)<0, 0, MeanFFD - seFFD), ymax = MeanFFD + seFFD), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean Total Duration") + facet_wrap(~dom_cat)



Aleks_CST_bil_allLang_n30_GRM_Dom <- merge(Aleks_CST_bil_allLang_n30_GRM, Part_overview_CST_n30, by = "RECORDING_SESSION_LABEL")
