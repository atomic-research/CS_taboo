library(tidyverse)
library(lme4)

Aleks_mono_n9 <- read.delim("~/Epic study with amazing statistically significant results/Results/Aleks_mono_n9.txt")
Aleks_mono_n9$cword_word_position <- as.character(Aleks_mono_n9$cword_word_position)
#nije radilo jer je bila varijabla faktor, i onda smo morali da prebacimo u char
Aleks_mono_n9$cword_word_position[is.na(Aleks_mono_n9$cword_word_position)] <- '1'
Aleks_mono_n9$cword_word_position <-as.numeric(Aleks_mono_n9$cword_word_position)


Aleks_CST_bil_allLang_n12_GRM <- rbind(Aleks_CST_bil_eng_n12_GRM,Aleks_CST_bil_spcs_n12_GRM)

Aleks_CST_bil_allLang_n12_GRM_Dom <- merge(Aleks_CST_bil_allLang_n12_GRM, Part_overview_CST_n12, by = "RECORDING_SESSION_LABEL")

Aleks_CST_bil_allLang_n12_GRM_Dom_exp <- Aleks_CST_bil_allLang_n12_GRM_Dom %>%
  filter(e_status == "e")

Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc <- Aleks_CST_bil_allLang_n12_GRM_Dom_exp %>%
  mutate(FIRST_FIXATION_DURATION_SC = scale(FIRST_FIXATION_DURATION),
         TOTAL_DURATION_SC = scale(TOTAL_DURATION),
         GAZE_DURATION_SC = scale(GAZE_DURATION),
         dominance_SC = scale(dominance))

model_bil_GD <- lmer(GAZE_DURATION_SC ~ lang * taboo_status * dominance_SC +(1|RECORDING_SESSION_LABEL), Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc)
summary(model_bil_GD)

model_mono_gd <- lmer(FIRST_FIXATION_DURATION ~ taboo_status + (1|RECORDING_SESSION_LABEL), Aleks_mono_n9_GRM)
summary(model_mono_gd)

Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat <- Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat %>%
  mutate(dom_cat=ifelse(dominance<mean(dominance), "Low Sp Dom", "High Sp Dom"))
# in spanish they looked at neutral words more than in English
#in spanish condition, they looked at the neutral words less the more dominant in spanish they were

write.table(Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum, "Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum.txt", sep = "\t")

ggplot(Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat, 
       aes(lang, GAZE_DURATION, fill = taboo_status)) + stat_summary(aes(),fun.y = "mean", geom = "bar", position = "dodge") + facet_wrap(~dom_cat)
#     scale_fill_manual(values=c("#999999", "#E69F00"))

Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum <- Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat %>%
  group_by(lang, taboo_status, dom_cat) %>%
  mutate(MeanTD = mean(TOTAL_DURATION, na.rm = TRUE), MeanGD = mean(GAZE_DURATION, na.rm = TRUE), MeanFFD = mean(FIRST_FIXATION_DURATION, na.rm = TRUE), sdTD = sd(TOTAL_DURATION, na.rm = TRUE), sdGD = sd(GAZE_DURATION, na.rm = TRUE), sdFFD = sd(FIRST_FIXATION_DURATION, na.rm = TRUE))

Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum <- Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum %>%
  mutate(seTD = sdTD/sqrt(2), seGD = sdGD/sqrt(2), seFFD = sdFFD/sqrt(2))

ggplot(Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum, aes(lang, MeanGD, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanGD - seGD)<0, 0, MeanGD - seGD), ymax = MeanGD + seGD), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean Gaze Duration") + facet_wrap(~dom_cat) + scale_fill_manual(values=c("#FFC300", "#FF5733"))

ggplot(Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum, aes(lang, MeanTD, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanTD - seTD)<0, 0, MeanTD - seTD), ymax = MeanTD + seTD), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean Total Duration") + facet_wrap(~dom_cat) + scale_fill_manual(values=c("#33C4FF", "#581845"))

theme_update(text = element_text(size=20))

ggplot(Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum, aes(lang, MeanFFD, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanFFD - seFFD)<0, 0, MeanFFD - seFFD), ymax = MeanFFD + seFFD), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean 1st Fixation Duration") + facet_wrap(~dom_cat) + scale_fill_manual(values=c("#FFC300", "#900C3F"))

Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum <- merge(Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum, Part_overview_CST_n12)

model_bil_GD <- lmer(FIRST_FIXATION_DURATION_SC ~ lang * taboo_status * cs_use_exp +(1|RECORDING_SESSION_LABEL), Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum)
summary(model_bil_GD)


Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum <- Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum %>%
  mutate(cs_use_exp_cat=ifelse(cs_use_exp<mean(cs_use_exp), "Low CS", "High CS"))
# in spanish they looked at neutral words more than in English
#in spanish condition, they looked at the neutral words less the more dominant in spanish they were

write.table(Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum, "Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum.txt", sep = "\t")


Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum_grByCS <- Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum %>%
  group_by(lang, taboo_status, cs_use_exp_cat) %>%
  mutate(MeanTD_cs = mean(TOTAL_DURATION, na.rm = TRUE), MeanGD_cs = mean(GAZE_DURATION, na.rm = TRUE), MeanFFD_cs = mean(FIRST_FIXATION_DURATION, na.rm = TRUE), sdTD_cs = sd(TOTAL_DURATION, na.rm = TRUE), sdGD_cs = sd(GAZE_DURATION, na.rm = TRUE), sdFFD_cs = sd(FIRST_FIXATION_DURATION, na.rm = TRUE))

Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum_grByCS <- Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum_grByCS %>%
  mutate(seTD_cs = sdTD_cs/sqrt(2), seGD_cs = sdGD_cs/sqrt(2), seFFD_cs = sdFFD_cs/sqrt(2))

ggplot(Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum_grByCS, aes(lang, MeanGD_cs, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanGD_cs - seGD_cs)<0, 0, MeanGD_cs - seGD_cs), ymax = MeanGD_cs + seGD_cs), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean Gaze Duration") + facet_wrap(~cs_use_exp_cat) + scale_fill_manual(values=c("#FFC300", "#FF5733"))

ggplot(Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum, aes(lang, MeanTD, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanTD - seTD)<0, 0, MeanTD - seTD), ymax = MeanTD + seTD), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean Total Duration") + facet_wrap(~dom_cat)

ggplot(Aleks_CST_bil_allLang_n12_GRM_Dom_exp_sc_domcat_sum, aes(lang, MeanFFD, fill = taboo_status)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin=ifelse((MeanFFD - seFFD)<0, 0, MeanFFD - seFFD), ymax = MeanFFD + seFFD), width = 0.25, position = position_dodge(0.9)) + xlab("Language") + ylab("Mean Total Duration") + facet_wrap(~dom_cat)
