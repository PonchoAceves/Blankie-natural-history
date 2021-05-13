# Removed pieces

# Figure 1 without N labels
#### Without labels

# Figure without labels not evaluated

Fig_1 <- all %>% 
  select(SurveyName, Prosoma.width) %>% 
  ggplot(aes(SurveyName, Prosoma.width)) +
  geom_point(aes(group = SurveyName), 
             position = position_jitter(width = 0.10), 
             shape = 19, 
             size = 3, 
             alpha = 0.75,
             colour = "#469eb4") +
  stat_summary(fun = "mean", fun.min = "mean", fun.max= "mean", 
               size= 0.7, colour= "#469eb4", alpha = 0.45, 
               geom = "errorbar", width=0.65) + 
  ylab("Prosoma width (mm)") + 
  xlab("") +
  theme_poncho()

Fig_1

# Figure 2 without labels

fig_2 <- ggplot(all2) + 
  geom_point(size = 3, alpha = 0.4,  
             aes(SurveyName, BlankieLength, 
                 shape = fourLplot, 
                 colour = fourLplot, 
                 group = newness), 
             position = position_jitterdodge(
               jitter.width = 0.50, 
               dodge.width = 0.6 
             )) + 
  stat_summary(aes(SurveyName, BlankieLength, group = newness), 
               subset(all2, newness == "old"), 
               fun = "mean", fun.min = "mean", fun.max= "mean", 
               size= 0.7, colour= "#4682b4", alpha = 0.80, 
               geom = "errorbar", width=0.30, 
               position = position_nudge(x = -0.15)) +
  stat_summary(aes(SurveyName, BlankieLength, group = newness), 
               subset(all2, newness == "new"), 
               fun = "mean", fun.min = "mean", fun.max= "mean", 
               size= 0.7, colour= "#b4464b", alpha = 0.80, 
               geom = "errorbar", width=0.30, 
               position = position_nudge(x = 0.15)) +
  
  scale_shape_manual(name="Retreat status", values=shapes_plo1, 
                     labels = c(paste0("Pre-existing", "\n", "Occupied"), 
                                paste0("Pre-existing", "\n", "Vacant"), 
                                paste0("New", "\n", "Occupied"), 
                                paste0("New", "\n", "Vacant"))) + 
  scale_colour_manual(name="Retreat status",
                      values=cols_plo1, 
                      labels = c(paste0("Pre-existing", "\n", "Occupied"), 
                                 paste0("Pre-existing", "\n", "Vacant"), 
                                 paste0("New", "\n", "Occupied"), 
                                 paste0("New", "\n", "Vacant"))) + 
  ylab("Retreat length (mm)") + 
  xlab("Survey time") + 
  theme_poncho() +
  scale_x_discrete(expand = c(0,0.05))

fig_2
# ggplot2::geom_vline(xintercept = 4.5, colour = "grey", lty = 2) +
# ggplot2::geom_vline(xintercept = 6.4, colour = "black", lty = 2)


# Figure 3

ggplot(all4.1, aes(x=SurveyName, y=mean)) +
  geom_point(aes(x=SurveyName, y=mean, 
                 color = "Occupied"), 
             subset(all4.1, occupied == "TRUE"), 
             size = 3, 
             position = position_nudge(x = 0.1)) + 
  guides(color = guide_legend(override.aes = list(size = 4) ) ) +
  # geom_line(aes(x=SurveyName, y=mean, 
  #               color = "Occupied"), 
  #           subset(all4.1, occupied == "TRUE"), 
  #           group = 1, 
  #           size = 1, 
  #           position = position_nudge(x = 0.1)) +
  geom_errorbar(aes(x=SurveyName, y=mean, 
                    color = "Occupied", 
                    ymin=ci.lwr, ymax=ci.upr), 
                subset(all4.1, occupied == "TRUE"), 
                width=0.25, 
                size=1, 
                position = position_nudge(x = 0.1)) + 
  geom_point(aes(x=SurveyName, y=mean, 
                 color = "Vacant"), 
             subset(all4.1, occupied == "FALSE"), 
             size = 3, 
             position = position_nudge(x = -0.1)) +
  
  # geom_line(aes(x=SurveyName, y=mean, 
  #               color = "Vacant"), 
  #           subset(all4.1, occupied == "FALSE"), 
  #           group = 1, 
  #           size = 1, 
  #           position = position_nudge(x = -0.1)) +
  geom_errorbar(aes(x=SurveyName, y=mean, 
                    color = "Vacant", 
                    ymin=ci.lwr, ymax=ci.upr), 
                subset(all4.1, occupied == "FALSE"), 
                width=0.25, 
                size=1, 
                position = position_nudge(x = -0.1)) + 
  ylab("Log (Retreat length mm)") + 
  xlab("Survey time") +
  scale_colour_manual(name=NULL,values=colours) +
  theme_classic() +
  theme(legend.position=c(0.5, 0.95), legend.direction = "horizontal") +
  theme(axis.title.x = element_text(size=18, face="bold", hjust = 0.5), 
        axis.title.y = element_text(size=18, face="bold", hjust = 0.5), 
        axis.text.x = element_text(size = 16), 
        axis.text.y = element_text(size = 16), 
        legend.text = element_text(size=16), 
        legend.title = element_text(size=16))



# Stats from jamovi

library(jmv)
library(jmvcore)


jmv::descriptives(
  formula = Prosoma.width + BlankieLength ~ SurveyName,
  data = all,
  missing = FALSE,
  median = FALSE,
  sd = FALSE,
  min = FALSE,
  max = FALSE,
  se = TRUE)


alljam <- 
  all %>% 
  mutate(fpID = str_split(BlankieID, "-", simplify = T)[ ,2], 
         temporal = paste0("0", surveyId), 
         newness = ifelse(fpID == temporal, "new", "old")) %>% 
  filter(!SurveyName %in% c("2018-07", "2018-10")) %>% 
  mutate(log.BlankieLength = log10(BlankieLength))

```

### Percent occupied

```{r}
alljam %>% 
  group_by(SurveyName, newness) %>% 
  summarise(Percent_Occupied = mean(occupied == "TRUE"), 
            total = n(), .groups = "keep") %>% 
  ggplot(aes(SurveyName, Percent_Occupied, fill = newness)) +
  expand_limits(y = 1)+
  geom_col(position = position_dodge()) +
  scale_y_continuous(labels = scales::percent_format()) +
  scale_fill_manual(name = NULL , values = c("#7aa6ea", "#e7b453")) +
  geom_text(aes(label = total, group = newness), 
            colour = "white",
            vjust = 1.85, 
            position = position_dodge(width = 0.85)) +
  theme_poncho() +
  theme(legend.direction = "horizontal", legend.position = c(0.5, 0.90))