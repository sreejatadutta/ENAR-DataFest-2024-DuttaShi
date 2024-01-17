################################################################################
## Tables and graphs for ENAR DataFest
## Sreejata Dutta
## 01/15/2024
################################################################################
## Clear workspace
rm(list=ls())

## Import required library
library(table1)
library(ggplot2)
library(RColorBrewer)
library(gridExtra)
library(ggpubr)

################################################################################
## Table 1
################################################################################
## Import two-step matched file
ps_matched = read.csv("TwoStepMatchedOut.csv")
ps_matched$bmihighlow = as.factor(ifelse(ps_matched$exposed, "high", "low"))
ps_matched$med_class = factor(ps_matched$bp_med_n_class,
                              levels=c("None", "One", "Two", "Three", "Four o"))
ps_matched$edu_levels = factor(ps_matched$edu_factor,
                               levels=c("High school or less", "Some college", 
                                        "College or above"))
#ps_matched_2011 = ps_matched[ps_matched$svy_year=="2011-2012",]
#ps_matched_2017 = ps_matched[ps_matched$svy_year!="2011-2012",]

table1(~demo_age_cat+
         demo_race+
         demo_gender+
         incomeratio+
         edu_levels+
         marital_factor+
         cc_smoke+
         alcohol_factor+
         htn_aware+
         bp_med_use+
         med_class+
         cc_diabetes+
         cc_ckd+
         cc_cvd_any|svy_year*bmihighlow, data = ps_matched)

################################################################################
## Figure 2
################################################################################
## Import overall files:Adjusted
overall_SBP = read.csv("AdjSBP_overall.csv")
overall_SBP$Estimate = round(overall_SBP$Estimate,2)
overall_SBP = overall_SBP[(overall_SBP$exposed!="_")&(overall_SBP$post!="_"),]
overall_DBP = read.csv("AdjDBP_overall.csv")
overall_DBP$Estimate = round(overall_DBP$Estimate,2)
overall_DBP = overall_DBP[(overall_DBP$exposed!="_")&(overall_DBP$post!="_"),]

p1 <- ggplot(data = overall_SBP, 
       aes(x=post, y = Estimate, group=exposed, color=exposed, label=Estimate)) + 
        geom_point(size=2.5) +
        geom_line(lwd=1.2) +
        # geom_text(check_overlap = TRUE, size=3, color="black") +
        geom_label(nudge_y = 0.5, 
                   color="gray40", 
                   label.size = NA, 
                   fill = alpha(c("white"),0.1),
                   size=2.5) +
        theme(axis.title = element_text(colour = "black", face="bold",
                                        size=10),
              axis.text = element_text(color="black"),
              panel.background = element_blank(),
              axis.line = element_line(),
              legend.position="bottom",
              plot.title = element_text(hjust = 0.5)) +
        xlab("Survey period") + 
        ylab("Adjusted systolic BP") +
        scale_x_discrete(labels = c('2011-2012','2017-2020')) +
        scale_color_manual(labels=c('Non-obese', 'Obese'), 
                           values=c("#6BAED6","#08306B")) +
        guides(color=guide_legend(title="")) +
  ylim(132,138) + 
  labs(title="Overall Systolic BP vs obesity")
p1

p2 <- ggplot(data = overall_DBP, 
             aes(x=post, y = Estimate, group=exposed, color=exposed, label=Estimate)) + 
  geom_point(size=2.5) +
  geom_line(lwd=1.2) +
  # geom_text(check_overlap = TRUE, size=3, color="black") +
  geom_label(nudge_y = 0.3,
             # nudge_x = 0.2,
             color="gray40", 
             label.size = NA, 
             fill = alpha(c("white"),0.1),
             size=2.5) +
  theme(axis.title = element_text(colour = "black", face="bold",
                                  size=10),
        axis.text = element_text(color="black"),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  xlab("Survey period") + 
  ylab("Adjusted diastolic BP") +
  scale_x_discrete(labels = c('2011-2012','2017-2020')) +
  scale_color_manual(labels=c('Non-obese', 'Obese'), 
                     values = c("#F16913", "#7F2704")) +
  guides(color=guide_legend(title=""))  +
  ylim(72,82)+ 
  labs(title="Overall Diastolic BP vs obesity")
p2

################################################################################
## Import overall files:Adjusted
################################################################################
bygroup_SBP = read.csv("AdjSBP_bygroup.csv")
bygroup_SBP$Estimate = round(bygroup_SBP$Estimate,2)
SBP_nomed = bygroup_SBP[(bygroup_SBP$bp_med_use=="No") &
                        (bygroup_SBP$exposed!="_")&
                        (bygroup_SBP$post!="_"),]
SBP_med =bygroup_SBP[(bygroup_SBP$bp_med_use=="Yes") &
                      (bygroup_SBP$exposed!="_") &
                      (bygroup_SBP$post!="_"),]

p3 <- ggplot(data = SBP_nomed, 
             aes(x=post, y = Estimate, group=exposed, color=exposed, label=Estimate)) + 
  geom_point(size=2.5) +
  geom_line(lwd=1.2) +
  # geom_text(check_overlap = TRUE, size=3, color="black") +
  geom_label(nudge_y = 0.5, 
             color="gray40", 
             label.size = NA, 
             fill = alpha(c("white"),0.1),
             size=2.5) +
  theme(axis.title = element_text(colour = "black", face="bold",
                                  size=10),
        axis.text = element_text(color="black"),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  xlab("Survey period") + 
  ylab("Adjusted systolic BP") +
  scale_x_discrete(labels = c('2011-2012','2017-2020')) +
  scale_color_manual(labels=c('Non-obese', 'Obese'), 
                     values=c("#6BAED6","#08306B")) +
  guides(color=guide_legend(title="")) +
  ylim(134,138)+ 
  labs(title="No meds: Systolic BP vs obesity")
p3

SBP_med$Estimate1 = formattable::digits(SBP_med$Estimate, digits=2)
p4 <- ggplot(data = SBP_med, 
             aes(x=post, y = Estimate1, group=exposed, color=exposed, label=Estimate1)) + 
  geom_point(size=2.5) +
  geom_line(lwd=1.2) +
  # geom_text(check_overlap = TRUE, size=3, color="black") +
  geom_label(nudge_y = 0.5, 
             color="gray40", 
             label.size = NA, 
             fill = alpha(c("white"),0.1),
             size=2.5) +
  theme(axis.title = element_text(colour = "black", face="bold",
                                  size=10),
        axis.text = element_text(color="black"),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  xlab("Survey period") + 
  ylab("Adjusted systolic BP") +
  scale_x_discrete(labels = c('2011-2012','2017-2020')) +
  scale_color_manual(labels=c('Non-obese', 'Obese'), 
                     values=c("#6BAED6","#08306B")) +
  guides(color=guide_legend(title="")) +
  ylim(134,138)+ 
  labs(title="With meds: Systolic BP vs obesity")
p4

bygroup_DBP = read.csv("AdjDBP_bygroup.csv")
bygroup_DBP$Estimate = round(bygroup_DBP$Estimate,2)
DBP_nomed = bygroup_DBP[(bygroup_DBP$bp_med_use=="No") &
                          (bygroup_DBP$exposed!="_")&
                          (bygroup_DBP$post!="_"),]
DBP_med =bygroup_DBP[(bygroup_DBP$bp_med_use=="Yes") &
                       (bygroup_DBP$exposed!="_") &
                       (bygroup_DBP$post!="_"),]


p5 <- ggplot(data = DBP_nomed, 
             aes(x=post, y = Estimate, 
                 group=exposed, color=exposed, label=Estimate)) + 
  geom_point(size=2.5) +
  geom_line(lwd=1.2) +
  # geom_text(check_overlap = TRUE, size=3, color="black") +
  geom_label(nudge_y = 0.5, 
             color="gray40", 
             label.size = NA, 
             fill = alpha(c("white"),0.1),
             size=2.5) +
  theme(axis.title = element_text(colour = "black", face="bold",
                                  size=10),
        axis.text = element_text(color="black"),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  xlab("Survey period") + 
  ylab("Adjusted diastolic BP") +
  scale_x_discrete(labels = c('2011-2012','2017-2020')) +
  scale_color_manual(labels=c('Non-obese', 'Obese'), 
                     values = c("#F16913", "#7F2704")) +
  guides(color=guide_legend(title="")) +
  ylim(72,82)+
  labs(title="No meds: Diastolic BP vs obesity")
p5

p6 <- ggplot(data = DBP_med, 
             aes(x=post, y = Estimate, group=exposed, color=exposed, label=Estimate)) + 
  geom_point(size=2.5) +
  geom_line(lwd=1.2) +
  # geom_text(check_overlap = TRUE, size=3, color="black") +
  geom_label(nudge_y = 0.5,
             color="gray40", 
             label.size = NA, 
             fill = alpha(c("white"),0.1),
             size=2.5) +
  theme(axis.title = element_text(colour = "black", face="bold",
                                  size=10),
        axis.text = element_text(color="black"),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  xlab("Survey period") + 
  ylab("Adjusted diastolic BP") +
  scale_x_discrete(labels = c('2011-2012','2017-2020')) +
  scale_color_manual(labels=c('Non-obese', 'Obese'), 
                     values = c("#F16913", "#7F2704")) +
  guides(color=guide_legend(title="")) +
  ylim(72,82)+ 
  labs(title="With meds: Diastolic BP vs obesity")
p6

tiff("Figure2_upd.tiff",
     units="in", width=8, height=10, res=600, compression="lzw")
# grid.arrange(p1,p4,p3,
#              p2,p6,p5,
#              nrow=2)
# graphics.off()

ggpubr::ggarrange(p1,p2,
          p4,p6,
          p3,p5,
          nrow=3, ncol=2, labels=c("(a)", "(b)", "(c)", 
                                   "(d)", "(e)", "(f)"))
graphics.off()
################################################################################
## Figure 1
################################################################################
data = data.frame(
  svy_period=rep(c("2011-2012", "2013-2014", "2015-2016","2017-2020"),2),
  Percent = c(46.3,49.2,42.8,42.9,
              43.9,41.7,43.6,45.1),
  group = c(rep("Controlled hypertension",4), rep("Hypertension",4))
)

data$group=factor(data$group, 
                  levels=c("Hypertension", "Controlled hypertension"))

tiff("Figure1.tiff", 
     units="in", width=6, height=5, res=600, compression="lzw")
ggplot(data=data,
       aes(x=svy_period, y = Percent, color=group, 
           group=group, label=Percent))+
  geom_point(size=2) + 
  geom_line(lwd=1.1) +
  geom_label(nudge_y = 0.5,
             # nudge_x = 0.05,
             color="gray40", 
             label.size = NA, 
             fill = alpha(c("white"),0.1),
             size=3) +
  theme(axis.title = element_text(colour = "black", face="bold"),
        axis.text.x = element_text(size=10),
        axis.text = element_text(color="black"),
        panel.background = element_blank(),
        axis.line = element_line(),
        legend.position="bottom",
        plot.title = element_text(hjust = 0.5)) +
  xlab("Survey period") + 
  ylab("Percent") +
  scale_color_brewer(palette = "Dark2") +
  guides(color=guide_legend(title="")) +
  ylim(40,50)+ 
  labs(title="")
graphics.off()
