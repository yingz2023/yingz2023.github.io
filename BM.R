df <- RHOAI.benchmarking_dat...Sheet1
library(tidyr)
library(dplyr)
library(psych)
library(stringr)

# Pivot to long format
df <- df %>%
  mutate(across(contains("time"), as.numeric))

long_data <- df %>%
  pivot_longer(
    cols = -par,  # Keep 'par' as identifier
    names_to = c("task", ".value"), #splits column names by
    names_sep = "_" #to create two new columns: task and the corresponding metric (e.g., comp, sat).
  ) %>%
  mutate(
    task = str_remove(task, "task"),
    group = 1
  )  # Remove "task" prefix


# View the transformed data
head(long_data)

#data
df <- long_data
#set values of benchmarks

zval <- 1.64 #z value at 90% confidence
#time specifications
df$tspec[df$task==1] <- 24 #time spec for task 1 in mins
#based on derivation from data, 80th percentile from sat above 4 could be used 
quantile(df$time[df$sat >= 4 & df$task==1], probs = seq(0, 1, by= 0.1))

df$tspec[df$task==2] <- 39 #time spec for task 2 in mins
#or based on derivation from data, 80th percentile from sat above 4
quantile(df$time[df$sat >= 4 & df$task==2], probs = seq(0, 1, by= 0.1))

df$tspec[df$task==3] <- 26.4 #time spec for task 3 in mins
#or based on derivation from data, 80th percentile from sat above 4
quantile(df$time[df$sat >= 4 & df$task==3], probs = seq(0, 1, by= 0.1), na.rm = TRUE)

#begin sum

#create satisfaction df -----
df %>% 
  group_by(task) %>% #group analyses broadly by product/version group and then by each task
  summarise(mean=mean(sat),sd=sd(sat),n=n()) %>% #get means, std deviation, and total observations
  mutate(se=(sd / sqrt(n))) %>% #std error
  mutate(marg=se*zval) %>% #margin of error based on zval
  mutate(lowerci=mean-marg) %>% #lower ci
  mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>% #keep lower ci above 0
  mutate(upperci=mean+marg) %>% #upper ci
  mutate(upperci = ifelse(upperci >= 5, 5, upperci)) %>% #keep upper ci below max
  mutate(point_est.z = pnorm((mean - 4)/sd)) %>% #z transform based on sd
  mutate(lowerci.z=pnorm((lowerci-4)/sd)) %>%  #z transform lower ci
  mutate(upperci.z=pnorm((upperci-4)/sd)) %>% #z transform upper ci
  mutate(point_est.nps=(point_est.z - .5) * 200)%>% #nps-ify
  mutate(lowerci.nps=(lowerci.z- .5 )* 200)%>% #nps-ify
  mutate(upperci.nps=(upperci.z- .5 )* 200)%>% #nps-ify
  mutate(Measure="Satisfaction") %>% #name measure as var
  mutate(spec=4) %>% #define spec var for raw plots
  rename(point.est=mean) -> df_sat

#creat time df----
df %>%
  filter(comp==1) %>% #only completed tasks
  group_by(task,tspec) %>% #group analyses broadly by product/version group and then by each task, including task time spec
  summarise(mean=geometric.mean(time),sd = sd(time),n=n()) %>% #get mean, sd and n
  mutate(se=(sd / sqrt(n))) %>% #calculate std error
  mutate(marg=se*zval) %>% #calculate margin of error
  mutate(lowerci=mean-marg) %>% #lower ci
  mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>% #keep lower ci above 0
  mutate(upperci=mean+marg) %>% #upper ci
  mutate(point_est.z = 1-pnorm((mean - tspec)/sd)) %>% #reverse proportion of z
  mutate(upperci.z=1-pnorm((lowerci-tspec)/sd)) %>% #upperci comes from lowerci after inversion
  mutate(lowerci.z=1-pnorm((upperci-tspec)/sd)) %>% #opposite as ^
  mutate(point_est.nps=(point_est.z - .5) * 200)%>% #nps-ify
  mutate(lowerci.nps=(lowerci.z- .5 )* 200)%>% #nps-ify
  mutate(upperci.nps=(upperci.z- .5 )* 200)%>%# nps-ify
  rename(point.est=mean,spec=tspec) %>% #rename some variables to fit into bind_rows
  mutate(Measure="Time") -> df_time

#create completion df ----
df %>%
  group_by(task) %>% #group analyses broadly by product/version group and then by each task
  summarise(pass=sum(comp),n=n()) %>% #get n successes and n trials
  mutate(prop = pass / n) %>% #exact proportion from succesess/trials
  #mutate(laplace = (pass + 1) / (n + 2)) %>% #laplace point estimate
  mutate(p_adj = (n * prop + (zval * zval) / 2) / (n + (zval * zval))) %>% #adjust p for wald calculation
  mutate(n_adj = n + (zval * zval)) %>% #adjust n for wald calculation
  mutate(marg =  zval * sqrt(p_adj * (1 - p_adj) / n_adj)) %>% #wald margin value
  mutate(lowerci = p_adj - marg) %>% #lower wald ci
  mutate(lowerci = ifelse(lowerci <= 0, 0, lowerci)) %>% #keep lower ci above 0
  mutate(upperci = p_adj + marg) %>% #upper wald ci
  mutate(upperci = ifelse(upperci >= 1, 1, upperci)) %>% #keep upper ci below 1
  mutate(point_est.z = qnorm(prop) ) %>% #z score transform based on .78 baseline and bernouli variance
  mutate(lowerci.z= qnorm(prop)-qnorm(marg) ) %>% #z score transform for conf intervals
  mutate(upperci.z = qnorm(prop)+qnorm(marg) ) %>% #z score transform for conf intervals
  rename(point.est=prop) %>% 
  mutate(point_est.nps=(point.est - .5) * 200)%>% #nps-ify
  mutate(lowerci.nps=(lowerci- .5 )* 200)%>% #nps-ify
  mutate(upperci.nps=(upperci- .5 )* 200)%>%# nps-ify
  mutate(Measure="Completion") %>% #name measure as var
  mutate(spec=.78 #define spec var for raw plots
  ) -> df_comp

#combining all dfs
bind_rows(df_comp,df_sat,df_time) -> df_summarised #df from row bind

#df_summarised <- df_summarised %>% 
  #mutate(Group=recode_factor(group,"1"="3.5","2"="3.11")) #rename product groups, ignore errors

#df_summarised$Group <- as.factor(df_summarised$Group)

#df_summarised$Group <- factor(df_summarised$Group, levels = c("3.5","3.11"))  #reorder factor groups

#add better labels for task var, mainly used for plotting but function as factor variable
df_summarised$task_named[df_summarised$task==1] <- "Task 1: Notebook"
df_summarised$task_named[df_summarised$task==2] <- "Task 2: Experiment"
df_summarised$task_named[df_summarised$task==3] <- "Task 3: Serving"
df_summarised <- df_summarised %>% mutate(Group = 1)

#Task level SUM score----- 
df_summarised %>% 
  group_by(task) %>% #keep product/versions separate, and only aggregate at task level
  summarise(point_est.z=mean(point_est.z), #average of point estimate std values
            lowerci.z=mean(lowerci.z), #average of lower CI std values
            upperci.z=mean(upperci.z), #average of upper CI std values
            point_est.nps=mean(point_est.nps), #average of pointe estimate NPS-like values
            lowerci.nps=mean(lowerci.nps), #average of lower CI NPS-like values
            upperci.nps=mean(upperci.nps) #average of upper CI NPS-like values
  ) -> df_task

#get better labels for group var
#df_task <- df_task %>%
# mutate(Group=as.factor(recode(group,"1"="3.5","2"="3.11")))
#df_task$Group <- as.factor(df_task$Group)
#df_task$Group <- factor(df_task$Group, levels = c("3.5","3.11"))



#Overall SUM score----

df_summarised %>%
  #group_by(group) %>% #keep products separate
  summarise(point_est.z=mean(point_est.z), #average point estimates
            lowerci.z=mean(lowerci.z), #average lower CI
            upperci.z=mean(upperci.z) #average upper CI
  ) %>%
  data.frame() %>% 
  #group_by(group) %>%
  summarise(mean=mean(point_est.z), 
            sd=sd(point_est.z),
            n=n(),
            lowerci.z=mean(lowerci.z),
            upperci.z=mean(upperci.z)) %>%
  mutate(se=(sd / sqrt(n))) %>%
  mutate(marg=se*zval) %>%
  mutate(lowerci.grand=mean-marg) %>%
  mutate(lowerci.grand = ifelse(lowerci.grand <= 0, 0, lowerci.grand)) %>%
  mutate(upperci.grand=mean+marg) %>%
  mutate(upperci.grand = ifelse(upperci.grand >= 5, 5, upperci.grand)) %>%
  #mutate(group=as.factor(group)) %>%
  rename(point_est.z=mean)%>%
  mutate(point_est.nps=(point_est.z - .5) * 200)%>%
  mutate(lowerci.nps=(lowerci.z- .5 )* 200)%>%
  mutate(upperci.nps=(upperci.z- .5 )* 200) -> df_SUM # Add new column `group` with value 1
  

#plotting ----
#raw measures -----
#completion
df_summarised %>%
  filter(Measure=="Completion") %>% #only completion
  ggplot(aes(x=task_named, y=point.est,fill=Group)) +  #vars to be plotted
  geom_bar(aes(fill=Group),position=position_dodge(), stat="identity") + #make a bar plot with color palette
  geom_abline(intercept=.78,slope=0, color = "gray",linetype = 2, size=2) + #horizontal benchmark line
  coord_cartesian(ylim=c(0,1)) + #limit y axis between 0-1
  geom_errorbar(aes(ymin=lowerci, ymax=upperci),position=position_dodge(.9), stat="identity",color="darkgray",width=.2) +
  scale_y_continuous(labels = scales::percent) +
  labs(x="Task", y="Estimated proportion") +
  ggtitle(label = "Estimated completion rates across all tasks",
          subtitle = "Confidence intervals at 90%, gray line indicates benchmark") +
  ggthemes::theme_tufte(base_family="GillSans") + 
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_blank())

ggsave(
  "plot_comp_raw.png",
  device = "png",
  bg =  "transparent",
  width = 8,
  height = 5)


#satisfaction
df_summarised %>%
  filter(Measure=="Satisfaction") %>%
  ggplot(aes(x=task_named, y=point.est,fill=Group,ymin=lowerci, ymax=upperci)) + 
  geom_bar(aes(fill=Group),position=position_dodge(), stat="identity") +
  geom_abline(intercept=4,slope=0, color = "gray",linetype = 2, size=2)+
  coord_cartesian(ylim=c(1,5)) +
  geom_errorbar(position=position_dodge(.9), stat="identity",color="darkgray",width=.2) +
  labs(x="Task", y="Satisfaction score") +
  ggtitle(label = "Average satisfaction measure scores across all tasks",
          subtitle = "Confidence Intervals at 90%, gray line indicates benchmark") +
  ggthemes::theme_tufte(base_family="GillSans") + 
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_blank()) 

ggsave(
  "plot_sat_raw.png",
  device = "png",
  bg =  "transparent",
  width = 8,
  height = 5)

#time
df_summarised %>%
  filter(Measure=="Time") %>%
  group_by(task) %>%
  ggplot(aes(x=task_named, y=point.est,fill=Group)) + 
  geom_bar(aes(fill=Group),position=position_dodge(), stat="identity") +
  geom_errorbar(aes(ymax=spec,ymin=spec),position=position_dodge(), stat="identity", color = "#3C3C3C",linetype = 2, size=2)+
  #coord_cartesian(ylim=c(0,1)) +
  geom_errorbar(aes(ymin=lowerci, ymax=upperci),position=position_dodge(.9), stat="identity",color="darkgray",width=.2) +
  labs(x="Task", y="Time geometric average in seconds") +
  ggtitle(label = "Raw time averages across all tasks",
          subtitle = "Confidence Intervals at 90%, gray lines indicate benchmark") +
  #scale_y_time(labels=date_labels("%M $S"))+
  facet_grid(.~task, scales="free")+
  ggthemes::theme_tufte(base_family="GillSans") + 
  scale_y_reverse() +
  theme(
    axis.text.x = element_text(size = 15),
    axis.title.x = element_blank(),
    #legend.position="none",
    strip.background = element_blank(), #remove face label area
    strip.text.x = element_blank() #remove facet label text
  ) 

ggsave(
  "plot_time_raw.png",
  device = "png",
  bg =  "transparent",
  width = 8,
  height = 5)



#task plot -----
library(ggplot2)
#all sub scores on nps version
df_summarised %>%
  group_by(task_named) %>%
  mutate(vjust_value=ifelse(point_est.nps<6,-.4,1.4)) %>%
  #mutate(hjust_value=ifelse(Group=="3.5",1.5,-1)) %>%
  ggplot(aes(y=task, x=point_est.nps,fill=Measure)) + 
  geom_bar(aes(fill=Measure),position=position_dodge(width = 0.7), stat="identity", width = 0.6) +
  coord_cartesian(xlim=c(-100,100)) +
  #geom_errorbar(aes(ymin=lowerci.nps, ymax=upperci.nps),position=position_dodge(.9), stat="identity",color="gray",width=.2) +
  #geom_abline(intercept=.5,slope=0, color = "gray",linetype = 2, size=2) +
  geom_text(aes(label = round(point_est.nps,0)), hjust = -0.2, size = 3, position = "identity") +
  # scale_y_continuous(breaks=c(0:3)) +
  labs(x="", y="Task") +
  ggtitle(label = "Standardized Scores for All Tasks",
          subtitle = "Confidence Intervals at 90%") +
  ggthemes::theme_tufte(base_family="GillSans") + 
  theme(
    #axis.text.x = element_text(size = 15),
    #axis.text.y = element_text(size = 15),
    #axis.title.x = element_text(size = 15),
    #axis.title.y = element_text(size = 15),
    #title = element_text(size = 18),
  ) +
  facet_wrap(.~task_named) 

ggsave(
  "plot_all_subtask_nps.png",
  plot = p.all_subtasks_nps,
  device = "png",
  bg =  "transparent",
  width = 10,
  height = 5)


#loop individual task submeasure plots

for(i in unique(df_summarised$task)){
  
  
  #nps
  df_summarised %>%
    filter(task==i) %>%
    mutate(vjust_value=ifelse(point_est.nps<0,-.4,1.4)) %>%
    mutate(hjust_value=ifelse(Group=="3.5",1.5,-.5)) %>%
    ggplot(aes(x=Measure, y=point_est.nps,fill=Group)) + 
    geom_bar(position=position_dodge(), stat="identity") +
    geom_errorbar(aes(ymin=lowerci.nps, ymax=upperci.nps),position=position_dodge(.9), stat="identity",color="gray",width=.2) +
    #geom_abline(intercept=.5,slope=0, color = "gray",linetype = 2, size=2) +
    geom_text(aes(label = round(point_est.nps,0),hjust = hjust_value,vjust=vjust_value,y=0), size = 5, position = "identity") +
    coord_cartesian(ylim=c(-100,100)) +
    labs(x="", y="SUM score") +
    ggtitle(label = paste0("Standardized Subscores for Task ",i),
            subtitle = "Confidence Intervals at 90%") +
    ggthemes::theme_tufte(base_family="GillSans") + 
    theme(
      axis.text.x = element_text(size = 15),
      axis.text.y = element_text(size = 15),
      axis.title.x = element_text(size = 15),
      axis.title.y = element_text(size = 15),
      title = element_text(size = 18),
      legend.position="none") -> p.tasktemp_nps
  
  ggsave(
    paste0("plot_task,",i,"_nps_small.png"),
    plot = p.tasktemp_nps,
    device = "png",
    bg =  "transparent",
    width = 8,
    height = 5)
  
}



#Task level plot

#add better labels for task var
df_task$task_named[df_task$task==1] <- "Task 1: Notebook"
df_task$task_named[df_task$task==2] <- "Task 2: Experiment"
df_task$task_named[df_task$task==3] <- "Task 3: Serving"

df_task %>%
  mutate(vjust_value=ifelse(point_est.nps<6,-.4,1.4)) %>%
  ggplot(aes(y=task_named, x=point_est.nps, fill=task)) + 
  geom_bar(aes(fill=task),position=position_dodge(), size = 0.2, stat="identity") +
  geom_errorbar(aes(xmin=lowerci.nps, xmax=upperci.nps),position=position_dodge(width = 0.5), stat="identity",color="gray",width=.2) +
  geom_text(aes(label = round(point_est.nps,0)), size = 5, position = "identity") +
  coord_cartesian(xlim=c(-100,100)) +
  labs(x="Standardized scores", y="") +
  ggtitle(label = "SUM scores for each measure on all tasks",
          subtitle = "Confidence Intervals at 90%") +
  #scale_y_continuous(breaks=c(-3:3)) +
  #scale_fill_manual(values=colpal)+
  ggthemes::theme_tufte(base_family="GillSans") + 
  theme(
    axis.text.x = element_text(size = 15),
#    axis.title.x = element_blank(), #-> p.all_tasks_nps
    axis.text.y = element_text(size = 15))

ggsave(
  "plot_all_task_nps.png",
  device = "png",
  bg =  "transparent",
  width = 10,
  height = 5)

#nps version final sum
df_SUM %>% 
  ggplot(aes(x=group, y=point_est.nps,fill=Group,ymin=lowerci.nps, ymax=upperci.nps)) + 
  geom_bar(position=position_dodge(), stat="identity") +
  coord_cartesian(ylim=c(-100,100)) +
  geom_errorbar(position=position_dodge(), stat="identity",color="gray",width=.2) +
  #geom_abline(intercept=.5,slope=0, color = "gray",linetype = 2, size=2) +
  geom_text(aes(label = round(point_est.nps,0),hjust=2,vjust=1.5,y=0), size = 10, position = "identity") +
  labs(x="", y="SUM scores") +
  ggtitle(label = "Final Product SUM Score",
          subtitle = "Confidence Intervals at 90%") +
  ggthemes::theme_tufte(base_family="GillSans") + 
  theme(
    axis.text.x = element_blank())
ggsave(
  "plot_final_nps.png",
  device = "png",
  bg =  "transparent",
  width = 7,
  height = 5)



