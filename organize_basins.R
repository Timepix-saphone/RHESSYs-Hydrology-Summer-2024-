# in this script we are exploring model output, specifically seeing if there are differences in runoff amount 

# set working directory and load packages 
library(tidyverse)

# load in model output 
# differences here is that whichever the label patch_fam is, is the 50% subpatch 
paved = readRDS("share_bas_wPavement_50.rds") %>% mutate(patch_fam = "impervious")
grass = readRDS("share_bas_wPavement_50grass.rds") %>% mutate(patch_fam = "grass")
tree = readRDS("share_bas_wPavement_50tree.rds") %>% mutate(patch_fam = "tree")


# combine all three dataframes as one and make the 'sh' column a factor for easier plotting 
df = rbind(paved, tree, grass) %>% 
  mutate(sh = as.factor(sh))
head(df)

# this dataframe contains values for daily water balance, years 2010-2020 
# categories include:
# irr: irrigation scenario - only 1 scen here [no mandate] meaning there is full irrigation during drought 
# sh:sharing coefficient which represents uncertainty in distance between tree roots and grass, and uncertainty in soil parameters 
# patch_fam: represents which subpatch takes up the most space, split is 50/25/25
# run: parameter uncertainty for turfgrass 

# plot average daily streamflow, yd=year day  
df %>% dplyr::filter(year==2017) %>% 
  group_by(patch_fam, sh, yd) %>% 
  summarise(streamflow=mean(streamflow), min=min(streamflow), max=max(streamflow)) %>% 
  ggplot() + geom_line(aes(x=yd, y=streamflow, col=sh)) + facet_grid("patch_fam")

df %>% dplyr::filter(year==2017) %>% 
  group_by(patch_fam, sh, yd) %>% 
  summarise(streamflow=mean(streamflow), min=min(streamflow), max=max(streamflow)) %>% 
  ggplot() + geom_line(aes(x=yd, y=max, col=sh)) + facet_grid("patch_fam")

# plot average daily recharge 
df %>% group_by(patch_fam, sh, yd) %>% 
  summarise(recharge=mean(recharge)) %>% 
  ggplot() + geom_line(aes(x=yd, y=recharge, col=sh)) + facet_grid("patch_fam")

# plot average monthly
df  %>% group_by(run, year, sh, patch_fam, month) %>% 
  summarise(streamflow=sum(streamflow)) %>% 
  group_by(sh, patch_fam, month) %>% 
  summarise(streamflow=mean(streamflow)) %>% 
  ggplot() + geom_line(aes(x=month, y=streamflow, col=sh)) + facet_grid("patch_fam") + ggtitle("avg monthly runoff")

# plot avg. total runoff within a year 
df %>% group_by(patch_fam, sh, year, run) %>% 
  summarise(streamflow=sum(streamflow)) %>% 
  group_by(patch_fam, sh, year) %>% 
  summarise(streamflow=mean(streamflow)) %>% 
  ggplot() + geom_line(aes(x=year, y=streamflow, col=sh)) + facet_grid("patch_fam") + ggtitle("max runoff")

# plot total annual runoff, with sharing coef uncertainty 
df %>% group_by(run, year, sh, patch_fam) %>% 
  summarise(streamflow=sum(streamflow)) %>% 
  ggplot(aes(x=patch_fam, y=streamflow, col=sh)) + geom_boxplot() + coord_flip() +
  ggtitle("total ann runoff")

# plot max ann runoff with sharing coef
df %>% group_by(run, year, sh, patch_fam) %>% 
  summarise(streamflow=max(streamflow)) %>% 
  ggplot(aes(x=sh, y=streamflow, col=patch_fam)) + geom_boxplot() + coord_flip() +
  ggtitle("max ann runoff")

# plot avg annual runoff with sharing coef 
df %>% group_by(run, year, sh, patch_fam) %>% 
  summarise(streamflow=sum(streamflow)) %>% 
  group_by(run, sh, patch_fam) %>% 
  summarise(streamflow=mean(streamflow)) %>% 
  ggplot(aes(x=sh, y=streamflow, col=patch_fam)) + geom_boxplot() + coord_flip() +
  ggtitle("avg ann runoff")

# annual runoff for all time series 
df %>% group_by(run, year, sh, patch_fam) %>% 
  summarise(streamflow=sum(streamflow)) %>% 
  #group_by(run, sh, patch_fam) %>% 
  #summarise(streamflow=mean(streamflow)) %>% 
  ggplot(aes(x=patch_fam, y=streamflow, col=patch_fam)) + geom_boxplot() + coord_flip() +
  ggtitle("avg ann runoff")


# cleaned up plot for presentation - avg annual streamflow across param uncertainty
df %>% group_by(run, year, sh, patch_fam) %>% 
  summarise(streamflow=sum(streamflow)) %>% 
  group_by(run, sh, patch_fam) %>% 
  summarise(streamflow=mean(streamflow)) %>% 
  ggplot(aes(x=reorder(patch_fam, streamflow), y=streamflow, fill=patch_fam)) + geom_boxplot() + 
  #coord_flip() + 
  labs(y="ann. runoff (m)", x="") + 
  theme_classic() +
  theme(legend.position="none", text=element_text(size=18)) +
  scale_fill_manual(values=c("darkolivegreen4","khaki2","azure2"), breaks = c("tree","grass","impervious")) +
  scale_x_discrete(breaks = c("tree","grass","impervious"),
                  labels=c("tree","turfgrass","pavement"))

# total runoff is higher with 50% impervious 
# max runoff is higher with 50% grass




## for later - brought in data, renamed patchID and later saved - not relevant anymore 
imp = read.csv("veg_p_wPavement_50.csv", sep="") %>% 
  mutate(patch_fam = "impervious") %>% 
  mutate(patchID = factor(patchID, levels = c(60853701, 60853702, 60853703),
                          labels = c("P-impervious","P-grass","P-tree")))

grass = read.csv("veg_p_wPavement_50grass.csv", sep="") %>% 
  mutate(patch_fam="grass_50") %>% 
  mutate(patchID = factor(patchID, levels = c(60853701, 60853702, 60853703),
                          labels = c("P-grass","P-tree","P-impervious")))

tree = read.csv("veg_p_wPavement_50tree.csv", sep="") %>% 
  mutate(patch_fam="tree_50/25/25") %>% 
  mutate(patchID = factor(patchID, levels = c(60853701, 60853702, 60853703),
                          labels = c("P-tree","P-grass","P-impervious")))