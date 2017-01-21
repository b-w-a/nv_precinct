# bryan wilcox archuleta 
# jan. 10, 2017
# nevada ei and graphs. 

# header

# library 
library(tidyverse)
library(ei)
library(eiCompare)

# data 

data <- read_csv("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/returns.csv")
head(data)

# president -------
# graphing -----

df <- gather(data,candidate, pct_vote ,c(pct_clinton, pct_trump, pct_obama, pct_romney))

drop <- which(df$pct_vote == 1 | df$pct_vote == 0 )
df <- df[-drop,]
drop <- which(df$pct_latino == 1)
df <- df[-drop,]

weighted <- ggplot(df, aes(x=pct_latino, y = pct_vote, color = candidate, weight = votes_2016, size = votes_2016)) + geom_point(alpha = .10) + 
  scale_color_manual(values = c('blue', 'turquoise', 'darkred', 'red'), 
                     breaks = c('pct_clinton', 'pct_obama', 'pct_romney', 'pct_trump'),
                     labels = c('Clinton', 'Obama','Romney', 'Trump'), 
                     name = "Candidate") + 
  stat_smooth(se = F) + 
  theme_bw() + scale_y_continuous(limits=c(0,1), breaks = c(seq(0,1,.1))) + 
  labs(title = "Nevada Presidential Vote: Official Precinct-Level Election Returns", 
       x = "Percent Latino Registered Voter in Precinct", y = "2016 Presidental Vote Share") + 
  guides(size=F)

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/all_candiates.png", weighted, height = 8, width = 8)

# net difference votes 
df <- data %>% dplyr::select(county_prec, obama , romney, clinton ,trump,pct_latino)
df <- df %>% mutate(clinton_margin = clinton - obama, 
                    direction = ifelse(clinton_margin > 0, "Clinton Improves", "Clinton Worsens"))


plot <- ggplot(df, aes(x=pct_latino, y = clinton_margin, color = direction)) + geom_point(alpha = .25) + 
  scale_color_manual(values = c('blue', 'red'), name = "Direction") + 
  theme_bw() + labs(title = "2016 Nevada Latino Vote", 
                    y = "Net Clinton Difference \n (Clinton 16 - Obama 12)", 
                    x = "Percent Latino Registered \n Voters in Precinct") + 
  geom_hline(yintercept = 0, lty =2 ) + 
  scale_y_continuous(limits=c(-250,250))

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/net_diff.png", plot, height = 8, width = 8)



# EI -------
# EI using EI compare 

df <- data %>% dplyr::select(pct_trump, pct_clinton, total_votes = votes_2016, pct_latino) %>% na.omit()


nv_2016 <- df %>%  mutate(pct_other = 1 - (pct_trump + pct_clinton),pct_nonlatino = 1-pct_latino)

head(nv_2016)

cands <- c("pct_clinton", "pct_trump", "pct_other")
groups <- c("~ pct_latino", "~ pct_nonlatino") 
table_names <- c("EI: Pct Latino", "EI: Pct Non Latino")

results_nv_2016 <- ei_est_gen(cands, groups,
                              "total_votes", data = nv_2016, 
                              table_names = table_names)

write_csv(results_nv_2016,"/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/2016_ei_estimates.csv")

# density 
model_clinton <- pct_clinton ~ pct_latino

ei_clinton <- ei(model_clinton, total="total_votes", erho=.5, data=df)

beta_clinton <- eiread(ei_clinton, "betab")
df_beta <- data.frame(beta = beta_clinton)

ei_est <- eiread(ei_clinton, "maggs")[1]

plot <- ggplot(df_beta, aes(x=beta)) + geom_density() +
  geom_vline(xintercept = .60, col = "red", lty = 2) + theme_bw() +
  #geom_vline(xintercept = .81, col = "turquoise") + 
  geom_vline(xintercept = (ei_est-.01), lty = 2) + 
  annotate("text", x = .5, y = 7.5, label = "Exit Poll \n Estimate = .60", size = 3) +
  #annotate("text", x = .75, y = 9.5, label = "Latino\n Decisions\n Estimate=.81", size = 3) +
  annotate("text", x = .75, y = 8.5, label = "EI Estimate\n = .88", size = 3) + 
  labs(x = "Estimated Latino Vote for Clinton", y = "Density", title = "Nevada Presidential Latino Vote") 

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/density_plot.png", plot, height = 8, width = 8)

pvalue_exit <- mean(df_beta$beta < .60, na.rm = T)  

pvalue_ld <- mean(df_beta$beta < .81, na.rm = T)
#mean(df_beta$beta < ei_est, na.rm = T)


# Senate --------
# graphing -----

df <- gather(data,candidate, pct_vote ,c(pct_berkley, pct_heller, pct_cortezmasto, pct_heck))

drop <- which(df$pct_vote == 1 | df$pct_vote == 0 )
df <- df[-drop,]
drop <- which(df$pct_latino == 1)
df <- df[-drop,]

weighted <- ggplot(df, aes(x=pct_latino, y = pct_vote, color = candidate, weight = votes_2016, size = votes_2016)) + geom_point(alpha = .1) + 
  scale_color_manual(values = c('blue', 'turquoise', 'darkred', 'red'), 
                     breaks = c('pct_berkley', 'pct_heller', 'pct_cortezmasto', 'pct_heck'),
                     labels = c('Berkley', 'Heller','Cortez-Masto', 'Heck'), 
                     name = "Candidate") + 
  stat_smooth(se = F) + 
  theme_bw() + scale_y_continuous(limits=c(0,1), breaks = c(seq(0,1,.1))) + 
  labs(title = "Nevada Senate Vote: Official Precinct-Level Election Returns", 
       x = "Percent Latino Registered Voter in Precinct", y = "2016 Senate Vote Share") + 
  guides(size=F)

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/all_candiates_senate.png", weighted, height = 8, width = 8)

# net difference votes 
df <- data %>% dplyr::select(county_prec, berkley , heller, cortezmasto ,heck,pct_latino)
df <- df %>% mutate(cortezmasto_margin = cortezmasto - berkley, 
                    direction = ifelse(cortezmasto_margin > 0, "Cortez-Masto Improves", "Cortez-Masto Worsens"))


plot <- ggplot(df, aes(x=pct_latino, y = cortezmasto_margin, color = direction)) + geom_point(alpha = .25) + 
  scale_color_manual(values = c('blue', 'red'), name = "Direction") + 
  theme_bw() + labs(title = "Nevada Senate Vote: Official Precinct-Level Election Returns", 
                    y = "Net Cortez-Masto Difference (Cortez-Masto 16 - Berkley 12)", 
                    x = "Percent Latino Registered Voters in Precinct") + 
  geom_hline(yintercept = 0, lty =2 ) + 
  scale_y_continuous(limits=c(-250,250))

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/net_diff_senate.png", plot, height = 8, width = 8)

# EI -------
# EI using EI compare 

df <- data %>% dplyr::select(pct_heck, pct_cortezmasto, total_votes = votes_2016, pct_latino) %>% na.omit()


nv_2016_senate <- df %>%  mutate(pct_other = 1 - (pct_heck + pct_cortezmasto),pct_nonlatino = 1-pct_latino)

range(nv_2016_senate$pct_cortezmasto)

# drop <- with(nv_2016_senate, which(pct_heck == 0 | pct_heck == 1 | pct_cortezmasto == 0 | pct_cortezmasto == 1))
# nv_2016_senate <- nv_2016_senate[-drop,]


cands <- c("pct_heck", "pct_cortezmasto", "pct_other")
groups <- c("~ pct_latino", "~ pct_nonlatino") 
table_names <- c("EI: Pct Latino", "EI: Pct Non Latino")

results_nv_2016_senate <- ei_est_gen(cands, groups,
                              "total_votes", data = nv_2016_senate, 
                              table_names = table_names)

write_csv(results_nv_2016_senate,"/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/2016_ei_estimates_senate.csv")

# density 
model_cortezmasto <- pct_cortezmasto ~ pct_latino

ei_cortezmasto <- ei(model_cortezmasto, total="total_votes", erho=.5, data=df)

beta_cortezmasto <- eiread(ei_cortezmasto, "betab")
df_beta <- data.frame(beta = beta_cortezmasto)

ei_est <- eiread(cortezmasto, "maggs")[1]

plot <- ggplot(df_beta, aes(x=beta)) + geom_density() +
  geom_vline(xintercept = .61, col = "red", lty = 2) + theme_bw() +
  #geom_vline(xintercept = .81, col = "turquoise") + 
  geom_vline(xintercept = (ei_est-.01), lty = 2) + 
  annotate("text", x = .48, y = 7.5, label = "Exit Poll \n Estimate = .61", size = 3) +
  #annotate("text", x = .75, y = 9.5, label = "Latino\n Decisions\n Estimate=.81", size = 3) +
  annotate("text", x = .75, y = 8.5, label = "EI Estimate\n = .88", size = 3) + 
  labs(x = "Estimated Latino Vote for Cortez-Masto", y = "Density", title = "Nevada Senate Latino Vote") 

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/density_plot_senate.png", plot, height = 8, width = 8)

pvalue_exit <- mean(df_beta$beta < .61, na.rm = T)  

pvalue_ld <- mean(df_beta$beta < .895, na.rm = T)
#mean(df_beta$beta < ei_est, na.rm = T)

# for nevada indy 

df <- gather(data,candidate, pct_vote ,c(pct_clinton, pct_trump, pct_obama, pct_romney))

drop <- which(df$pct_vote == 1 | df$pct_vote == 0 )
df <- df[-drop,]
drop <- which(df$pct_latino == 1)
df <- df[-drop,]

weighted <- ggplot(df, aes(x=pct_latino, y = pct_vote, color = candidate, weight = votes_2016, size = votes_2016)) + geom_point(alpha = .10) + 
  scale_color_manual(values = c('blue', 'turquoise', 'darkred', 'red'), 
                     breaks = c('pct_clinton', 'pct_obama', 'pct_romney', 'pct_trump'),
                     labels = c('Clinton', 'Obama','Romney', 'Trump'), 
                     name = "Candidate") + 
  stat_smooth(se = F) + 
  theme_bw() + scale_y_continuous(limits=c(0,1), breaks = c(seq(0,1,.1))) + 
  labs(title = "Nevada Presidential Vote: Official Precinct-Level Election Returns", 
       x = "Percent Latino Registered Voter in Precinct", y = "2016 Presidental Vote Share") + 
  guides(size=F)

#ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/all_candiates.png", weighted, height = 8, width = 8)

# net difference votes 
df <- data %>% dplyr::select(county_prec, berkley , heller, cortezmasto ,heck,pct_latino)
df <- df %>% mutate(cortezmasto_margin = cortezmasto - berkley, 
                    direction = ifelse(cortezmasto_margin > 0, "Cortez-Masto Improves", "Cortez-Masto Worsens"))


plot <- ggplot(df, aes(x=pct_latino, y = cortezmasto_margin, color = direction)) + geom_point(alpha = .25) + 
  scale_color_manual(values = c('blue', 'red'), name = "Direction") + 
  theme_bw() + labs(title = "Nevada Senate Vote: Official Precinct-Level Election Returns", 
                    y = "Net Cortez-Masto Difference (Cortez-Masto 16 - Berkley 12)", 
                    x = "Percent Latino Registered Voters in Precinct") + 
  geom_hline(yintercept = 0, lty =2 ) + 
  scale_y_continuous(limits=c(-250,250))

#ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/net_diff_senate.png", plot, height = 8, width = 8)
library(gridExtra)

grob <- arrangeGrob(weighted, plot, nrow = 1)

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/nv_indy.png", grob, height = 8, width = 14)

# precinct wins calcualtions

length(which(df$pct_latino > .50))

length(which(df$pct_latino > .50 & df$cortezmasto_margin > 0))
length(which(df$pct_latino > .50 & df$cortezmasto_margin == 0))
length(which(df$pct_latino > .50 & df$cortezmasto_margin < 0))

# spanish 
df <- gather(data,candidate, pct_vote ,c(pct_clinton, pct_trump, pct_obama, pct_romney))

drop <- which(df$pct_vote == 1 | df$pct_vote == 0 )
df <- df[-drop,]
drop <- which(df$pct_latino == 1)
df <- df[-drop,]

weighted <- ggplot(df, aes(x=pct_latino, y = pct_vote, color = candidate, weight = votes_2016, size = votes_2016)) + geom_point(alpha = .10) + 
  scale_color_manual(values = c('blue', 'turquoise', 'darkred', 'red'), 
                     breaks = c('pct_clinton', 'pct_obama', 'pct_romney', 'pct_trump'),
                     labels = c('Clinton', 'Obama','Romney', 'Trump'), 
                     name = "Candidato") + 
  stat_smooth(se = F) + 
  theme_bw() + scale_y_continuous(limits=c(0,1), breaks = c(seq(0,1,.1))) + 
  labs(title = "Voto Presidencial de Nevada: Resultados Oficiales a Nivel de Distro Electoral", 
       x = "Porcentaje de Votantes Latinos Registrados en Distrito Electoral", y = "Porcentaje del Voto Presidencial") + 
  guides(size=F)

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/all_candiates_spanish.png", weighted, height = 8, width = 10)

# net difference votes 
df <- data %>% dplyr::select(county_prec, berkley , heller, cortezmasto ,heck,pct_latino)
df <- df %>% mutate(cortezmasto_margin = cortezmasto - berkley, 
                    direction = ifelse(cortezmasto_margin > 0, "Cortez-Masto Mejora", "Cortez-Masto Empeora"))


plot <- ggplot(df, aes(x=pct_latino, y = cortezmasto_margin, color = direction)) + geom_point(alpha = .25) + 
  scale_color_manual(values = c('red', 'blue'), name = "Direcci\u{f3}n", breaks = c("Cortez-Masto Mejora", "Cortez-Masto Empeora")) + 
  theme_bw() + labs(title = "Voto del Senado de Nevada: Resultados Oficiales a Nivel de Distro Electoral", 
                    y = "Diferencia neta de Cortez-Masto (Cortez-Masto 16 - Berkely 12)", 
                    x = "Porcentaje de Votantes Latinos Registrados en Distrito Electoral") + 
  geom_hline(yintercept = 0, lty =2 ) + 
  scale_y_continuous(limits=c(-250,250))

ggsave("/Users/bryanwilcox/Dropbox/2016 Voter Turnout/data/nevada/nv_precinct/net_diff_senate_spanish.png", plot, height = 8, width = 10)

El voto presidenciales de Nevada
Resultados oficiales de la elecci\u{f3}n nivel de recinto

Candidate = Candidato

y-axis: por ciento del voto presidencial gan\u{f3}
x-axis: por ciento latino en el recinto de votaci\u{f3}n



El voto del Senado de Nevada
Resultados oficiales de la elecci\u{f3}n nivel de recinto

Dirección 
Improves=aumenta
Worsens=disminuye

y-axis: Diferencia total de votos ganados ‘16 menos ‘12
x-axis: por ciento latino en el recinto de votaci\u{f3}n
