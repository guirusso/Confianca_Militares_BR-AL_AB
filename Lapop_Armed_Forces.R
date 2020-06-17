#### Guilherme Russo
### Analyzing the AmericasBarometer data

# Working directory and packages
# setwd("")
df<-readRDS("LAPOP_BR_06-19.rds") # This is a harmonized file of AB surveys in Brazil

packages<-c("survey", "WriteXLS", "readstata13", "gridExtra", "gganimate", "transformr")
lapply(packages, require, character.only=T)

years<-c(2006, 2008, 2010, 2012, 2014, 2017, 2019)

df$urban<-droplevels(df$urban)

### Preparing the 'independent variables' #####
table(df$female) # female

aggregate(age~year, df, mean)

table(df$educ_cat) # education categories

table(df$occupation, df$year) # Occupation

table(df$region, df$year) # Region (strata)

table(df$marit_status, df$year) # Marital status

#
#
#
#

# Dependent variables #
table(df$trust_armforces)
prop.table(table(df$trust_armforces))

table(df$trust_media)
prop.table(table(df$trust_media))

table(df$trust_armforces)
prop.table(table(df$trust_armforces))

table(df$trust_armforces)
prop.table(table(df$trust_armforces))

design<-svydesign(~cluster, strata=df$region, weights=df$weight, data=df, nest=T)

arm_forces_year<-svyby(~trust_armforces, ~year, design, svymean, na.rm=T)
years<-c(2006, 2008, 2010, 2012, 2014, 2017, 2019)

congress_year<-svyby(~trust_congress, ~year, design, svymean, na.rm=T)
#lines(years, congress_year$trust_congress, type="o", col="blue", lty=2, pch=9)

parties_year<-svyby(~trust_parties, ~year, design, svymean, na.rm=T)
#lines(years, parties_year$trust_parties, type="o", col="red", lty=2, pch=9)

stf_year<-svyby(~trust_stf, ~year, design, svymean, na.rm=T)
stf_year<-stf_year[stf_year$trust_stf>0,]
#lines(stf_year$year, stf_year$trust_stf, type="o", col="green", lty=2, pch=9)

elections_year<-svyby(~trust_elections, ~year, design, svymean, na.rm=T)
#lines(years, elections_year$trust_elections, type="o", col="grey70", lty=2, pch=9)

# ggploting
colnames(arm_forces_year)[2]<-"value"
arm_forces_year$var<-"Forças Armadas"

colnames(parties_year)[2]<-"value"
parties_year$var<-"Partidos"

graph_br<-bind_rows(arm_forces_year, parties_year)

cor_cepesp1= rgb(28, 47, 103, maxColorValue = 255) # From the brandbook, azul escuro
cor_cepesp2= rgb(0, 150, 214, maxColorValue = 255) # azul piscina
cor_cepesp3= rgb(178, 178, 178, maxColorValue = 255)

ggplot(graph_br, aes(x=year, y=value, group=var, 
                     label=paste(round(value, 1)))) +
  geom_line(aes(color=var), linetype="dotted", size=1) + 
  geom_point(aes(color=var), size=3) +
  labs(x="Ano", y = "", title="Confiança n___", subtitle="Em escala de 1 a 7") +
  scale_color_manual(values=c(cor_cepesp1,cor_cepesp3)) +
  scale_x_continuous(breaks=graph_br$year, limits=c(2001, 2019)) +
  theme_classic() + theme(
    plot.title = element_text(hjust = "0.5",face = "bold", size=22), 
    plot.subtitle = element_text(hjust="0.5", size=12),
    legend.position = "center",
    axis.text.x = element_text(color="black"),
    axis.text.y = element_blank(),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  geom_text(check_overlap = TRUE, nudge_y = .3, size=4) + 
  annotate(geom="text", x=2003, y=graph_br$value[graph_br$var=="Forças Armadas"][1], 
           label="as Forças Armadas",
           color=cor_cepesp1, size=6) +
  annotate(geom="text", x=2003, y=graph_br$value[graph_br$var=="Partidos"][1], 
           label="os Partidos Políticos",
           color=cor_cepesp3, size=6) +
  ylim(c(0,7))

#
#
#
# Now moving to the 2004-18 AmericasBarometer datafiles that can be downloaded in LAPOP's website
dat<-read.dta13("/Users/guilhermerusso/Library/Mobile Documents/com~apple~CloudDocs/Data/LAPOP/2018/2004-2018 LAPOP AmericasBarometer Merge (v1.0FREE).dta")
table(dat$b12)

#B12. Até que ponto o(a) sr./sra. tem confiança nas Forças Armadas [o Exército]?
#B13. Até que ponto o(a) sr./sra. tem confiança no Congresso Nacional? B18. Até que ponto o(a) sr./sra. tem confiança na Polícia Militar ?
#B21. Até que ponto o(a) sr./sra. tem confiança nos partidos políticos?
#B21A. Até que ponto o(a) sr./sra. tem confiança no Presidente da República? B31. Até que ponto o(a) sr./sra. tem confiança no Supremo Tribunal Federal?

# These file does not contain information from all countries, so...

# Brazil
table(dat$wave)
dat$wave<-as.character(dat$wave)
dat2<-dat %>% filter(wave=="2018/19") %>% select(pais, upm, estratopri, wt, b12, b13, b21, b21a)
dat2$estratopri<-as.character(dat2$estratopri)
dat2$upm<-as.character(dat2$upm)

# Uruguay
dat_uru<-read.dta13("/Users/guilhermerusso/Library/Mobile Documents/com~apple~CloudDocs/Data/LAPOP/2018/Uruguay LAPOP AmericasBarometer 2019 v1.0_W.dta")
dat2_uru<-dat_uru %>% select(pais, upm, estratopri, wt, b12, b13, b21, b21a)
dat2_uru$estratopri<-as.character(dat2_uru$estratopri)
dat2_uru$upm<-as.character(dat2_uru$upm)

# Ecuador
dat_ecu<-read.dta13("/Users/guilhermerusso/Library/Mobile Documents/com~apple~CloudDocs/Data/LAPOP/2018/Ecuador LAPOP AmericasBarometer 2019 v1.0_W.dta")
dat2_ecu<-dat_ecu %>% select(pais, upm, estratopri, wt, b12, b13, b21, b21a)
dat2_ecu$estratopri<-as.character(dat2_ecu$estratopri)
dat2_ecu$upm<-as.character(dat2_ecu$upm)

# Chile
dat_chi<-read.dta13("/Users/guilhermerusso/Library/Mobile Documents/com~apple~CloudDocs/Data/LAPOP/2018/Chile LAPOP AmericasBarometer 2019 v1.0_W.dta")
dat2_chi<-dat_chi %>% select(pais, upm, estratopri, wt, b12, b13, b21, b21a)
dat2_chi$estratopri<-as.character(dat2_chi$estratopri)
dat2_chi$upm<-as.character(dat2_chi$upm)

# Chile
dat_bra<-read.dta13("/Users/guilhermerusso/Library/Mobile Documents/com~apple~CloudDocs/Data/LAPOP/2018/Brazil LAPOP AmericasBarometer 2019 v1.0_W.dta")
dat2_bra<-dat_bra %>% select(pais, upm, estratopri, wt, b12, b13, b21, b21a)
dat2_bra$estratopri<-as.character(dat2_bra$estratopri)
dat2_bra$upm<-as.character(dat2_bra$upm)

# Bolivia
dat_bol<-read.dta13("/Users/guilhermerusso/Library/Mobile Documents/com~apple~CloudDocs/Data/LAPOP/2018/Bolivia LAPOP AmericasBarometer 2019 v1.0_W.dta")
dat2_bol<-dat_bol %>% select(pais, upm, estratopri, wt, b12, b13, b21, b21a)
dat2_bol$estratopri<-as.character(dat2_bol$estratopri)
dat2_bol$upm<-as.character(dat2_bol$upm)

# Argentina
dat_arg<-read.dta13("/Users/guilhermerusso/Library/Mobile Documents/com~apple~CloudDocs/Data/LAPOP/2018/Argentina LAPOP AmericasBarometer 2019 v1.0_W.dta")
dat2_arg<-dat_arg %>% select(pais, upm, estratopri, wt, b12, b13, b21, b21a)
dat2_arg$estratopri<-as.character(dat2_arg$estratopri)
dat2_arg$upm<-as.character(dat2_arg$upm)

# Combining results per dataset
dat<-bind_rows(dat2, dat2_uru, dat2_ecu, dat2_chi, dat2_bol, dat2_bra, dat2_arg)

# Survey Design
design<-svydesign(ids=~upm, strata=~estratopri, weights=~wt, data=dat, nest=T)
graph_af<-as.data.frame(svyby(~b12, ~pais, design, svymean, na.rm=T))
graph_af$pais<-c("Argentina", "Bolívia", "Brasil", "Chile", "Colômbia", "República Dominicana", 
                 "Equador", "El Salvador", "Guatemala", "Honduras", "Jamaica", "México", 
                 "Nicarágua", "Paraguai", "Perú", "Uruguai")

# Graphs
p1<-ggplot(graph_af, aes(x=reorder(pais, b12), y=b12, fill=b12, label=paste(round(b12,1)))) +
  geom_bar(stat="identity", 
           fill=c(cor_cepesp2, rep(cor_cepesp3, 4), cor_cepesp2, rep(cor_cepesp3, 8), cor_cepesp1, cor_cepesp3)) +
  labs(x="", y="Em escala de 1 a 7") + coord_flip() +
  ggtitle("Confiança nas Forças Armadas") +
  theme_classic() + theme(
    plot.title = element_text(hjust = 1,face = "bold", size=18), 
    plot.subtitle = element_text(hjust="0.5"),
    legend.position = "center",
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="black"),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=18),
    axis.title.x = element_text(size=14),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  geom_text(check_overlap = TRUE, nudge_y = .4) +
  ylim(c(0,6))

graph_pp<-as.data.frame(svyby(~b21, ~pais, design, svymean, na.rm=T))
graph_pp$pais<-c("Argentina", "Bolívia", "Brasil", "Chile", "Colômbia", "República Dominicana", 
                 "Equador", "El Salvador", "Guatemala", "Honduras", "Jamaica", "México", 
                 "Nicarágua", "Paraguai", "Perú", "Uruguai")

p2<-ggplot(graph_pp, aes(x=reorder(pais, b21), y=b21, fill=b21, label=paste(round(b21,1)))) +
  geom_bar(stat="identity", fill=c(cor_cepesp3, cor_cepesp1, rep(cor_cepesp3, 3), cor_cepesp2, rep(cor_cepesp3, 9), cor_cepesp2)) +
  labs(x="", y="Em escala de 1 a 7") + coord_flip() +
  ggtitle("Confiança nos Partidos Políticos") +
  theme_classic() + theme(
    plot.title = element_text(hjust = 1,face = "bold", size=18), 
    plot.subtitle = element_text(hjust="0.5"),
    legend.position = "center",
    axis.text.x = element_blank(),
    axis.text.y = element_text(color="black"),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=18),
    axis.title.x = element_text(size=14),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  geom_text(nudge_y = .4) +
  ylim(c(0,6))

grid.arrange(p1, p2, nrow = 1)

####
# Who supports the military in Brazil?
df$yob<-df$year-df$age
df$yob_sq<-df$yob*df$yob

# Regressions ----
reg1<-lm(trust_armforces~female + yob + yob_sq + educ_cat + region + marit_status + as.factor(year), df)

reg2<-lm((trust_armforces - trust_parties)~female + yob + yob_sq  + educ_cat + region + marit_status + as.factor(year), df)

####################
# Predicted Scores #
####################
table(df$yob)

pred_age<-data.frame(
  expand.grid(seq(1923, 2003, 1))
)
head(pred_age)
colnames(pred_age)<-c("yob")
pred_age$female<-0
pred_age$yob_sq<-pred_age$yob^2
pred_age$educ_cat<-"Secondary complete"
  
pred_age$age<-2019-pred_age$yob
pred_age$age_sq<-pred_age$age^2
cor(pred_age$age_sq, pred_age$yob_sq)

reg1<-lm(trust_armforces~female + yob + yob_sq + educ_cat + region + marit_status + as.factor(year), df)

pred_age$region<-"Sudeste"
pred_age$marit_status<-"Casadx"
pred_age$year<-2019

pred_reg<-as.data.frame(predict.lm(reg1, pred_age, se.fit=T))
pred<-cbind(pred_age, pred_reg)
critval<- 1.96 ## approx 95% CI
pred$lwr <- pred$fit - (critval * pred$se.fit)
pred$upr <- pred$fit + (critval * pred$se.fit)

graph1<-data.frame(age=rep(pred$age, 3), value=c(pred$fit, pred$lwr, pred$upr), 
                   var=c(rep("fit", nrow(pred)), rep("lwr", nrow(pred)), rep("upr", nrow(pred))))

graph1<-ggplot(graph1, aes(age, value, group=var)) + 
  geom_line(aes(linetype=var, size=var)) +
  labs(x="Idade", y = "", title="Confiança nas Forças Armadas", subtitle="Valores preditos em escala de 1 a 7") +
  scale_x_continuous(breaks=seq(20, 90, 10), limits=c(16, 96)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = "0.5",face = "bold", size=18), 
    plot.subtitle = element_text(hjust="0.5", size=12),
    legend.position = "center",
    axis.text.x = element_text(color="black"),
    axis.text.y = element_text(color="black"),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  scale_linetype_manual(values=c("solid", "dotted", "dotted"))+
  scale_size_manual(values=c(2, .5, .5)) +
  ylim(c(4.5, 6)) +
  transition_reveal(along = age)
  
# 
reg2<-lm((trust_armforces - trust_parties)~female + yob + yob_sq  + educ_cat + region + marit_status + as.factor(year), df)

pred_reg<-as.data.frame(predict.lm(reg2, pred_age, se.fit=T))
pred<-cbind(pred_age, pred_reg)
critval<- 1.96 ## approx 95% CI
pred$lwr <- pred$fit - (critval * pred$se.fit)
pred$upr <- pred$fit + (critval * pred$se.fit)

graph2<-data.frame(age=rep(pred$age, 3), value=c(pred$fit, pred$lwr, pred$upr), 
                   var=c(rep("fit", nrow(pred)), rep("lwr", nrow(pred)), rep("upr", nrow(pred))))

ggplot(graph2, aes(age, value, group=var)) + 
  geom_line(aes(linetype=var, size=var)) +
  labs(x="Idade", y = "", title="Diferença de Confiança nas Forças Armadas e Partidos Políticos", 
       subtitle="Valores preditos em escala de 1 a 7") +
  scale_x_continuous(breaks=seq(20, 90, 10), limits=c(16, 96)) +
  theme_classic() +
  theme(
    plot.title = element_text(hjust = "0.5",face = "bold", size=18), 
    plot.subtitle = element_text(hjust="0.5", size=12),
    legend.position = "center",
    axis.text.x = element_text(color="black"),
    axis.text.y = element_text(color="black"),
    #    axis.title.y = element_text(size=12, face = "bold"),
    text=element_text(size=18),
    axis.title.x = element_text(size=18),
    axis.line = element_blank(),
    axis.ticks = element_blank()) +
  scale_linetype_manual(values=c("solid", "dotted", "dotted"))+
  scale_size_manual(values=c(2, .5, .5)) +
  ylim(c(2, 3.5)) +
  transition_reveal(along = age)