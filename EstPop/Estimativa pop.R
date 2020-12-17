require(sidrar)
require(ggplot2)
require(tidyverse)
require(lubridate)
require(extrafont)
require(stats)
options(scipen = 9999)
options(OutDec= ",")

####Dados Sobre Estimativa Populacional####
#Retirados da tabela 6579 do SIDRA
#estbr  <- get_sidra(api = "/t/6579/n1/all/v/all/p/all")
#estuf  <- get_sidra(api = "/t/6579/n3/all/v/all/p/all") 
#estslz <- get_sidra(api = "/t/6579/n6/2111300/v/all/p/all")
#estmun <- read.csv2("tabela6579.csv")
#colnames(estmun) <- t
#t<- c("Município", '2001', '2002', '2003','2004',
#      '2005','2006','2008','2009','2011','2012','2013','2014','2015','2016',
#      '2017','2018','2019')

####Gráfico 1 - Estimativa Populacional das UFs####
estuf  <- get_sidra(api = "/t/6579/n3/all/v/all/p/all")
estuf <- estuf[,c(2, 6, 9)]
estuf <- estuf %>% filter(Ano == "2019")

estuf %>% 
  mutate(pop = round(Valor/1000000, 1)) %>%
  mutate(hilite = ifelse(estuf$`Unidade da Federação` == "Maranhão", 1, 0)) %>%
  ggplot(aes(x = reorder(`Unidade da Federação`,pop), y = pop, fill = hilite)) + 
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "#4F33FF", high = "#B29B00") +
  labs(x = "Unidades da Federação", 
       y = NULL) + 
  geom_text(aes(label = pop), size=2.5, family = "Arial", 
                  color = "#6f7275", position=position_dodge(width=0.5), 
                  vjust = -0.5, hjust = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "#6f7275"), legend.position = "none", 
        panel.background = element_blank(), axis.ticks = element_line(color = "#6f7275"),
        axis.title.x = element_text(color = "#282929", size = 11, family = "Times New Roman"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, family = "Times New Roman"),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

####Gráfico 2 - 20 cidades mais populações do Brasil em 2019 ####
estmun2019 <- get_sidra(api = "/t/6579/n6/all/v/all/p/last%201")
pop20 <- head(estmun2019[order(-estmun2019$Valor),],20)
pop20 %>%
  mutate(hilite = ifelse(pop20$`Município (Código)` == "2111300", 1, 0)) %>%
  mutate(pop = round(Valor/1000000,2)) %>%
ggplot(aes(x = reorder(`Município`,-Valor), y = pop, fill = hilite)) + 
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "#4F33FF", high = "#B29B00") +
  labs(x = "Municípios", 
       y = NULL) + 
  geom_text(aes(label = pop), size=4, family = "Arial", 
            color = "#6f7275", position=position_dodge(width=0.5), 
            vjust = -0.5, hjust = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "#6f7275"), legend.position = "none", 
        panel.background = element_blank(), axis.ticks = element_line(color = "#6f7275"),
        axis.title.x = element_text(color = "#282929", size = 11, family = "Times New Roman"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, family = "Times New Roman", size =11),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())
sum(pop20$Valor)
####Gráfico 3 - População das capitais####
estcap <- get_sidra(api = "/t/6579/n6/1100205,1200401,1302603,1400100,1501402,1600303,2111300,2211001,2304400,2408102,2507507,2611606,2704302,2800308,2927408,3106200,3205309,3304557,3550308,4106902,4205407,4314902,5103403,5002704,5208707,5300108,1721000/v/all/p/last%201")
estcap %>%
  mutate(hilite = ifelse(estcap$`Município (Código)` == "2111300", 1, 0)) %>%
  mutate(pop = round(Valor/1000000,1)) %>%
  ggplot(aes(x = reorder(`Município`,-Valor), y = pop, fill = hilite)) + 
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "#4F33FF", high = "#B29B00") +
  labs(x = "Capitais", 
       y = NULL) + 
  geom_text(aes(label = pop), size=3, family = "Arial", 
            color = "#6f7275", position=position_dodge(width=0.5), 
            vjust = -0.5, hjust = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "#6f7275"), legend.position = "none", 
        panel.background = element_blank(), axis.ticks = element_line(color = "#6f7275"),
        axis.title.x = element_text(color = "#282929", size = 11, family = "Times New Roman"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, family = "Times New Roman"),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

####Gráfico 4 - Comportamento da população de São Luís 2010-2019####
#Para 2010, foi utilizado o censo do ano. Para os demais, as estimativas anuais.
estslz <- get_sidra(api = "/t/6579/n6/2111300/v/all/p/last%209")
cens <- get_sidra(api = "/t/631/n6/2111300/v/allxp/p/last%201/c2/0/c183/0/d/v93%200")
cens <- cens[,c(1,2,3,4,5,6,11,12,13)]
estslz <- rbind(estslz,cens)
estslz %>%
  mutate(hilite = ifelse(Ano == "2010", 1, ifelse(Ano == "2019", 2, 0))) %>%
  mutate(pop = round(Valor/1000000,1)) %>%
  ggplot(aes(x = Ano, y = Valor, group = 1)) +
  geom_point(size = 2.5, colour = "#4F33FF") +
  geom_line(size = 1.2, colour = "#4F33FF") +
    labs(x = NULL, 
       y = NULL) +
  geom_label(aes(label=round(Valor,2)), colour="white", label.padding=unit(0.05,"lines"), 
             size=3.5) +
  geom_text(aes(label = Valor), size=3, family = "Arial", 
            color = "black", position=position_dodge(width=0.5), fontface = "bold") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "#6f7275"), legend.position = "none", 
        panel.background = element_blank(), axis.ticks = element_line(color = "#6f7275"),
        axis.title.x = element_text(color = "#282929", size = 11, family = "Times New Roman"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, family = "Times New Roman"),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())


####Gráfico 5 - Variação percentual da população de São Luís 2011-2019####
#Os dados são o mesmo do gráfico 4.
estslz <- get_sidra(api = "/t/6579/n6/2111300/v/all/p/last%209")
cens <- get_sidra(api = "/t/631/n6/2111300/v/allxp/p/last%201/c2/0/c183/0/d/v93%200")
cens <- cens[,c(1,2,3,4,5,6,11,12,13)]
estslz <- rbind(estslz,cens)
estslz <- estslz %>%
  mutate(hilite = ifelse(Ano == "2010", 1, ifelse(Ano == "2019", 2, 0))) %>%
  mutate(pop = round(Valor/1000000,1))
estslz <- estslz[order(estslz$Ano),]
estslz$VarPercentual <- round(((estslz$Valor/lag(estslz$Valor)-1) *100),2)

ggplot(data = subset(estslz, !is.na(VarPercentual)),aes(x = Ano, y = VarPercentual, group = 1)) +
    geom_line(size = 1.2, colour = "#4F33FF") +
  labs(x = "Ano", 
       y = "Variação Percentual (%)") +
  geom_label(aes(label=round(VarPercentual,2)), colour="white", label.padding=unit(0.001,"lines"), 
             size=3) +
  geom_text(aes(label = VarPercentual), size=3, family = "Arial", 
            color = "black", position=position_dodge(width=0.5), fontface = "bold") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "#6f7275"), legend.position = "none", 
        panel.background = element_blank(), axis.ticks = element_line(color = "#6f7275"),
        axis.title.x = element_text(color = "#282929", size = 11, family = "Times New Roman"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, family = "Times New Roman"),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

####Gráfico 6 - Taxa Geométrica de Crescimento populacional em São Luís 2011-2019####
#Metodologia disponível em http://www.ripsa.org.br/fichasIDB/pdf/ficha_A.3.pdf
estslz$TxGeom <- round(sqrt((estslz$Valor/lag(estslz$Valor)-1))*100, 2)

ggplot(data = subset(estslz, !is.na(TxGeom)),aes(x = Ano, y = TxGeom, group = 1)) +
  geom_line(size = 1.2, colour = "#4F33FF") +
  labs(x = NULL, 
       y = NULL) +
  geom_label(aes(label=round(TxGeom,2)), colour="white", label.padding=unit(0.001,"lines"), 
             size=3) +
  geom_text(aes(label = TxGeom), size=3, family = "Arial", 
            color = "black", position=position_dodge(width=0.5), fontface = "bold") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "#6f7275"), legend.position = "none", 
        panel.background = element_blank(), axis.ticks = element_line(color = "#6f7275"),
        axis.title.x = element_text(color = "#282929", size = 11, family = "Times New Roman"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, family = "Times New Roman"),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())


####Gráfico 7 - Taxa Geométrica de Crescimento das capitais####
cap<- c("1100205", "1200401", "1302603", "1400100", "1501402", "1600303",
        "2111300", "2211001", "2304400", "2408102", "2507507", "2611606",
        "2704302", "2800308", "2927408", "3106200", "3205309", "3304557",
        "3550308", "4106902", "4205407", "4314902", "5103403", "5002704",
        "5208707", "5300108", "1721000")

geomcap <- read.csv2("txgeommun.csv")
geomcap <- geomcap[,c(4:6)]
colnames(geomcap) <- c("Código", "UF", "Taxa")
geomcap <- geomcap %>% filter(Código %in% estcap)
geomcap$hilite <- ifelse(geomcap$`Código` == "2111300", 1, 0)
geomcap$Taxa <- gsub("%", "", geomcap$Taxa)
geomcap$Taxa <- gsub(",", ".", geomcap$Taxa)
geomcap$Taxa <- as.numeric(as.character(geomcap$Taxa))

ggplot(geomcap, aes(x = reorder(`UF`,-Taxa), y = Taxa, fill = hilite)) + 
  geom_bar(stat = "identity") + 
  scale_fill_gradient(low = "#4F33FF", high = "#B29B00") +
  labs(x = "Municípios", 
       y = NULL) + 
  geom_text(aes(label = Taxa), size=4, family = "Arial", 
            color = "#6f7275", position=position_dodge(width=0.5), 
            vjust = -0.5, hjust = 0.5) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line = element_line(colour = "#6f7275"), legend.position = "none", 
        panel.background = element_blank(), axis.ticks = element_line(color = "#6f7275"),
        axis.title.x = element_text(color = "#282929", size = 11, family = "Times New Roman"),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.3, family = "Times New Roman", size =11),
        axis.text.y = element_blank(), axis.ticks.y = element_blank())

####Gráfico 8 - Taxa Geométrica de crescimento das UFs#####
sta <- read_state(code_state="all", year=2018)
TxGeom <- read.csv2("txgeommun.csv")
TxGeom$Taxa <- gsub("%", "", TxGeom$Taxa)
TxGeom$Taxa <- gsub(",", ".", TxGeom$Taxa)
TxGeom$Taxa <- as.numeric(as.character(TxGeom$Taxa))
TxGeom <- TxGeom[,c(1,4,7)]
colnames(TxGeom) <- c("UF", "Município", "Taxa")
TxGeom <- TxGeom %>%
  group_by(UF) %>%
  summarise(Estados = round(mean(Taxa),2))
colnames(TxGeom) <- c("abbrev_state", "Taxa")
sta <- left_join(sta, TxGeom)
tarara <- head(sta,1)
sta$int <- cut(sta$Taxa, breaks = c(-100, 0, 0.5, 1, 1.5, 2, 100))

no_axis <- theme(axis.title=element_blank(),
                 axis.text=element_blank(),
                 axis.ticks=element_blank(), 
                 panel.grid.major = element_blank(),
                 panel.grid.minor = element_blank(),
                 legend.title = element_blank(),
                 legend.text = element_text(family = "Times New Roman", size = 11 ),
                 legend.position = "bottom", 
                 legend.background = element_blank())
ggplot() + 
  geom_sf(data=sta, aes(fill=int), color= "black", size = .05) + 
  labs(subtitle=NULL, size=8) + 
  scale_fill_manual(labels = c("Menos que 0%", 
                               "0% a menos que 0,5%", 
                               "0,5 a menos que 1%", 
                               "1% a menos que 1,5%", 
                               "1,5% a menos que 2%", 
                               "2% e superiores"),values = c("#f0f7da", 
                                                             "#c9df8a", 
                                                             "#77ab59", 
                                                             "#36802d", 
                                                             "#234d20", 
                                                             "#7D5C65")) + 
  theme_minimal() +
  no_axis


####Misc#####
#Todas as cidades com mais de 1 milhão
milhoes <- estmun %>% filter(Valor >= 1000000)
write.csv2(milhoes, "superior a 1m.csv")
#Soma 
estmun <- <- estmun[order(-estmun$Valor),]
halfpop <- round((sum(estmun$Valor)*0.50),0)
ind <- length(which(cumsum(estmun$Valor) <= halfpop))
tra <- cumsum(estmun$Valor > halfpop)
sum(estmun$Valor[1:ind])
#Cidades MA
estma <- read.csv2("Estimativa Mun Ma.csv")
estma$X2019 <- as.numeric(estma$X2019)
estma <- estma[order(-estma$X2019),]
