#Carregando as bibliotecas
library(readxl)
library(writexl)
library(tidyr)  # manipulação de dados
library(dplyr) # manipulação de banco de dados
library(reshape2) # manipulação de dados
library(multcompView)
library(descr) # estatística descritiva
library(car) #estatisticas (levenetest)
library(ggplot2) # gráficos
library(ggsci) #paleta de cores


#########################################################################

#importando os dados
data <- read_excel("agentes_data.xlsx")
#visualizando o banco de dados
head(data)
summary(data)

#criando uma coluna para identificar se o produto utilizado é purificado, comercial ou controle
data_n <- data %>%
  mutate(purif = case_when(
    endsWith(produto, "P") ~ "purificado",
    endsWith(produto, "C") ~ "comercial",
    endsWith(produto, "M") ~ "nenhum"
  ))
head(data_n)

#criando uma coluna para identificar se qual produto (independente se purificado ou comercial)
data_n <- data_n %>%
  mutate(tipo = case_when(
    startsWith(produto, "agente1") ~ "agente1",
    startsWith(produto, "agente2") ~ "agente2",
    startsWith(produto, "N") ~ "nenhum",
    startsWith(produto, "agente3") ~ "agente3"
  ))
head(data_n)

#convertendo as varíáveis concentração, produto, purif e micro em fatores
data_n$conc <- as.factor(data_n$conc)
data_n$micro <- as.factor(data_n$micro)
data_n$produto <- as.factor(data_n$produto)
data_n$purif <- as.factor(data_n$purif)
data_n$tipo <- as.factor(data_n$tipo)

head(data_n)
summary(data_n)

#Limpando os dados removendo as linhas que não são de interesse e/ou apresentam erros 

data <- data_n[!data_n$micro=="CT",] #removendo os controles sem microorganismo
data <- subset.data.frame(data, is.na(problemas )) #removendo frascos que perderam conteúdo
data_f <- data[data$biomassa>=0,] #removendo valores negativos de biomassa
#data_f <- data_f[!data_f$produto=="NENHUM",] #removendo controles

data_f2 <- data_f[!data_f$conc=="12000",] #removendo conc de 12000
data_f3 <- data_f2[!data_f2$conc=="1200",] #removendo conc de 1200

#Seletor para diferentes conjunto de dados

dt <- data_f # com todas conc
dt <- data_f2 # sem conc de 12000 ppm 
dt <- data_f3 # sem conc de 12000 ppm e 1200 ppm


# selecionando apenas as features relevantes para a análise
df <- data.frame(dt[,c('biomassa','produto', 'purif', 'tipo', 'conc', 'micro')])
summary(df)

#salvando os dados para não perder o tratamento realizado
#write_xlsx(df,"agentes_data_trat.xlsx")

#_______________________ Análise estatística __________________________________________

###Teste de normalidade dos dados
#Histograma para visualizar a distribuição
ggplot(df, aes(x=biomassa)) +
  geom_histogram(aes(y=..density..), color="black", fill="white") +
  geom_density(alpha=.2, fill="#FF6666")

#      teste de shapiro para confirmar
shapiro.test(df$biomassa)


##########################################################################################

#_______________________________________________________________________
# Análise estatística considerando cada fator individualmente
# 1 fator
#   - produto
#   - purif
#   - tipo
#   - conc
#   - micro

#______________________Teste de homogeneidade das variâncias
leveneTest(biomassa ~ produto, df) #p=  (>0.05, são homogêneas)
leveneTest(biomassa ~ purif, df)
leveneTest(biomassa ~ tipo, df)
leveneTest(biomassa ~ conc, df)
leveneTest(biomassa ~ micro, df)


#_________________ cálculo das médias e desvios
fatores <- c('produto', 'purif', 'tipo', 'conc', 'micro')

tab_1 <- list()
for (i in fatores) {
  tab_1[[i]] <- data.frame( df %>% group_by(across(i)) %>%
    summarise(media = mean(biomassa),
              mediana = median(biomassa),
              quant = quantile(biomassa, probs = 0.75),
              desvio = sd(biomassa)) %>%
    arrange(desc(media)))
}
#______________________cálculo ANOVA e test post-hoc de tukey
for (j in 2:6) {
  anova <- aov(biomassa ~ df[[j]], data=df)
  tukey <- TukeyHSD(anova)
  ind <- multcompLetters4(anova, tukey, threshold=0.05)
  cld <- as.data.frame.list(ind$'df[[j]]')
  tab_1[[(j-1)]]$cld <- cld$Letters
  print(tab_1[[(j-1)]])
}
#________________________gráfico de barras
for (k in 1:5) {
  print(
    ggplot(tab_1[[k]], aes_string(x=fatores[k], y="media", fill=fatores[k])) +
    geom_bar(stat = "identity", position = position_dodge(), alpha=0.9) +
    labs(fill=fatores[k]) +
    #facet_wrap(.~conc) +
    geom_errorbar(data = tab_1[[k]], aes(ymin=media-desvio, ymax=media+desvio),
                position = position_dodge(0.9), width = 0.25, colour = "gray35") +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA), legend.position = 'top') +
    scale_fill_npg() +
    labs(x = "Produto", y = "Biomassa (g)") + #colocar títulos eixos
    geom_text(data = tab_1[[k]], aes(label=cld), position = position_dodge(0.90), size = 4,
            vjust=-0.8, hjust=-0.1, colour = "gray25")
  )
}
#_________________________gráfico boxplot
for (k in 1:5) {
  print(
    ggplot(df, aes_string(x=fatores[k], y="biomassa", fill = fatores[k])) +
      geom_boxplot() +
      theme_classic() +
      theme(panel.border = element_rect(fill = NA)) +
      scale_fill_npg() +
      labs(x = "Produto", y = "Biomassa") + #colocar títulos eixos
      ggtitle(paste(title = "Biomassa" )) + theme(plot.title = element_text(size=10)) + #colocar título
      geom_text(data = tab_1[[k]], aes_string(x = fatores[k], y = "quant", label="cld"), position = position_dodge(0.90),
            size = 5, vjust=-0.8, hjust=-0.5, colour = "gray25")
  )
}

#_________________________________________________________________________________
#________________________________________________________________________
# 2 fatores
#   - produto:micro
#   - purif:micro
#   - tipo:micro
#   - conc:micro

#_____________Teste de homogeneidade das variâncias
leveneTest(biomassa ~ produto*micro, df) #p= (>0.05, são homogêneas)
leveneTest(biomassa ~ purif*micro, df)
leveneTest(biomassa ~ tipo*micro, df)
leveneTest(biomassa ~ conc*micro, df)

#_________________cálculo das médias e desvios
fatores <- c('produto', 'purif', 'tipo', 'conc', 'micro')
tab_2 <- list()
for (i in fatores[1:4]) {
  tab_2[[i]] <- df %>% group_by(across(i),micro) %>%
    summarise(media = mean(biomassa),
              mediana = median(biomassa),
              quant = quantile(biomassa, probs = 0.75),
              desvio = sd(biomassa)) %>%
    arrange(desc(media))
}

#_______________________cálculo ANOVA e test post-hoc de tukey
for (j in 2:5) {
  anova <- aov(as.formula(biomassa ~ df[[j]]*df[['micro']]), data=df)
  tukey <- TukeyHSD(anova)
  ind <- multcompLetters4(anova, tukey, threshold=0.05)
  cld <- as.data.frame.list(ind$'df[[j]]:df[["micro"]]')
  tab_2[[(j-1)]]$cld <- cld$Letters
  print(tab_2[[(j-1)]])
}
#________________________gráfico de barras
for (m in 1:4) {
  print(
    ggplot(tab_2[[m]], aes_string(x="micro", y="media", fill=fatores[m])) +
    geom_bar(stat = "identity", position = position_dodge(), alpha=0.9) +
    labs(fill=fatores[m]) +
    #facet_wrap(.~conc) +
    geom_errorbar(data = tab_2[[m]], aes(ymin=media-desvio, ymax=media+desvio),
                position = position_dodge(0.9), width = 0.25, colour = "gray35") +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA), legend.position = 'top') +
    scale_fill_npg() +
    labs(x = "Micro", y = "Biomassa (g)") + #colocar títulos eixos
    geom_text(data = tab_2[[m]], aes(label=cld), position = position_dodge(0.90), size = 4,
            vjust=-0.8, hjust=-0.1, colour = "gray25")
  )
}
#_________________________gráfico boxplot
for (n in 1:4) {
  print(
    ggplot(df, aes_string(x="micro", y="biomassa", fill = fatores[n])) +
      geom_boxplot() +
      theme_classic() +
      theme(panel.border = element_rect(fill = NA)) +
      scale_fill_npg() +
      labs(x = "Micro", y = "Biomassa") + #colocar títulos eixos
      ggtitle(paste(title = "Biomassa" )) + theme(plot.title = element_text(size=10)) + #colocar título
      geom_text(data = tab_2[[n]], aes_string(x = "micro", y = "quant", label="cld"), position = position_dodge(0.90),
            size = 5, vjust=-0.8, hjust=-0.5, colour = "gray25")
  )
}
#_________________________________________________________________________________
#________________________________________________________________________
# 2 fatores
#   - produto:conc
#   - purif:conc
#   - tipo:conc
#   - micro:conc

# #_____________Teste de homogeneidade das variâncias
leveneTest(biomassa ~ produto*conc, df) #p= (>0.05, são homogêneas)
leveneTest(biomassa ~ purif*conc, df)
leveneTest(biomassa ~ tipo*conc, df)
leveneTest(biomassa ~ micro*conc, df)

# #_________________cálculo das médias e desvios
fatores2 <- c('produto', 'purif', 'tipo', 'micro','conc')
tab_22 <- list()
 for (i in fatores2[1:4]) {
   tab_22[[i]] <- df %>% group_by(across(i),conc) %>%
     summarise(media = mean(biomassa),
               mediana = median(biomassa),
               quant = quantile(biomassa, probs = 0.75),
               desvio = sd(biomassa)) %>%
     arrange(desc(media))
 }

 #_______________________cálculo ANOVA e test post-hoc de tukey
 for (j in 2:5) {
   anova <- aov(as.formula(biomassa ~ df[[j]]*df[['conc']]), data=df)
   tukey <- TukeyHSD(anova)
   tukey <- tukey %>% replace(is.na(.), "ALL")
   ind <- multcompLetters4(anova, tukey, threshold=0.100)
   cld <- as.data.frame.list(ind$'df[[j]]:df[["conc"]]')
   tab_22[[(j-1)]]$cld <- cld$Letters
   print(tab_22[[(j-1)]])
 }


# #________________________gráfico de barras
 for (m in 1:4) {
  print(
    ggplot(tab_22[[m]], aes_string(x="conc", y="media", fill=fatores2[m])) +
    geom_bar(stat = "identity", position = position_dodge(), alpha=0.9) +
    labs(fill=fatores2[m]) +
    #facet_wrap(.~conc) +
    geom_errorbar(data = tab_22[[m]], aes(ymin=media-desvio, ymax=media+desvio),
                position = position_dodge(0.9), width = 0.25, colour = "gray35") +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA), legend.position = 'top') +
    scale_fill_npg() +
    labs(x = "Micro", y = "Biomassa (g)") #+ #colocar títulos eixos
    #geom_text(data = tab_22[[m]], aes(label=cld), position = position_dodge(0.90), size = 4,
    #        vjust=-0.8, hjust=-0.1, colour = "gray25")
  )
}
# #_________________________gráfico boxplot
for (n in 1:4) {
  print(
    ggplot(df, aes_string(x="conc", y="biomassa", fill = fatores2[n])) +
      geom_boxplot() +
      theme_classic() +
      theme(panel.border = element_rect(fill = NA)) +
      scale_fill_npg() +
      labs(x = "Micro", y = "Biomassa") + #colocar títulos eixos
      ggtitle(paste(title = "Biomassa" )) + theme(plot.title = element_text(size=10)) #+ #colocar título
      #geom_text(data = tab_22[[n]], aes_string(x = "micro", y = "quant", label="cld"), position = position_dodge(0.90),
       #     size = 5, vjust=-0.8, hjust=-0.5, colour = "gray25")
  )
}
#_________________________________________________________________________________
#________________________________________________________________________________
#___________________________________________________________________________________
# 3 fatores
#   - produto:micro:conc
#   - purif:micro:conc
#   - tipo:micro:conc

#_____________Teste de homogeneidade das variâncias
leveneTest(biomassa ~ produto*micro*conc, df) #p=0,2991 (>0.05, são homogêneas)
leveneTest(biomassa ~ purif*micro*conc, df)
leveneTest(biomassa ~ tipo*micro*conc, df)

#_________________cálculo das médias e desvios
fatores <- c('produto', 'purif', 'tipo', 'conc', 'micro')
mic <- c('AP', 'PB', 'PC')
con <- c('0', '12', '120', '1200', '12000', '500')
df_c <- list()
for (i in con) {
  df_c[[i]] <- subset.data.frame(df, conc==i)
}

tab_3 <- list()
for (i in fatores[1:3]) {
  tab_3[[i]] <- data.frame(df %>% group_by(across(i),micro,conc) %>%
    summarise(media = mean(biomassa),
              mediana = median(biomassa),
              quant = quantile(biomassa, probs = 0.75),
              desvio = sd(biomassa)) %>%
    arrange(desc(media)))
}

#_______________________cálculo ANOVA e test post-hoc de tukey
for (j in 2:4) {
  anova <- aov(as.formula(biomassa ~ df[[j]]*df[['micro']]*df[['conc']]), data=df)
  tukey <- TukeyHSD(anova)
  tukey[[3]] <- na.omit(tukey[[3]])
  ind <- multcompLetters4(anova, tukey, threshold=0.100)
  cld <- as.data.frame.list(ind$'df[[j]]:df[["micro"]]:df[["conc"]]')
  tab_3[[(j-1)]]$cld <- cld$Letters
  print(tab_3[[(j-1)]])
}

#__________________________gráfico de barras
for (m in 1:3) {
  print(
    ggplot(tab_3[[m]], aes_string(x="conc", y="media", fill=fatores[m])) +
    geom_bar(stat = "identity", position = position_dodge(), alpha=0.9) +
    labs(fill=fatores[m]) +
    facet_wrap(.~micro) +
    geom_errorbar(data = tab_3[[m]], aes(ymin=media-desvio, ymax=media+desvio),
                position = position_dodge(0.9), width = 0.25, colour = "gray35") +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA), legend.position = 'top') +
    scale_fill_npg() +
    labs(x = "Concentracao (ppm)", y = "Biomassa (g)") #+ #colocar títulos eixos
    #geom_text(data = tab_22[[m]], aes(label=cld), position = position_dodge(0.90), size = 4,
    #        vjust=-0.8, hjust=-0.1, colour = "gray25")
  )
  print(
    ggplot(tab_3[[m]], aes_string(x="micro", y="media", fill=fatores[m])) +
    geom_bar(stat = "identity", position = position_dodge(), alpha=0.9) +
    labs(fill=fatores[m]) +
    facet_wrap(.~conc) +
    geom_errorbar(data = tab_3[[m]], aes(ymin=media-desvio, ymax=media+desvio),
                position = position_dodge(0.9), width = 0.25, colour = "gray35") +
    theme_classic() +
    theme(panel.border = element_rect(fill = NA), legend.position = 'top') +
    scale_fill_npg() +
    labs(x = "Micro", y = "Biomassa (g)") #+ #colocar títulos eixos
    #geom_text(data = tab_22[[m]], aes(label=cld), position = position_dodge(0.90), size = 4,
    #        vjust=-0.8, hjust=-0.1, colour = "gray25")
  )
}

# #_________________________gráfico boxplot
for (n in 1:3) {
  print(
    ggplot(df, aes_string(x="conc", y="biomassa", fill = fatores[n])) +
      geom_boxplot() +
      facet_wrap(.~micro) +
      theme_classic() +
      theme(panel.border = element_rect(fill = NA)) +
      scale_fill_npg() +
      labs(x = "Concentracao", y = "Biomassa") + #colocar títulos eixos
      ggtitle(paste(title = "Biomassa" )) + theme(plot.title = element_text(size=10)) #+ #colocar título
      #geom_text(data = tab_22[[n]], aes_string(x = "micro", y = "quant", label="cld"), position = position_dodge(0.90),
       #     size = 5, vjust=-0.8, hjust=-0.5, colour = "gray25")
  )
  print(
    ggplot(df, aes_string(x="micro", y="biomassa", fill = fatores[n])) +
      geom_boxplot() +
      facet_wrap(.~conc) +
      theme_classic() +
      theme(panel.border = element_rect(fill = NA)) +
      scale_fill_npg() +
      labs(x = "Microorganismo", y = "Biomassa") + #colocar títulos eixos
      ggtitle(paste(title = "Biomassa" )) + theme(plot.title = element_text(size=10)) #+ #colocar título
      #geom_text(data = tab_22[[n]], aes_string(x = "micro", y = "quant", label="cld"), position = position_dodge(0.90),
       #     size = 5, vjust=-0.8, hjust=-0.5, colour = "gray25")
  )
}


ggsave("box-plot.png", dpi = 150) #salvando o último plot gerado

