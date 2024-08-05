# ========================================================================== #
# ========================================================================== #

###########################################################################=## 
############################ TRABALHO FINAL ################################## 
#============================ ### SCRIPT ### =============================== #
###########################################################################=## 

# ========================================================================== #
# ========================================================================== #


# ========================================================================== #
##### Infos do aluno #####
# Nome: Samuel Sousa de Azevedo Maia
# Data: Julho de 2024
# Curso:  MQ 2024 - TRI
# ========================================================================== #

# # Limpar envinroment
# rm(list = ls())
# Para liberar memória
# gc()


# ========================================================================== #
# ========================================================================== #
##### 1. PREPARAÇÃO: PACOTES E BASE DE DADOS #####
# ========================================================================== #

## DIRETÓRIO E PACOTES
setwd("/Users/samuelmaia/IRT-2024/project-final/poverty-statistical/POF/Dados_20230713")

## Pacotes
list.of.packages <- c("survey","dplyr", "lavaan", "ggplot2", "tidyr", "psych", "stats", "plotly", "semPlot", "devtools", "factoextra", "car", "caret", "reshape2", "RSQLite", "boot")

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


# Carregar os pacotes necessários
library(survey)
library(lavaan)
library(dplyr)
library(ggplot2)
library(tidyr)
library(psych)
library(stats)
library(plotly)
library(semPlot)
library(devtools)
library(factoextra)
library(car)
library(caret)
library(reshape2)
library(RSQLite)
library(boot)


## BASE DE DADOS (fonte: IBGE, 2023)
POF6 <- readRDS("CONDICOES_VIDA.rds")
DOMICILIO <- readRDS("DOMICILIO.rds")
# ========================================================================== #
# ========================================================================== #


# ========================================================================== #
# ========================================================================== #
##### 2. CARREGAMENTOS DOS DADOS, MERGE E PREPARO DA AMOSTRA #####
# ========================================================================== #
# Fonte: "PROGRAMA PARA GERAÇÃO DAS ESTIMATIVAS DA tabela 2 da publicação" do IBGE (2023).

# Iniciando a base de trabalho
pos_estrato <-
    readxl::read_excel(
        "Pos_estratos_totais.xlsx",
        skip=5
    ) # [1]

POF6_novo <- merge(
    POF6,
    pos_estrato,
    by.x = "COD_UPA" ,
    by.y = "COD_UPA(UF+SEQ+DV)"
) 

# Limpando as variáveis que duplicaram no merge
POF6_novo <- transform(
    POF6_novo,
    ESTRATO_POF.y = NULL,
    ESTRATO_POF = ESTRATO_POF.x,
    ESTRATO_POF.x = NULL,
    uf = NULL
)

# Atualização da base de trabalho com a base por domicílio anterior:
POF6_novo <- merge(
    POF6_novo, 
    DOMICILIO
)


# Estimando o total de unidades de consumo (UCs) por pós-estrato projetada para a data da POF
tot_uc <- aggregate(PESO_FINAL~pos_estrato,
    sum,
    data=POF6_novo)

colnames(tot_uc) <- c("pos_estrato",
    "Freq")

# Especificação que faz o R produzir erros-padrões convervadores no lugar de buggar
options( survey.lonely.psu = "adjust" )

sample.pof <- svydesign(id = ~COD_UPA ,
    strata = ~ESTRATO_POF ,
    data = POF6_novo ,
    weights = ~PESO ,
    nest = TRUE	)

desenho.pof <- postStratify(design = sample.pof ,
    ~ pos_estrato ,
    tot_uc)

rm(sample.pof,
    POF6_novo)

###### Salvemos o desenho.pof ###
saveRDS(
    desenho.pof, 
    file = "desenho.pof.rds"
)
# ========================================================================== #
# ========================================================================== #
##### 3. PREPARAÇÃO DAS VARIÁVEIS DE pof_dataframe #####
# ========================================================================== #
#
##### Foram selecionadas 36 variáveis, convertidas em binárias primeiro a partir da metodologia adotada para Medición mutidimensional de la pobreza segundo o Consejo Nacional de Evaluación de la Política de Desarrollo Social (KJ08, 2023). Após uma exploração inicial dos resultados, outras variáveis foram consideradas.

### Significado das variáveis no arquivo "Dicionario de Privacoes POF 17 18"


# ==================================== #
###### 3.1 INDICADORES DE PRIVAÇÃO #######
# ==================================== #

#####  Conversão de todas as variáveis em binárias (1 para privação e 0 para ausência da privação). 

# ==================== #
###### > 3.1.1 Privação na alimentação (p_alimentacao) ###########
# ==================== #

## ALIMENTAÇÃO SAUDÁVEL - variável mexicana "ia_1ad"
a_ns <- ifelse(desenho.pof$variables$V6110 == 1, 1, 0) # valores: 1 últimos três meses, moradores deste domicílio ficaram sem dinheiro para ter uma alimentação saudável e variada; 2 não

## NÃO FEZ REFEIÇÃO - variável mexicana "ia_2ad"
a_pr <- ifelse(desenho.pof$variables$V6112 == 1, 1, 0) # valores: 1 últimos três meses, algum morador de 18 anos ou mais de idade deixou de fazer alguma refeição porque não havia dinheiro para comprar comida; 2 não; 0 branco NA


## COMEU MENOS - variável mexicana "ia_3ad"
a_cm <- ifelse(desenho.pof$variables$V6113 == 1, 1, 0) # valores: 1 últimos três meses, algum morador de 18 anos ou mais de idade, alguma vez comeu menos do que achou que devia porque não havia dinheiro para comprar comida; 2 não; 0 branco NA


### ALIMENTOS ACABARAM - variável mexicana "ia_4ad"
a_ca <- ifelse(desenho.pof$variables$V6109 == 1, 1, 0) # valores: 1 últimos três meses, os alimentos acabaram antes que os moradores deste domicílio tivessem dinheiro para comprar mais comida; 2 não


## SENTIU FOME - variável mexicana "ia_5ad"
a_fm <- ifelse(desenho.pof$variables$V6114 == 1, 1, 0) # valores: 1 últimos três meses, algum morador de 18 anos ou mais de idade, alguma vez sentiu fome, mas não comeu porque não havia dinheiro para comprar comida; 2 não; 0 branco NA


## DIA SEM COMER - variável mexicana "ia_6ad"# Convertendo em binária
a_sc <- ifelse(desenho.pof$variables$V6115 == 1, 1, 0) # valores: 1 últimos três meses, algum morador de 18 anos ou mais de idade, alguma vez, fez apenas uma refeição ao dia ou ficou um dia inteiro sem comer porque não havia dinheiro para comprar comida; 2 não; 0 branco NA
# ==================== #



# ==================== #
###### > 3.1.2 Privação na moradia (icd) #####
# ==================== #

## TIPO DA MORADIA
m_tp <- ifelse(desenho.pof$variables$V0201 == 3, 1, 0)  # valores: 1 casa 2 apartamento 3 habitação em casa de cômodos, cortiço ou cabeça de porco. Privação para habitação em casa de cômodos, cortiço ou cabeça de porco


## PAREDES - variável mexicana "icv_muros"
m_pr <- ifelse(desenho.pof$variables$V0202 == 3 | desenho.pof$variables$V0202 == 4, 1, 0)  # valores: 1 alvenaria ou taipa com revestimento; 2 alvenaria sem revestimento; 3 taipa sem revestimento; 4 madeira apropriada para construção; 5 aproveitada; 6 outro material.  Privação para taipa sem revestimento e madeira aproveitada)


## TELHADO - variável mexicana "icv_techos"
m_te <- ifelse(desenho.pof$variables$V0203 == 5 | desenho.pof$variables$V0203 == 2, 1, 0) # valores: 1 telha sem laje de concreto; 2 telha com laje de concreto; 3 somente laje de concreto; 4 madeira apropriada para construção; 5 zinco, alumínio ou chapa metálica; outro material. Privação para zinco etc. e telha sem laje de concreto


## PISO - variável mexicana "icv_pisos"
m_ps <- ifelse(desenho.pof$variables$V0204 == 4 | desenho.pof$variables$V0204 == 3, 1, 0) # valores: 1 cerâmica, lajota ou pedra; 2 madeira apropriada para construção; 3 cimento; 4 terra; 5 outro material. Privado para terra e cimento


# POUCO ESPAÇO
m_es <- ifelse(desenho.pof$variables$V61061 == 1, 1, 0) # No seu domicílio há problema de pouco espaço? Valores: 1 sim, 2 não. Privado para sim


## CARÊNCIA NA CONDIÇÃO DA MORADIA
# Uma pessoa mora em um domicílio em carência quando há pelo menos uma das seguintes características: # 1. Pontua no indicador PAREDES (p_moradia_paredes_b); # 2. Pontua no indicador PISOS (p_moradia_pisos_b); # 3. Pontua no indicador TELHADO (p_moradia_telhado_b)
m_ct <- ifelse(m_pr == 1 | m_ps == 1 | m_te == 1, 1, 0)


# CARÁTER DO DOMICÍLIO
d_cd <- ifelse(desenho.pof$variables$V0217 == 4, 1, 0) # Este domicílio é: 1 – Próprio de algum morador – já pago; 2 – Próprio de algum morador – ainda pagando; 3 – Alugado; 4 – Cedido por empregador; 5 – Cedido por familiar; 6 – Cedido de outra forma; 7 – Outra condição. Privado para cedido por empregador


# FALTA DE BANHEIRO
m_bn <- ifelse(desenho.pof$variables$V02111 == 0, 1, 0) # Quantos banheiros (com chuveiro e vaso sanitário) de uso exclusivo dos moradores existem neste domicílio, inclusive os localizados no terreno ou na propriedade? Valores inteiros de 0 a 10. Privado para 0.
# ==================== #



# ==================== #
######## > 3.1.3 Privação nos servicos básicos (p_servicos) #######
# ==================== #

## ACESSO A ÁGUA - variável mexicana "isb_agua"
s_ag <- ifelse(desenho.pof$variables$V0207 == 3 | desenho.pof$variables$V0207 == 6 | desenho.pof$variables$V0207 == 5, 1, 0)  # valores: 1 rede geral de distribuição; 2 poço profundo ou artesiano; 3 poço raso, freático ou cacimba; 4 fonte ou nascente; 5 água da chuva armazenada; 6 outra forma. privado para poso raso, outra forma e água da chuva


## ESCOADOURO - variável mexicana "isb_dren"
s_es <- ifelse(desenho.pof$variables$V0212 == 3 | desenho.pof$variables$V0212 == 4 | desenho.pof$variables$V0212 == 5, 1, 0) # valores: 0 branco NA; 1 rede geral, pluvial ou fossa ligada à rede; 2 fossa não ligada à rede; 3 vala; 4 rio, lado ou mar; 5 outro forma. Privado para vala, rio, lago ou mar e outra forma


## ENERGIA ELÉTRICA - variável mexicana "isb_luz"
s_el <- ifelse(desenho.pof$variables$V61052 == 4, 1, 0)  # valores: como avalia condições de fornecimento de energia elétrica?  1 bom; 2 satisfatório; 3 ruim; 4 não tem. Privado para não tem


## COMBUSTÍVEL - variável mexicana "isb_combus"
s_cb <- ifelse(desenho.pof$variables$V02162 == 1 | desenho.pof$variables$V02164 == 1, 1, 0) # carente se respondeu "sim" a pelo menos uma das duas questões: V02162 "Lenha ou carvão é utilizado neste domicílio na preparação dos alimentos?" ou V02164 "Outro combustível (óleo, querosene, etc.) é utilizado neste domicílio na preparação dos alimentos?


## PAVIMENTAÇÃO
s_pv <- ifelse(desenho.pof$variables$V0220 == 2, 1, 0)  # valores: 1 existe pavimentação na rua onde se localiza este domicílio 2 não existe. Privado para não existe

# DESTINO DO LIXO
s_lx <- ifelse(desenho.pof$variables$V0213 == 3 | desenho.pof$variables$V0213 == 4 | desenho.pof$variables$V0213 == 5, 1, 0) # Qual o (principal) destino dado ao lixo? 1 – Coletado diretamente por serviço de limpeza; 2 – Coletado em caçamba de serviço de limpeza; 3 – Queimado (na propriedade); 4 – Enterrado (na propriedade); 5 – Jogado em terreno baldio ou logradouro; 6 – Outro destino. Privado para queimado (na propriedade), enterrado (na propriedade) e jogado em terreno baldio ou logradouro



## CARÊNCIA NO DOMICÍLIO - variável mexicana "sbv"
# Uma pessoa mora em um domicílio em carência quando há pelo menos uma das seguintes características: # 1. Pontua no indicador ACESSO A ÁGUA (p_servicos_agua_b); # 2. Pontua no indicador ESCOADOURO (p_servicos_escoadouro_b); # 3. Pontua no indicador COMBUSTÍVEL (p_servicos_combus); # 4. Pontua no indicador LIXO
s_ct <- ifelse(s_ag == 1 | s_es == 1 | s_cb == 1 | s_pv == 1 | s_lx == 1, 1, 0) 
#==================== #



# ==================== #
########## > 3.1.4 Privação na qualidade de vida (p_qvida) #######
# ==================== #

## RENDIMENTO LEVA ATÉ O FIM DO MÊS
q_rd <- ifelse(desenho.pof$variables$V6101 == 1, 1, 0)  # Na sua opinião, o rendimento total da sua família permite que você(s) leve(m) a vida até o fim do mês com: 1 – Muita dificuldade; 2 – Dificuldade; 3 – Alguma dificuldade; 4 – Alguma facilidade; 5 – Facilidade; 6 – Muita facilidade. Privado para muita dificuldade


## VESTUÁRIO, padrão de vida em relação a
q_vs <- ifelse(desenho.pof$variables$V61043 == 3, 1, 0) # Como avalia o padrão de vida da sua família em relação a vestuário? 1 – Bom; 2 – Satisfatório; 3 – Ruim. Privado para ruim


## EDUCAÇÃO, padrão de vida em relação a
q_ed <- ifelse(desenho.pof$variables$V61044 == 3, 1, 0) # Como avalia o padrão de vida da sua família em relação a educação? 1 – Bom; 2 – Satisfatório; 3 – Ruim. Privado para ruim


## SAÚDE, padrão de vida em relação a
q_sd <- ifelse(desenho.pof$variables$V61045 == 3, 1, 0) # Como avalia o padrão de vida da sua família em relação a saúde? 1 – Bom; 2 – Satisfatório; 3 – Ruim. Privado para ruim


## LAZER, padrão de vida em relação a
q_lz <- ifelse(desenho.pof$variables$V61046 == 3, 1, 0)  # Como avalia o padrão de vida da sua família em relação a lazer? 1 – Bom; 2 – Satisfatório; 3 – Ruim. Privado para ruim


## ILUMINAÇÃO
lur <- ifelse(desenho.pof$variables$V61053 == 4, 1, 0) # Como avalia as condições de moradia da sua família em relação ao serviço de iluminação de rua? 1 – Bom, 2 – Satisfatório, 3 – Ruim, 4 – Não tem. Privado para não tem


## GOTEIRA
gt <- ifelse(desenho.pof$variables$V61063 == 1, 1, 0) # No seu domicílio há problema de telhado com goteira? 1 – Sim; 2 – Não. Privado para sim


## FUNDAÇÃO
fdu <- ifelse(desenho.pof$variables$V61064 == 1, 1, 0) # No seu domicílio há problema de fundação, paredes ou chão úmidos? 1 – Sim; 2 – Não. Privado para sim


## MADEIRA DETERIORIDADOS
mdt <- ifelse(desenho.pof$variables$V61065 == 1, 1, 0) # No seu domicílio há problema de madeira das janelas, portas ou assoalhos deteriorados? 1 – Sim; 2 – Não. Privado para sim

## PRAGAS
ist <- ifelse(desenho.pof$variables$V61066 == 1, 1, 0) # No seu domicílio há problema de mosquitos ou outros insetos, ratos, etc.? 1 – Sim; 2 – Não. Privado para sim


## AMBIENTAIS
abt <- ifelse(desenho.pof$variables$V61067 == 1, 1, 0) # No seu domicílio há problema de fumaça, mau cheiro, barulho ou outros problemas ambientais causados pelo trânsito ou indústria? 1 – Sim; 2 – Não. Privado para sim


## POLUIÇÃO
pol <- ifelse(desenho.pof$variables$V61068 == 1, 1, 0) # No seu domicílio há problema de estar localizado próximo a rio, baía, lago, açude ou represa poluídos? 1 – Sim; 2 – Não. Privado para sim


## INUNDAÇÃO
inu <- ifelse(desenho.pof$variables$V61069 == 1, 1, 0) # No seu domicílio há problema de estar localizado em área sujeita a inundação? 1 – Sim; 2 – Não. Privado para sim


## DESLIZAMENTO
dzt <- ifelse(desenho.pof$variables$V610610 == 1, 1, 0) # No seu domicílio há problema de estar localizado em encosta ou área sujeita a deslizamento? 1 – Sim; 2 – Não. Privado para sim


## VIOLÊNCIA
vlc <- ifelse(desenho.pof$variables$V610611 == 1, 1, 0) # No seu domicílio há problema de violência ou vandalismo na sua área de residência? 1 – Sim; 2 – Não. Privado para sim
# =============== #


# =============== #
##### INCLUINDO AS NOVAS VARIÁVEIS NA BASE
desenho.pof$variables <- cbind(
    desenho.pof$variables,
    a_ca, a_pr, a_cm, a_ns, a_fm, a_sc, m_tp, m_pr, m_te, m_ps, m_es, d_cd, m_bn, s_ag, s_es, s_el, s_cb, s_pv, s_ct, q_rd, q_vs, q_ed, q_sd, q_lz, lur, gt, fdu, mdt, ist, abt, pol, inu, dzt, vlc, s_lx
)

pof_dataframe <- desenho.pof$variables

### Salvemos o dataframe que inclui as variáveis criadas ###
saveRDS(
    pof_dataframe, 
    file = "pof_dataframe.rds"
    )
# ==================================== #



# ==================================== #
####### 2.4 PREVALÊNCIAS DAS PRIVAÇÕES #######
# ==================================== #

# Selecionar as variáveis de interesse do dataframe
cols <- c(
    "a_ca", "a_pr", "a_cm", "a_ns", "a_fm", "a_sc", "m_tp", "m_pr", "m_te", "m_ps", "m_es", "d_cd", "m_bn", "s_ag", "s_es", "s_el", "s_cb", "s_pv", "s_ct", "q_rd", "q_vs", "q_ed", "q_sd", "q_lz", "lur", "gt", "fdu", "mdt", "ist", "abt", "pol", "inu", "dzt", "vlc", "s_lx"
)

# Cálculo levando em conta as propriedades amostrais com o pacote `survey`
options(scipen=999, survey.lonely.psu="adjust")

des <- svydesign(data=pof_dataframe, id=~1, CLUSTER=~psu, weights=~PESO_FINAL)

propr <- data.frame(svymean(pof_dataframe[, cols],des,na.rm=T))

propr <- round(propr*100,1)

print(propr)


#### Plotando os resultados
# Criar o dataframe com os resultados: prevalência e erro padrão
data <- data.frame(
    Variavel = c("a_ca", "a_pr", "a_cm", "a_ns", "a_fm", "a_sc", "m_tp", "m_pr", "m_te", "m_ps", "m_es", "d_cd", "m_bn", "s_ag", "s_es", "s_el", "s_cb", "s_pv", "s_ct.1", "q_rd", "q_vs", "q_ed", "q_sd", "q_lz", "lur", "gt", "fdu", "mdt", "ist", "abt", "pol", "inu", "dzt", "vlc", "s_lx"),
    Mean = c(47.5, 15.5, 30.6, 73.2, 14.1, 11.0, 0.2, 6.1, 24.4, 25.0, 42.8, 1.4, 3.6, 8.5, 5.8, 0.9, 23.8, 29.4, 42.7, 27.2, 15.8, 18.5, 38.4, 48.2, 10.8, 40.7, 41.7, 34.0, 71.3, 29.7, 18.9, 13.4, 4.3, 44.8, 12.0),
    SE = c(0.5, 0.3, 0.4, 0.4, 0.3, 0.3, 0.0, 0.2, 0.4, 0.4, 0.5, 0.1, 0.1, 0.2, 0.2, 0.1, 0.4, 0.4, 0.5, 0.4, 0.4, 0.4, 0.5, 0.5, 0.2, 0.5, 0.5, 0.4, 0.4, 0.4, 0.4, 0.3, 0.2, 0.5, 0.3)
)

# Reordenar o dataframe de acordo com a média em ordem decrescente
data <- data[order(data$Mean, decreasing = TRUE), ]

# Definir os níveis do fator com base na ordem das médias
data$Variavel <- factor(data$Variavel, levels = data$Variavel)

# Criar o gráfico de barras
ggplot(data, aes(x = Variavel, y = Mean)) +
    geom_bar(stat = "identity", fill = "grey") +
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), width = 0.2) +
    geom_text(aes(label = paste0(Mean, "%")), angle = 90, hjust = -0.2, size = 3) +
    theme_minimal() +
    labs(title = ,
        x = "Tipos de privação",
        y = "Percentuais com erro padrão") +
    theme(
        axis.text.x = element_text(angle = 90, hjust = 1, size = 8),
        axis.text.y = element_text(angle = 90),
        aspect.ratio = 0.6
    ) +
    scale_x_discrete(labels = data$Variavel) +
    ylim(0, max(data$Mean) * 1.32) +
    theme(panel.spacing = unit(1, "lines"))
# ==================================== #
# ========================================================================== #
# ========================================================================== #



# ========================================================================== #
# ========================================================================== #
##### 3. ESTIMAÇÕES COM MODELO BIFATORIAL ####
# ========================================================================== #

# ==================================== #
###### 3.1 ANÁLISE DE COMPONENTENS PRINCIPAIS (PCA) ###########
# ==================================== #

# ==================== #
###### > 3.1.1 Preliminares: testes e preparação dos dados ###########
# ==================== #

### 3.1.1.a) Seleção das variáveis de interesse

### Lembrando que nosso dataframe é este:
print(pof_dataframe)
### que nossas variáveis de interesse são estas:
print(cols)
### e que nosso desennho de pesquisa é este:
print(desenho.pof)


# Lista das variáveis de interesse
vars_interesse <- c(
    "a_ca", "a_pr", "a_cm", "a_ns", "a_fm", "a_sc", "m_tp", "m_pr", "m_te", 
    "m_ps", "m_es", "d_cd", "m_bn", "s_ag", "s_es", "s_el", "s_cb", "s_pv", 
    "s_ct", "q_rd", "q_vs", "q_ed", "q_sd", "q_lz", "lur", "gt", "fdu", "mdt", 
    "ist", "abt", "pol", "inu", "dzt", "vlc", "s_lx", "ESTRATO_POF", 
    "COD_UPA", "PESO", "PESO_FINAL"
)

# Selecionar as variáveis de interesse do dataframe
pof_df_sb <- pof_dataframe[, vars_interesse]

# Remover linhas com NAs
pof_df_sb_clean <- na.omit(pof_df_sb)

# Criar um novo objeto survey.design com o dataframe reduzido e limpo
desenho.pof_subset <- svydesign(
    id = ~COD_UPA,
    strata = ~ESTRATO_POF,
    data = pof_df_sb_clean,
    weights = ~PESO,
    nest = TRUE
)

# Verificar as variáveis no objeto desenho.pof_subset
print(names(desenho.pof_subset$variables))
# ==================== #



# ==================== #
### 3.1.1.b) Testes preliminares (VIF Bartlett,KMO)

#####>>> Teste de multicolinearidade

# Ajustar o modelo de regressão para calcular VIF
modelo_vif <- svyglm(a_ca ~ a_pr + a_cm + a_ns + a_fm + a_sc + m_tp + m_pr + m_te + m_ps + m_es + d_cd + m_bn + s_ag + s_es + s_el + s_cb + s_pv + s_ct + q_rd + q_vs + q_ed + q_sd + q_lz + lur + gt + fdu + mdt + ist + abt + pol + inu + dzt + vlc + s_lx, design = desenho.pof_subset)

# Calcular o VIF
vif_values <- vif(modelo_vif)

print(vif_values)

alias_info <- alias(modelo_vif)
print(alias_info$Complete)

if(any(vif_values >= 5)) {
    print("Alta colinearidade detectada. Sugere-se investigar a multicolinearidade.")
} else if(any(vif_values >= 1)) {
    print("Colinearidade moderada detectada. Pode ser aceitável.")
} else {
    print("Sem colinearidade detectada.")
}
# Resultado
# [1] "Colinearidade moderada detectada. Pode ser aceitável."



#####>>> Teste de Esfericidade de Bartlett
# Selecionar apenas as variáveis de interesse
data_subset <- desenho.pof_subset$variables[, c(
    "a_ca", "a_pr", "a_cm", "a_ns", "a_fm", "a_sc", "m_tp", "m_pr", "m_te", "m_ps", "m_es", "d_cd", "m_bn", "s_ag", "s_es", "s_el", "s_cb", "s_pv", "s_ct", "q_rd", "q_vs", "q_ed", "q_sd", "q_lz", "lur", "gt", "fdu", "mdt", "ist", "abt", "pol", "inu", "dzt", "vlc", "s_lx"
)]

# Calcular a matriz de correlação
cor_matrix <- cor(data_subset, use = "pairwise.complete.obs")

# Teste
bartlett_test <- cortest.bartlett(cor_matrix, n = nrow(data_subset))
print(bartlett_test)

print(bartlett_test)

if(bartlett_test$p.value < 0.05) {
    print("Rejeitamos a hipótese nula. A análise fatorial pode ser apropriada.")
} else {
    print("Não rejeitamos a hipótese nula. As variáveis podem não ser adequadas para análise fatorial.")
}
# Resultado
# [1] "Rejeitamos a hipótese nula. A análise fatorial pode ser apropriada."



#####>>> Adequação da Amostra (KMO)
kmo_test <- KMO(cor_matrix)
print(kmo_test)

kmo_value <- kmo_test$MSA

if(kmo_value >= 0.90) {
    print("Adequação da amostra é excelente.")
} else if(kmo_value >= 0.80) {
    print("Adequação da amostra é muito boa.")
} else if(kmo_value >= 0.70) {
    print("Adequação da amostra é boa.")
} else if(kmo_value >= 0.60) {
    print("Adequação da amostra é medíocre.")
} else {
    print("Adequação da amostra é ruim.")
}
# Resultado
# [1] "Adequação da amostra é boa."
# ==================== #



# ==================== #
##### 3.1.1.c) Primeiro gráfico de cotevelo

### Tratamento inicial
# Remover os NAs
data_subset_clean <- data_subset[complete.cases(data_subset), ]

# Normalização das variáveis
data_subset_scaled <- scale(data_subset_clean)


### Quantos fatores utilizar?
fa.parallel(data_subset_scaled, fa="fa")
# Resultado: 3 parece forte, e 7 inclui outros 4 bem menos fortes
# ==================== #



# ==================== #
###### > 3.1.2 Primeira análise ###########
# ==================== #

###>>> Tratamento inicial
# Extrair pesos usando função `weights`
pesos <- weights(desenho.pof_subset)

# Selecionar as colunas relevantes para clustering, excluindo IDs e weights
data_subset <- pof_df_sb_clean[, !names(pof_df_sb_clean) %in% c("COD_UPA", "ESTRATO_POF", "PESO", "PESO_FINAL")]

# Normalizar os dados com a média e desvio padrão
means <- apply(data_subset, 2, function(x) svymean(~x, desenho.pof_subset, na.rm = TRUE))

sds <- apply(data_subset, 2, function(x) sqrt(svyvar(~x, desenho.pof_subset, na.rm = TRUE)))

# Escalonar os dados
data_subset_scaled <- scale(data_subset, center = means, scale = sds)

# Ponderando com os weights
weighted_data <- sweep(data_subset_scaled, 1, pesos, "*")
# ==================== #



# ==================== #
### 3.1.2.a) Primeiro PCA

# Cálculo da matrix de covariância ponderada
cov_matrix <- cov.wt(data_subset_scaled, wt = pesos, cor = TRUE)$cov

# Realizar PCA usando a matrix de covariância ponderada
pca_result <- prcomp(data_subset_scaled, center = TRUE, scale. = TRUE)

### Resultados 
#PCA
summary(pca_result)
# Carregamentos
loadings <- pca_result$rotation
# Visualização das cargas fatoriais
print(loadings)
# ==================== #



# ==================== #
### 3.1.2.b) Primeira análise de cluster

# Realizar clustering (k-means)
set.seed(123)
kmeans_result <- kmeans(weighted_data, centers = 3) # Change centers as needed
# ==================== #



# ==================== #
### Plotemos
# Printemos os centroides de cada cluster
centroids <- kmeans_result$centers
print(centroids)

# Exibir as cargas das variáveis nas componentes principais
principal(data_subset, nfactors = 3)

# Dataframe BIDIMENSIONAL para plotar os resultados
pca_data <- data.frame(
    PC1 = pca_result$x[, 1], 
    PC2 = pca_result$x[, 2], 
    Cluster = factor(kmeans_result$cluster)
)

# Plotando os resultados BIDIMENSIONAIS do PCA com os clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point() +
    theme_minimal() +
    labs(title = "PCA of POF6 Data with K-means Clusters", 
        x = "Principal Component 1", 
        y = "Principal Component 2", 
        color = "Cluster")


# Dataframe TRIDIMENSIONAL para plotar os resultados
pca_data_3d <- data.frame(
    PC1 = pca_result$x[, 1], 
    PC2 = pca_result$x[, 2],
    PC3 = pca_result$x[, 3], 
    Cluster = factor(kmeans_result$cluster)
)

# Plotando os resultados TRIDIMENSIONAIS do PCA com os clusters
plot_ly(pca_data_3d, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster, colors = c('#FF4136', '#2ECC40', '#0074D9', '#B10DC9')) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'PC1'),
        yaxis = list(title = 'PC2'),
        zaxis = list(title = 'PC3')),
        title = "3D PCA of POF6 Data with K-means Clusters")
# ==================== #



# ==================== #
### 3.1.1.d) Segundo PCA, com rotação

rotated_pcav <- principal(data_subset, nfactors = 2, rotate = "varimax")

# Resultados:
print(rotated_pcav)

rotated_pcap <- principal(data_subset, nfactors = 2, rotate = "promax")
# Resultados
print(rotated_pcap)

# Foram encontradas muitas variáveis com cargas muito baixas
# ==================== #





# ==================== #
###### > 3.1.3 Segunda análise ###########
# ==================== #

### 3.1.2.a) Segunda seleção das variáveis de interesse

#### O objetivo agora é dropar as variáveis com carga baixa nos diferentes componentes (h2) e baixa singularidade (u2)  

#### Abandonando variáveis com baixa carga
# Limiar: variáveis com cargas abaixo de 0,3 em todos os componentes ou com comunalidades abaixo de 0,4

red_interesse <- c(
    "a_ca", "a_pr", "a_cm", "a_fm", "a_sc",
    "m_ps", "s_ag", "s_es", "s_cb", "s_pv", 
    "s_ct", "lur", "gt", "fdu", "mdt", 
    "abt", "s_lx", "ESTRATO_POF", 
    "COD_UPA", "PESO", "PESO_FINAL"
)

# Selecionar as variáveis de interesse do dataframe
pof_df_sb <- pof_dataframe[, red_interesse]

# Remover linhas com NAs
pof_df_sb_clean <- na.omit(pof_df_sb)

# Criar um novo objeto survey.design com o dataframe reduzido e limpo
desenho.pof_subset <- svydesign(
    id = ~COD_UPA,
    strata = ~ESTRATO_POF,
    data = pof_df_sb_clean,
    weights = ~PESO,
    nest = TRUE
)

# Verificar as variáveis no objeto desenho.pof_subset
print(names(desenho.pof_subset$variables))

# Remover os NAs
data_subset_clean <- data_subset[complete.cases(data_subset), ]

# Normalização das variáveis
data_subset_scaled <- scale(data_subset_clean)
# ==================== #



# ==================== #
### 3.1.2.b) Novo PCA e análise de cluster

###>>> Tratamento inicial
# Extrair pesos usando função `weights`
pesos <- weights(desenho.pof_subset)

# Selecionar as colunas relevantes para clustering, excluindo IDs e weights
data_subset <- pof_df_sb_clean[, !names(pof_df_sb_clean) %in% c("COD_UPA", "ESTRATO_POF", "PESO", "PESO_FINAL")]

# Normalizar os dados com a média e desvio padrão
means <- apply(data_subset, 2, function(x) svymean(~x, desenho.pof_subset, na.rm = TRUE))
sds <- apply(data_subset, 2, function(x) sqrt(svyvar(~x, desenho.pof_subset, na.rm = TRUE)))

# Escalonar os dados
data_subset_scaled <- scale(data_subset, center = means, scale = sds)

# Ponderando com os weights
weighted_data <- sweep(data_subset_scaled, 1, pesos, "*")


# ==================== #
# Cálculo da matrix de covariância ponderada
cov_matrix <- cov.wt(data_subset_scaled, wt = pesos, cor = TRUE)$cov

# Realizar PCA usando a matrix de covariância ponderada
pca_result <- prcomp(data_subset_scaled, center = TRUE, scale. = TRUE)

### Resultados
summary(pca_result)
# Carregamentos
loadings <- pca_result$rotation
# Visualização das cargas fatoriais
print(loadings)
# ==================== #



# ==================== #
# Realizar clustering (k-means)
set.seed(123) # For reproducibility
kmeans_result <- kmeans(weighted_data, centers = 3)

# Printemos os centroides de cada cluster
centroids <- kmeans_result$centers
print(centroids)

# Exibir as cargas das variáveis nas componentes principais
loadings <- pca_result$rotation
print(loadings)

# Cargas com rotação
rotated_pca <- principal(data_subset_scaled, nfactors = 3, rotate = "varimax")
# Resultados
print(rotated_pca)

# Cargas com rotação
rotated_pca_1 <- principal(data_subset_scaled, nfactors = 3, rotate = "promax")
# Resultados
print(rotated_pca_1)
# ==================== #



# ==================== #
###### > 3.1.4 Terceira análise ###########
# ==================== #

### 3.1.2.a) Terceira seleção das variáveis de interesse

red2_interesse <- c(
    "a_pr", "a_fm", "a_sc",
    "s_cb", "s_pv", "s_lx", 
    "lur", "gt", "fdu", "mdt", 
    "ESTRATO_POF", 
    "COD_UPA", "PESO", "PESO_FINAL"
)

# Selecionar as variáveis de interesse do dataframe
pof_df_sb <- pof_dataframe[, red2_interesse]

# Remover linhas com NAs
pof_df_sb_clean <- na.omit(pof_df_sb)

# Criar um novo objeto survey.design com o dataframe reduzido e limpo
desenho.pof_subset <- svydesign(
    id = ~COD_UPA,
    strata = ~ESTRATO_POF,
    data = pof_df_sb_clean,
    weights = ~PESO,
    nest = TRUE
)

# Verificar as variáveis no objeto desenho.pof_subset
print(names(desenho.pof_subset$variables))

# Remover os NAs
data_subset_clean <- data_subset[complete.cases(data_subset), ]

# Normalização das variáveis
data_subset_scaled <- scale(data_subset_clean)
# ==================== #



# ==================== #
### 3.1.4.b) Terceiro PCA e terceira análise de cluster

###>>> Tratamento inicial
# Extrair pesos usando função `weights`
pesos <- weights(desenho.pof_subset)

# Selecionar as colunas relevantes para clustering, excluindo IDs e weights
data_subset <- pof_df_sb_clean[, !names(pof_df_sb_clean) %in% c("COD_UPA", "ESTRATO_POF", "PESO", "PESO_FINAL")]

# Normalizar os dados com a média e desvio padrão
means <- apply(data_subset, 2, function(x) svymean(~x, desenho.pof_subset, na.rm = TRUE))
sds <- apply(data_subset, 2, function(x) sqrt(svyvar(~x, desenho.pof_subset, na.rm = TRUE)))

# Escalonar os dados
data_subset_scaled <- scale(data_subset, center = means, scale = sds)

# Ponderando com os weights
weighted_data <- sweep(data_subset_scaled, 1, pesos, "*")


# ==================== #
# Cálculo da matrix de covariância ponderada
cov_matrix <- cov.wt(data_subset_scaled, wt = pesos, cor = TRUE)$cov

# Realizar PCA usando a matrix de covariância ponderada
pca_result <- prcomp(data_subset_scaled, center = TRUE, scale. = TRUE)

### Resultados
summary(pca_result)
# Carregamentos
loadings <- pca_result$rotation
# Visualização das cargas fatoriais
print(loadings)
# ==================== #



# ==================== #
### Plotemos

# Realizar clustering (k-means)
set.seed(123) # For reproducibility
kmeans_result <- kmeans(weighted_data, centers = 3)


# Printemos os centroides de cada cluster
centroids <- kmeans_result$centers
print(centroids)

# Exibir as cargas das variáveis nas componentes principais
loadings <- pca_result$rotation
print(loadings)


# Dataframe BIDIMENSIONAL para plotar os resultados
pca_data <- data.frame(
    PC1 = pca_result$x[, 1], 
    PC2 = pca_result$x[, 2], 
    Cluster = factor(kmeans_result$cluster)
)

# Plotando os resultados BIDIMENSIONAIS do PCA com os clusters
ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
    geom_point() +
    theme_minimal() +
    labs(title = "PCA of POF6 Data with K-means Clusters", 
        x = "Principal Component 1", 
        y = "Principal Component 2", 
        color = "Cluster")


# Dataframe TRIDIMENSIONAL para plotar os resultados
pca_data_3d <- data.frame(
    PC1 = pca_result$x[, 1], 
    PC2 = pca_result$x[, 2],
    PC3 = pca_result$x[, 3], 
    Cluster = factor(kmeans_result$cluster)
)

# Plotando os resultados TRIDIMENSIONAIS do PCA com os clusters
plot_ly(pca_data_3d, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Cluster, colors = c('#FF4136', '#2ECC40', '#0074D9', '#B10DC9')) %>%
    add_markers() %>%
    layout(scene = list(xaxis = list(title = 'PC1'),
        yaxis = list(title = 'PC2'),
        zaxis = list(title = 'PC3')),
        title = "3D PCA of POF6 Data with K-means Clusters")
# ==================== #



# ==================== #
# Cargas com rotação
rotated_pca <- principal(data_subset_scaled, nfactors = 3, rotate = "varimax")
# Resultados
print(rotated_pca)

# Cargas com rotação
rotated_pca_1 <- principal(data_subset_scaled, nfactors = 3, rotate = "promax")
# Resultados
print(rotated_pca_1)
# ==================== #



# ==================== #
###### > 3.1.3 Resultados ###########
# ==================== #
# 
# Os resultados mostram que as variáveis são bem representadas pelos três componentes principais, com cargas fatoriais adequadas e comunalidades altas. Isso sugere que a estrutura do modelo é robusta.
#
#   RC1 - SERVIÇOS BÁSICOS: 
# Variáveis:  s_cb, s_pv, s_lx, lur
#
#   RC2 - ALIMENTAÇÃO: 
# Variáveis: a_pr, a_fm, a_sc
#
#   RC3 - MORADIA: 
# Variáveis: gt, fdu, mdt
# ==================== #
# ==================================== #





# ==================================== #
###### 3.2 ANÁLISE FATORIAL CONFIRMÁRIA (CFA) #######
# ==================================== #
# Pacote usado: `lavaan` (Rosseel, 2012) 


# ==================== #
###### > 3.2.1 Modelo: especificação, avaliação e ajuste ###########
# ==================== #

###>>> 3.2.1.a) Primeiro modelo teórico: três fatores médios com um fator superior (h)
cfa_model <- '
# Fatores latentes
    Alimentacao =~ a_pr + a_fm + a_sc
    Servicos =~ s_cb + s_pv + s_lx
    Moradia =~ lur + gt + fdu + mdt
    h =~ Alimentacao + Servicos + Moradia
'

# Criar o objeto survey.design com o dataframe limpo
survey_design <- svydesign(
    id = ~COD_UPA,
    strata = ~ESTRATO_POF,
    data = pof_df_sb_clean,
    weights = ~PESO,
    nest = TRUE
)


# Função para calcular a correlação ponderada
weighted_correlation <- function(x, y, weights) {
    wm <- function(z, w) sum(z * w) / sum(w)
    wx <- x - wm(x, weights)
    wy <- y - wm(y, weights)
    return(sum(wx * wy * weights) / sqrt(sum(wx^2 * weights) * sum(wy^2 * weights)))
}

# Selecionar as variáveis
vars <- c("a_pr", "a_fm", "a_sc", "s_cb", "s_pv", "s_lx", "lur", "gt", "fdu", "mdt")
pof_df_sb_clean_selected <- pof_df_sb_clean[, vars]
weights <- weights(survey_design)

# Calcular a matriz de correlação ponderada
cor_matrix_weighted <- matrix(0, ncol = length(vars), nrow = length(vars))

for (i in 1:length(vars)) {
    for (j in 1:length(vars)) {
        cor_matrix_weighted[i, j] <- weighted_correlation(pof_df_sb_clean_selected[, i], pof_df_sb_clean_selected[, j], weights)
    }
}

colnames(cor_matrix_weighted) <- vars
rownames(cor_matrix_weighted) <- vars

print(cor_matrix_weighted)


# Ajustar o modelo CFA com a matriz de correlação ponderada
fit_cfa_model_superior <- cfa(cfa_model, sample.cov = cor_matrix_weighted, sample.nobs = nrow(pof_df_sb_clean_selected))

# Ver os resultados
summary(fit_cfa_model_superior, fit.measures = TRUE, standardized = TRUE)

# ==================== #
#### Representação gráfica do primeiro modelo com `semPaths`
semPaths(
    fit_cfa_model_superior, # Nome do objeto do modelo ajustado
    whatLabels = "std", # Rotular os coeficientes padronizados
    intercepts = FALSE, # Não mostrar interceptos
    style = "lisrel", # Estilo do diagrama
    rotation = 3, # Rotação do diagrama
    nCharNodes = 0, # Número de caracteres nos nós
    nCharEdges = 0, # Número de caracteres nas arestas
    curveAdjacent = TRUE, # Curvar arestas adjacentes
    title = TRUE, # Mostrar título
    layout = "tree2", # Layout em árvore
    curvePivot = TRUE # Pivotar as curvas
)
# ==================== #




# ==================== #
###>>> 3.2.1.b) Segundo modelo teórico: três fatores médios com um fator superior (h)

## Varíaveis dropadas de "Moradia": carregamentos baixos em "gt" e "fdu"

cfa_model2 <- '
# Fatores latentes
    Alimentacao =~ a_pr + a_fm + a_sc
    Servicos =~ s_cb + s_pv + s_lx
    Moradia =~ lur + mdt
    h =~ Alimentacao + Servicos + Moradia
'

# Criar o objeto survey.design com o dataframe limpo
survey_design <- svydesign(
    id = ~COD_UPA,
    strata = ~ESTRATO_POF,
    data = pof_df_sb_clean,
    weights = ~PESO,
    nest = TRUE
)


# Função para calcular a correlação ponderada
weighted_correlation <- function(x, y, weights) {
    wm <- function(z, w) sum(z * w) / sum(w)
    wx <- x - wm(x, weights)
    wy <- y - wm(y, weights)
    return(sum(wx * wy * weights) / sqrt(sum(wx^2 * weights) * sum(wy^2 * weights)))
}

# Selecionar as variáveis
vars <- c("a_pr", "a_fm", "a_sc", "s_cb", "s_pv", "s_lx", "lur", "mdt")
pof_df_sb_clean_selected <- pof_df_sb_clean[, vars]
weights <- weights(survey_design)

# Calcular a matriz de correlação ponderada
cor_matrix_weighted <- matrix(0, ncol = length(vars), nrow = length(vars))
for (i in 1:length(vars)) {
    for (j in 1:length(vars)) {
        cor_matrix_weighted[i, j] <- weighted_correlation(pof_df_sb_clean_selected[, i], pof_df_sb_clean_selected[, j], weights)
    }
}
colnames(cor_matrix_weighted) <- vars
rownames(cor_matrix_weighted) <- vars

print(cor_matrix_weighted)


# Ajustar o modelo CFA com a matriz de correlação ponderada
fit_cfa_model_superior2 <- cfa(cfa_model2, sample.cov = cor_matrix_weighted, sample.nobs = nrow(pof_df_sb_clean_selected))

# Ver os resultados
summary(fit_cfa_model_superior2, fit.measures = TRUE, standardized = TRUE)

# ==================== #
#### Representação gráfica do modelo com `semPaths`
semPaths(
    fit_cfa_model_superior2, # Nome do objeto do modelo ajustado
    whatLabels = "std", # Rotular os coeficientes padronizados
    intercepts = FALSE, # Não mostrar interceptos
    style = "lisrel", # Estilo do diagrama
    rotation = 3, # Rotação do diagrama
    nCharNodes = 0, # Número de caracteres nos nós
    nCharEdges = 0, # Número de caracteres nas arestas
    curveAdjacent = TRUE, # Curvar arestas adjacentes
    title = TRUE, # Mostrar título
    layout = "tree2", # Layout em árvore
    curvePivot = TRUE # Pivotar as curvas
)
# ==================== #

# Resultado
# A remoção de "gt" e "fdu" levou a uma melhoria significativa nos índices de ajuste
# ==================== #






# ==================== #
###>>> 3.2.1.c) Terceiro modelo teórico: três fatores médios sem o fator superior (h)

## Varíaveis dropadas de "Moradia": carregamentos baixos em "gt" e "fdu"

cfa_model3 <- '
# Fatores latentes
    Alimentacao =~ a_pr + a_fm + a_sc
    Servicos =~ s_cb + s_pv + s_lx
    Moradia =~ lur + mdt
'

# Criar o objeto survey.design com o dataframe limpo
survey_design <- svydesign(
    id = ~COD_UPA,
    strata = ~ESTRATO_POF,
    data = pof_df_sb_clean,
    weights = ~PESO,
    nest = TRUE
)


# Função para calcular a correlação ponderada
weighted_correlation <- function(x, y, weights) {
    wm <- function(z, w) sum(z * w) / sum(w)
    wx <- x - wm(x, weights)
    wy <- y - wm(y, weights)
    return(sum(wx * wy * weights) / sqrt(sum(wx^2 * weights) * sum(wy^2 * weights)))
}

# Selecionar as variáveis
vars <- c("a_pr", "a_fm", "a_sc", "s_cb", "s_pv", "s_lx", "lur", "mdt")
pof_df_sb_clean_selected <- pof_df_sb_clean[, vars]
weights <- weights(survey_design)

# Calcular a matriz de correlação ponderada
cor_matrix_weighted <- matrix(0, ncol = length(vars), nrow = length(vars))
for (i in 1:length(vars)) {
    for (j in 1:length(vars)) {
        cor_matrix_weighted[i, j] <- weighted_correlation(pof_df_sb_clean_selected[, i], pof_df_sb_clean_selected[, j], weights)
    }
}
colnames(cor_matrix_weighted) <- vars
rownames(cor_matrix_weighted) <- vars

print(cor_matrix_weighted)


# Ajustar o modelo CFA com a matriz de correlação ponderada
fit_cfa_model3 <- cfa(cfa_model3, sample.cov = cor_matrix_weighted, sample.nobs = nrow(pof_df_sb_clean_selected))

# Ver os resultados
summary(fit_cfa_model3, fit.measures = TRUE, standardized = TRUE)

# ==================== #
#### Representação gráfica do modelo com função `semPaths`
semPaths(
    fit_cfa_model3, # Nome do objeto do modelo ajustado
    whatLabels = "std", # Rotular os coeficientes padronizados
    intercepts = FALSE, # Não mostrar interceptos
    style = "lisrel", # Estilo do diagrama
    rotation = 3, # Rotação do diagrama
    nCharNodes = 0, # Número de caracteres nos nós
    nCharEdges = 0, # Número de caracteres nas arestas
    curveAdjacent = TRUE, # Curvar arestas adjacentes
    title = TRUE, # Mostrar título
    layout = "tree2", # Layout em árvore
    curvePivot = TRUE # Pivotar as curvas
)
# ==================== #



# ==================== #
###>>> 3.2.1.d) Quarto modelo teórico: dois fatores médios com o fator superior (h)

## Fator "Moradia" dropado

cfa_model4 <- '
# Fatores latentes
    Alimentacao =~ a_pr + a_fm + a_sc
    Servicos =~ s_cb + s_pv + s_lx
    h =~ Alimentacao + Servicos
'

# Criar o objeto survey.design com o dataframe limpo
survey_design <- svydesign(
    id = ~COD_UPA,
    strata = ~ESTRATO_POF,
    data = pof_df_sb_clean,
    weights = ~PESO,
    nest = TRUE
)


# Função para calcular a correlação ponderada
weighted_correlation <- function(x, y, weights) {
    wm <- function(z, w) sum(z * w) / sum(w)
    wx <- x - wm(x, weights)
    wy <- y - wm(y, weights)
    return(sum(wx * wy * weights) / sqrt(sum(wx^2 * weights) * sum(wy^2 * weights)))
}

# Selecionar as variáveis
vars <- c("a_pr", "a_fm", "a_sc", "s_cb", "s_pv", "s_lx")
pof_df_sb_clean_selected <- pof_df_sb_clean[, vars]
weights <- weights(survey_design)

# Calcular a matriz de correlação ponderada
cor_matrix_weighted <- matrix(0, ncol = length(vars), nrow = length(vars))
for (i in 1:length(vars)) {
    for (j in 1:length(vars)) {
        cor_matrix_weighted[i, j] <- weighted_correlation(pof_df_sb_clean_selected[, i], pof_df_sb_clean_selected[, j], weights)
    }
}
colnames(cor_matrix_weighted) <- vars
rownames(cor_matrix_weighted) <- vars

print(cor_matrix_weighted)


# Ajustar o modelo CFA com a matriz de correlação ponderada
fit_cfa_model_superior4 <- cfa(cfa_model4, sample.cov = cor_matrix_weighted, sample.nobs = nrow(pof_df_sb_clean_selected))

# Ver os resultados
summary(fit_cfa_model_superior4, fit.measures = TRUE, standardized = TRUE)

# ==================== #
#### Representação gráfica do modelo com a função `semPaths`
semPaths(
    fit_cfa_model_superior4, # Nome do objeto do modelo ajustado
    whatLabels = "std", # Rotular os coeficientes padronizados
    intercepts = FALSE, # Não mostrar interceptos
    style = "lisrel", # Estilo do diagrama
    rotation = 3, # Rotação do diagrama
    nCharNodes = 0, # Número de caracteres nos nós
    nCharEdges = 0, # Número de caracteres nas arestas
    curveAdjacent = TRUE, # Curvar arestas adjacentes
    title = TRUE, # Mostrar título
    layout = "tree2", # Layout em árvore
    curvePivot = TRUE # Pivotar as curvas
)
# ==================== #




# ==================== #
###>>> 3.2.1.e) Quinto modelo teórico: dois fatores médios sem o fator superior (h)

## Dropados: fatores "Moradia" e "h"

cfa_model5 <- '
# Fatores latentes
    Alimentacao =~ a_pr + a_fm + a_sc
    Servicos =~ s_cb + s_pv + s_lx
'

# Criar o objeto survey.design com o dataframe limpo
survey_design <- svydesign(
    id = ~COD_UPA,
    strata = ~ESTRATO_POF,
    data = pof_df_sb_clean,
    weights = ~PESO,
    nest = TRUE
)


# Função para calcular a correlação ponderada
weighted_correlation <- function(x, y, weights) {
    wm <- function(z, w) sum(z * w) / sum(w)
    wx <- x - wm(x, weights)
    wy <- y - wm(y, weights)
    return(sum(wx * wy * weights) / sqrt(sum(wx^2 * weights) * sum(wy^2 * weights)))
}

# Selecionar as variáveis
vars <- c("a_pr", "a_fm", "a_sc", "s_cb", "s_pv", "s_lx")
pof_df_sb_clean_selected <- pof_df_sb_clean[, vars]
weights <- weights(survey_design)

# Calcular a matriz de correlação ponderada
cor_matrix_weighted <- matrix(0, ncol = length(vars), nrow = length(vars))
for (i in 1:length(vars)) {
    for (j in 1:length(vars)) {
        cor_matrix_weighted[i, j] <- weighted_correlation(pof_df_sb_clean_selected[, i], pof_df_sb_clean_selected[, j], weights)
    }
}
colnames(cor_matrix_weighted) <- vars
rownames(cor_matrix_weighted) <- vars

print(cor_matrix_weighted)


# Ajustar o modelo CFA com a matriz de correlação ponderada
fit_cfa_model5 <- cfa(cfa_model5, sample.cov = cor_matrix_weighted, sample.nobs = nrow(pof_df_sb_clean_selected))

# Ver os resultados
summary(fit_cfa_model5, fit.measures = TRUE, standardized = TRUE)

# ==================== #
#### Representação gráfica com a função `semPaths`
semPaths(
    fit_cfa_model5, # Nome do objeto do modelo ajustado
    whatLabels = "std", # Rotular os coeficientes padronizados
    intercepts = FALSE, # Não mostrar interceptos
    style = "lisrel", # Estilo do diagrama
    rotation = 3, # Rotação do diagrama
    nCharNodes = 0, # Número de caracteres nos nós
    nCharEdges = 0, # Número de caracteres nas arestas
    curveAdjacent = TRUE, # Curvar arestas adjacentes
    title = TRUE, # Mostrar título
    layout = "tree2", # Layout em árvore
    curvePivot = TRUE # Pivotar as curvas
)
# ==================== #




# ==================== #
###### > 3.2.2 Análise fatorial e análise de cluster ###########
# ==================== #

# ==================== #
##### 3.2.2.a) Análise fatorial

### Tratamento dos dados
# Selecionar as variáveis
vars <- c("a_pr", "a_fm", "a_sc", "s_cb", "s_pv", "s_lx")
pof_df_sb_clean_selected <- pof_df_sb_clean[, vars]
weights <- weights(survey_design)

# Escalonar os dados
pof_df_sb_clean_selected_scaled <- scale(pof_df_sb_clean_selected)



# Cálculo da matrix de covariância ponderada
weighted_correlation <- function(x, y, weights) {
    wm <- function(z, w) sum(z * w) / sum(w)
    wx <- x - wm(x, weights)
    wy <- y - wm(y, weights)
    return(sum(wx * wy * weights) / sqrt(sum(wx^2 * weights) * sum(wy^2 * weights)))
}

cor_matrix_weighted <- matrix(0, ncol = length(vars), nrow = length(vars))
for (i in 1:length(vars)) {
    for (j in 1:length(vars)) {
        cor_matrix_weighted[i, j] <- weighted_correlation(pof_df_sb_clean_selected_scaled[, i], pof_df_sb_clean_selected_scaled[, j], weights)
    }
}
colnames(cor_matrix_weighted) <- vars
rownames(cor_matrix_weighted) <- vars

# Calcular a matriz de distância
distance_matrix <- as.dist(1 - cor_matrix_weighted)
# ==================== #



# ==================== #
##### 3.2.2.b) Análise de cluster

### Realizar clustering
# Executar a análise de cluster hierárquica
hc <- hclust(distance_matrix, method = "ward.D2")

# Plotar o dendrograma
plot(hc, main = "Dendrograma da Análise de Cluster", xlab = "", sub = "", cex = 0.9)
# ==================== #
# ==================================== #
# ========================================================================== #
# ========================================================================== #



# ========================================================================== #
# ========================================================================== #
####### FIM ####
# ========================================================================== #
# ========================================================================== #