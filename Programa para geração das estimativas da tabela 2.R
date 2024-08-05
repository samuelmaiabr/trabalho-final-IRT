#                            POF 2017-2018

#  PROGRAMA PARA GERAÇÃO DAS ESTIMATIVAS DA tabela 2 da publicação: 
#  Pesquisa de Orçamentos Familiares 2017-2018: Análise da segurança alimentar no Brasil
#  referentes aos dados da POF 2017-2018

# [1] Leitura do arquivo com as populações dos pós estratos. 
#     Estas populações são necessárias para implementar o ajuste de pós-estratificação
#     dos pesos originais que resultam nos pesos finais.

# "....." indica a pasta/diretório de trabalho no HD local separados por "/"
# onde se encontram os arquivos .xls ou .xlsx de documentação dos microdados
# Exemplo: setwd("c:/POF2018/Documentacao_aaaammdd")

setwd(".....") # ..... é o caminho para a pasta "/Documentacao__aaaammdd/"

pos_estrato <-
  readxl::read_excel(
    "Pos_estratos_totais.xlsx",skip=5) # [1]


# Leitura das bases de trabalho

# É preciso executar antes o arquivo "Leitura dos Microdados - R.R"
# que se encontra no arquivo compactado "Programas_de_Leitura.zip"
# Este passo é necessário para gerar os arquivos com a extensão .rds
# correspondentes aos arquivos com extensão .txt dos microdados da POF

# "....." indica a pasta/diretório de trabalho no HD local separados por "/"
# onde se encontram os arquivos .txt descompactados do arquivo Dados_aaaammdd.zip
# Exemplo: setwd("c:/POF2018/Dados_aaaammdd/")

setwd(".....") # ..... é o caminho para a pasta onde se encontram os arquivos .txt descompactados do arquivo Dados_aaaammdd.zip

POF6 <- readRDS("CONDICOES_VIDA.rds")

DOMICILIO <- readRDS("DOMICILIO.rds")

# Iniciando a base de trabalho
POF6_novo <- merge(POF6,
                   pos_estrato,
                   by.x = "COD_UPA" ,
                   by.y = "COD_UPA(UF+SEQ+DV)") 

rm(pos_estrato,POF6)

# Limpando as variáveis que duplicaram no merge
POF6_novo <- transform(POF6_novo,
                       ESTRATO_POF.y = NULL,
                       ESTRATO_POF = ESTRATO_POF.x,
                       ESTRATO_POF.x = NULL,
                       uf = NULL)

### Base com variáveis de domicílios
# selecionando apenas a variável derivada da EBIA
domicilio <- DOMICILIO[,c("COD_UPA",
                          "NUM_DOM",
                          "V6199")] # EBIA

# Atualização da base de trabalho com a base por domicílio anterior:
POF6_novo <- merge( POF6_novo, domicilio)

rm(DOMICILIO,domicilio)

# Estimando o total de UCs por pós estrato projetada para a data da POF
tot_uc <- aggregate(PESO_FINAL~pos_estrato,
                    sum,
                    data=POF6_novo)

colnames(tot_uc) <- c("pos_estrato",
                      "Freq")

# Carregando pacote survey():
library(survey)			# load survey package (analyzes complex design surveys)

# Especificação que faz o R produzir erros-padrões convervadores no lugar de buggar
options( survey.lonely.psu = "adjust" )
# Equivalente à opção MISSUNIT no SUDAAN

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

# Criação de variável auxiliar para as estimativas de prevalências
desenho.pof <- transform(desenho.pof,
                         one = 1)

# Especificação das variáveis da EBIA
desenho.pof <- update( desenho.pof ,
                       ebia_pub = factor( V6199,
                                          labels = c("Segurança",
                                                     "Insegurança leve",
                                                     "Insegurança moderada",
                                                     "Insegurança grave")),
                       ebia_insan = car::recode( V6199 ,
                                                 " 1 ='Segurança' ;
                                               c(2,3,4)='Insegurança' " ))


############################################################
## Desenho amostral de domicílios

POF_dom <- subset( desenho.pof , 
                   NUM_UC == 1) # Só para as UCs=1

# Cálculo das prevalências de (in)segurança alimentar nos domicílios:

# Cálculo todas as categorias nos domicílios:
prev_dom_g01 <- svymean( ~ factor(ebia_pub),
                         POF_dom, 
                         na.rm=T)

# Cálculo com as categorias da EBIA agregadas nos domicílios:
prev_dom_g02 <- svymean( ~ factor(ebia_insan),
                         POF_dom, 
                         na.rm=T)

Tab3_c1 <- rbind(prev_dom_g01[1],
                 prev_dom_g02[1],
                 prev_dom_g01[2],
                 prev_dom_g01[3],
                 prev_dom_g01[4])

# Cálculo por situação do domicílio com todas as categorias EBIA nos domicílios:
prev_dom_g03 <- svyby( ~ factor(ebia_pub),
                       ~ factor(TIPO_SITUACAO_REG,
                                labels=c("Urbana",
                                         "Rural")),
                       POF_dom, 
                       svymean,
                       na.rm=T)

# Cálculo por situação do domicílio com as categorias EBIA agregadas nos domicílios:
prev_dom_g04 <- svyby( ~ factor(ebia_insan),
                       ~ factor(TIPO_SITUACAO_REG,
                                labels=c("Urbana",
                                         "Rural")),
                       POF_dom, 
                       svymean,
                       na.rm=T)

Tab3_c2_c3 <- rbind(prev_dom_g03[,2],
                    prev_dom_g04[,2],
                    prev_dom_g03[,3],
                    prev_dom_g03[,4],
                    prev_dom_g03[,5])

# Parte referente à POF e domicílios particulares da tabela 3 da publicação: Pesquisa de Orçamentos Familiares 2017-2018: Análise da segurança alimentar no Brasil
EBIA_POF_dom <- data.frame(Total=round(Tab3_c1[,1]*100,1),
                           Urbana=round(Tab3_c2_c3[,1]*100,1),
                           Rural=round(Tab3_c2_c3[,2]*100,1),
                           row.names = c(" Com segurança alimentar",
                                         " Com insegurança alimentar",
                                         "Leve",
                                         "Moderada",
                                         "Grave"))

# Estimativas dos totais de domicílios com todas as categorias EBIA nos domicílios:
cont_tot_dom_g01 <- svyby( ~ one, 
                           ~ factor(ebia_pub,
                                    c("Segurança",
                                      "Insegurança leve",
                                      "Insegurança moderada",
                                      "Insegurança grave")),
                           POF_dom, 
                           svytotal,
                           na.rm=T)

# Estimativas dos totais de domicílios com categorias EBIA agregadas nos domicílios:
cont_tot_dom_g02 <- svyby( ~ one, 
                           ~ factor(ebia_insan,
                                    levels=c("Insegurança",
                                             "Segurança")),
                           POF_dom, 
                           svytotal,
                           na.rm=T)

Tab2_c1 <- rbind(cont_tot_dom_g01[1,2],
                 cont_tot_dom_g02[1,2],
                 cont_tot_dom_g01[2,2],
                 cont_tot_dom_g01[3,2],
                 cont_tot_dom_g01[4,2])

# Estimativas dos totais de domicílios com todas as categorias EBIA nos domicílios 
# por situação do domicílio:
cont_tot_dom_g03 <- svyby( ~ one, 
                           ~ factor(ebia_pub,
                                    c("Segurança",
                                      "Insegurança leve",
                                      "Insegurança moderada",
                                      "Insegurança grave"))
                           + factor(TIPO_SITUACAO_REG,
                                    labels=c("Urbana",
                                             "Rural")),
                           POF_dom, 
                           svytotal,
                           na.rm=T)

# Estimativas dos totais de domicílios com categorias EBIA agregadas nos domicílios
# por situação do domicílio:
cont_tot_dom_g04 <- svyby( ~ one, 
                           ~ factor(ebia_insan,
                                    c("Insegurança",
                                      "Segurança"))
                           + factor(TIPO_SITUACAO_REG,
                                    labels=c("Urbana",
                                             "Rural")),
                           POF_dom, 
                           svytotal,
                           na.rm=T)

Tab2_c2_c3 <- rbind(cont_tot_dom_g03[c(1,5),3],
                    cont_tot_dom_g04[c(1,3),3],
                    cont_tot_dom_g03[c(2,6),3],
                    cont_tot_dom_g03[c(3,7),3],
                    cont_tot_dom_g03[c(4,8),3])

# Parte referente à POF e domicílios particulares da tabela 2 da publicação: Pesquisa de Orçamentos Familiares 2017-2018: Análise da segurança alimentar no Brasil
# Observação: alguns valores calculados abaixo divergem ligeiramente dos publicados
# decorrente de ajustes feitos na publicação para consistência de totais de linhas e colunas 
# como consequência do arredondamento de valores
cont_tot_dom_POF <- data.frame(Total=round(Tab2_c1[,1]/1000,0),
                               Urbana=round(Tab2_c2_c3[,1]/1000,0),
                               Rural=round(Tab2_c2_c3[,2]/1000,0),
                               row.names = c(" Com segurança alimentar",
                                             " Com insegurança alimentar",
                                             "Leve",
                                             "Moderada",
                                             "Grave"))

############################################################
## Desenho amostral de moradores

morador <- readRDS("MORADOR.rds")

##########################################################################

# Desenho para a estimativa dos moradores segundo situação de 
# segurança alimentar

morador_novo <- 
  merge( morador,
         POF_dom$variables[POF_dom$variables$NUM_UC == 1,], # equivalente à base de domicílios
         by=c("COD_UPA",
              "NUM_DOM"))

rm(morador)

# Limpando as variáveis que duplicaram no merge
morador_novo <- transform(morador_novo,
                          NUM_UC.y = NULL,
                          COD_INFORMANTE.y = NULL,
                          TIPO_SITUACAO_REG.y = NULL,
                          ESTRATO_POF.y = NULL,
                          UF.y = NULL,
                          PESO.y = NULL,
                          PESO_FINAL.y = NULL,
                          RENDA_TOTAL.y = NULL,
                          NUM_UC = NUM_UC.x,
                          COD_INFORMANTE = COD_INFORMANTE.x,
                          TIPO_SITUACAO_REG = TIPO_SITUACAO_REG.x,
                          ESTRATO_POF = ESTRATO_POF.x,
                          UF = UF.x,
                          PESO = PESO.x,
                          PESO_FINAL = PESO_FINAL.x,
                          RENDA_TOTAL = RENDA_TOTAL.x,
                          NUM_UC.x = NULL,
                          COD_INFORMANTE.x = NULL,
                          TIPO_SITUACAO_REG.x = NULL,
                          ESTRATO_POF.x = NULL,
                          UF.x = NULL,
                          PESO.x = NULL,
                          PESO_FINAL.x = NULL,
                          RENDA_TOTAL.x = NULL)

# Desenho básico por morador
POF.morador.desenho <- svydesign(id = ~COD_UPA ,
                                 strata = ~ESTRATO_POF ,
                                 data = morador_novo ,
                                 weights = ~PESO ,
                                 nest = TRUE	)
  
# Prepatação dos totais de pós-estratificação
tot_morador <- aggregate(PESO_FINAL~pos_estrato,
                         sum,
                         data=morador_novo)
colnames(tot_morador) <- c("pos_estrato","Freq")

# Desnho final pós-estratificado:
POF_morad <- postStratify( design = POF.morador.desenho ,
                           ~ pos_estrato ,
                           tot_morador)
  
# Conferência de total
POF_morad <- transform(POF_morad,
                       one = 1)
  
#svytotal(~one,
#         POF_morad)

rm(POF.morador.desenho)


######################################################################

# Cálculo das prevalências de (in)segurança alimentar dos moradores nos domicílios:

# Cálculo todas as categorias dos moradores nos domicílios:
prev_mor_g01 <- svymean( ~ factor(ebia_pub),
                         POF_morad, 
                         na.rm=T)

# Cálculo com as categorias da EBIA agregadas dos moradores nos domicílios:
prev_mor_g02 <- svymean( ~ factor(ebia_insan),
                         POF_morad, 
                         na.rm=T)

Tab3_c4 <- rbind(prev_mor_g01[1],
                 prev_mor_g02[1],
                 prev_mor_g01[2],
                 prev_mor_g01[3],
                 prev_mor_g01[4])

# Cálculo por situação do domicílio com todas as categorias EBIA dos moradores nos domicílios:
prev_mor_g03 <- svyby( ~ factor(ebia_pub),
                       ~ factor(TIPO_SITUACAO_REG,
                                labels=c("Urbana",
                                         "Rural")),
                       POF_morad, 
                       svymean,
                       na.rm=T)

# Cálculo por situação do domicílio com as categorias EBIA agregadas dos moradores nos domicílios:
prev_mor_g04 <- svyby( ~ factor(ebia_insan),
                       ~ factor(TIPO_SITUACAO_REG,
                                labels=c("Urbana",
                                         "Rural")),
                       POF_morad, 
                       svymean,
                       na.rm=T)

Tab3_c5_c6 <- rbind(prev_mor_g03[,2],
                    prev_mor_g04[,2],
                    prev_mor_g03[,3],
                    prev_mor_g03[,4],
                    prev_mor_g03[,5])

# Parte referente à POF e moradores da tabela 3 da publicação: Pesquisa de Orçamentos Familiares 2017-2018: Análise da segurança alimentar no Brasil
EBIA_POF_morad <- data.frame(Total=round(Tab3_c4[,1]*100,1),
                             Urbana=round(Tab3_c5_c6[,1]*100,1),
                             Rural=round(Tab3_c5_c6[,2]*100,1),
                             row.names = c(" Com segurança alimentar",
                                           " Com insegurança alimentar",
                                           "Leve",
                                           "Moderada",
                                           "Grave"))

# Estimativas dos totais de moradores com todas as categorias EBIA dos moradores nos domicílios:
cont_tot_mor_g01 <- svyby( ~ one, 
                           ~ factor(ebia_pub,
                                    c("Segurança",
                                      "Insegurança leve",
                                      "Insegurança moderada",
                                      "Insegurança grave")),
                           POF_morad, 
                           svytotal,
                           na.rm=T)

# Estimativas dos totais de moradores com categorias EBIA agregadas dos moradores nos domicílios:
cont_tot_mor_g02 <- svyby( ~ one, 
                           ~ factor(ebia_insan,
                                    levels=c("Insegurança",
                                             "Segurança")),
                           POF_morad, 
                           svytotal,
                           na.rm=T)

Tab2_c4 <- rbind(cont_tot_mor_g01[1,2],
                 cont_tot_mor_g02[1,2],
                 cont_tot_mor_g01[2,2],
                 cont_tot_mor_g01[3,2],
                 cont_tot_mor_g01[4,2])

# Estimativas dos totais de moradores com todas as categorias EBIA dos moradores nos domicílios 
# por situação do domicílio:
cont_tot_mor_g03 <- svyby( ~ one, 
                           ~ factor(ebia_pub,
                                    c("Segurança",
                                      "Insegurança leve",
                                      "Insegurança moderada",
                                      "Insegurança grave"))
                           + factor(TIPO_SITUACAO_REG,
                                    labels=c("Urbana",
                                             "Rural")),
                           POF_morad, 
                           svytotal,
                           na.rm=T)

# Estimativas dos totais de moradores com categorias EBIA agregadas dos moradores nos domicílios
# por situação do domicílio:
cont_tot_mor_g04 <- svyby( ~ one, 
                           ~ factor(ebia_insan,
                                    c("Insegurança",
                                      "Segurança"))
                           + factor(TIPO_SITUACAO_REG,
                                    labels=c("Urbana",
                                             "Rural")),
                           POF_morad, 
                           svytotal,
                           na.rm=T)

Tab2_c5_c6 <- rbind(cont_tot_mor_g03[c(1,5),3],
                    cont_tot_mor_g04[c(1,3),3],
                    cont_tot_mor_g03[c(2,6),3],
                    cont_tot_mor_g03[c(3,7),3],
                    cont_tot_mor_g03[c(4,8),3])

# Parte referente à POF e moradores da tabela 2 da publicação: Pesquisa de Orçamentos Familiares 2017-2018: Análise da segurança alimentar no Brasil
# Observação: alguns valores calculados abaixo divergem ligeiramente dos publicados
# decorrente de ajustes feitos na publicação para consistência de totais de linhas e colunas 
# como consequência do arredondamento de valores
cont_tot_mor_POF <- data.frame(Total=round(Tab2_c4[,1]/1000,0),
                               Urbana=round(Tab2_c5_c6[,1]/1000,0),
                               Rural=round(Tab2_c5_c6[,2]/1000,0),
                               row.names = c(" Com segurança alimentar",
                                             " Com insegurança alimentar",
                                             "Leve",
                                             "Moderada",
                                             "Grave"))

message("Parte referente à POF e domicílios particulares da tabela 2 da publicação- \nPesquisa de Orçamentos Familiares 2017-2018: \nAnálise da segurança alimentar no Brasil")
cont_tot_dom_POF
message("Parte referente à POF e moradores da tabela 2 da publicação- \nPesquisa de Orçamentos Familiares 2017-2018: \nAnálise da segurança alimentar no Brasil")
cont_tot_mor_POF
message("Parte referente à POF e domicílios particulares da tabela 3 da publicação- \nPesquisa de Orçamentos Familiares 2017-2018: \nAnálise da segurança alimentar no Brasil")
EBIA_POF_dom
message("Parte referente à POF e moradores da tabela 3 da publicação- \nPesquisa de Orçamentos Familiares 2017-2018: \nAnálise da segurança alimentar no Brasil")
EBIA_POF_morad
