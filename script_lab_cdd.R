###########################################################
### Script que lê o Censo Demográfico 2022 - Domicílio ####
### Data: 14 de setembro de 2022   ########################
### Nome: Kleber Oliveira  ################################
###########################################################


### Inicia a formatação
options( survey.lonely.psu = "adjust" )  # Opção para permitir estimar variância quando houver apenas 1 observação na UPA
options(OutDec=",") # Troca . por , nos outputs
options(scipen=999)
Sys.setlocale("LC_ALL", "en_US.UTF-8")
gc(verbose=FALSE, reset=FALSE, full=TRUE)


# Bibliotecas
pacman::p_load(
  tidyverse,
  readxl,
  readr, 
  survey,
  janitor,
  xtable,
  convey,
  ggplot2,
  ggpubr  
)


## Importa a base de pessoas
cd_dompes2022_id <- readRDS("cd_dompes2022_id.rds")

cd_dompes2022_id_design <- readRDS("cd_dompes2022_id_design.rds")
saveRDS(cd_dompes2022_id_design, "cd_dompes2022_id_design.rds")


##### Parte 1 - Indicadores demográficos

## Quesito 1
# item a
rz_sex_munic <- svyby(
    ~factor(P0160),
    ~NM_MUN,
    cd_dompes2022_id_design,    
    FUN = svytotal,
    na.rm = T, 
    keep.var = F
  )

names(rz_sex_munic) <- c("Munic", "Feminino", "Masculino")

rz_sex_munic <- rz_sex_munic %>% 
  mutate(rz_sex = (Masculino/Feminino)*100)

rz_sex_munic <- rz_sex_munic %>% 
  arrange(desc(rz_sex))

# item b
## Tabula a proporção de ocupados por atividade
ativ_princ <- svyby(
  ~I(P1030 == "Agricultura, pecuária, produção florestal, pesca e aquicultura"), 
  ~NM_MUN, 
  design = cd_dompes2022_id_design, 
  FUN = svymean,
  na.rm = T, 
  keep.var = F
)

names(ativ_princ) <- c("Munic", "prop_demais", "prop_primario")

## Faz o merge com a tabela de razão de sexo e ocupados por atividade

rzs_ativprinc <- merge(
  x = rz_sex_munic,
  y = ativ_princ, 
  by.x = "Munic",
  by.y = "Munic",
  all.x = T
)

rzs_ativprinc <- rzs_ativprinc %>% 
  select('Munic', 'rz_sex', 'prop_primario')

## Correlação de Pearson (relação linear)

correlacao_rzs_ativ <- cor(
  rzs_ativprinc$rz_sex, 
  rzs_ativprinc$prop_primario, 
  method = 'pearson',
  use = "complete.obs"
)

## Gráfico de dispersão
#### Utilizando o pacote ggpubr

graf_rzs_ativ <-ggscatter(
  rzs_ativprinc,
  x = "rz_sex",
  y = "prop_primario",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson"
) +
  labs(
    x = "Razão de sexo",
    y = "Proporção de ocupados no setor primário"    
  ) +
  theme_minimal()



## Quesito 2
## Fiz uma correção no rótulo ##
cd_dompes2022_id_design <- update(
  cd_dompes2022_id_design,
  P0950 = case_when(
    P0950 %in% "Não ocupada" ~ "Desocupada",
    TRUE ~ as.character(P0950)
  )) 


tx_desocup <- svyby(
  ~I(P0950 == "Desocupada"), 
  ~NM_MUN, 
  design = subset(
    cd_dompes2022_id_design, 
     P0930 == 1),
  FUN = svymean,
  na.rm = T, 
  keep.var = F
)

tx_desocup <- as.data.frame(tx_desocup)

names(tx_desocup) <- c('Munic', 'txocup', 'txdesocup')

## Faz o merge entre desocupação e ocupados por atividade

txdesoc_ativ <- merge(
  x = tx_desocup, 
  y = ativ_princ,
  by.x = 'Munic', 
  by.y = 'Munic',
  all.x = T
)

graf_txdesoc_ativ <- ggscatter(
  txdesoc_ativ,
  x = 'txdesocup',
  y = 'prop_primario',
  add = 'reg.line', 
  conf.int = T,
  cor.coef = T,
  cor.method = 'pearson'
) +
  labs(
    x = "Taxa de desocupação",
    y = "Proporção de ocupados no setor primário"    
  ) +
  theme_minimal()


## Quesito 3 

tx_analf <- svyby(
  ~I(P0640 == "Não"),
  ~NM_MUN,
  design = subset(
    cd_dompes2022_id_design,
    P0181>=15 & P0181 <=64
  ),
  FUN = svymean,
  na.rm = T,
  keep.var = F
)


## Quesito 4

renda_fonte <- svyby(
  ~P1080+P1100+P1110, 
  ~NM_MUN, 
  design = cd_dompes2022_id_design, 
  FUN = svymean, 
  na.rm = T, 
  keep.var = F
)

### Estima a participação de cada fonte no total
names(renda_fonte) <- c('Munic', 'trab', 'transf', 'total')

renda_fonte <- renda_fonte %>% 
  mutate(
    prop_trab = round((trab/total)*100, 2),
    prop_transf = round((transf/total)*100,2)
  )

### Correlação entre o nível de renda e a participação da renda de outras fontes

outfontes_renda <- ggscatter(
  renda_fonte,
  x = 'transf', 
  y = 'total',
  add = 'reg.line', 
  cor.coef = T, 
  conf.int = T, 
  cor.method = 'pearson',
  na.rm = T
)

##### Parte 2 - Indicadores domiciliáres domiciliares

## Quesito 1 
#### Proporção de domicílios chefiados por mulher

chefe_dom_fem <- svyby(
  ~I(D0171 == 2),
  ~NM_MUN, 
  subset(
    cd_dompes2022_id_design, 
    P0170 == 1),
  FUN = svymean,
  na.rm = T, 
  keep.var = F 
)

## Quesito 2
#### renda domiciliar per capita dos domicílios por sexo do chefe

renda_chefe_dom <- svyby(
  ~D0360,
  ~NM_MUN+P0160, 
  subset(
    cd_dompes2022_id_design, 
    P0170 == 1),
  FUN = svymean,
  na.rm = T, 
  keep.var = F 
)

names(renda_chefe_dom) <- c('Munic', 'sexo', 'rdpc')

renda_chefe_dom <- renda_chefe_dom %>% 
  pivot_wider(
    names_from = 'sexo', 
    values_from = 'rdpc'
  )

renda_chefe_dom <- renda_chefe_dom %>% 
  mutate(renda_hm = Masculino/Feminino*100)

### Quesito 3
## Top 10 de pior renda de chefe feminino

renda_chefe_dom <- renda_chefe_dom %>% 
  arrange(Feminino)

### Quesito 4
## Top 10 de pior hiato

renda_chefe_dom <- renda_chefe_dom %>% 
  arrange(desc(renda_hm))


## Quesito 5 
## Proporção de domicílios sem acesso à rede geral de água (D0290 ==2)

agua_canalizada <- svyby(
  ~I(D0300 == 1), 
  ~NM_MUN, 
  subset(
    cd_dompes2022_id_design, 
    P0170 ==1
  ), 
  svymean, 
  keep.var = F,
  na.rm = T
)

names(agua_canalizada) <- c('Munic', 'sem', 'com')

agua_canalizada <- agua_canalizada %>% 
  arrange(desc(sem))

## Proporção de domicílios sem esgotamento (D0250 >3)
esgoto <- svyby(
  ~I(D0250 >3), 
  ~NM_MUN, 
  subset(
    cd_dompes2022_id_design, 
    P0170 ==1
  ), 
  svymean, 
  keep.var = F,
  na.rm = T
)

names(esgoto) <- c('Munic', 'adequado', 'inadequado')

esgoto <- esgoto %>% 
  arrange(desc(inadequado))


## Proporção de domicílios com internet (D0330 ==1)
internet <- svyby(
  ~I(D0330 == 1), 
  ~NM_MUN, 
  subset(
    cd_dompes2022_id_design, 
    P0170 ==1
  ), 
  svymean, 
  keep.var = F,
  na.rm = T
)

names(internet) <- c('Munic', 'nao tem', 'tem')

internet <- internet %>% 
arrange(desc(`nao tem`))

