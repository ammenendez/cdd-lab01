###########################################################
### Script que lê o Censo Demográfico 2022 - Domicílio ####
### Data: 31 de outubro de 2022    ########################
### Nome: Andrés Ignácio Martínez Menéndez ################
### Baseado no script de Kleber Oliveira ##################
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

## Item 1a - exporta os dez municípios com maior predominância de cada sexo
resultados_dir <- file.path("relatorio", "resultados")
dir.create(resultados_dir, recursive = TRUE, showWarnings = FALSE)

top10_homens <- rz_sex_munic %>%
  arrange(desc(rz_sex)) %>%
  slice_head(n = 10) %>%
  mutate(predominancia = "Masculina", posicao = row_number()) %>%
  select(predominancia, posicao, Munic, rz_sex)

top10_mulheres <- rz_sex_munic %>%
  arrange(rz_sex) %>%
  slice_head(n = 10) %>%
  mutate(predominancia = "Feminina", posicao = row_number()) %>%
  select(predominancia, posicao, Munic, rz_sex)

resultado_1a <- bind_rows(top10_homens, top10_mulheres)
write.csv2(
  resultado_1a,
  file = file.path(resultados_dir, "item_1a_top10_razao_sexo.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

escapa_latex <- function(x) {
  gsub("([#$%&_{}])", "\\\\\\1", x, perl = TRUE)
}

formata_razao <- function(x) {
  formatC(x, format = "f", digits = 2, decimal.mark = ",", big.mark = ".")
}

linhas_tabela_1a <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Dez municípios com maior predominância masculina e feminina, Sergipe, 2022}",
  "\\label{tab:razao-sexo-top10}",
  "\\small",
  "\\begin{tabular}{r l l r}",
  "\\toprule",
  "Posição & Município & Predominância & Homens por 100 mulheres \\\\",
  "\\midrule",
  "\\multicolumn{4}{l}{\\textbf{Maior predominância masculina}} \\\\",
  vapply(seq_len(nrow(top10_homens)), function(i) {
    paste0(i, " & ", escapa_latex(top10_homens$Munic[i]),
           " & Masculina & ", formata_razao(top10_homens$rz_sex[i]), " \\\\")
  }, character(1)),
  "\\midrule",
  "\\multicolumn{4}{l}{\\textbf{Maior predominância feminina}} \\\\",
  vapply(seq_len(nrow(top10_mulheres)), function(i) {
    paste0(i, " & ", escapa_latex(top10_mulheres$Munic[i]),
           " & Feminina & ", formata_razao(top10_mulheres$rz_sex[i]), " \\\\")
  }, character(1)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\fonte{Elaboração própria com base nos microdados do Censo Demográfico 2022.}",
  "\\nota{Razão de sexo calculada como o número de homens para cada 100 mulheres.}",
  "\\end{table}"
)

writeLines(
  linhas_tabela_1a,
  con = file.path(resultados_dir, "item_1a_top10_razao_sexo.tex"),
  useBytes = TRUE
)

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

outdec_original <- getOption("OutDec")
options(OutDec = ".")

graf_rzs_ativ <-ggscatter(
  rzs_ativprinc,
  x = "rz_sex",
  y = "prop_primario",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  cor.coeff.args = list(
    cor.coef.name = "r",
    r.digits = 3,
    p.accuracy = 0.001,
    p.decimal.mark = "."
  )
) +
  labs(
    x = "Razão de sexo (homens por 100 mulheres)",
    y = "Proporção de ocupados no setor primário"
  ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  theme_minimal()

options(OutDec = outdec_original)

## Item 1b - associação entre razão de sexo e participação do setor primário
dados_1b <- rzs_ativprinc %>%
  select(Munic, rz_sex, prop_primario) %>%
  drop_na()

correlacao_1b <- correlacao_rzs_ativ

write.csv2(
  dados_1b,
  file = file.path(resultados_dir, "item_1b_indicadores_municipais.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

options(OutDec = ".")
ggsave(
  filename = file.path(resultados_dir, "item_1b_dispersao.png"),
  plot = graf_rzs_ativ,
  width = 7,
  height = 4.5,
  dpi = 300
)
options(OutDec = outdec_original)

formata_decimal_tex <- function(x, digitos = 3) {
  sub(",", "{,}", formatC(x, format = "f", digits = digitos, decimal.mark = ","))
}

texto_1b <- paste0(
  "A correlação de Pearson entre a razão de sexo e a proporção de ocupados no setor primário, calculada para ",
  nrow(dados_1b),
  " municípios, foi $r = ", formata_decimal_tex(unname(correlacao_1b)),
  "$. O coeficiente positivo indica que municípios com razão de sexo mais alta tendem a apresentar maior proporção de ocupados no setor primário. ",
  "Assim, os resultados não sustentam que os municípios com menor razão de sexo sejam justamente aqueles mais baseados no setor primário. ",
  "A associação é municipal e descritiva e não demonstra causalidade."
)

writeLines(
  texto_1b,
  con = file.path(resultados_dir, "item_1b_resultado.tex"),
  useBytes = TRUE
)



## Quesito 2
## Taxa de desocupação: desocupados entre as pessoas na força de trabalho
cd_dompes2022_id_design <- update(
  cd_dompes2022_id_design,
  P0950 = case_when(
    trimws(as.character(P0950)) %in% c("Não ocupada", "Desocupada") ~ "Desocupada",
    trimws(as.character(P0950)) == "Ocupada" ~ "Ocupada",
    TRUE ~ NA_character_
  )
)

tx_desocup <- svyby(
  ~I(P0950 == "Desocupada"),
  ~NM_MUN,
  design = subset(cd_dompes2022_id_design, P0930 == 1),
  FUN = svymean,
  na.rm = TRUE,
  keep.var = FALSE
)
tx_desocup <- as.data.frame(tx_desocup)
names(tx_desocup) <- c("Munic", "taxa_ocupacao_na_forca", "txdesocup")

taxa_desocupacao_municipal <- tx_desocup %>%
  select(Munic, txdesocup) %>%
  mutate(tx_desocup_pct = 100 * txdesocup) %>%
  arrange(desc(txdesocup))

write.csv2(
  taxa_desocupacao_municipal,
  file = file.path(resultados_dir, "item_2a_taxa_desocupacao_municipios.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

top3_desocup <- taxa_desocupacao_municipal %>%
  slice_max(txdesocup, n = 3, with_ties = FALSE)
bottom3_desocup <- taxa_desocupacao_municipal %>%
  slice_min(txdesocup, n = 3, with_ties = FALSE) %>%
  arrange(txdesocup)

top3_bottom3_desocup <- bind_rows(
  top3_desocup %>% mutate(grupo = "Maiores taxas", posicao = row_number()),
  bottom3_desocup %>% mutate(grupo = "Menores taxas", posicao = row_number())
)
write.csv2(
  top3_bottom3_desocup,
  file = file.path(resultados_dir, "item_2a_top3_taxas_desocupacao.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

linhas_tabela_2a <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Três municípios com maiores e menores taxas de desocupação, Sergipe, 2022}",
  "\\label{tab:item2-taxa-desocupacao}",
  "\\small",
  "\\begin{tabular}{l r}",
  "\\toprule",
  "\\multicolumn{2}{l}{\\textbf{Maiores taxas}} \\\\",
  "Município & Taxa de desocupação (\\%) \\\\",
  "\\midrule",
  vapply(seq_len(nrow(top3_desocup)), function(i) {
    paste0(escapa_latex(top3_desocup$Munic[i]), " & ",
           formata_razao(top3_desocup$tx_desocup_pct[i]), " \\\\")
  }, character(1)),
  "\\midrule",
  "\\multicolumn{2}{l}{\\textbf{Menores taxas}} \\\\",
  "Município & Taxa de desocupação (\\%) \\\\",
  "\\midrule",
  vapply(seq_len(nrow(bottom3_desocup)), function(i) {
    paste0(escapa_latex(bottom3_desocup$Munic[i]), " & ",
           formata_razao(bottom3_desocup$tx_desocup_pct[i]), " \\\\")
  }, character(1)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\begingroup",
  "\\fonte{Elaboração própria com base nos microdados do Censo Demográfico 2022.}",
  "\\nota{Taxa calculada como pessoas desocupadas entre as pessoas de 14 anos ou mais na força de trabalho (P0930 = 1).}\\par",
  "\\endgroup",
  "\\end{table}"
)
writeLines(
  linhas_tabela_2a,
  con = file.path(resultados_dir, "item_2a_top3_taxas_desocupacao.tex"),
  useBytes = TRUE
)

maior_tx_desocup <- taxa_desocupacao_municipal %>% slice_max(txdesocup, n = 1)
menor_tx_desocup <- taxa_desocupacao_municipal %>% slice_min(txdesocup, n = 1)
tx_desocup_estado <- svymean(
  ~I(P0950 == "Desocupada"),
  design = subset(cd_dompes2022_id_design, P0930 == 1),
  na.rm = TRUE
)
taxa_desocup_estado <- unname(coef(tx_desocup_estado)[2])
texto_2a <- paste0(
  "A Tabela~\\ref{tab:item2-taxa-desocupacao} destaca os três municípios com maiores e menores taxas. No conjunto dos municípios, a taxa de desocupação variou de ",
  formata_razao(100 * menor_tx_desocup$txdesocup),
  "\\% em ", escapa_latex(menor_tx_desocup$Munic), " a ",
  formata_razao(100 * maior_tx_desocup$txdesocup), "\\% em ",
  escapa_latex(maior_tx_desocup$Munic),
  ". Para Sergipe, a estimativa estadual foi de ",
  formata_razao(100 * taxa_desocup_estado), "\\%."
)
writeLines(
  texto_2a,
  con = file.path(resultados_dir, "item_2a_resultado.tex"),
  useBytes = TRUE
)

## Nível de ocupação: ocupados como proporção da população de 14 anos ou mais
tx_ocupacao_14mais <- svyby(
  ~I(P0960 == 1),
  ~NM_MUN,
  design = subset(cd_dompes2022_id_design, P0920 == 1),
  FUN = svymean,
  na.rm = TRUE,
  keep.var = FALSE
)
names(tx_ocupacao_14mais) <- c("Munic", "nao_ocupados_14mais", "tx_ocupacao_14mais")

tx_ocupacao_setor <- merge(
  x = tx_ocupacao_14mais %>% select(Munic, tx_ocupacao_14mais),
  y = ativ_princ %>% select(Munic, prop_primario),
  by = "Munic",
  all = FALSE
) %>%
  arrange(Munic)

dados_2b <- tx_ocupacao_setor %>% drop_na()
correlacao_2b <- cor(
  dados_2b$tx_ocupacao_14mais,
  dados_2b$prop_primario,
  method = "pearson",
  use = "complete.obs"
)
write.csv2(
  dados_2b,
  file = file.path(resultados_dir, "item_2b_ocupacao_setor_primario.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

outdec_grafico_2b <- getOption("OutDec")
options(OutDec = ".")
graf_txocup_primario <- ggscatter(
  dados_2b,
  x = "tx_ocupacao_14mais",
  y = "prop_primario",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = TRUE,
  cor.method = "pearson",
  cor.coeff.args = list(
    cor.coef.name = "r",
    r.digits = 3,
    p.accuracy = 0.001,
    p.decimal.mark = "."
  )
) +
  labs(
    x = "Taxa de ocupação (ocupados entre pessoas de 14 anos ou mais)",
    y = "Proporção de ocupados no setor primário"
  ) +
  scale_x_continuous(labels = scales::label_percent(accuracy = 1)) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 1)) +
  theme_minimal()
ggsave(
  filename = file.path(resultados_dir, "item_2b_ocupacao_setor_primario.png"),
  plot = graf_txocup_primario,
  width = 7,
  height = 4.5,
  dpi = 300
)
options(OutDec = outdec_grafico_2b)

texto_2b <- paste0(
  "A correlação de Pearson entre a taxa de ocupação da população de 14 anos ou mais e a proporção de ocupados no setor primário, calculada para ",
  nrow(dados_2b),
  " municípios, foi $r = ", formata_decimal_tex(unname(correlacao_2b)),
  "$. A associação observada é negativa e de magnitude modesta: municípios com maior taxa de ocupação tendem a apresentar menor participação relativa do setor primário entre os ocupados. Essa relação municipal é descritiva e não demonstra causalidade."
)
writeLines(
  texto_2b,
  con = file.path(resultados_dir, "item_2b_resultado.tex"),
  useBytes = TRUE
)


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

tx_analf <- as.data.frame(tx_analf)
names(tx_analf) <- c("Munic", "prop_alfabetizada", "tx_analf")
taxa_analf_municipal <- tx_analf %>%
  select(Munic, tx_analf) %>%
  mutate(tx_analf_pct = 100 * tx_analf) %>%
  arrange(desc(tx_analf))

write.csv2(
  taxa_analf_municipal,
  file = file.path(resultados_dir, "item_3_taxa_analfabetismo_municipios.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

top3_analf <- taxa_analf_municipal %>%
  slice_max(tx_analf, n = 3, with_ties = FALSE)
bottom3_analf <- taxa_analf_municipal %>%
  slice_min(tx_analf, n = 3, with_ties = FALSE) %>%
  arrange(tx_analf)

tx_analf_estado <- svymean(
  ~I(P0640 == "Não"),
  design = subset(cd_dompes2022_id_design, P0181 >= 15 & P0181 <= 64),
  na.rm = TRUE
)
taxa_analf_estado <- unname(coef(tx_analf_estado)[2])

resumo_3 <- bind_rows(
  top3_analf %>% mutate(grupo = "Maiores taxas", posicao = row_number()),
  bottom3_analf %>% mutate(grupo = "Menores taxas", posicao = row_number()),
  tibble(
    Munic = "Sergipe",
    tx_analf = taxa_analf_estado,
    tx_analf_pct = 100 * taxa_analf_estado,
    grupo = "Estimativa estadual",
    posicao = NA_integer_
  )
)

write.csv2(
  resumo_3,
  file = file.path(resultados_dir, "item_3_top3_taxa_analfabetismo.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

linhas_tabela_3 <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Municípios com maiores e menores taxas de analfabetismo e estimativa estadual, população de 15 a 64 anos, Sergipe, 2022}",
  "\\label{tab:item3-analfabetismo}",
  "\\small",
  "\\begin{tabular}{l r}",
  "\\toprule",
  "\\multicolumn{2}{l}{\\textbf{Maiores taxas municipais}} \\\\",
  "Município & Taxa de analfabetismo (\\%) \\\\",
  "\\midrule",
  vapply(seq_len(nrow(top3_analf)), function(i) {
    paste0(escapa_latex(top3_analf$Munic[i]), " & ",
           formata_razao(top3_analf$tx_analf_pct[i]), " \\\\")
  }, character(1)),
  "\\midrule",
  "\\multicolumn{2}{l}{\\textbf{Menores taxas municipais}} \\\\",
  "Município & Taxa de analfabetismo (\\%) \\\\",
  "\\midrule",
  vapply(seq_len(nrow(bottom3_analf)), function(i) {
    paste0(escapa_latex(bottom3_analf$Munic[i]), " & ",
           formata_razao(bottom3_analf$tx_analf_pct[i]), " \\\\")
  }, character(1)),
  "\\midrule",
  "\\textbf{Estimativa estadual (Sergipe)} & ",
  formata_razao(100 * taxa_analf_estado), " \\\\",
  "\\bottomrule",
  "\\end{tabular}",
  "\\begingroup",
  "\\fonte{Elaboração própria com base nos microdados do Censo Demográfico 2022.}",
  "\\nota{Pessoas de 15 a 64 anos; estimativas municipais e estadual ponderadas. A estimativa estadual foi calculada diretamente para Sergipe, não pela média simples das taxas municipais.}\\par",
  "\\endgroup",
  "\\end{table}"
)
writeLines(
  linhas_tabela_3,
  con = file.path(resultados_dir, "item_3_top3_taxa_analfabetismo.tex"),
  useBytes = TRUE
)

texto_3 <- paste0(
  "A taxa estadual de analfabetismo entre as pessoas de 15 a 64 anos foi ",
  formata_razao(100 * taxa_analf_estado),
  "\\%. A taxa municipal mais alta foi registrada em ",
  escapa_latex(top3_analf$Munic[1]), " (", formata_razao(top3_analf$tx_analf_pct[1]),
  "\\%); a mais baixa, em ", escapa_latex(bottom3_analf$Munic[1]), " (",
  formata_razao(bottom3_analf$tx_analf_pct[1]), "\\%)."
)
writeLines(
  texto_3,
  con = file.path(resultados_dir, "item_3_resultado.tex"),
  useBytes = TRUE
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

# Exporta os resultados do item 4 para uso no relatório.
write.csv2(
  renda_fonte,
  file = file.path(resultados_dir, "item_4_renda_fonte_municipios.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

top3_renda_fonte <- renda_fonte %>% arrange(desc(total)) %>% slice_head(n = 3)
bottom3_renda_fonte <- renda_fonte %>% arrange(total) %>% slice_head(n = 3) %>% arrange(desc(total))
resumo_4 <- bind_rows(top3_renda_fonte, bottom3_renda_fonte)
write.csv2(
  resumo_4,
  file = file.path(resultados_dir, "item_4_top3_bottom3_renda_fonte.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

linhas_tabela_4 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Médias de rendimento por fonte nos municípios com maiores e menores rendimentos totais, Sergipe, 2022}",
  "\\label{tab:item4-renda-fontes}",
  "\\scriptsize",
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  "Município & Trabalho (R\\$) & Transferências (R\\$) & Total (R\\$) & Trabalho (\\%) & Transferências (\\%) \\\\",
  "\\midrule",
  "\\multicolumn{6}{l}{\\textbf{Maiores médias de rendimento total}} \\\\",
  vapply(seq_len(nrow(top3_renda_fonte)), function(i) {
    x <- top3_renda_fonte[i, ]
    paste0(escapa_latex(x$Munic), " & ", formata_razao(x$trab), " & ",
           formata_razao(x$transf), " & ", formata_razao(x$total), " & ",
           formata_razao(x$prop_trab), " & ", formata_razao(x$prop_transf), " \\\\")
  }, character(1)),
  "\\midrule",
  "\\multicolumn{6}{l}{\\textbf{Menores médias de rendimento total}} \\\\",
  vapply(seq_len(nrow(bottom3_renda_fonte)), function(i) {
    x <- bottom3_renda_fonte[i, ]
    paste0(escapa_latex(x$Munic), " & ", formata_razao(x$trab), " & ",
           formata_razao(x$transf), " & ", formata_razao(x$total), " & ",
           formata_razao(x$prop_trab), " & ", formata_razao(x$prop_transf), " \\\\")
  }, character(1)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\fonte{Elaboração própria com base nos microdados do Censo Demográfico 2022.}",
  "\\nota{As médias e participações são as estimadas pelo código para os registros válidos conjuntamente nas três variáveis de rendimento.}",
  "\\end{table}"
)
writeLines(linhas_tabela_4, file.path(resultados_dir, "item_4_renda_fonte_top_bottom.tex"), useBytes = TRUE)

cor_transf_total <- cor(renda_fonte$transf, renda_fonte$total, use = "complete.obs", method = "pearson")
writeLines(
  paste0("A correlação de Pearson entre o rendimento médio de transferências e o rendimento médio total entre os municípios foi ",
         formata_razao(cor_transf_total), ". Essa associação é positiva, mas fraca: municípios com maior rendimento médio de transferências tendem a apresentar rendimento médio total um pouco maior, embora haja bastante variação entre eles. A correlação descreve uma associação municipal e, por si só, não demonstra que as transferências causam o aumento do rendimento total nem que municípios com maior renda dependam mais delas."),
  file.path(resultados_dir, "item_4_resultado.tex"),
  useBytes = TRUE
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
  cor.coeff.args = list(
    cor.coef.name = "r",
    r.digits = 3,
    p.accuracy = 0.001,
    p.decimal.mark = "."
  ),
  na.rm = T
)
outfontes_renda <- outfontes_renda +
  labs(x = "Rendimento médio de outras fontes (R$)", y = "Rendimento médio total (R$)") +
  theme_minimal()
outdec_grafico_4 <- getOption("OutDec")
options(OutDec = ".")
ggsave(
  filename = file.path(resultados_dir, "item_4b_renda_transferencias_total.png"),
  plot = outfontes_renda, width = 8, height = 5, dpi = 300
)
options(OutDec = outdec_grafico_4)

### Quesito 5: participação de outras fontes e nível de rendimento
participacao_outras <- renda_fonte %>%
  arrange(desc(prop_transf))
top3_outras <- participacao_outras %>% slice_head(n = 3)

write.csv2(
  participacao_outras,
  file = file.path(resultados_dir, "item_5_participacao_outras_fontes_municipios.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)
write.csv2(
  top3_outras,
  file = file.path(resultados_dir, "item_5_top3_participacao_outras_fontes.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

linhas_tabela_5 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Municípios com maior participação de rendimentos de outras fontes, Sergipe, 2022}",
  "\\label{tab:item5-outras-fontes}",
  "\\small",
  "\\begin{tabular}{lrrr}",
  "\\toprule",
  "Município & Rendimento total médio (R\\$) & Outras fontes médias (R\\$) & Participação (\\%) \\\\",
  "\\midrule",
  vapply(seq_len(nrow(top3_outras)), function(i) {
    x <- top3_outras[i, ]
    paste0(escapa_latex(x$Munic), " & ", formata_razao(x$total), " & ",
           formata_razao(x$transf), " & ", formata_razao(x$prop_transf), " \\\\")
  }, character(1)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\fonte{Elaboração própria com base nos microdados do Censo Demográfico 2022.}",
  "\\nota{Participação calculada como a razão entre P1100 e P1110, conforme o procedimento do item 4.}",
  "\\end{table}"
)
writeLines(linhas_tabela_5, file.path(resultados_dir, "item_5_top3_outras_fontes.tex"), useBytes = TRUE)

cor_total_participacao <- cor.test(
  renda_fonte$total, renda_fonte$prop_transf, method = "pearson"
)
cor_5 <- unname(cor_total_participacao$estimate)
p_5 <- cor_total_participacao$p.value
texto_5 <- paste0(
  "Os maiores percentuais de rendimentos de outras fontes foram observados em ",
  paste(escapa_latex(top3_outras$Munic), collapse = ", "),
  ", conforme a Tabela~\\ref{tab:item5-outras-fontes}. A correlação de Pearson entre o rendimento total médio e a participação dessas fontes, apresentada na Figura~\\ref{fig:item5-total-participacao}, foi $r = ",
  formata_decimal_tex(cor_5), "$",
  if (p_5 < 0.001) " ($p < 0{,}001$)" else paste0(" ($p = ", formata_decimal_tex(p_5), "$)"),
  ". A associação negativa indica que municípios com maior rendimento total tendem a apresentar menor participação relativa de outras fontes; portanto, essa participação é relativamente mais elevada em municípios de menor rendimento. Isso não demonstra, por si só, dependência da economia municipal em relação ao governo: P1100 reúne outras fontes de renda sem permitir separar aposentadorias, pensões, PBF ou demais componentes, e a correlação municipal não estabelece causalidade."
)
writeLines(texto_5, file.path(resultados_dir, "item_5_resultado.tex"), useBytes = TRUE)

graf_item5 <- ggscatter(
  renda_fonte,
  x = "total",
  y = "prop_transf",
  add = "reg.line",
  conf.int = TRUE,
  cor.coef = FALSE,
  na.rm = TRUE
) +
  labs(
    x = "Rendimento médio total (R$)",
    y = "Participação de outras fontes no rendimento total"
  ) +
  scale_y_continuous(labels = scales::label_number(suffix = "%", accuracy = 1, decimal.mark = ",")) +
  theme_minimal()

outdec_grafico_5 <- getOption("OutDec")
options(OutDec = ".")
ggsave(
  filename = file.path(resultados_dir, "item_5_total_participacao_outras_fontes.png"),
  plot = graf_item5, width = 8, height = 5, dpi = 300
)
options(OutDec = outdec_grafico_5)

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

# Proporção municipal e estadual de domicílios com responsável mulher.
names(chefe_dom_fem) <- c("Munic", "prop_homens", "prop_mulheres")
chefe_dom_fem <- chefe_dom_fem %>%
  mutate(
    prop_homens = 100 * prop_homens,
    prop_mulheres = 100 * prop_mulheres
  )

write.csv2(
  chefe_dom_fem,
  file = file.path(resultados_dir, "item_2_1_dom_chefe_mulher_municipios.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

top3_chefe_mulher <- chefe_dom_fem %>% arrange(desc(prop_mulheres)) %>% slice_head(n = 3)
bottom3_chefe_mulher <- chefe_dom_fem %>% arrange(prop_mulheres) %>% slice_head(n = 3) %>% arrange(desc(prop_mulheres))
resumo_chefe_mulher <- bind_rows(top3_chefe_mulher, bottom3_chefe_mulher)
write.csv2(
  resumo_chefe_mulher,
  file = file.path(resultados_dir, "item_2_1_top3_bottom3_dom_chefe_mulher.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

estimativa_estadual_chefe_mulher <- svymean(
  ~I(D0171 == 2),
  subset(cd_dompes2022_id_design, P0170 == 1),
  na.rm = TRUE
)
prop_estadual_chefe_mulher <- 100 * unname(coef(estimativa_estadual_chefe_mulher)[2])

dados_assoc_chefe_mulher <- inner_join(
  rz_sex_munic %>% select(Munic, rz_sex),
  chefe_dom_fem %>% select(Munic, prop_mulheres),
  by = "Munic"
)
teste_assoc_chefe_mulher <- cor.test(
  dados_assoc_chefe_mulher$rz_sex,
  dados_assoc_chefe_mulher$prop_mulheres,
  method = "pearson"
)
cor_assoc_chefe_mulher <- unname(teste_assoc_chefe_mulher$estimate)
p_assoc_chefe_mulher <- teste_assoc_chefe_mulher$p.value

linhas_tabela_2_1 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Proporção de domicílios chefiados por mulher nos municípios com maiores e menores percentuais, Sergipe, 2022}",
  "\\label{tab:item2-1-chefe-mulher}",
  "\\small",
  "\\begin{tabular}{lr}",
  "\\toprule",
  "Município & Domicílios chefiados por mulher (\\%) \\\\",
  "\\midrule",
  "\\multicolumn{2}{l}{\\textbf{Maiores proporções}} \\\\",
  vapply(seq_len(nrow(top3_chefe_mulher)), function(i) {
    paste0(escapa_latex(top3_chefe_mulher$Munic[i]), " & ",
           formata_razao(top3_chefe_mulher$prop_mulheres[i]), " \\\\")
  }, character(1)),
  "\\midrule",
  "\\multicolumn{2}{l}{\\textbf{Menores proporções}} \\\\",
  vapply(seq_len(nrow(bottom3_chefe_mulher)), function(i) {
    paste0(escapa_latex(bottom3_chefe_mulher$Munic[i]), " & ",
           formata_razao(bottom3_chefe_mulher$prop_mulheres[i]), " \\\\")
  }, character(1)),
  "\\midrule",
  paste0("Sergipe & ", formata_razao(prop_estadual_chefe_mulher), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\fonte{Elaboração própria com base nos microdados do Censo Demográfico 2022.}",
  "\\nota{Estimativas ponderadas; domicílios identificados pela condição P0170 = 1 e responsáveis mulheres por D0171 = 2.}",
  "\\end{table}"
)
writeLines(linhas_tabela_2_1, file.path(resultados_dir, "item_2_1_dom_chefe_mulher.tex"), useBytes = TRUE)

texto_2_1 <- paste0(
  "Em Sergipe, ", formata_razao(prop_estadual_chefe_mulher),
  "\\% dos domicílios tinham uma mulher como responsável. A Tabela~\\ref{tab:item2-1-chefe-mulher} apresenta os três municípios com maiores e menores proporções, ordenados do maior para o menor dentro de cada grupo. A estimativa estadual foi calculada diretamente para o conjunto de domicílios do estado. No nível municipal, a razão de sexo apresentou associação negativa moderada com a proporção de domicílios chefiados por mulheres ($r = ",
  formata_decimal_tex(cor_assoc_chefe_mulher),
  if (p_assoc_chefe_mulher < 0.001) "; p < 0{,}001$). " else paste0("; p = ", formata_decimal_tex(p_assoc_chefe_mulher), "$). "),
  "Assim, municípios com mais homens para cada 100 mulheres tendem a apresentar menor proporção de domicílios com responsável mulher. Essa associação municipal não estabelece causalidade nem descreve necessariamente relações entre indivíduos."
)
writeLines(texto_2_1, file.path(resultados_dir, "item_2_1_resultado.tex"), useBytes = TRUE)

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

# Exporta a renda domiciliar per capita por sexo da pessoa responsável.
write.csv2(
  renda_chefe_dom,
  file = file.path(resultados_dir, "item_2_2_rdpc_por_sexo_municipios.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

top3_rdpc_feminino <- renda_chefe_dom %>% arrange(desc(Feminino)) %>% slice_head(n = 3)
bottom3_rdpc_feminino <- renda_chefe_dom %>% arrange(Feminino) %>% slice_head(n = 3) %>% arrange(desc(Feminino))
resumo_rdpc_sexo <- bind_rows(top3_rdpc_feminino, bottom3_rdpc_feminino)
write.csv2(
  resumo_rdpc_sexo,
  file = file.path(resultados_dir, "item_2_2_top3_bottom3_rdpc_por_sexo.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

rdpc_estadual_sexo <- svyby(
  ~D0360,
  ~P0160,
  subset(cd_dompes2022_id_design, P0170 == 1),
  FUN = svymean,
  na.rm = TRUE,
  keep.var = FALSE
) %>%
  transmute(sexo = as.character(P0160), rdpc = statistic)
rdpc_estadual_feminino <- rdpc_estadual_sexo$rdpc[rdpc_estadual_sexo$sexo == "Feminino"]
rdpc_estadual_masculino <- rdpc_estadual_sexo$rdpc[rdpc_estadual_sexo$sexo == "Masculino"]
diferenca_rdpc_estadual <- rdpc_estadual_masculino - rdpc_estadual_feminino
percentual_diferenca_rdpc_estadual <- 100 * diferenca_rdpc_estadual / rdpc_estadual_feminino

linhas_tabela_2_2 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Renda domiciliar per capita por sexo da pessoa responsável nos municípios com maiores e menores médias femininas, Sergipe, 2022}",
  "\\label{tab:item2-2-rdpc-sexo}",
  "\\small",
  "\\begin{tabular}{lrr}",
  "\\toprule",
  "Município & Responsável mulher (R\\$) & Responsável homem (R\\$) \\\\",
  "\\midrule",
  "\\multicolumn{3}{l}{\\textbf{Maiores médias entre domicílios com responsável mulher}} \\\\",
  vapply(seq_len(nrow(top3_rdpc_feminino)), function(i) {
    x <- top3_rdpc_feminino[i, ]
    paste0(escapa_latex(x$Munic), " & ", formata_razao(x$Feminino), " & ",
           formata_razao(x$Masculino), " \\\\")
  }, character(1)),
  "\\midrule",
  "\\multicolumn{3}{l}{\\textbf{Menores médias entre domicílios com responsável mulher}} \\\\",
  vapply(seq_len(nrow(bottom3_rdpc_feminino)), function(i) {
    x <- bottom3_rdpc_feminino[i, ]
    paste0(escapa_latex(x$Munic), " & ", formata_razao(x$Feminino), " & ",
           formata_razao(x$Masculino), " \\\\")
  }, character(1)),
  "\\midrule",
  paste0("Sergipe & ", formata_razao(rdpc_estadual_feminino), " & ",
         formata_razao(rdpc_estadual_masculino), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\fonte{Elaboração própria com base nos microdados do Censo Demográfico 2022.}",
  "\\nota{Estimativas ponderadas de renda domiciliar per capita (D0360), segundo o sexo da pessoa responsável (P0160).}",
  "\\end{table}"
)
writeLines(linhas_tabela_2_2, file.path(resultados_dir, "item_2_2_rdpc_por_sexo.tex"), useBytes = TRUE)

texto_2_2 <- paste0(
  "Em Sergipe, a renda domiciliar per capita média foi de R\\$ ",
  formata_razao(rdpc_estadual_feminino),
  " nos domicílios com responsável mulher e R\\$ ",
  formata_razao(rdpc_estadual_masculino),
  " naqueles com responsável homem. A diferença é de R\\$ ",
  formata_razao(diferenca_rdpc_estadual),
  ", evidenciando desigualdade de gênero na renda: a média dos domicílios com responsável homem é ",
  formata_razao(percentual_diferenca_rdpc_estadual),
  "\\% maior. A Tabela~\\ref{tab:item2-2-rdpc-sexo} apresenta, para os três municípios com maiores e menores médias de renda no grupo com responsável mulher, as médias dos dois grupos. A Figura~\\ref{fig:item2-2-rdpc-seis} compara esses mesmos municípios e marca as médias estaduais."
)
writeLines(texto_2_2, file.path(resultados_dir, "item_2_2_resultado.tex"), useBytes = TRUE)

dados_grafico_2_2 <- resumo_rdpc_sexo %>%
  mutate(Munic = factor(Munic, levels = rev(Munic))) %>%
  select(Munic, Feminino, Masculino) %>%
  pivot_longer(
    cols = c(Feminino, Masculino),
    names_to = "sexo_responsavel",
    values_to = "rdpc"
  ) %>%
  mutate(
    sexo_responsavel = recode(
      sexo_responsavel,
      Feminino = "Responsável mulher",
      Masculino = "Responsável homem"
    )
  )

linhas_estaduais_2_2 <- tibble(
  sexo_responsavel = c("Responsável mulher", "Responsável homem"),
  rdpc = c(rdpc_estadual_feminino, rdpc_estadual_masculino)
)

grafico_2_2 <- ggplot(
  dados_grafico_2_2,
  aes(x = rdpc, y = Munic, color = sexo_responsavel, group = Munic)
) +
  geom_vline(
    data = linhas_estaduais_2_2,
    aes(xintercept = rdpc, color = sexo_responsavel),
    linetype = "dashed",
    linewidth = 0.7,
    alpha = 0.8,
    inherit.aes = FALSE,
    show.legend = FALSE
  ) +
  geom_line(color = "grey65", linewidth = 0.7) +
  geom_point(size = 2.8) +
  scale_color_manual(
    values = c("Responsável mulher" = "#2166AC", "Responsável homem" = "#B35806")
  ) +
  scale_x_continuous(
    labels = scales::label_number(prefix = "R$ ", big.mark = ".", decimal.mark = ",")
  ) +
  labs(
    x = "Renda domiciliar per capita média",
    y = NULL,
    color = "Sexo da pessoa responsável"
  ) +
  theme_minimal() +
  theme(legend.position = "bottom")

ggsave(
  filename = file.path(resultados_dir, "item_2_2_rdpc_seis_municipios.png"),
  plot = grafico_2_2,
  width = 8,
  height = 5,
  dpi = 300
)

### Quesito 3
## Top 10 de pior renda de chefe feminino

pior10_rdpc_feminino <- renda_chefe_dom %>%
  arrange(Feminino) %>%
  slice_head(n = 10) %>%
  arrange(desc(Feminino)) %>%
  mutate(posicao = row_number()) %>%
  select(posicao, Munic, Feminino)

write.csv2(
  pior10_rdpc_feminino,
  file = file.path(resultados_dir, "item_2_3_dez_menores_rdpc_chefe_mulher.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

linhas_tabela_2_3 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Dez municípios com menores rendas domiciliares per capita em domicílios com responsável mulher, Sergipe, 2022}",
  "\\label{tab:item2-3-menor-rdpc-mulher}",
  "\\small",
  "\\begin{tabular}{rlr}",
  "\\toprule",
  "Posição & Município & Renda domiciliar per capita (R\\$) \\\\",
  "\\midrule",
  vapply(seq_len(nrow(pior10_rdpc_feminino)), function(i) {
    x <- pior10_rdpc_feminino[i, ]
    paste0(x$posicao, " & ", escapa_latex(x$Munic), " & ",
           formata_razao(x$Feminino), " \\\\")
  }, character(1)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\fonte{Elaboração própria com base nos microdados do Censo Demográfico 2022.}",
  "\\nota{A seleção considera os dez menores valores; a tabela os apresenta em ordem decrescente, conforme a orientação do exercício.}",
  "\\end{table}"
)
writeLines(linhas_tabela_2_3, file.path(resultados_dir, "item_2_3_dez_menores_rdpc_chefe_mulher.tex"), useBytes = TRUE)

texto_2_3 <- paste0(
  "A Tabela~\\ref{tab:item2-3-menor-rdpc-mulher} apresenta os dez municípios com menores médias de renda domiciliar per capita nos domicílios com responsável mulher, ordenados do maior para o menor entre os dez selecionados. Os valores variam de R\\$ ",
  formata_razao(max(pior10_rdpc_feminino$Feminino)),
  " em ", escapa_latex(pior10_rdpc_feminino$Munic[1]),
  " a R\\$ ", formata_razao(min(pior10_rdpc_feminino$Feminino)),
  " em ", escapa_latex(pior10_rdpc_feminino$Munic[which.min(pior10_rdpc_feminino$Feminino)]),
  ". Todos os dez municípios ficam abaixo da média estadual de R\\$ ",
  formata_razao(rdpc_estadual_feminino), "."
)
writeLines(texto_2_3, file.path(resultados_dir, "item_2_3_resultado.tex"), useBytes = TRUE)

### Quesito 4
## Dez maiores hiatos absolutos entre rendas domiciliares per capita
maiores_hiatos_rdpc <- renda_chefe_dom %>%
  mutate(hiato_rdpc = Masculino - Feminino) %>%
  arrange(desc(hiato_rdpc)) %>%
  slice_head(n = 10) %>%
  mutate(posicao = row_number()) %>%
  select(posicao, Munic, Feminino, Masculino, hiato_rdpc)

write.csv2(
  maiores_hiatos_rdpc,
  file = file.path(resultados_dir, "item_2_4_top10_maiores_hiatos_rdpc.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

linhas_tabela_2_4 <- c(
  "\\begin{table}[htbp]",
  "\\centering",
  "\\caption{Dez maiores hiatos absolutos de renda domiciliar per capita segundo sexo da pessoa responsável, Sergipe, 2022}",
  "\\label{tab:item2-4-maiores-hiatos-rdpc}",
  "\\small",
  "\\begin{tabular}{rlrrr}",
  "\\toprule",
  "Posição & Município & Resp. mulher (R\\$) & Resp. homem (R\\$) & Hiato (R\\$) \\\\",
  "\\midrule",
  vapply(seq_len(nrow(maiores_hiatos_rdpc)), function(i) {
    x <- maiores_hiatos_rdpc[i, ]
    paste0(x$posicao, " & ", escapa_latex(x$Munic), " & ",
           formata_razao(x$Feminino), " & ", formata_razao(x$Masculino), " & ",
           formata_razao(x$hiato_rdpc), " \\\\")
  }, character(1)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\fonte{Elaboração própria com base nos microdados do Censo Demográfico 2022.}",
  "\\nota{Hiato calculado como renda per capita média de domicílios com responsável homem menos a de domicílios com responsável mulher.}",
  "\\end{table}"
)
writeLines(linhas_tabela_2_4, file.path(resultados_dir, "item_2_4_top10_maiores_hiatos_rdpc.tex"), useBytes = TRUE)

texto_2_4 <- paste0(
  "O hiato foi calculado subtraindo a renda domiciliar per capita média dos domicílios com responsável mulher da média daqueles com responsável homem. A Tabela~\\ref{tab:item2-4-maiores-hiatos-rdpc} apresenta os dez municípios com maiores diferenças absolutas, em ordem decrescente. Entre eles, o maior hiato foi de R\\$ ",
  formata_razao(maiores_hiatos_rdpc$hiato_rdpc[1]),
  " em ", escapa_latex(maiores_hiatos_rdpc$Munic[1]),
  "."
)
writeLines(texto_2_4, file.path(resultados_dir, "item_2_4_resultado.tex"), useBytes = TRUE)


## Quesito 5
## Dez municípios com menores proporções de água encanada até dentro da moradia (D0300 == 1)

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
  mutate(
    sem = 100 * sem,
    com = 100 * com
  ) %>%
  arrange(com)

write.csv2(
  agua_canalizada,
  file = file.path(resultados_dir, "item_2_5_agua_encanada_municipios.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

dez_menores_agua_encanada <- agua_canalizada %>%
  slice_head(n = 10) %>%
  mutate(posicao = row_number()) %>%
  select(posicao, Munic, com)
write.csv2(
  dez_menores_agua_encanada,
  file = file.path(resultados_dir, "item_2_5_dez_menores_agua_encanada.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

estimativa_estadual_agua <- svymean(
  ~I(D0300 == 1),
  subset(cd_dompes2022_id_design, P0170 == 1),
  na.rm = TRUE
)
prop_estadual_agua_encanada <- 100 * unname(coef(estimativa_estadual_agua)[2])

linhas_tabela_2_5 <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Dez municípios com menores proporções de domicílios com água encanada até dentro da moradia, Sergipe, 2022}",
  "\\label{tab:item2-5-agua-encanada}",
  "\\small",
  "\\begin{tabular}{rlr}",
  "\\toprule",
  "Posição & Município & Água encanada dentro da moradia (\\%) \\\\",
  "\\midrule",
  vapply(seq_len(nrow(dez_menores_agua_encanada)), function(i) {
    x <- dez_menores_agua_encanada[i, ]
    paste0(x$posicao, " & ", escapa_latex(x$Munic), " & ",
           formata_razao(x$com), " \\\\")
  }, character(1)),
  "\\midrule",
  paste0("\\multicolumn{2}{l}{Sergipe} & ", formata_razao(prop_estadual_agua_encanada), "\\% \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\fonte{Elaboração própria com base nos microdados do Censo Demográfico 2022.}",
  "\\nota{Domicílio contado como atendido quando D0300 = 1, isto é, a água chega encanada até dentro da moradia.}",
  "\\end{table}"
)
writeLines(linhas_tabela_2_5, file.path(resultados_dir, "item_2_5_dez_menores_agua_encanada.tex"), useBytes = TRUE)

texto_2_5 <- paste0(
  "Em Sergipe, ", formata_razao(prop_estadual_agua_encanada),
  "\\% dos domicílios tinham água encanada até dentro da moradia. A Tabela~\\ref{tab:item2-5-agua-encanada} apresenta os dez municípios com menores proporções, do menor para o maior. O menor percentual foi de ",
  formata_razao(dez_menores_agua_encanada$com[1]),
  "\\% em ", escapa_latex(dez_menores_agua_encanada$Munic[1]),
  "."
)
writeLines(texto_2_5, file.path(resultados_dir, "item_2_5_resultado.tex"), useBytes = TRUE)

## Proporção municipal de domicílios com esgotamento sanitário inadequado (D0250 > 3)
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
  mutate(
    adequado = 100 * adequado,
    inadequado = 100 * inadequado
  ) %>%
  arrange(desc(inadequado))

write.csv2(
  esgoto,
  file = file.path(resultados_dir, "item_2_6_esgotamento_inadequado_municipios.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

top3_esgoto_inadequado <- esgoto %>% arrange(desc(inadequado)) %>% slice_head(n = 3)
bottom3_esgoto_inadequado <- esgoto %>% arrange(inadequado) %>% slice_head(n = 3) %>% arrange(desc(inadequado))
resumo_esgoto_inadequado <- bind_rows(top3_esgoto_inadequado, bottom3_esgoto_inadequado)
write.csv2(
  resumo_esgoto_inadequado,
  file = file.path(resultados_dir, "item_2_6_top3_bottom3_esgotamento_inadequado.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

estimativa_estadual_esgoto <- svymean(
  ~I(D0250 > 3),
  subset(cd_dompes2022_id_design, P0170 == 1),
  na.rm = TRUE
)
prop_estadual_esgoto_inadequado <- 100 * unname(coef(estimativa_estadual_esgoto)[2])

linhas_tabela_2_6 <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Proporção de domicílios com esgotamento sanitário inadequado nos municípios com maiores e menores percentuais, Sergipe, 2022}",
  "\\label{tab:item2-6-esgoto-inadequado}",
  "\\small",
  "\\begin{tabular}{lr}",
  "\\toprule",
  "Município & Esgotamento inadequado (\\%) \\\\",
  "\\midrule",
  "\\multicolumn{2}{l}{\\textbf{Maiores proporções}} \\\\",
  vapply(seq_len(nrow(top3_esgoto_inadequado)), function(i) {
    x <- top3_esgoto_inadequado[i, ]
    paste0(escapa_latex(x$Munic), " & ", formata_razao(x$inadequado), " \\\\")
  }, character(1)),
  "\\midrule",
  "\\multicolumn{2}{l}{\\textbf{Menores proporções}} \\\\",
  vapply(seq_len(nrow(bottom3_esgoto_inadequado)), function(i) {
    x <- bottom3_esgoto_inadequado[i, ]
    paste0(escapa_latex(x$Munic), " & ", formata_razao(x$inadequado), " \\\\")
  }, character(1)),
  "\\midrule",
  paste0("Sergipe & ", formata_razao(prop_estadual_esgoto_inadequado), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\fonte{Elaboração própria com base nos microdados do Censo Demográfico 2022.}",
  "\\nota{Proporção estimada com pesos amostrais para domicílios com P0170 = 1. D0250 registra o destino do esgoto do banheiro; o valor 3 corresponde a fossa séptica ou fossa-filtro não ligada à rede. O critério do exercício classifica como inadequados os valores superiores a 3.}",
  "\\end{table}"
)
writeLines(linhas_tabela_2_6, file.path(resultados_dir, "item_2_6_esgotamento_inadequado.tex"), useBytes = TRUE)

texto_2_6 <- paste0(
  "Em Sergipe, ", formata_razao(prop_estadual_esgoto_inadequado),
  "\\% dos domicílios foram classificados com esgotamento sanitário inadequado. A variável D0250 registra o destino do esgoto do banheiro; o valor 3 corresponde a fossa séptica ou fossa-filtro não ligada à rede. Neste exercício, foram considerados inadequados os domicílios cujo valor de D0250 é superior a 3 \\cite{ibgedicmicrodados2022}. A Tabela~\\ref{tab:item2-6-esgoto-inadequado} apresenta os três municípios com maiores e menores proporções; em cada grupo, os valores estão ordenados do maior para o menor. A maior proporção foi de ",
  formata_razao(top3_esgoto_inadequado$inadequado[1]),
  "\\% em ", escapa_latex(top3_esgoto_inadequado$Munic[1]),
  ". O CSV em resultados reúne as estimativas dos 75 municípios."
)
writeLines(texto_2_6, file.path(resultados_dir, "item_2_6_resultado.tex"), useBytes = TRUE)


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
  mutate(
    `nao tem` = 100 * `nao tem`,
    tem = 100 * tem
  ) %>%
  arrange(desc(`nao tem`))

## Item 2.7: compara cobertura de internet e esgotamento sanitário adequado
comparacao_internet_esgoto <- inner_join(
  esgoto %>% select(Munic, esgoto_adequado = adequado),
  internet %>% select(Munic, internet = tem),
  by = "Munic"
) %>%
  mutate(
    diferenca_pp = internet - esgoto_adequado,
    internet_supera_esgoto = diferenca_pp > 0
  )

write.csv2(
  comparacao_internet_esgoto,
  file = file.path(resultados_dir, "item_2_7_internet_esgoto_municipios.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

municipios_internet_supera <- comparacao_internet_esgoto %>%
  filter(internet_supera_esgoto) %>%
  arrange(desc(diferenca_pp))
maiores_diferencas_2_7 <- municipios_internet_supera %>% slice_head(n = 3)
menores_diferencas_positivas_2_7 <- municipios_internet_supera %>%
  arrange(diferenca_pp) %>%
  slice_head(n = 3) %>%
  arrange(desc(diferenca_pp))
resumo_2_7 <- bind_rows(maiores_diferencas_2_7, menores_diferencas_positivas_2_7)
write.csv2(
  resumo_2_7,
  file = file.path(resultados_dir, "item_2_7_resumo_diferencas.csv"),
  row.names = FALSE,
  fileEncoding = "UTF-8"
)

estimativa_estadual_internet <- svymean(
  ~I(D0330 == 1),
  subset(cd_dompes2022_id_design, P0170 == 1),
  na.rm = TRUE
)
prop_estadual_internet <- 100 * unname(coef(estimativa_estadual_internet)[2])
prop_estadual_esgoto_adequado <- 100 - prop_estadual_esgoto_inadequado
diferenca_estadual_internet_esgoto <- prop_estadual_internet - prop_estadual_esgoto_adequado

linhas_tabela_2_7 <- c(
  "\\begin{table}[H]",
  "\\centering",
  "\\caption{Diferença entre a proporção de domicílios com internet e com esgotamento adequado, Sergipe, 2022}",
  "\\label{tab:item2-7-internet-esgoto}",
  "\\small",
  "\\begin{tabular}{lrrr}",
  "\\toprule",
  "Município & Internet (\\%) & Esgotamento adequado (\\%) & Diferença (p.p.) \\\\",
  "\\midrule",
  "\\multicolumn{4}{l}{\\textbf{Maiores diferenças positivas}} \\\\",
  vapply(seq_len(nrow(maiores_diferencas_2_7)), function(i) {
    x <- maiores_diferencas_2_7[i, ]
    paste0(escapa_latex(x$Munic), " & ", formata_razao(x$internet), " & ",
           formata_razao(x$esgoto_adequado), " & ", formata_razao(x$diferenca_pp), " \\\\")
  }, character(1)),
  "\\midrule",
  "\\multicolumn{4}{l}{\\textbf{Menores diferenças positivas}} \\\\",
  vapply(seq_len(nrow(menores_diferencas_positivas_2_7)), function(i) {
    x <- menores_diferencas_positivas_2_7[i, ]
    paste0(escapa_latex(x$Munic), " & ", formata_razao(x$internet), " & ",
           formata_razao(x$esgoto_adequado), " & ", formata_razao(x$diferenca_pp), " \\\\")
  }, character(1)),
  "\\midrule",
  paste0("Sergipe & ", formata_razao(prop_estadual_internet), " & ",
         formata_razao(prop_estadual_esgoto_adequado), " & ",
         formata_razao(diferenca_estadual_internet_esgoto), " \\\\"),
  "\\bottomrule",
  "\\end{tabular}",
  "\\fonte{Elaboração própria com base nos microdados do Censo Demográfico 2022.}",
  "\\nota{Esgotamento considerado adequado para valores da variável D0250 até 3, seguindo o critério do exercício, incluindo fossa séptica ou fossa-filtro não ligada à rede (valor 3); diferença em pontos percentuais.}",
  "\\end{table}"
)
writeLines(linhas_tabela_2_7, file.path(resultados_dir, "item_2_7_internet_esgoto.tex"), useBytes = TRUE)

texto_2_7 <- paste0(
  "O laboratório deseja saber se a proporção de domicílios com acesso à internet supera a proporção com esgotamento sanitário adequado nos municípios de Sergipe. A resposta é sim: em ", nrow(municipios_internet_supera), " dos 75 municípios, a proporção de domicílios com acesso à internet (D0330 igual a 1) supera a proporção com esgotamento considerado adequado pelo critério do exercício, que classifica como adequados os valores da variável D0250 até 3. Em Sergipe, as proporções são de ",
  formata_razao(prop_estadual_internet), "\\% para internet e ",
  formata_razao(prop_estadual_esgoto_adequado), "\\% para esgotamento adequado, diferença de ",
  formata_razao(diferenca_estadual_internet_esgoto), " pontos percentuais. A Tabela~\\ref{tab:item2-7-internet-esgoto} mostra os municípios com as maiores diferenças positivas e aqueles em que as proporções são mais próximas, mas ainda há mais cobertura de internet. Há também diferenças no modo de provisão: a família pode contratar internet de um provedor quando existe cobertura; já ampliar a rede coletora de esgoto até as residências exige infraestrutura coletiva e decisões de planejamento e investimento público. Além disso, o critério adotado considera adequada a fossa séptica ou fossa-filtro não ligada à rede (valor 3 de D0250), de modo que a medida não representa apenas o alcance da rede coletora. O contraste evidencia desigualdade territorial entre serviços essenciais, mas os dados agregados por município não permitem concluir que as famílias tenham escolhido internet em vez de saneamento."
)
writeLines(texto_2_7, file.path(resultados_dir, "item_2_7_resultado.tex"), useBytes = TRUE)

