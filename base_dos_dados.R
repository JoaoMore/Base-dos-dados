library('tidyverse')
library('basedosdados')
library('sf')
library('geobr')
library('dotenv')
library('readxl')
library('ggtext')
library('patchwork')
theme_set(theme_void() + 
            theme(legend.text = element_text(family = "Montserrat", face = "bold",
                                             size = 10, color = 'gray20'),
                  plot.title = element_text(family = 'Montserrat', face = 'bold', 
                                            size = 15, hjust = .5, color = 'gray20'),
                  legend.position = 'bottom', legend.key.size = unit(.85,"line"),
                  plot.caption = element_markdown()))

load_dot_env(file = '.env')
set_billing_id(Sys.getenv("billing_project_id"))

lapply(dir(pattern = '(.RData)'),load,.GlobalEnv)

# Obtenção dos dados ------------------------------------------------------

## Base dos dados --------------------------------------------------------

# Dados dos candidatos

query.candidatos <- bdplyr("br_tse_eleicoes.candidatos")

candidatos <- bd_collect(query.candidatos)

# Bens dos candidatos
query.bens <- bdplyr("br_tse_eleicoes.bens_candidato")

bens <- bd_collect(query.bens)


## Códigos municipais IBGE -------------------------------------------------

codigos.rn <- read_xls('RELATORIO_DTB_BRASIL_MUNICIPIO.xls') %>% #baixado do site do IBGE
  filter(Nome_UF == 'Rio Grande do Norte')

## Dados espaciais dos municipios do RN ------------------------------------

rn.shape <- read_municipality(code_muni = 24, year = 2018)

# Filtragem - Eleições municipais do RN em 2020 --------------------------------

bens <- bens %>% 
  filter(ano == 2020, sigla_uf == 'RN')

save(bens, file = 'data/bens.RData')

candidatos <- candidatos %>% 
  filter(ano == 2020, sigla_uf == 'RN')

save(candidatos, file = 'data/candidatos.RData')


# Análise e plot -------------------------------------------------------------------------

# Juntando os bens dos candidatos com seus dados pessoais
df <- left_join(bens, candidatos)

# adicionando o nome do municipio da eleição ao banco de dados dos candidatos+bens

df1 <- left_join(df, codigos.rn %>% select(Nome_Município, `Código Município Completo`),
                 by = c('id_municipio' = 'Código Município Completo'))

# Valor total em bens dos candidatos de cada município, por gênero e raça

maiores.bens.mun <- df1 %>% 
  group_by(sequencial ,id_municipio, genero, raca) %>% 
  summarise(total = sum(valor_item, na.rm = T)) %>% 
  ungroup(raca, genero, sequencial) %>% 
  slice_max(total) %>% 
  ungroup()

# join do dataset com informações dos candidatos com dataset shapefile

bens.mun.shape <- maiores.bens.mun %>% 
  mutate(id_municipio = as.numeric(id_municipio)) %>% 
  left_join(., rn.shape, by = c('id_municipio' = 'code_muni'))

# Raca do candidato com maior patrimonio de cada municipio
raca <- ggplot(bens.mun.shape) +
  geom_sf(aes(geometry = geom, fill = raca), color = 'gray80') +
  labs(title = 'Raça', fill = NULL,) +
  scale_fill_manual(labels = c('Amarela','Branca','Parda','Preta'), 
                    values = c('#0BB44B','#B71D8B','#FFB718','#18BECB'))

# genero do candidato com maior patrimonio de cada municipio
genero <- ggplot(bens.mun.shape) +
  geom_sf(aes(geometry = geom, fill = genero), color = 'gray90') +
  labs(title = 'Gênero', fill = NULL) +
  scale_fill_manual(labels = c("Feminino", "Masculino") ,
                    values = c('firebrick1', 'deepskyblue4'))

# Juntando os dois graficos e adicionando texto globais

wrap_plots(list(raca, genero), ncol = 2, ) +
  plot_annotation(
    title = "Eleições Municipais 2020",
    subtitle = "Entre os candidatos com maior patrimônio em cada município, <br>
    a maioria são <b style='color:steelblue4'>homens</b> e <b style='color:#B71D8B'>brancos</b>",
    caption = 
      '<b>Fonte</b> : <b>TSE</b> (Tribunal Superior Eleitoral) | <b>Base dos Dados</b>', 
    theme = theme(legend.text = element_text(family = "Montserrat", face = "bold",
                                             size = 10, color = 'gray20'),
                  plot.title = element_text(family = 'Montserrat', face = 'bold', 
                                            size = 15, hjust = .5, color = 'gray20',
                                            margin = margin(0, 0, 10, 0)),
                  plot.subtitle = element_markdown(family = 'Montserrat', face = 'bold', 
                                               size = 13, hjust = .5, color = 'gray20',
                                               margin = margin(0, 0, 30, 0)),
                  legend.position = 'bottom', legend.key.size = unit(.85,"line"),
                  plot.caption = element_markdown(family = 'Montserrat', size = 10,
                                                  hjust = .5, vjust = -20, 
                                                  margin = margin(30, 0, 0, 0))))


