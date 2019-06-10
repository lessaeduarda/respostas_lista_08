
# Lista 08
# Professor: Davi Moreira
# Disciplina: Análise de Dados
# Aluna: Maria Eduarda R. N. Lessa


    # Questão 2:

## 5.5.1.1:

# Instalar pacotes necessários caso estes ainda não tenham sido:
if(require(tidyverse)==F)install.packages('tidyverse');require(tidyverse)
if(require(rvest)==F)install.packages('rvest');require(rvest)
if(require(httr)==F)install.packages('httr');require(httr)
if(require(xml2)==F)install.packages('xml2');require(xml2)

# Fornecer link para análise:
conteudo <-readLines("https://pt.wikipedia.org/wiki/Lista_de_munic%C3%ADpios_do_Brasil_por_IDH")

# Consultar linha para "Pernambuco":
grep("Pernambuco", conteudo) # Checar linha de PE
grep("Rio Grande do Sul", conteudo) # Checar a linha do RS (que está imediatamente após PE)
grep("Tocantins", conteudo) # Checar a linha do RS (que está antes de PE)

# Checar a primeira linha de UFs:
grep("S%C3%A3o_Paulo", conteudo) # 109

# Checar a ?ltima linha de UFs:
grep("Rio Grande do Sul", conteudo) # 1081

# Os UFs estão apresentados a cada 9 linhas a partir da linha 109, até 1081;
# como é possível observar no código fonte da página. 

# Criar função:
indice <- 109
nomes_uf <- NULL
i <- 1
while(indice < 1081){
  if(i == 1){
    nomes_uf[i] <- conteudo[indice]
  } else{
      nomes_uf[i] <- conteudo [indice + 9]
      } 
  indice <- indice + 9
  i <- i + 1
}

# "Limpar" informações:
nomes_uf <- gsub("[[:print:]]+\">","", nomes_uf)
nomes_uf <- gsub("</a>","", nomes_uf)
nomes_uf <- gsub("[£|¡]","", nomes_uf)
nomes_uf <- gsub("EspÃrito","Espírito", nomes_uf)
nomes_uf <- gsub("SÃo","São",nomes_uf)
nomes_uf <- gsub("ParanÃ","Paraná", nomes_uf)
nomes_uf <- gsub("GoiÃs","Goiás", nomes_uf)

# Checar UFs: 
nomes_uf

## 5.5.2.1

# Fornecer link para análise:
conteudo2 <- readLines("https://www.camara.leg.br/internet/deputado/DepNovos_Lista.asp?Legislatura=54&Partido=QQ&SX=QQ&Todos=None&UF=QQ&condic=QQ&forma=lista&nome=&ordem=nome&origem=")

# Encontrar linha do primeiro e último deputado no código fonte:
grep("ABELARDO CAMARINHA", conteudo2) # 245
grep("ZONTA", conteudo2) # 6275

# Ao olhar a linha com o nome do deputado é possível perceber que o link da 
# página está duas linhas acima (neste caso, na linha #243). Também é notável
# que os links aparecem a cada 9 linhas e o link do último deputado, Zonta, fica
# na linha #6273. 

# Criar função:
indice2 <- 243
pags_deps <- NULL
i <- 1
while(indice2 < 6273){
  if(i==1){
    pags_deps[i] <- conteudo2[indice2]
  } else{
    pags_deps[i] <- conteudo2 [indice2 + 9]
  } 
  indice2 <- indice2 + 9
  i <- i + 1
}

# "Limpar" informações:
# "Limpar" informações:
pags_deps <- gsub(">","", pags_deps)
pags_deps <- gsub("\t\t\t\t\t\t","", pags_deps)
pags_deps <- gsub("<a href=","", pags_deps)

# Checar vetor das páginas dos deputados:
pags_deps


## 5.7.0.1:

require(httr)
require(XML);
require(xml2);

# Etapa 1, Conhecer detalhadamente o caminho para acesso aos dados:
# Etapa 2, Armazenar todos os caminhos de acesso aos dados de forma amig?vel ao programa:
link <- paste0("http://www.camara.leg.br/SitCamaraWS/Deputados.asmx/ObterDeputados")

# Etapa 3, Obter os dados:
response <- GET(link)

# Etapa 4, Processar os dados obtidos:
data <-xmlParse(response,encoding ="UTF-8")
ls <-xmlToList(data)

names(ls$deputado)

ideCadastro <- NULL
condicao <- NULL
matricula <- NULL
idParlamentar <- NULL
nome <- NULL 
nomeParlamentar <- NULL
urlFoto <- NULL
sexo <- NULL
uf <- NULL
partido <- NULL
email <- NULL

for(i in 1:length(ls)){
  ideCadastro[i] <- ls[[i]]$ideCadastro
  condicao[i] <- ls[[i]]$condicao
  matricula[i] <- ls[[i]]$matricula
  idParlamentar[i] <-ls[[i]]$idParlamentar
  nome[i] <- ls[[i]]$nome
  nomeParlamentar[i] <- ls[[i]]$nomeParlamentar
  urlFoto[i] <- ls[[i]]$urlFoto
  sexo[i] <- ls[[i]]$sexo
  uf[i] <- ls[[i]]$uf
  partido[i] <-ls[[i]]$partido
  email[i] <-ls[[i]]$email
  }

bd <-data.frame(ideCadastro, condicao, matricula, idParlamentar, nome,
                nomeParlamentar, urlFoto, sexo, uf, partido, email)

# Filtrar "ideCadastro" dos deputados de Pernambuco, através do código da UF:
ideCadastroPE <- ideCadastro[uf == "PE"]

# Gerar links dos xmls com os detalhes de cada um dos 25 deputados de PE:
link.dep <- sprintf('https://www.camara.leg.br/SitCamaraWS/Deputados.asmx/ObterDetalhesDeputado?ideCadastro=%s&numLegislatura=56', ideCadastroPE)

# Checar:
link.dep

# Criar base para obter os dados de cada um dos 25 links:
bd.dep.pe <- as.data.frame(for(i in link.dep){
  response.dep <- GET(link.dep[i])
  data.dep <-xmlParse(response.dep,encoding ="UTF-8")
  ls.dep <-xmlToList(data.dep)
  
  email <- NULL
  nomeProfissao <- NULL
  dataNascimento <- NULL
  dataFalecimento <- NULL
  ufRepresentacaoAtual <- NULL 
  situacaoNaLegislaturaAtual <- NULL
  ideCadastro <- NULL
  nomeParlamentarAtual <- NULL
  nomeCivil <- NULL
  sexo <- NULL
  partidoAtual <- NULL
  gabinete <- NULL
  comissoes <- NULL
  cargosComissoes <- NULL
  periodosExercicio <- NULL
  historicoNomeParlamentar <- NULL
  filiacoesPartidarias <- NULL
  historicoLider <- NULL
  
  for(i in 1:length(ls.dep)){
    email[i] <-ls.dep[[i]]$email
    nomeProfissao[i] <- ls.dep[[i]]$nomeProfissao
    dataNascimento[i] <-ls.dep[[i]]$dataNascimento
    dataFalecimento[i] <-ls.dep[[i]]$dataFalecimento
    ufRepresentacaoAtual[i] <- ls.dep[[i]]$ufRepresentacaoAtual
    situacaoNaLegislaturaAtual[[i]] <- ls.dep[[i]]$situacaoNaLegislaturaAtual
    ideCadastro[i] <- ls.dep[[i]]$ideCadastro
    nomeParlamentarAtual[i] <- ls.dep[[i]]$nomeParlamentarAtual
    nomeCivil[i] <- ls.dep[[i]]$nomeCivil
    sexo[i] <- ls.dep[[i]]$sexo
    partidoAtual[i] <-ls.dep[[i]]$partidoAtual
    gabinete[i] <- ls.dep[[i]]$gabinete
    comissoes[i] <- ls.dep[[i]]$comissoes
    cargosComissoes[i] <- ls.dep[[i]]$cargosComissoes
    periodosExercicio[i] <- ls.dep[[i]]$periodosExercicio
    historicoNomeParlamentar[i] <- ls.dep[[i]]$historicoNomeParlamentar
    filiacoesPartidarias[i] <- ls.dep[[i]]$filiacoesPartidarias
    historicoLider[i] <- ls.dep[[i]]$historicoLider
    }})


---

  
  # Questão 3:

# Solicitar pacotes necessários:
install.packages("rvest")
library(rvest)
install.packages("httr")
library(httr)
install.packages("xml2")
library(xml2)
library(magrittr)
install.packages("XML")
library(XML)
install.packages("purrr")
require(purrr)

# Definir url com objeto dinâmico (%d) no lugar do número da legislatura:
url_base <- "https://www2.camara.leg.br/deputados/pesquisa/layouts_deputados_resultado_pesquisa?nome=&Partido=QQ&UF=QQ&SX=QQ&Legislatura=%d&condic=QQ&ordem=nome&forma=lista&Pesquisa="

# Fornecer números das legislaturas, criar e aplicar função nos 15 links para 
# coletar dados acerca dos deputados a partir dos nodes identificados: 
map_df(41:55, function(i){
  
  url <- read_html(sprintf(url_base, i))
  
  data.frame(Nomes = html_text(html_nodes(url, ".demaisInformacoes span")),
             Links = html_attr(html_nodes(url, ".demaisInformacoes a"), "href"),
             stringsAsFactors = FALSE
  )
}) -> Infos_Deputados


# Checar quantidade de deputados por legislatura nos links:

# 463 - 41
# 576 - 42
# 487 - 43
# 338 - 44
# 396 - 45
# 485 - 46
# 569 - 47
# 584 - 48
# 620 - 49
# 635 - 50
# 642 - 51
# 629 - 52
# 636 - 53
# 672 - 54
# 623 - 55

# Criar vetor:
Legislatura <- c(rep(41, times = 463), rep(42, times = 576), rep(43, times = 487),
                 rep(44, times = 338), rep(45, times = 396), rep(46, times = 485),
                 rep(47, times = 569), rep(48, times = 584), rep(49, times = 620),
                 rep(50, times = 635), rep(51, times = 642), rep(52, times = 629),
                 rep(53, times = 636), rep(54, times = 672), rep(55, times = 623))

# Juntar informações:
require(dplyr)
require(magrittr)
Lista_Deputados <- mutate(Infos_Deputados, Legislatura)

# Checar:
head(Lista_Deputados)

# Salvar em .RData:
save(Lista_Deputados, file = "Lista_Deputados.RData")


---
  
  
  # Questão 4:

# Carregar pacotes necessários:
require(rgdal)
require(maptools)
require(ggmap)
require(mapproj)
require(ggplot2)
require(tidyverse)

# Carregar shapefile municípios de PE:
shapefile_muni_pe <- readOGR(dsn=path.expand("C:/Users/Duda/Desktop/PPGCP/Análise de Dados/lista_08/municipios_pe"),
                             layer="26MUE250GC_SIR")
plot(shapefile_muni_pe)

# Checar:
shapefile_muni_pe@data

# Converter shapefile em dataframe:
shapefile_df <- fortify(shapefile_muni_pe)
shapefile_data <-fortify (shapefile_muni_pe@data)
shapefile_data$id <- row.names(shapefile_data)
shapefile_df <-full_join (shapefile_df, shapefile_data,by="id")

# Checar:
names(shapefile_df)
head(shapefile_df)

# Criar vetor "target" com os códigos do IBGE para os 15 municípios da RMR:
target <- c("2600054", "2601052", "2602902", "2603454", "2606200", "2606804",
            "2607604","2607208","2607752","2607901","2609402","2609600",
            "2610707","2611606","2613701")

# Filtrar municípios da RMR pela variável do código do município (CD_GEOCMU):
shapefile_RMR <- shapefile_df %>% filter(CD_GEOCMU %in% target)

# Carregar a base de dados do PNUD:
require(magrittr)
require(dplyr)
load("dados_PNUD_pe.RData")

# Filtrar apenas os municípios da RMR e selecionar variáveis IDHM e código do 
# município (CO_MUNICIPIO):
PNUD_RMR <- subset(dados_PNUD_pe, CO_MUNICIPIO %in% target) %>% 
  select(IDHM, CO_MUNICIPIO)

# Observação: Na lista 04, manipulei a base do PNUD e renomeei a variável 
# que continha o código do município de "Codmun7" para "CO_MUNICIPIO". É esta
# base que utilizo para fazer esta questão, por isso o nome da variável é
# diferente do que consta no documento da aula. 

# Unir as bases PNUD_RMR e shapefile_df através do código dos municípios:
PNUD_RMR$CO_MUNICIPIO <- as.factor(PNUD_RMR$CO_MUNICIPIO)
shapefile_RMR <- shapefile_RMR %>% left_join(PNUD_RMR,
                                           by = c("CD_GEOCMU"= as.character(
                                             "CO_MUNICIPIO")))

# Checar base:
head(shapefile_RMR)


# Criar mapa com nome dos municípios e IDHM:
mapa_RMR <-ggplot() + geom_polygon(data = shapefile_RMR, 
                               aes(x = long, y = lat, group = group,
                                   fill = IDHM),
                               colour ="darkgray", size = .2) +
  ggtitle("IDHM dos municípios da RMR") +
  geom_text(aes(x = long, y = lat, label = nome), size = 2,
            data = aggregate(shapefile_RMR, 
                             list(nome = shapefile_RMR$NM_MUNICIP),mean)) +
  coord_equal() +
  theme_void() + 
  scale_fill_gradient2(low = "#fc8d59",mid = "#ffffbf",high = "#99d594",
                       midpoint = median(shapefile_RMR$IDHM),
                       limits = range(shapefile_RMR$IDHM)) + 
  coord_map()

# Plotar:
mapa_RMR

# Salvar: 
ggsave(filename ="mapa_RMR.png", mapa_RMR)

---