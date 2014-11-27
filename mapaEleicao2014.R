# Packages
# ======================================================================================

require("rgdal")
require("rgeos")
require("maptools")
require("plyr")
library("XML")
library("pbapply")
library("plotrix")
library("RColorBrewer")

# inputs
# =======================================================================================
municipiosShape_mainFolder  <- "./shapes/municipios/"
estadosShape_folder  <- "./shapes/estados/"
votacaoCsv_folder <- "./vot2014/"
csvConversaoId  <- "./conversao_ID_IBGE_TSE2.csv"
csv_segundo_turno <- 'https://dl.dropboxusercontent.com/u/891794/dfMunicipios.csv'

# https://scontent-b-mia.xx.fbcdn.net/hphotos-xpf1/v/t1.0-9/10359221_10204130302151847_4476543488279263099_n.jpg?oh=2bc51d420264d51b8fb4b52a359de85a&oe=54B0D984
paletaRdBu  <- rev(brewer.pal(11, "RdBu"))
colfunc <- colorRampPalette(paletaRdBu)
paleta  <- colfunc(500)

# Functions
# =======================================================================================

dir.create2  <- function(dir){
  if(!file.exists(dir)){
    dir.create(dir, recursive = TRUE)
  }
}

download_unzip_removezip  <- function(zip_url, destfolder){
  zip_file  <- gsub(".*/", "", zip_url)
  zip_dest  <- paste(destfolder, zip_file, sep = "/")
  download.file(zip_url, zip_dest)
  files  <- unzip(zip_dest, exdir = destfolder, list = TRUE)
  file.remove(zip_dest)
  files
}

getBrZipShapesUrl  <- function(){
  url_estadosIndex  <- "http://dados.gov.br/dataset/malha-geometrica-dos-municipios-brasileiros"
  html_estadosIndex  <- htmlParse(url_estadosIndex)
  url_estadosRelative  <- xpathApply(doc = html_estadosIndex,
                                     path ="//*[@id='dataset-resources']/ul/li[*]/a",
                                     xmlGetAttr, name ="href")
  url_estadosAbsolut  <- paste0("http://dados.gov.br/", url_estadosRelative)
  html_estados  <- lapply(url_estadosAbsolut, htmlParse)
  getShapeUrl  <- function(html_estado){
    xpathSApply(html_estado, "//*[@id='content']/div[3]/section/div[1]/p/a", xmlGetAttr, name ="href")
  }
  sapply(html_estados, getShapeUrl)
}

downloadMunicipiosPorEstadosShapes  <- function(folder){
  dir.create2(folder)
  url_shape  <- getBrZipShapesUrl()
  lapply(url_shape, download_unzip_removezip, destfolder = folder)
  allDirs <- list.dirs(folder)
  selectShapesDir  <- grepl("/\\w{2,2}$", allDirs)
  allDirs[selectShapesDir]
}

downloadTse_boletimUrnas  <- function(folder){
  dir.create2(folder)
  mainTse_boletimUrnas_url  <- "http://www.tse.jus.br/hotSites/pesquisas-eleitorais/resultados_anos/boletim_urna/boletim_urna_2_turno-2014.html"
  mainTse_boletimUrnas_html  <- htmlParse(mainTse_boletimUrnas_url)
  zip_urls  <- xpathSApply(mainTse_boletimUrnas_html, "//*[@id='tabela_correspondencia']/li[*]/a[1]", xmlGetAttr, name ="href")
  files  <- lapply(zip_urls, download_unzip_removezip, destfolder = folder)
  filesNames  <- sapply(files, function(x) x$Name[1])
  paste(folder, filesNames, sep = "/")
}



read_shape_folder <- function(folder){
  shapes <- dir(folder)
  shapes <- unique(unlist(lapply(shapes, function(x) gsub('\\..*', '', x))))
  ogr  <- readOGR(folder, shapes[grep('MUE', shapes)])
  row.names(ogr)  <- as.character(ogr$CD_GEOCODM)
  ogr
}

create_shape_df <- function(shape) {
  shape@data$id = rownames(shape@data)
  shape.points = fortify(shape, region = 'id')
  join(shape.points, shape@data, by="id")
}

formatStrings  <- function(x){
  x  <- iconv(x, to="ASCII//TRANSLIT")
  x  <- gsub(" |/", "_", x)
  x  <- tolower(x)
  x
}
formatColumnNames <- function(df){
  colnames(df)  <- formatStrings(colnames(df))
  df
}

readBu  <- function(bu_path){
  bu  <- read.csv(bu_path, header = FALSE, sep = ";", stringsAsFactors = FALSE, encoding = "latin1")
  colnames(bu)  <- c("DATA DA GERAÇÃO", "HORA DA GERAÇÃO", "CÓDIGO DO PLEITO", "CÓDIGO DA ELEIÇÃO"
                     , "SIGLA DA UF", "CÓDIGO DO CARGO/CÓDIGO DA PERGUNTA", "DESCRIÇÃO DO CARGO/DESCRIÇÃO DA PERGUNTA"
                     , "NÚMERO DA ZONA ELEITORAL", "NÚMERO DA SEÇÃO ELEITORAL", "NÚMERO DO LOCAL DE VOTAÇÃO"
                     , "NÚMERO DO PARTIDO", "NOME DO PARTIDO", "CÓDIGO DO MUNICÍPIO", "NOME DO MUNICÍPIO",
                     "DATA DO BU RECEBIDO", "QUANTIDADE DE ELEITORES APTOS", "QUANTIDADE DE ELEITORES FALTOSOS",
                     "QUANTIDADE DE COMPARECIMENTO", "CÓDIGO DO TIPO DA ELEIÇÃO", "CÓDIGO DO TIPO DA URNA",
                     "DESCRIÇÃO DO TIPO DA URNA", "NÚMERO DO VOTÁVEL", "NOME DO VOTÁVEL", "QUANTIDADE DE VOTOS"
                     , "CÓDIGO DO TIPO DO VOTÁVEL", "NÚMERO DE URNA EFETIVADA", "CÓDIGO DA CARGA URNA 1 EFETIVADA"
                     , "CÓDIGO DA CARGA URNA 2 EFETIVADA", "DATA DA CARGA DE URNA EFETIVADA",
                     "CÓDIGO DO FLASHCARD DE URNA EFETIVADA", "CARGO PERGUNTA SEÇÃO")
  formatColumnNames(bu)
}



getBoletimUrnaInfosPorSeccao  <- function(bu_path, cargo){
  bu  <- readBu(bu_path)
  select_cargo  <- bu$descricao_do_cargo_descricao_da_pergunta == cargo
  bu_cargo  <- bu[select_cargo,]
  bu_cargo$zona_secao_numlocalVot_NumUrnaEfe  <- paste(bu_cargo$numero_da_zona_eleitoral, bu_cargo$numero_da_secao_eleitoral,
                                                       bu_cargo$numero_do_local_de_votacao, bu_cargo$numero_de_urna_efetivada,
                                                       sep = "_")

  bu_porSecao  <- split(bu_cargo, bu_cargo$zona_secao_numlocalVot_NumUrnaEfe)

  numero_zona  <- sapply(bu_porSecao, function(x) x$numero_da_zona_eleitoral[1])
  dfSecoes  <- as.data.frame(numero_zona, stringsAsFactors = FALSE)

  dfSecoes$numero_secao  <- sapply(bu_porSecao, function(x) x$numero_da_secao_eleitoral[1])
  dfSecoes$numero_de_urna_efetivada  <- sapply(bu_porSecao, function(x) x$numero_de_urna_efetivada[1])
  dfSecoes$uf  <- sapply(bu_porSecao, function(x) x$sigla_da_uf[1])
  dfSecoes$municipio  <- sapply(bu_porSecao, function(x) x$codigo_do_municipio[1])
  dfSecoes$nAptos  <- sapply(bu_porSecao, function(x) x$quantidade_de_eleitores_aptos[1])
  dfSecoes$nPresentes  <- sapply(bu_porSecao, function(x) x$quantidade_de_comparecimento[1])
  dfSecoes$porcentagemPresentes  <- (dfSecoes$nPresentes/dfSecoes$nAptos) * 100

  candidatos  <- unique(bu_cargo$nome_do_votavel)
  listCandidatos  <- list()
  for(cand in candidatos){
    votosCand  <- lapply(bu_porSecao, function(x) x$quantidade_de_votos[x$nome_do_votavel == cand])
    votosCand[sapply(votosCand, length) == 0]  <- 0
    listCandidatos[[cand]]  <- unlist(votosCand)
  }

  dfCandidatos  <- as.data.frame(t(do.call(rbind, listCandidatos)))
  votosNaoValidos  <- c("BRANCO", "NULO")
  colunasCandidtos  <- colnames(dfCandidatos)[!colnames(dfCandidatos) %in% votosNaoValidos]
  dfCandidatos$votosTotal  <- rowSums(dfCandidatos)
  dfCandidatos$votosValidos  <- rowSums(dfCandidatos[,colunasCandidtos])
  for(cand in colunasCandidtos){
    percentageColumnName  <- paste("porcentagem_validos", cand, sep = "_" )
    dfCandidatos[,percentageColumnName]  <- dfCandidatos[,cand]/dfCandidatos$votosValidos
  }
  for(cand in candidatos){
    percentageColumnName  <- paste("porcentagem_total", cand, sep = "_" )
    dfCandidatos[,percentageColumnName]  <- dfCandidatos[,cand]/dfCandidatos$votosTotal
  }
  dfSecoes  <- cbind(dfSecoes, dfCandidatos)
  dfSecoes  <- formatColumnNames(dfSecoes)
  rownames(dfSecoes)  <- NULL
  list(df = dfSecoes, candidatos = formatStrings(colunasCandidtos))
}


getBoletimUrnaInfosPorMunicipio  <- function(bu_path, cargo){
  listSecoes  <- getBoletimUrnaInfosPorSeccao(bu_path, cargo)
  dfSecoes  <- listSecoes$df
  candidatos  <- listSecoes$candidatos

  dfsPorMunicipios <- split(dfSecoes, dfSecoes$municipio)
  municipio  <- names(dfsPorMunicipios)
  df  <- as.data.frame(municipio, stringsAsFactors = FALSE)
  df$uf  <- sapply(dfsPorMunicipios, function(x) x$uf[1])
  for(numVar in c(candidatos, "naptos", "votostotal", "votosvalidos",  "branco", "nulo")){
    df[, numVar]  <- sapply(dfsPorMunicipios, function(x) sum(x[, numVar]))
  }
  for(cand in candidatos){
    percentageColumnName  <- paste("porcentagem_validos", cand, sep = "_" )
    df[,percentageColumnName]  <- df[,cand]/df$votosvalidos
  }
  for(votoTipo in c(candidatos, "branco",  "nulo") ){
    percentageColumnName  <- paste("porcentagem_total", votoTipo, sep = "_" )
    df[,percentageColumnName]  <- df[,votoTipo]/df$votostotal
  }
  df$porcentagemPresentes  <- df$votostotal/df$naptos
  df
}


# based in http://stackoverflow.com/questions/13355176/gradient-legend-in-base
makeLegendGradiente  <- function(xl, yb, xr, yt, paleta, lwd = 0.1, cex = 0.9, nMarks  = 12, distText = 1){
  legend_image <- as.raster(matrix(colfunc(20), ncol=1))
  text(x=1.5, y = seq(0,1,l=5), labels = seq(0,1,l=5))
  rasterImage(legend_image, 0, 0, 1,1)

  nCores  <- length(paleta)

  rect(xl,
       head(seq(yb, yt, (yt-yb)/nCores),-1),
       xr,
       tail(seq(yb,yt,(yt-yb)/nCores),-1),
       col = paleta,
       lwd = lwd)

  textLengend <-  round(seq(from = 0, to = 100, length.out  = nMarks))
  atY  <- seq(from = yb, to = yt, length.out = nMarks)
  text(x = xl - distText, y = atY,
       labels = textLengend, cex = cex)

}

# # downloads
# # ==========================================================================================
# municipiosShape_folder <- downloadMunicipiosPorEstadosShapes(municipiosShape_mainFolder)
# estadosShape_folder  <- downloadEstadosNoBrasilShapes(estadosShape_folder)
 boletimTxts  <- downloadTse_boletimUrnas(votacaoCsv_folder)

# Caso tenha baixado
municipiosShape_folder <- list.dirs(municipiosShape_mainFolder)[-1]
boletimTxts  <- list.files(votacaoCsv_folder, pattern = "\\.txt$", full.names = TRUE)


# le e formata Shapes
# =========================================================================================
shapesMunicipios <- llply(municipiosShape_folder, read_shape_folder)
merged_shapesMunicipios  <- do.call(rbind, shapesMunicipios)
row.names(merged_shapesMunicipios)  <- as.character(merged_shapesMunicipios$CD_GEOCODM)


# Le e Formata votacoes
# ==========================================================================================
votacaoPorMunicipio  <- pblapply(boletimTxts, getBoletimUrnaInfosPorMunicipio, cargo = "PRESIDENTE")
votacao  <- do.call(rbind, votacaoPorMunicipio)
#names(votacao) <- names(votacaoPorMunicipio[[1]])



# Merge os IDs dos municipios TSE (aonde esta a votacao) -- IBGE (aonde estao os shapes)
# =======================================================================================
tabelaConversaoID  <- read.csv(csvConversaoId)[,c("Cod_TSE", "Cod_IBGE")]

# conserta os 7 municipios ausentes
municipiosAusentes  <- data.frame(Cod_TSE = c(33650, 34010, 36919, 22292, 16250,
                                              87912, 87912), # Esses dois "pseudo muncipios" não estão no ibge,
                                  # então os botei em pelotas, que é próxima.
                                  Cod_IBGE = c(2903300, 2905107, 2919504, 2516409,
                                               2401305, 4300001, 4300002))
tabelaConversaoID  <- rbind(tabelaConversaoID, municipiosAusentes)

votacaoComIBGE  <- merge(votacao, tabelaConversaoID, by.x = "municipio", by.y = "Cod_TSE")

# retira os que nao tem shapefiles
votacaoComIBGE2  <- votacaoComIBGE[votacaoComIBGE$Cod_IBGE %in% row.names(merged_shapesMunicipios),]


# Cores
# ===========================================================================================
nCores  <- length(paleta)
intervalMarks  <- seq(from = 0, to = 1, length.out = nCores + 1)
intervals  <- findInterval(votacaoComIBGE2$porcentagem_validos_dilma, intervalMarks)
cores  <- paleta[intervals]


# Faz o df com as informacoes para o mapa
# =====================================================================================
dfParaMapa  <- data.frame(porcentagemDilma = votacaoComIBGE2$porcentagem_validos_dilma,
                          totalVotos = votacaoComIBGE2$votostotal,
                          cor = cores, row.names =  votacaoComIBGE2$Cod_IBGE,
                          stringsAsFactors = FALSE)



# SPDF
# =========================================================================================

spdf_municipios <- SpatialPolygonsDataFrame(merged_shapesMunicipios, dfParaMapa, match.ID = TRUE)


# Deformando os arquivos
# =========================================================================================

writePolyShape(spdf_municipios, "~/Desktop/municipios.shp")

# Para deformar
# * Abrir os arquivo no ScapeToad
# * Clique em add Layer
# * Seleciona o layer municipios.shp
# * clica em Create Cartogram
# * "next"
# * no spatial coverage selecionar estados ou municipios
# * "next"
# * no Cartogram attribute selecionar totalVotos
# * no attribute type selecionar "mass"
# * "next"
# * "next"
# * "Transformation Quality": High (a máxima)
# * "compute"
# * "export do shape"
# * select layer to export --> municipios(2)
# * (salve como municipios_deformados)
###################################################################################



# Plota
# ====================================================================================

municipiosDeformado_shape  <- "~/Desktop/municipios_deformados.dbf"
spdf_municipiosDeformado  <- readOGR(normalizePath("~/Desktop/"), "municipios_deformados")

myBlack  <- "#1C201F"
alphaLine  <- 0.7
lwdLine  <- 1

op <- par(mfrow = c(1,2), family = "Palatino",
          oma = c(0,0,0,0) + 0.1,
          mar = c(0,0,0,0) + 0.1)
plot(spdf_municipios, col = as.character(spdf_municipios$cor), lwd = 0.1)
plot(spdf_municipiosDeformado, col = as.character(spdf_municipios$cor), lwd = 0.1)
par(new = TRUE, mfrow = c(1,1))
plot(1:100,  type="n", axes = FALSE)
xl <- 7
yb <- 0
xr <- 5
yt <- 15
makeLegendGradiente(xl,yb, xr, yt, paleta, lwd = 0, nMarks = 6, distText = -2, cex = 0.7)
text(x = (xr + xl)/2, y = yt + 4, labels = "% votes for the winner\n(Dilma Roussef)", cex = 0.75, col = myBlack)
textbox(x = c(20, 70), y = 15 + 4.5, box = FALSE, leading = 1,
        textlist = c("Brazilian votting pattern for the second round of presidential election using a density equalizing deformation on total number of votes. The deformation was done in ScapeToad, unsing the tecnique described in Gastner/Newman (2004) Diffusion-based method for producing density-equalizing maps."),
        col = myBlack)

textbox(x = c(20, 70), y = 2.5, box = FALSE, leading = 1,
        textlist = c("Code: github.com/diogro/cart_mapas_eleicao"),
        col = myBlack)

textbox(x = c(77, 100), y = c(15 + 4.5), box = FALSE, leading = 0.7, cex = 1,
        textlist = "Sources:",
        col = myBlack)
textbox(x = c(77, 100), y = c(16.5), box = FALSE, leading = 0.7, cex = 0.7,
        textlist = c("Color Palette: 500 interpolation of the RdBu Palette from Cynthia Brewer"),
        col = myBlack)
textbox(x = c(77, 100), y = c(13.5), box = FALSE, leading = 0.7, cex = 0.7,
        textlist = c("Data vote: Brazilian Superior Electoral Court (TSE)"),
        col = myBlack)
textbox(x = c(77, 100), y = c(10.5), box = FALSE, leading = 0.7, cex = 0.7,
        textlist = c("Shape Files: Brazilian Institute of Geography and Statistics (IBGE)"),
        col = myBlack)



textbox(x = c(77, 100), y = c(6), box = FALSE, leading = 0.7, cex = 1,
        textlist = "Authors:",
        col = myBlack)
textbox(x = c(77, 100), y = c(3), box = FALSE, leading = 0.7, cex = 0.7,
        textlist = c("Diogo Melo"),
        col = myBlack)
textbox(x = c(77, 100), y = c(1), box = FALSE, leading = 0.7, cex = 0.7,
        textlist = c("Daniel Mariani"),
        col = myBlack)
