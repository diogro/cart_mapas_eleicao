# Packages
# ======================================================================================

require("rgdal")
require("rgeos")
require("maptools")
require("plyr")
library("XML")
library("pbapply")
library("plotrix")

# inputs
# =======================================================================================
municipiosShape_mainFolder  <- "./shapes/municipios/"
estadosShape_folder  <- "./shapes/estados/"
votacaoCsv_folder <- "./vot2014/"
iframeDaFolha  <- "./daFolha.txt"
csvConversaoId  <- "./conversao_ID_IBGE_TSE2.csv"
corMaxDilma  <-  "#450003"
corMinDilma  <-  "#F4F5F0"


# Functions
# =======================================================================================
# via http://stackoverflow.com/questions/13289009/check-if-character-string-is-a-valid-color-representation
areColors <- function(x) {
  sapply(x, function(X) {
    tryCatch(is.matrix(col2rgb(X)),
             error = function(e) FALSE)
  })
}


dir.create2  <- function(dir){
  if(!file.exists(dir)){
    dir.create(dir, recursive = TRUE)
  }
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

downloadMunicipiosPorEstadosShapes  <- function(shape_folder){
  dir.create2(shape_folder)
  url_shape  <- getBrZipShapesUrl()
  localPath_zipShape  <- gsub(".*/", shape_folder, url_shape)
  nShapes  <- length(url_shape)
  for(shapeIndex in 1:nShapes){
    download.file(url = url_shape[shapeIndex], localPath_zipShape[shapeIndex])
  }
  lapply(localPath_zipShape, unzip, exdir = shape_folder)
  sapply(localPath_zipShape, file.remove)
  allDirs <- list.dirs(shape_folder)
  selectShapesDir  <- grepl("/\\w{2,2}$", allDirs)
  allDirs[selectShapesDir]
}


downloadEstadosNoBrasilShapes  <- function(shape_folder){
  dir.create2(shape_folder)
  url_uspShape  <- "http://www.usp.br/nereus/wp-content/uploads/Brasil.zip"
  localPath_zipShape  <- gsub(".*/",  shape_folder, url_uspShape)
  download.file(url = url_uspShape, localPath_zipShape)
  unzip(localPath_zipShape, exdir = shape_folder)
  file.remove(localPath_zipShape)
  normalizePath(shape_folder)
}

downloadTse  <- function(folder){
  dir.create2(folder)
  file_url <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_partido_munzona/votacao_partido_munzona_2014.zip"
  zipPatch  <- gsub(".*/", folder,  file_url)
  download.file(file_url, destfile = zipPatch)
  unzip(zipPatch, exdir = folder)
  file.remove(zipPatch)
}


readVotacoes  <- function(folder, filtrar_cargo = NULL, remove_votosExterior = TRUE){
  tablesPatch  <- list.files(folder, pattern = "\\.txt$", full.names = TRUE)
  tabelaPorEstado  <- pblapply(tablesPatch, read.csv, stringsAsFactors = FALSE, encoding = "latin9",
                               sep = ";", header = FALSE)
  todosEstadosTabela  <- do.call(rbind, tabelaPorEstado)
  colnames(todosEstadosTabela)  <- c("DATA_GERACAO", "HORA_GERACAO", "ANO_ELEICAO",
                                     "NUM_TURNO", "DESCRICAO_ELEICAO", "SIGLA_UF",
                                     "SIGLA_UE", "CODIGO_MUNICIPIO", "NOME_MUNICIPIO",
                                     "NUMERO_ZONA","CODIGO_CARGO","DESCRICAO_CARGO",
                                     "TIPO_LEGENDA","NOME_COLIGACAO", "COMPOSICAO_LEGENDA",
                                     "SIGLA_PARTIDO", "NUMERO_PARTIDO", "NOME_PARTIDO",
                                     "QTDE_VOTOS_NOMINAIS", "QTDE_VOTOS_LEGENDA",
                                     "TRANSITO")
  if(!is.null(filtrar_cargo)){
    todosEstadosTabela  <- todosEstadosTabela[todosEstadosTabela$DESCRICAO_CARGO == filtrar_cargo,]
  }
  if(remove_votosExterior){
    todosEstadosTabela  <- todosEstadosTabela[todosEstadosTabela$SIGLA_UF != "ZZ",]
  }
  todosEstadosTabela
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
  shape.df = join(shape.points, shape@data, by="id")
  shape.df
}

getColorDaFolha  <- function(iframeDaFolha){

  # Foi extraido do mapa da folha:
  # http://arte.folha.uol.com.br/poder/2014/10/26/apuracao_comparativos/#/sao-paulo/3550308-sao-paulo
  segundoTurnoString  <- readLines(iframeDaFolha)
  segundoTurnoVec  <- unlist(strsplit(segundoTurnoString, "<path "))


  cores  <- gsub("^fill=\"|\" data-fill=\".*$", "", segundoTurnoVec)[-1]
  cores  <- gsub("^.* fill=\"|\"></path>$", "", cores)[-1]
  cores[!areColors(cores)]  <- "#ffffff"
  id  <- gsub("^.* id=\"municipio_|\" (data-uf|class)=.*$", "", segundoTurnoVec)[-1]
  id  <- gsub("^id=\"municipio_", "", id)[-1]

  dupMun  <- duplicated(id)
  id  <- id[!dupMun]
  cores  <- cores[!dupMun]

  # FALTA UM MUNICIPIO NA FOLHA
  id  <- c(id, 2206720)
  cores  <- c(cores, "#ffffff")
  colMunicipios  <- data.frame(cores)
  rownames(colMunicipios)  <- id
  colMunicipios
}



changeCores  <- function(spdf){
  cores  <- as.character(spdf$cores)
  grandezaDaVotacaoParadilma  <- rep(NA, length(cores))
  grandezaDaVotacaoParadilma[cores == "#006F9D"]  <- 1
  grandezaDaVotacaoParadilma[cores == "#378eb2"]  <- 2
  grandezaDaVotacaoParadilma[cores == "#98c5d7"]  <- 3
  grandezaDaVotacaoParadilma[cores == "#f7b6ba"]  <- 4
  grandezaDaVotacaoParadilma[cores == "#ed6169"]  <- 5
  grandezaDaVotacaoParadilma[cores == "#E30513"]  <- 6

  colfunc <- colorRampPalette(c(corMinDilma, corMaxDilma))
  coresNovas  <- colfunc(6)
  for(i in 1:length(coresNovas)){
    cores[grandezaDaVotacaoParadilma==i]  <- coresNovas[i]
  }
  spdf$cores  <- cores
  spdf

}


legenda  <- function(paletaUnica = TRUE, cexLegend= 0.9){
  par_xpdOriginal  <- par("xpd" = TRUE)
  par_familyOriginal  <- par(family = "Palatino")

  if(paletaUnica){
    colfunc <- colorRampPalette(c(corMinDilma, corMaxDilma))
    cores  <- colfunc(6)
  } else {
    cores  <- c("#006F9D", "#378eb2", "#98c5d7", "#f7b6ba", "#ed6169", "#E30513")
  }

  # Modo nada sutil de fazer isso
  par(xpd = TRUE)
  rect(xleft = -70, ybottom = -40, xright = -67.5, ytop = -37.5, col = cores[1])
  rect(xleft = -67.5, ybottom = -40, xright = -65, ytop = -37.5, col = cores[2])
  rect(xleft = -65, ybottom = -40, xright = -62.5, ytop = -37.5, col = cores[3])
  rect(xleft = -62.5, ybottom = -40, xright = -60, ytop = -37.5, col = cores[4])
  rect(xleft = -60, ybottom = -40, xright = -57.5, ytop = -37.5, col = cores[5])
  rect(xleft = -57.5, ybottom = -40, xright = -55, ytop = -37.5, col = cores[6])

  cexLegend  <- 0.9
  corLegend  <- "#1C201F"
  text(x = -70, y = -41, labels = 0, cex = cexLegend, col = corLegend)
  text(x = -67.5, y = -41, labels = 20, cex = cexLegend, col = corLegend)
  text(x = -65, y = -41, labels = 35, cex = cexLegend, col = corLegend)
  text(x = -62.5, y = -41, labels = 50, cex = cexLegend, col = corLegend)
  text(x = -60, y = -41, labels = 65, cex = cexLegend, col = corLegend)
  text(x = -57.5, y = -41, labels = 80, cex = cexLegend, col = corLegend)
  text(x = -55, y = -41, labels = 100, cex = cexLegend, col = corLegend)
  text(x = -62.5, y = -36.5, "% de votos para  Dilma Rousseff", cex = cexLegend, col = corLegend)
}

por  <- function(cex = 0.7){
  text(x = -35, y = -38.7, labels = "Por:\nDiogo Melo\nDaniel Mariani", adj = 0, cex = cex, col = "#1C201F")
}

plotEleicao  <- function(spdf, paletaUnica = TRUE, lwdFronteira = 0.1, legenda = TRUE){
  if(paletaUnica){
    spdf  <- changeCores(spdf)
  }
  plot(spdf, col = as.character(spdf$cores), lwd = lwdFronteira)
  if(legenda){
    legenda(paletaUnica = paletaUnica)
    por()
  }
}


addLegend4Charts  <- function(paletaUnica = TRUE){
  par_xpdOriginal  <- par("xpd" = TRUE)
  par_familyOriginal  <- par(family = "Palatino")

  if(paletaUnica){
    colfunc <- colorRampPalette(c(corMinDilma, corMaxDilma))
    cores  <- colfunc(6)
  } else {
    cores  <- c("#006F9D", "#378eb2", "#98c5d7", "#f7b6ba", "#ed6169", "#E30513")
  }
  corLegend  <- "#1C201F"

  # Legenda
  par(xpd = TRUE)
  par(new = TRUE)
  cexLegend  <- 0.7
  par(mfrow=c(1,1))
  plot(1:100, 1:100, axes = FALSE, type = "n")
  topY  <- 52
  bottomY  <- 50
  xLeft  <- 35
  xSize  <- 3
  distText  <- 1
  numeros  <- c(0, 20, 35, 50, 65, 80)
  names(numeros)  <- cores
  for(cor in cores){
    rect(xleft = xLeft, ybottom = bottomY, xright = xLeft + xSize , ytop = topY, col = cor)
    text(x = xLeft, y = bottomY - distText, labels = numeros[cor], cex = cexLegend, col = corLegend)
    xLeft  <- xLeft + xSize
  }
  text(x = xLeft, y = bottomY - distText, labels = 100, cex = cexLegend, col = corLegend)
  text(x = 44, y = topY + distText , "% de votos para  Dilma Rousseff", cex = cexLegend, col = corLegend)

  text(x = 90, y = 0, labels = "Por:\nDiogo Melo\nDaniel Mariani", cex = 0.65, col = corLegend, adj = 0)
  textbox(c(28, 65), 2,
          c("Estados/muncípios dos mapas da direita foram expandidos ou encolhidos de acordo com o número de votantes da região."),
          box = FALSE, cex = 0.75)

  par(xpd = FALSE)
  par(new = FALSE)
}



# # downloads
# # ==========================================================================================
 #municipiosShape_folder <- downloadMunicipiosPorEstadosShapes(municipiosShape_mainFolder)
 #estadosShape_folder  <- downloadEstadosNoBrasilShapes(estadosShape_folder)
 #downloadTse(votacaoCsv_folder)

municipiosShape_folder <- list.dirs(municipiosShape_mainFolder)[-1]
estadosShape_folder <- list.dirs(estadosShape_folder)[-1]


# le e formata Shapes
# =========================================================================================
shapesMunicipios <- llply(municipiosShape_folder, read_shape_folder)
merged_shapesMunicipios  <- do.call(rbind, shapesMunicipios)

shapeEstados <- readOGR(layer =  'UFEBRASIL', estadosShape_folder)
row.names(shapeEstados)  <- c("RO", "AC", "AM", "RR", "PA", "AP", "TO", "MA", "PI", "CE", "RN", "PB",
                              "PE", "AL", "SE", "BA", "MG", "ES", "RJ",  "SP", "PR", "SC","RS", "MS",
                              "MT", "GO", "DF")



# Formata votacoes
# ==========================================================================================
votacao  <- readVotacoes(votacaoCsv_folder, filtrar_cargo = "Presidente", remove_votosExterior = TRUE)
votacaoPorMunicipio  <- split(votacao, votacao$CODIGO_MUNICIPIO)
votacaoPorUF  <- split(votacao, votacao$SIGLA_UF)

totalVotosPorMunicipio  <- sapply(votacaoPorMunicipio, function(x) sum(x$QTDE_VOTOS_NOMINAIS))
totalVotosPorUF  <- sapply(votacaoPorUF, function(x) sum(x$QTDE_VOTOS_NOMINAIS))




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

codigosIBGE  <- tabelaConversaoID$Cod_IBGE
totalVotosPorMunicipio  <- totalVotosPorMunicipio[as.character(tabelaConversaoID$Cod_TSE)]

# 8 municipios que estão no tabela de conversão não estão no TSE.
# como esses não tem correspondência para votação são removidos
select_NoTSE  <- is.na(totalVotosPorMunicipio)
sum(select_NoTSE)
codigosIBGE  <- codigosIBGE[!select_NoTSE]
totalVotosPorMunicipio  <- totalVotosPorMunicipio[!select_NoTSE]

# 10 municipios que estão no TSE não tem shapes.
# como não seria possível representa-los, são removidos
selectTemShape  <- codigosIBGE %in% row.names(merged_shapesMunicipios)
sum(selectTemShape)
totalVotosPorMunicipio  <- totalVotosPorMunicipio[selectTemShape]


# Votacao como DF
# =====================================================================================
totalVotosPorMunicipio_df  <- data.frame(totalVotosPorMunicipio, stringsAsFactors = FALSE)
rownames(totalVotosPorMunicipio_df)  <- codigosIBGE[selectTemShape]
totalVotosPorUF_df  <- data.frame(totalVotosPorUF, stringsAsFactors = FALSE)



# Cores -- representam a votacao
# ==========================================================================================

#### Por municipio
colMunicipios <- getColorDaFolha(iframeDaFolha)
totalVotosPorMunicipio_df$cores  <- as.character(colMunicipios[as.character(rownames(totalVotosPorMunicipio_df)),])

#### Por estado
votacao_dilmaUF  <- c("AC" , 63.68 , "#f7b6ba"  ,
                      "AM" , 65.02 , "#f7b6ba"  ,
                      "RR" , 41.1  , "#98c5d7"  ,
                      "PA" , 57.41 , "#f7b6ba"  ,
                      "AP" , 61.45 , "#f7b6ba"  ,
                      "MA" , 78.76 , "#ed6169"  ,
                      "PI" , 78.30 , "#ed6169"  ,
                      "CE" , 76.75 , "#ed6169"  ,
                      "RN" , 69.96 , "#f7b6ba"  ,
                      "PB" , 64.26 , "#f7b6ba"  ,
                      "PE" , 70.20 , "#ed6169"  ,
                      "AL" , 62.12 , "#f7b6ba"  ,
                      "SE" , 67.01 , "#f7b6ba"  ,
                      "BA" , 70.16 , "#ed6169"  ,
                      "MG" , 52.41 , "#f7b6ba"  ,
                      "ES" , 46.15 , "#98c5d7"  ,
                      "RJ" , 59.94 , "#f7b6ba"  ,
                      "SP" , 35.69 , "#98c5d7"  ,
                      "PR" , 39.02 , "#98c5d7"  ,
                      "SC" , 35.41 , "#98c5d7"  ,
                      "RS" , 44.47 , "#98c5d7"  ,
                      "MS" , 43.67 , "#98c5d7"  ,
                      "GO" , 42.89 , "#98c5d7"  ,
                      "DF" , 38.1  , "#98c5d7"  ,
                      "MT" , 45.33 , "#98c5d7"  ,
                      "RO" , 45.15 , "#98c5d7"  ,
                      "TO" , 59.49 , "#f7b6ba")
votacao_dilmaDf  <- as.data.frame(matrix(votacao_dilma, ncol = 3, byrow = TRUE), stringsAsFactors = FALSE)
rownames(votacao_dilmaDf)  <- votacao_dilmaDf$V1
totalVotosPorUF_df$cores  <- votacao_dilmaDf[rownames(totalVotosPorUF_df), "V3"]



# SPDF
# =========================================================================================

spdf_municipios <- SpatialPolygonsDataFrame(merged_shapesMunicipios, totalVotosPorMunicipio_df, match.ID = TRUE)
spdf_estados <- SpatialPolygonsDataFrame(shapeEstados, totalVotosPorUF_df, match.ID = TRUE)


# Deformando os arquivos
# =========================================================================================

writePolyShape(spdf_municipios, "./municipios.shp")
writePolyShape(spdf_estados, "./estados.shp")

# Para deformar
# * Abrir os arquivo no ScapeToad
# * Clique em add Layer
# * Seleciona o layer (municipios.shp ou estados.shp)
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
# * select layer to export --> municipios(2) ou estados(2)

###################################################################################



# Plota
# ====================================================================================

estadoDeformado_shape  <- "./estados_deformado.shp"
municipiosDeformado_shape  <- "./municipios_deformado.shp"

spdf_estadosDeformado  <- readShapePoly(estadoDeformado_shape)
spdf_municipiosDeformado  <- readShapePoly(municipiosDeformado_shape)
spdf_estados  <- readShapePoly("./estados.shp")
spdf_municipios  <- readShapePoly("./municipios.shp")

op <- par(mfrow = c(2,2),
          oma = c(5,4,0,0) + 0.1,
          mar = c(0,0,1,1) + 0.1)
par(mfrow=c(2,2))
plotEleicao(spdf_estados, paletaUnica = TRUE, legenda = FALSE)
plotEleicao(spdf = spdf_estadosDeformado, paletaUnica = TRUE, legenda = FALSE)
plotEleicao(spdf_municipios, paletaUnica = TRUE, legenda = FALSE)
plotEleicao(spdf_municipiosDeformado, paletaUnica = TRUE, legenda = FALSE)
addLegend4Charts(paletaUnica = TRUE)
