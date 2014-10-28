# Packages
# ======================================================================================

require("rgdal") # requires sp, will use proj.4 if installed
require("rgeos") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
library(devtools)
install_git("git://github.com/gsk3/taRifx.geo.git")
library(taRifx.geo)
library("XML")
library("pbapply")


# inputs
# =======================================================================================
mainShape_folder  <- "~/Desktop/shapes/"
votacaoCsv_folder <- "~/Desktop/vot2014/"
iframeDaFolha  <- "~/Desktop/daFolha.txt"
csvConversaoId  <- "~/Desktop/conversao_ID_IBGE_TSE2.csv"


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

downloadEstadosBrShapes  <- function(shape_folder){
  dir.create2(shape_folder)
  url_shape  <- getBrZipShapesUrl()
  localPath_zipShape  <- gsub(".*/", shape_folder, url_shape)
  nShapes  <- length(url_shape)
  for(shapeIndex in 1:nShapes){
    download.file(url = url_shape[shapeIndex], localPath_zipShape[shapeIndex])
  }
  lapply(localPath_zipShape, unzip, exdir = shape_folder) 
  sapply(localPath_zipShape, file.remove)
  allDirs <- list.dirs(mainShape_folder)
  selectShapesDir  <- grepl("/\\w{2,2}$", allDirs)
  allDirs[selectShapesDir]
}

downloadTse  <- function(folder){
  dir.create2(folder)
  file_url <- "http://agencia.tse.jus.br/estatistica/sead/odsele/votacao_partido_munzona/votacao_partido_munzona_2014.zip"
  zipPatch  <- gsub(".*/", folder,  file_url)
  download.file(file_url, destfile = zipPatch)
  unzip(zipPatch, exdir = folder)
  file.remove(zipPatch)  
}

readVotacoes  <- function(folder){
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
  todosEstadosTabela
}

read_shape_folder <- function(folder){
  shapes <- dir(folder)
  shapes <- unique(unlist(lapply(shapes, function(x) gsub('\\..*', '', x))))
  ogr  <- read_shape(shapes[grep('MUE', shapes)], folder)
  row.names(ogr)  <- as.character(ogr$CD_GEOCODM)
  ogr
}

read_shape <- function(layer, folder){
  readOGR(dsn = folder, layer=layer)
}



create_shape_df <- function(shape) {
  shape@data$id = rownames(shape@data)
  shape.points = fortify(shape, region = 'id')
  shape.df = join(shape.points, shape@data, by="id")
  shape.df
}



# Downloads
# ==========================================================================================
estadosShape_folder <- downloadEstadosBrShapes(mainShape_folder)
downloadTse(votacaoCsv_folder)



# Formata Shapes
# =========================================================================================

shapes <- llply(estadosShape_folder, read_shape_folder)
merged_shapes  <- do.call(rbind, shapes)


# Formata votacoes
# ==========================================================================================

# Votacao 
votacao  <- readVotacoes(votacaoCsv_folder)
votacaoNoGringo  <- votacao[votacao$SIGLA_UE != "ZZ",]
votacaoPresidente  <- votacao[votacaoNoGringo$DESCRICAO_CARGO == "Presidente",]
tabelasPorMunicipio  <- split(votacaoPresidente, votacaoPresidente$CODIGO_MUNICIPIO)

# Total de votos em cada municipio
totalVotosPorMunicipio  <- sapply(tabelasPorMunicipio, function(x) sum(x$QTDE_VOTOS_NOMINAIS)) 




# Merge os IDs
# =======================================================================================
#### Para juntar os votos com a votação precisa converter o ID do TSE para o IBGE
tabelaConversaoID  <- read.csv(csvConversaoId)[,1:2]
# conserta os 7 municipios ausentes
tabelaConversaoID  <- rbind(tabelaConversaoID, c(33650, 2903300))
tabelaConversaoID  <- rbind(tabelaConversaoID, c(34010, 2905107))
tabelaConversaoID  <- rbind(tabelaConversaoID, c(36919, 2919504))
tabelaConversaoID  <- rbind(tabelaConversaoID, c(22292, 2516409))
tabelaConversaoID  <- rbind(tabelaConversaoID, c(16250 , 2401305))
# Esses dois "pseudo muncipios" não estão no ibge, então os botei em pelotas, que é próxima
tabelaConversaoID  <- rbind(tabelaConversaoID, c(87912, 4300001))
tabelaConversaoID  <- rbind(tabelaConversaoID, c(87912, 4300002))


# Codigos do ibge na tabela de conversao
codigosIBGE  <- tabelaConversaoID$Cod_IBGE
totalVotos  <- totalVotosPorMunicipio[as.character(tabelaConversaoID$Cod_TSE)]
select_remove  <- is.na(totalVotos)
codigosIBGE  <- codigosIBGE[!select_remove]
totalVotos  <- totalVotos[!select_remove]
selectTemShape  <- codigosIBGE %in% row.names(merged_shapes)
totalVotosDf  <- data.frame(totalVotos[selectTemShape])
rownames(totalVotosDf)  <- codigosIBGE[selectTemShape]



# Cores
# ==========================================================================================

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
cores[cores == ""]

# Para não ficar binario
cores[cores == "#ed6169"]  <- "#B51A2E"
cores[cores == "#f7b6ba"]  <- "#882F4A"
cores[cores == "#98c5d7"]  <- "#5A4465"
cores[cores == "#378eb2"]  <- "#2D5981"

# FALTA UM MUNICIPIO NA FOLHA
id  <- c(id, 2206720)
cores  <- c(cores, "#ffffff")



colMunicipios  <- data.frame(cores)
rownames(colMunicipios)  <- id

totalVotosDf$cores  <- colMunicipios[row.names(totalVotosDf),]



# SPDF
# =========================================================================================

spdf <- SpatialPolygonsDataFrame(merged_shapes, totalVotosDf, match.ID = TRUE)


###################################################################################
# Abrir esse arquivo no ScapeT Toad  e depois salva o brazil_r_useworld_r_use.shp(2)
# em ~/Desktop/x.shp
writePolyShape(spdf, "~/Desktop/brazil_r_useworld_r_use.shp")
###################################################################################


cartogram <-readShapePoly("~/Desktop/x.shp")
cartogram$cores  <- 

cartogram2 <- SpatialPolygonsDataFrame(cartogram, colMunicipios, match.ID = TRUE)

plot(cartogram, col = as.character(cartogram$cores), lwd = 0.1, )

