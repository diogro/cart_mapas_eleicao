require("rgdal") # requires sp, will use proj.4 if installed
require("rgeos") # requires sp, will use proj.4 if installed
require("maptools")
require("ggplot2")
require("plyr")
library(devtools)
install_git("git://github.com/gsk3/taRifx.geo.git")
library(taRifx.geo)

lapply(dir(pattern = '.zip'), unzip, exdir = 'shapes')

shape_folder = dir("shapes")
read_shape_folder <- function(folder){
   folder <- paste0('shapes/',folder)
   shapes <- dir(folder)
   shapes <- unique(unlist(lapply(shapes, function(x) gsub('\\..*', '', x))))
   read_shape(shapes[grep('MUE', shapes)], folder)
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
shapes <- llply(shape_folder, read_shape_folder)
merged_shapes <- rbind(shapes[[1]], shapes[[2]], fix.duplicated.IDs=TRUE)

#merge trava no 6, mas ele sozinho plota direito, é um erro de rownames, parece meio bobo
for (i in 3:length(shapes)){
    print(i)
    merged_shapes <- rbind(merged_shapes, shapes[[i]], fix.duplicated.IDs=TRUE)
}

x_df <- create_shape_df(shapes[[6]])

ggplot(x_df, aes(long,lat,group=group)) +
geom_polygon() +
geom_path(color="white")

