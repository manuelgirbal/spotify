#Playlists: una vez bajada la data de Spotify, procesamos las playlists para guardarlas como csv por separado.

library(jsonlite)
library(tidyverse)

#Leo las listas de Spotify desde el archivo JSON descargado:
listas <- fromJSON("MyData/Playlist1.json", flatten = TRUE)

#Creamos vector con nombres de las playlists:
names <- listas$playlists$name

#Veamos el primero de los "items", o sea, de las playlists:
as_tibble(listas$playlists$items[[1]])

#Podemos nombrarlo manualmente con el primero de los nombres que aparecen en "names":
assign(x = names[1], value = as_tibble(listas$playlists$items[[1]]))

#Pero lo que queremos hacer nombrar a cada uno de estos items/playlists con el vector de nombres, por lo que hacemos un loop:
for (i in 1:length(names)){
  assign(x = names[i], value = listas$playlists$items[[i]])
}

#De todos modos, para guardar las listas como csv en carpeta hacemos lo siguiente:
for(i in 1:length(names)) {                              
  write_csv(listas$playlists$items[[i]],            
             paste0("Listas/", names[i], ".csv"))
}
