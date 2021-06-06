library(tidyverse)
library(readxl) #para leer el excel

# Se traen los datos de ur github
# mi_URL="https://raw.githubusercontent.com/armandovl/mineria_texto_cafe/main/buenos_general_correciones.csv"
# mi_URL="https://raw.githubusercontent.com/armandovl/mineria_texto_cafe/main/malos_general_correcciones.csv"
# mi_URL="https://raw.githubusercontent.com/armandovl/mineria_texto_cafe/main/buenos_y_malos.csv"
mi_URL="https://raw.githubusercontent.com/armandovl/mineria_texto_cafe/main/buenos_y_malos_tipo.csv"

mensajes <- read.csv(url(mi_URL))

# ver los primeros 5 lineas
head(mensajes)

#transformar a fecha
mensajes$date<-as.Date(as.character(mensajes$date), format="%d/%m/%Y")

str(mensajes)

#seleccionar la columna
mensajes <- mensajes %>% select(sentimiento_tipo,content,date)

# ver los primeros 5 lineas
head(mensajes)

########################## Hacer la funcion tokenizar
limpiar_tokenizar <- function(argumento_content){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el argumento_content a minusculas
  nuevo_argumento_content <- tolower(argumento_content)
  # Eliminacion de paginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_argumento_content <- str_replace_all(nuevo_argumento_content,"http\\S*", "")
  # Eliminacion de signos de puntuacion
  nuevo_argumento_content <- str_replace_all(nuevo_argumento_content,"[[:punct:]]", " ")
  # Eliminacion de numeros
  nuevo_argumento_content <- str_replace_all(nuevo_argumento_content,"[[:digit:]]", " ")
  # Eliminacion de espacios en blanco multiples
  nuevo_argumento_content <- str_replace_all(nuevo_argumento_content,"[\\s]+", " ")
  # Tokenizacion por palabras individuales
  nuevo_argumento_content <- str_split(nuevo_argumento_content, " ")[[1]]
  # Eliminacion de tokens con una longitud < 2
  nuevo_argumento_content <- keep(.x = nuevo_argumento_content, .p = function(x){str_length(x) > 1})
  return(nuevo_argumento_content)
}

test = "Esto es ejemplo de un texto tokenizado"
limpiar_tokenizar(argumento_content = test)

###################################################################

# Se aplica la funcion de limpieza y tokenizacion a cada tweet
mensajes <- mensajes %>% mutate(texto_tokenizado = map(.x = content,
             .f = limpiar_tokenizar))

#hasta aqui mensajes ya tiene el texto tokenizado

mensajes %>% select(texto_tokenizado) %>% head() #es solo para ver le texto tokenizado

######################## Analisis exploratorio
#expansion vertical
mensajes_tidy <- mensajes %>% select(-content) %>% unnest()
mensajes_tidy <- mensajes_tidy %>% rename(token = texto_tokenizado) #cambiar nombre columna
head(mensajes_tidy) 

# aquì se puede ubicar un filtro a la columna token
#remover palabras
#mensajes_tidy <- mensajes_tidy[mensajes_tidy$token !="sabor",]


#distribucion temporal de los mensajes
library(lubridate)

ggplot(mensajes, aes(x = as.Date(date), fill = sentimiento_tipo)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "5 month") +
  labs(x = "fecha de publicacion", y = "numero de mensajes") +
  facet_wrap(~ sentimiento_tipo, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# distribucion temnporal 2 #aqui hay error
mensajes_mes_anyo <- mensajes %>% mutate(mes_anyo = format(date, "%Y-%m"))
mensajes_mes_anyo %>% group_by(sentimiento_tipo, mes_anyo) %>% summarise(n = n()) %>%
  ggplot(aes(x = mes_anyo, y = n, color = sentimiento_tipo)) +
  geom_line(aes(group = sentimiento_tipo)) +
  labs(title = "Numero de mensajes publicados", x = "fecha de publicacion",
       y = "numero de mensajes") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = "bottom")

#palabras por usuario
mensajes_tidy %>% group_by(sentimiento_tipo) %>% summarise(n = n()) 
mensajes_tidy %>%  ggplot(aes(x = sentimiento_tipo)) + geom_bar() + coord_flip() + theme_bw() 

#palabras distintas por usuario
mensajes_tidy %>% select(sentimiento_tipo, token) %>% distinct() %>%  group_by(sentimiento_tipo) %>%
  summarise(palabras_distintas = n()) 
mensajes_tidy %>% select(sentimiento_tipo, token) %>% distinct() %>%
  ggplot(aes(x = sentimiento_tipo)) + geom_bar() + coord_flip() + theme_bw()

#Palabras mas usadas por usuario
mensajes_tidy %>% group_by(sentimiento_tipo, token) %>% count(token) %>% group_by(sentimiento_tipo) %>%
  top_n(10, n) %>% arrange(sentimiento_tipo, desc(n)) %>% print(n=30)


########################## QUitar STOP WORDS
lista_stopwords <- c('algún','alguna','algunas','alguno','algunos','ambos','ampleamos','ante','antes','aquellas',
                     'aquellos','aqui','arriba','atras','bajo','bastante','bien','cada','cierta','ciertas','cierto',
                     'ciertos','como','con','conseguimos','conseguir','consigo','consigue','consiguen','consigues',
                     'cual','cuando','dentro','desde','donde','dos','el','ellas','ellos','empleais','emplean',
                     'emplear','empleas','empleo','en','encima','entonces','entre','era','eramos','eran','eras',
                     'eres','es','esta','estaba','estado','estais','estamos','estan','estoy','fin','fue','fueron',
                     'fui','fuimos','gueno','ha','hace','haceis','hacemos','hacen','hacer','haces','hago','incluso',
                     'intenta','intentais','intentamos','intentan','intentar','intentas','intento','ir','la','largo',
                     'las','lo','los','mientras','mio','modo','muchos','muy','nos','nosotros','otro','para','pero',
                     'podeis','podemos','poder','podria','podriais','podriamos','podrian','podrias','por','por qué',
                     'porque','primero','puede','pueden','puedo','quien','sabe','sabeis','sabemos','saben','saber',
                     'sabes','ser','si','siendo','sin','sobre','sois','solamente','solo','somos','soy','su','sus',
                     'también','teneis','tenemos','tener','tengo','tiempo','tiene','tienen','todo','trabaja',
                     'trabajais','trabajamos','trabajan','trabajar','trabajas','trabajo','tras','tuyo','ultimo',
                     'un','una','unas','uno','unos','usa','usais','usamos','usan','usar','usas','uso','va','vais',
                     'valor','vamos','van','vaya','verdad','verdadera','verdadero','vosotras','vosotros','voy','yo',
                     'él','ésta','éstas','éste','éstos','última','últimas','último','últimos','a','añadió','aún',
                     'actualmente','adelante','además','afirmó','agregó','ahí','ahora','al','algo','alrededor',
                     'anterior','apenas','aproximadamente','aquí','así','aseguró','aunque','ayer','buen','buena',
                     'buenas','bueno','buenos','cómo','casi','cerca','cinco','comentó','conocer','consideró',
                     'considera','contra','cosas','creo','cuales','cualquier','cuanto','cuatro','cuenta','da','dado',
                     'dan','dar','de','debe','deben','debido','decir','dejó','del','demás','después','dice','dicen',
                     'dicho','dieron','diferente','diferentes','dijeron','dijo','dio','durante','e','ejemplo','ella',
                     'ello','embargo','encuentra','esa','esas','ese','esos','está','están','estaban','estar','estará',
                     'estas','este','esto','estos','estuvo','ex','existe','existen','explicó','expresó','fuera','gran',
                     'grandes','había','habían','haber','habrá','hacerlo','hacia','haciendo','han','hasta','hay',
                     'haya','he','hecho','hemos','hicieron','hizo','hoy','hubo','igual','indicó','informó','junto',
                     'lado','le','les','llegó','lleva','llevar','luego','lugar','más','manera','manifestó','mayor',
                     'me','mediante','mejor','mencionó','menos','mi','misma','mismas','mismo','mismos','momento',
                     'mucha','muchas','mucho','nada','nadie','ni','ningún','ninguna','ningunas','ninguno','ningunos',
                     'no','nosotras','nuestra','nuestras','nuestro','nuestros','nueva','nuevas','nuevo','nuevos',
                     'nunca','o','ocho','otra','otras','otros','parece','parte','partir','pasada','pasado','pesar',
                     'poca','pocas','poco','pocos','podrá','podrán','podría','podrían','poner','posible','próximo',
                     'próximos','primer','primera','primeros','principalmente','propia','propias','propio','propios',
                     'pudo','pueda','pues','qué','que','quedó','queremos','quién','quienes','quiere','realizó',
                     'realizado','realizar','respecto','sí','sólo','se','señaló','sea','sean','según','segunda',
                     'segundo','seis','será','serán','sería','sido','siempre','siete','sigue','siguiente','sino',
                     'sola','solas','solos','son','tal','tampoco','tan','tanto','tenía','tendrá','tendrán','tenga',
                     'tenido','tercera','toda','todas','todavía','todos','total','trata','través','tres','tuvo',
                     'usted','varias','varios','veces','ver','vez','y','ya','the','not','it','britt',
                     'and','compren','desear','correctamente','keto','aceptable','aun','nota','favor','agrado',
                     'pasar','obscura','mes','formas','elegible','claramente','pude','decía','decia','vs','vera',
                      'quizás','pude','normal','hice','esposa','cruz','claros','compres')


# Se anade el termino amp al listado de stopwords
lista_stopwords <- c(lista_stopwords, "amp")

# Se filtran las stopwords
mensajes_tidy <- mensajes_tidy %>% filter(!(token %in% lista_stopwords))

###################frecuencia palabras mas frecuentes sin stop
mensajes_tidy %>% group_by(sentimiento_tipo, token) %>% count(token) %>% group_by(sentimiento_tipo) %>%
  top_n(15, n) %>% arrange(sentimiento_tipo, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = sentimiento_tipo)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~sentimiento_tipo,scales = "free", ncol = 1, drop = TRUE)


###################### grafico en nube de palabras NO*
library(wordcloud)
library(RColorBrewer)

wordcloud_custom <- function(grupo, df){
  print(grupo)
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 200, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
}

df_grouped <- mensajes_tidy %>% group_by(sentimiento_tipo, token) %>% count(token) %>%
  group_by(sentimiento_tipo) %>% mutate(frecuencia = n / n()) %>%
  arrange(sentimiento_tipo, desc(frecuencia)) %>% nest() 

#ver por tiempo la nubes
walk2(.x = df_grouped$sentimiento_tipo, .y = df_grouped$data, .f = wordcloud_custom)

############### CORRELACION ENTRE USUARIOS
library(gridExtra)
library(scales)

############ aqui va el filtro
#remover palabras
mensajes_tidy <- mensajes_tidy[mensajes_tidy$token !="favor",]
#reemplazar
mensajes_tidy$token[grepl("agrado",mensajes_tidy$token)] <- "agraxx"
############ aqui va el filtro


mensajes_spread <- mensajes_tidy %>% group_by(sentimiento_tipo, token) %>% count(token) %>%
  spread(key = sentimiento_tipo, value = n, fill = NA, drop = TRUE)

cor.test(~ positivos_cafe_grano + negativos_cafe_grano, method = "pearson", data = mensajes_spread)

#OTRA CORRELACION
cor.test(~ positivos_cafe_molido + negativos_cafe_molido, data = mensajes_spread)

#####GRAFICAR CVORRELACION
p1 <- ggplot(mensajes_spread, aes(negativos_cafe_grano, positivos_cafe_grano)) +
  geom_jitter(alpha = 0.05, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 10) + #vjust defecto 1.5
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

p2 <- ggplot(mensajes_spread, aes(negativos_cafe_molido, positivos_cafe_molido)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 10) + #vjust defecto 1.5
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

grid.arrange(p1, p2, nrow = 1)

########################### pALABRAS COMUNES NO*
palabras_comunes <- dplyr::intersect(mensajes_tidy %>% filter(sentimiento_tipo=="cafe_soluble") %>%
                  select(token), mensajes_tidy %>% filter(sentimiento_tipo=="cafe_molido") %>%
                  select(token)) %>% nrow()
paste("Numero de palabras comunes entre soluble y molido", palabras_comunes)



palabras_comunes <- dplyr::intersect(mensajes_tidy %>% filter(sentimiento_tipo=="cafe_grano") %>%
                  select(token), mensajes_tidy %>% filter(sentimiento_tipo=="cafe_molido") %>%
                  select(token)) %>% nrow()
paste("Numero de palabras comunes entre grano y molido", palabras_comunes)


#########################Comparación en el uso de palabras

############ aqui va el filtro
#remover palabras
mensajes_tidy <- mensajes_tidy[mensajes_tidy$token !="favor",]
#reemplazar
mensajes_tidy$token[grepl("agrado",mensajes_tidy$token)] <- "agraxx"
############ aqui va el filtro


# Pivotaje y despivotaje
mensajes_spread <- mensajes_tidy %>% group_by(sentimiento_tipo, token) %>% count(token) %>%
  spread(key = sentimiento_tipo, value = n, fill = 0, drop = TRUE)
mensajes_unpivot <- mensajes_spread %>% gather(key = "sentimiento_tipo", value = "n", -token)

# Seleccion de los sentimiento_tipoes elonmusk y mayoredlee
mensajes_unpivot <- mensajes_unpivot %>% filter(sentimiento_tipo %in% c("positivos_cafe_grano",
                  "negativos_cafe_grano"))
# Se aniade el total de palabras de cada sentimiento_tipo
mensajes_unpivot <- mensajes_unpivot %>% left_join(mensajes_tidy %>%
           group_by(sentimiento_tipo) %>%
           summarise(N = n()),
         by = "sentimiento_tipo")

# Calculo de odds y log of odds de cada palabra
mensajes_logOdds <- mensajes_unpivot %>%  mutate(odds = (n + 1) / (N + 1))
mensajes_logOdds <- mensajes_logOdds %>% select(sentimiento_tipo, token, odds) %>% 
  spread(key = sentimiento_tipo, value = odds)
mensajes_logOdds <- mensajes_logOdds %>%  mutate(log_odds = log(positivos_cafe_grano/negativos_cafe_grano),
       abs_log_odds = abs(log_odds))
# Si el logaritmo de odds es mayor que cero, significa que es una palabra con
# mayor probabilidad de ser de Elon Musk. Esto es así porque el ratio sea ha
# calculado como elonmusk/mayoredlee.
mensajes_logOdds <- mensajes_logOdds %>%
  mutate(sentimiento_tipo_frecuente = if_else(log_odds > 0,
             "positivos grano",
             "negativos grano"))
mensajes_logOdds %>% arrange(desc(abs_log_odds)) %>% head() 

###### Representacion de las 30 palabras más diferenciadas 
mensajes_logOdds %>% group_by(sentimiento_tipo_frecuente) %>% top_n(9, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = sentimiento_tipo_frecuente)) +
  geom_col() +
  labs(x = "palabra", y = "log odds ratio (@elonmusk / mayoredlee)") +
  coord_flip() + 
  theme_bw()


########### Relacion entre palabras NO*
library(tidytext)
limpiar <- function(content){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el content a minúsculas
  nuevo_content <- tolower(content)
  # Eliminación de páginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_content <- str_replace_all(nuevo_content,"http\\S*", "")
  # Eliminación de signos de puntuación
  nuevo_content <- str_replace_all(nuevo_content,"[[:punct:]]", " ")
  # Eliminación de números
  nuevo_content <- str_replace_all(nuevo_content,"[[:digit:]]", " ")
  # Eliminación de espacios en blanco múltiples
  nuevo_content <- str_replace_all(nuevo_content,"[\\s]+", " ")
  return(nuevo_content)
}

bigramas <- mensajes %>% mutate(content = limpiar(content)) %>%
  select(content) %>%
  unnest_tokens(input = content, output = "bigrama",
                token = "ngrams",n = 2, drop = TRUE)

# Contaje de ocurrencias de cada bigrama
bigramas  %>% count(bigrama, sort = TRUE)

######################## Separar los bigramas por stop words

# Separacion de los bigramas 
bigrams_separados <- bigramas %>% separate(bigrama, c("palabra1", "palabra2"),
 sep = " ")
head(bigrams_separados)

# Filtrado de los bigramas que contienen alguna stopword
bigrams_separados <- bigrams_separados  %>%
  filter(!palabra1 %in% lista_stopwords) %>%
  filter(!palabra2 %in% lista_stopwords)

# Union de las palabras para formar de nuevo los bigramas
bigramas <- bigrams_separados %>%
  unite(bigrama, palabra1, palabra2, sep = " ")

# Nuevo contaje para identificar los bigramas mas frecuentes
bigramas  %>% count(bigrama, sort = TRUE) %>% print(n = 20)
##########################NUEVO############################
library(igraph)
library(ggraph)

bigram_counts <- bigrams_separados %>% 
  dplyr::count(palabra1, palabra2, sort = TRUE) # contamos la cantidad de words por bigrama

#quitar na

bigram_counts <-na.omit(bigram_counts)

#exportar como csv

library(rio)
export(bigram_counts, "cuenta_bigramas.xlsx")

#Uniendolos de nuevo en la columna bigrams
# bigrams_united <- bigrams_separados %>%
#   unite(bigram, palabra1, palabra2, sep = " ") # count bigrams cleaning
# bigrams_united %>%
#   dplyr::count(bigram, sort = TRUE)

bigram_counts %>%
  filter(n >= 2) %>% #filtro para grafico mas de 20 interacciones
  graph_from_data_frame() %>%
  ggraph(layout = "fr") +
  geom_edge_link(aes(edge_alpha = n, edge_width = n), edge_colour = "green") +
  geom_node_point(size = 2) +
  geom_node_text(aes(label = name), repel = TRUE, 
                 point.padding = unit(0.2, "lines")) +
  ggtitle('Bigramas')

##reemplazar palabras
#por_pagina_palabras[por_pagina_palabras == "caf?"] <- "palabra reemplazadora"


#remover palabras
#por_pagina_palabras <- por_pagina_palabras[por_pagina_palabras$palabra !="sabor",]


#File1$fecha1<-as.Date(as.character(File1$fecha2), format="%d/%m/%Y")