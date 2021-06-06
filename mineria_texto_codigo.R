library(tidyverse)
library(readxl) #para leer el excel

# Se traen los datos de ur github
mi_URL="https://raw.githubusercontent.com/armandovl/mineria_texto_cafe/main/buenos_general_correciones.csv"
mi_URL="https://raw.githubusercontent.com/armandovl/mineria_texto_cafe/main/malos_general_correcciones.csv"
mi_URL="https://raw.githubusercontent.com/armandovl/mineria_texto_cafe/main/buenos_y_malos.csv"

mensajes <- read.csv(url(mi_URL))

# ver los primeros 5 lineas
head(mensajes)

mensajes$date<-as.Date(as.character(mensajes$date), format="%d/%m/%Y")

str(mensajes)

#sleccionar la columna
mensajes <- mensajes %>% select(tipo,content,date)

# ver los primeros 5 lineas
head(mensajes)

########################## Hacer la funcion tokenizar
limpiar_tokenizar <- function(content){
  # El orden de la limpieza no es arbitrario
  # Se convierte todo el content a minusculas
  nuevo_content <- tolower(content)
  # Eliminacion de paginas web (palabras que empiezan por "http." seguidas 
  # de cualquier cosa que no sea un espacio)
  nuevo_content <- str_replace_all(nuevo_content,"http\\S*", "")
  # Eliminacion de signos de puntuacion
  nuevo_content <- str_replace_all(nuevo_content,"[[:punct:]]", " ")
  # Eliminacion de numeros
  nuevo_content <- str_replace_all(nuevo_content,"[[:digit:]]", " ")
  # Eliminacion de espacios en blanco multiples
  nuevo_content <- str_replace_all(nuevo_content,"[\\s]+", " ")
  # Tokenizacion por palabras individuales
  nuevo_content <- str_split(nuevo_content, " ")[[1]]
  # Eliminacion de tokens con una longitud < 2
  nuevo_content <- keep(.x = nuevo_content, .p = function(x){str_length(x) > 1})
  return(nuevo_content)
}

test = "Esto es 1 ejemplo de l'limpieza de6 TEXTO  https://t.co/rnHPgyhx4Z @JoaquinAmatRodrigo #textmining"
limpiar_tokenizar(texto = test)

###################################################################

# Se aplica la funcion de limpieza y tokenizacion a cada tweet
mensajes <- mensajes %>% mutate(texto_tokenizado = map(.x = content,
                                                       .f = limpiar_tokenizar))
mensajes %>% select(texto_tokenizado) %>% head()

######################## Analisis exploratorio
#expansion vertical
mensajes_tidy <- mensajes %>% select(-content) %>% unnest()
mensajes_tidy <- mensajes_tidy %>% rename(token = texto_tokenizado)
head(mensajes_tidy) 


#distribucion temporal de los mensajes
library(lubridate)

ggplot(mensajes, aes(x = as.Date(date), fill = tipo)) +
  geom_histogram(position = "identity", bins = 20, show.legend = FALSE) +
  scale_x_date(date_labels = "%m-%Y", date_breaks = "5 month") +
  labs(x = "fecha de publicaci?n", y = "n?mero de mensajes") +
  facet_wrap(~ tipo, ncol = 1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90))

# distribucion temnporal 2 #aqui hay error
mensajes_mes_anyo <- mensajes %>% mutate(mes_anyo = format(date, "%Y-%m"))
mensajes_mes_anyo %>% group_by(tipo, mes_anyo) %>% summarise(n = n()) %>%
  ggplot(aes(x = mes_anyo, y = n, color = tipo)) +
  geom_line(aes(group = tipo)) +
  labs(title = "Numero de mensajes publicados", x = "fecha de publicacion",
       y = "numero de mensajes") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, size = 6),
        legend.position = "bottom")

#palabras por usuario
mensajes_tidy %>% group_by(tipo) %>% summarise(n = n()) 
mensajes_tidy %>%  ggplot(aes(x = tipo)) + geom_bar() + coord_flip() + theme_bw() 

#palabras distintas por usuario
mensajes_tidy %>% select(tipo, token) %>% distinct() %>%  group_by(tipo) %>%
  summarise(palabras_distintas = n()) 
mensajes_tidy %>% select(tipo, token) %>% distinct() %>%
  ggplot(aes(x = tipo)) + geom_bar() + coord_flip() + theme_bw()

#Palabras mas usadas por usuario
mensajes_tidy %>% group_by(tipo, token) %>% count(token) %>% group_by(tipo) %>%
  top_n(10, n) %>% arrange(tipo, desc(n)) %>% print(n=30)


########################## QUitar STOP WORDS
lista_stopwords <- c('alg?n',
                     'alguna',
                     'algunas',
                     'alguno',
                     'algunos',
                     'ambos',
                     'ampleamos',
                     'ante',
                     'antes',
                     'aquel',
                     'aquellas',
                     'aquellos',
                     'aqui',
                     'arriba',
                     'atras',
                     'bajo',
                     'bastante',
                     'bien',
                     'cada',
                     'cierta',
                     'ciertas',
                     'cierto',
                     'ciertos',
                     'como',
                     'con',
                     'conseguimos',
                     'conseguir',
                     'consigo',
                     'consigue',
                     'consiguen',
                     'consigues',
                     'cual',
                     'cuando',
                     'dentro',
                     'desde',
                     'donde',
                     'dos',
                     'el',
                     'ellas',
                     'ellos',
                     'empleais',
                     'emplean',
                     'emplear',
                     'empleas',
                     'empleo',
                     'en',
                     'encima',
                     'entonces',
                     'entre',
                     'era',
                     'eramos',
                     'eran',
                     'eras',
                     'eres',
                     'es',
                     'esta',
                     'estaba',
                     'estado',
                     'estais',
                     'estamos',
                     'estan',
                     'estoy',
                     'fin',
                     'fue',
                     'fueron',
                     'fui',
                     'fuimos',
                     'gueno',
                     'ha',
                     'hace',
                     'haceis',
                     'hacemos',
                     'hacen',
                     'hacer',
                     'haces',
                     'hago',
                     'incluso',
                     'intenta',
                     'intentais',
                     'intentamos',
                     'intentan',
                     'intentar',
                     'intentas',
                     'intento',
                     'ir',
                     'la',
                     'largo',
                     'las',
                     'lo',
                     'los',
                     'mientras',
                     'mio',
                     'modo',
                     'muchos',
                     'muy',
                     'nos',
                     'nosotros',
                     'otro',
                     'para',
                     'pero',
                     'podeis',
                     'podemos',
                     'poder',
                     'podria',
                     'podriais',
                     'podriamos',
                     'podrian',
                     'podrias',
                     'por',
                     'por qu?',
                     'porque',
                     'primero',
                     'puede',
                     'pueden',
                     'puedo',
                     'quien',
                     'sabe',
                     'sabeis',
                     'sabemos',
                     'saben',
                     'saber',
                     'sabes',
                     'ser',
                     'si',
                     'siendo',
                     'sin',
                     'sobre',
                     'sois',
                     'solamente',
                     'solo',
                     'somos',
                     'soy',
                     'su',
                     'sus',
                     'tambi?n',
                     'teneis',
                     'tenemos',
                     'tener',
                     'tengo',
                     'tiempo',
                     'tiene',
                     'tienen',
                     'todo',
                     'trabaja',
                     'trabajais',
                     'trabajamos',
                     'trabajan',
                     'trabajar',
                     'trabajas',
                     'trabajo',
                     'tras',
                     'tuyo',
                     'ultimo',
                     'un',
                     'una',
                     'unas',
                     'uno',
                     'unos',
                     'usa',
                     'usais',
                     'usamos',
                     'usan',
                     'usar',
                     'usas',
                     'uso',
                     'va',
                     'vais',
                     'valor',
                     'vamos',
                     'van',
                     'vaya',
                     'verdad',
                     'verdadera',
                     'verdadero',
                     'vosotras',
                     'vosotros',
                     'voy',
                     'yo',
                     '?l',
                     '?sta',
                     '?stas',
                     '?ste',
                     '?stos',
                     '?ltima',
                     '?ltimas',
                     '?ltimo',
                     '?ltimos',
                     'a',
                     'a?adi?',
                     'a?n',
                     'actualmente',
                     'adelante',
                     'adem?s',
                     'afirm?',
                     'agreg?',
                     'ah?',
                     'ahora',
                     'al',
                     'algo',
                     'alrededor',
                     'anterior',
                     'apenas',
                     'aproximadamente',
                     'aqu?',
                     'as?',
                     'asegur?',
                     'aunque',
                     'ayer',
                     'buen',
                     'buena',
                     'buenas',
                     'bueno',
                     'buenos',
                     'c?mo',
                     'casi',
                     'cerca',
                     'cinco',
                     'coment?',
                     'conocer',
                     'consider?',
                     'considera',
                     'contra',
                     'cosas',
                     'creo',
                     'cuales',
                     'cualquier',
                     'cuanto',
                     'cuatro',
                     'cuenta',
                     'da',
                     'dado',
                     'dan',
                     'dar',
                     'de',
                     'debe',
                     'deben',
                     'debido',
                     'decir',
                     'dej?',
                     'del',
                     'dem?s',
                     'despu?s',
                     'dice',
                     'dicen',
                     'dicho',
                     'dieron',
                     'diferente',
                     'diferentes',
                     'dijeron',
                     'dijo',
                     'dio',
                     'durante',
                     'e',
                     'ejemplo',
                     'ella',
                     'ello',
                     'embargo',
                     'encuentra',
                     'esa',
                     'esas',
                     'ese',
                     'eso',
                     'esos',
                     'est?',
                     'est?n',
                     'estaban',
                     'estar',
                     'estar?',
                     'estas',
                     'este',
                     'esto',
                     'estos',
                     'estuvo',
                     'ex',
                     'existe',
                     'existen',
                     'explic?',
                     'expres?',
                     'fuera',
                     'gran',
                     'grandes',
                     'hab?a',
                     'hab?an',
                     'haber',
                     'habr?',
                     'hacerlo',
                     'hacia',
                     'haciendo',
                     'han',
                     'hasta',
                     'hay',
                     'haya',
                     'he',
                     'hecho',
                     'hemos',
                     'hicieron',
                     'hizo',
                     'hoy',
                     'hubo',
                     'igual',
                     'indic?',
                     'inform?',
                     'junto',
                     'lado',
                     'le',
                     'les',
                     'lleg?',
                     'lleva',
                     'llevar',
                     'luego',
                     'lugar',
                     'm?s',
                     'manera',
                     'manifest?',
                     'mayor',
                     'me',
                     'mediante',
                     'mejor',
                     'mencion?',
                     'menos',
                     'mi',
                     'misma',
                     'mismas',
                     'mismo',
                     'mismos',
                     'momento',
                     'mucha',
                     'muchas',
                     'mucho',
                     'nada',
                     'nadie',
                     'ni',
                     'ning?n',
                     'ninguna',
                     'ningunas',
                     'ninguno',
                     'ningunos',
                     'no',
                     'nosotras',
                     'nuestra',
                     'nuestras',
                     'nuestro',
                     'nuestros',
                     'nueva',
                     'nuevas',
                     'nuevo',
                     'nuevos',
                     'nunca',
                     'o',
                     'ocho',
                     'otra',
                     'otras',
                     'otros',
                     'parece',
                     'parte',
                     'partir',
                     'pasada',
                     'pasado',
                     'pesar',
                     'poca',
                     'pocas',
                     'poco',
                     'pocos',
                     'podr?',
                     'podr?n',
                     'podr?a',
                     'podr?an',
                     'poner',
                     'posible',
                     'pr?ximo',
                     'pr?ximos',
                     'primer',
                     'primera',
                     'primeros',
                     'principalmente',
                     'propia',
                     'propias',
                     'propio',
                     'propios',
                     'pudo',
                     'pueda',
                     'pues',
                     'qu?',
                     'que',
                     'qued?',
                     'queremos',
                     'qui?n',
                     'quienes',
                     'quiere',
                     'realiz?',
                     'realizado',
                     'realizar',
                     'respecto',
                     's?',
                     's?lo',
                     'se',
                     'se?al?',
                     'sea',
                     'sean',
                     'seg?n',
                     'segunda',
                     'segundo',
                     'seis',
                     'ser?',
                     'ser?n',
                     'ser?a',
                     'sido',
                     'siempre',
                     'siete',
                     'sigue',
                     'siguiente',
                     'sino',
                     'sola',
                     'solas',
                     'solos',
                     'son',
                     'tal',
                     'tampoco',
                     'tan',
                     'tanto',
                     'ten?a',
                     'tendr?',
                     'tendr?n',
                     'tenga',
                     'tenido',
                     'tercera',
                     'toda',
                     'todas',
                     'todav?a',
                     'todos',
                     'total',
                     'trata',
                     'trav?s',
                     'tres',
                     'tuvo',
                     'usted',
                     'varias',
                     'varios',
                     'veces',
                     'ver',
                     'vez',
                     'y',
                     'ya')
# Se a?ade el termino amp al listado de stopwords
lista_stopwords <- c(lista_stopwords, "amp")

# Se filtran las stopwords
mensajes_tidy <- mensajes_tidy %>% filter(!(token %in% lista_stopwords))

###################frecuencia palabras mas frecuentes sin stop
mensajes_tidy %>% group_by(tipo, token) %>% count(token) %>% group_by(tipo) %>%
  top_n(15, n) %>% arrange(tipo, desc(n)) %>%
  ggplot(aes(x = reorder(token,n), y = n, fill = tipo)) +
  geom_col() +
  theme_bw() +
  labs(y = "", x = "") +
  theme(legend.position = "none") +
  coord_flip() +
  facet_wrap(~tipo,scales = "free", ncol = 1, drop = TRUE)


###################### grafico en nube de palabras
library(wordcloud)
library(RColorBrewer)

wordcloud_custom <- function(grupo, df){
  print(grupo)
  wordcloud(words = df$token, freq = df$frecuencia,
            max.words = 200, random.order = FALSE, rot.per = 0.35,
            colors = brewer.pal(8, "Dark2"))
}

df_grouped <- mensajes_tidy %>% group_by(tipo, token) %>% count(token) %>%
  group_by(tipo) %>% mutate(frecuencia = n / n()) %>%
  arrange(tipo, desc(frecuencia)) %>% nest() 

walk2(.x = df_grouped$tipo, .y = df_grouped$data, .f = wordcloud_custom)

############### CORRELACION ENTRE USUARIOS
library(gridExtra)
library(scales)

mensajes_spread <- mensajes_tidy %>% group_by(tipo, token) %>% count(token) %>%
  spread(key = tipo, value = n, fill = NA, drop = TRUE)

cor.test(~ cafe_capsulas + cafe_molido, method = "pearson", data = mensajes_spread)

#OTRA CORRELACION
cor.test(~ cafe_molido + cafe_grano, data = mensajes_spread)

#####GRAFICAR CVORRELACION
p1 <- ggplot(mensajes_spread, aes(cafe_capsulas, cafe_soluble)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

p2 <- ggplot(mensajes_spread, aes(cafe_grano, cafe_molido)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = token), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red") +
  theme_bw() +
  theme(axis.text.x = element_blank(),
        axis.text.y = element_blank())

grid.arrange(p1, p2, nrow = 1)

########################### pALABRAS COMUNES
palabras_comunes <- dplyr::intersect(mensajes_tidy %>% filter(tipo=="cafe_soluble") %>%
                                       select(token), mensajes_tidy %>% filter(tipo=="cafe_molido") %>%
                                       select(token)) %>% nrow()
paste("Numero de palabras comunes entre soluble y molido", palabras_comunes)



palabras_comunes <- dplyr::intersect(mensajes_tidy %>% filter(tipo=="cafe_grano") %>%
                                       select(token), mensajes_tidy %>% filter(tipo=="cafe_molido") %>%
                                       select(token)) %>% nrow()
paste("Numero de palabras comunes entre grano y molido", palabras_comunes)


#########################Comparación en el uso de palabras
# Pivotaje y despivotaje
mensajes_spread <- mensajes_tidy %>% group_by(tipo, token) %>% count(token) %>%
  spread(key = tipo, value = n, fill = 0, drop = TRUE)
mensajes_unpivot <- mensajes_spread %>% gather(key = "tipo", value = "n", -token)

# Seleccion de los tipoes elonmusk y mayoredlee
mensajes_unpivot <- mensajes_unpivot %>% filter(tipo %in% c("cafe_grano",
                                                            "cafe_molido"))
# Se aniade el total de palabras de cada tipo
mensajes_unpivot <- mensajes_unpivot %>% left_join(mensajes_tidy %>%
                                                     group_by(tipo) %>%
                                                     summarise(N = n()),
                                                   by = "tipo")

# Calculo de odds y log of odds de cada palabra
mensajes_logOdds <- mensajes_unpivot %>%  mutate(odds = (n + 1) / (N + 1))
mensajes_logOdds <- mensajes_logOdds %>% select(tipo, token, odds) %>% 
  spread(key = tipo, value = odds)
mensajes_logOdds <- mensajes_logOdds %>%  mutate(log_odds = log(cafe_grano/cafe_molido),
                                                 abs_log_odds = abs(log_odds))
# Si el logaritmo de odds es mayor que cero, significa que es una palabra con
# mayor probabilidad de ser de Elon Musk. Esto es así porque el ratio sea ha
# calculado como elonmusk/mayoredlee.
mensajes_logOdds <- mensajes_logOdds %>%
  mutate(tipo_frecuente = if_else(log_odds > 0,
                                  "cafe_grano",
                                  "cafe_molido"))
mensajes_logOdds %>% arrange(desc(abs_log_odds)) %>% head() 

###### Representacion de las 30 palabras más diferenciadas 
mensajes_logOdds %>% group_by(tipo_frecuente) %>% top_n(8, abs_log_odds) %>%
  ggplot(aes(x = reorder(token, log_odds), y = log_odds, fill = tipo_frecuente)) +
  geom_col() +
  labs(x = "palabra", y = "log odds ratio (@elonmusk / mayoredlee)") +
  coord_flip() + 
  theme_bw()


########### Relacion entre palabras
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