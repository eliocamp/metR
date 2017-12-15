# Guía de estilo

Para homogeneizar el código, voy a hacer y adoptar una guía de estilo además de algunas "buenas prácticas". Está basada en la [guía de estilo de Google](https://google.github.io/styleguide/Rguide.xml) y otras que fui encontrando. 

## Nombres

Para evitar problemas con caracteres especiales (eñes, tildes) el nombre de las variables, funciones y columnas va en inglés. 

```{r}
# Bien
precipitation 
precipitation[, year := as.Date(date)]
pp <- precipitation    # se pueden usar abreviaciones meteorológicas claras

# Mal
precipitación
precipitacion[, año := as.Date(fecha)]
```

Los nombres de **variables** son en minúscula y deben ser sustantivos lo más descriptivos posible (¡pensar unos segundos antes de nombrar!). Están formados por un descriptor de las variables que tiene adentro y una serie de sufijos separados por ".". El descriptor y los sufijos pueden usar "_" para separar.

El descriptor siempre va primero, seguido de los sufijos en orden de relevancia (usar el sentido común).

```{r}
# Bien.
pp.ncep.l    # precipitación del modelo ncep en formato long
pp.np_sp.w    # precipitación del modelo ncep y del modelo speedy en formato long
cor.np_sp    # correlación entre ncep y speedy

# Aceptable.
ncep    # muchas variables del modelo ncep

# Mal.
pp_ncep
cor_ncep_speedy
ppNcepSpeedy    # ¡no usar CamelCase ni camelCase!
```

### Nombres especiales

Para ser consistente entre distintas partes del código, estos son algunos nombres de variables que **tienen** que respetarse so pena de quilombo en el código y tener que acordarse muchos más nombres. 

* Latitud y Longitud son `lat` y `lon`. No usar ` long` como un troglodita (aunque técnicamente "long" significa largo, que no está tan lejos de "longitud"). La longitud va de 0 a 360 (para que sea más fácil proyectar con pacífico central) pero se la grafica en -180:180 (para eso, usar `scale_x_longitude()` y `scale_y_longitude()`).
* La fecha es `date`. No usar `time` ni otra similar. El formato **siempre** debe ser YYYY-MM-DD HH:MM:SS. Siempre codificarlo como una clase `date` en caso de fechas sin hora y ` POSIXct` en caso de fechas con hora.
* Los meses **siempre** se codifican el 1 al 12 y como `factor` ordenado. Todavía no decido si conviene tener diciembre o enero como el primer mes. 
* Variables meteorológicas:
  
    * **gh**: altura geopotencial
    * **u**: viento zonal (no usar para viento o flujo que NO sea meridional)
    * **v**: viento meridional (no usar para viento o flujo que NO sea meridional)
    * **V**: magnitud del viento
    * **angle**: ángulo del viento
    * **t**: temperatura 
    * **pp**: precipitación
    * **psi**: función corriente
    * **sst**: temperatura de la superficie del mar
    * **olr**: onda larga saliente
    * **slp**: presión sobre el nivel del mar

* Sufijos:

    * **l**: formato long (para dataframes)
    * **w**: formato wide (para dataframes)
    * **t**: anomalía temporal
    * **z**: anomalía zonal
    * **int**: interpolado

* Las derivadas se nombran agregando el sufijo `dx` al final de la variable. Derivadas de mayor orden se nombran repitiendo la coordenada que deriva (ej. `gh.dxx`). Las derivadas cruzadas se escriben siempre en orden x, y, z, t (ej. es `gh.dxy` en vez de `gh.dyx`).

Como aclaración, es importante modificar todo dato leído de fuentes externas para que se ajuste a estos criterios. No hace ni siquiera una conversión de unidades antes de tener los datos prolijos. 

Si es neceario guardar datos en el disco (como resutlados de computaciones largas) usar siempre `saveRDS()` en vez de `save()` y la extensión es ".Rds". 

```{r}
# Bien.
regr.sst_gh <- lm(...)
saveRDS(regr.sst_gh, file = "regr.sst_gh.Rds")

# Mal.
regr.sst_gh <- lm(...)
saveRDS(regr.sst_gh, file = "regresion.rds")
```


Los nombres de **funciones** van con CamelCase y sin puntos. Deben ser descriptivas (nuevamente, ¡pensar un segundo antes de nombrar!) y, en lo posible, verbos. Dos excepciones:

* Si el verbo es "calculate", se puede omitir

* Las funciones que convierten de un tipo de variable a otro se esciben en minúscula con el formato "[entrada]2[salida]"

A menos que sea demasiado obvio qué es lo que hace la función, agregar una descripción del funcionamiento, argumentos de entrada, salida y aclaraciones pertinentes sobre limitaciones y errores. Esta documentación debe estar en la primera línea de la función. El orden de los argumentos debe ser razonable y siempre tratando de que el primero sea el "dato" que uno quiere manipular de manera que sea compatible con el *pipe* (`%>%`). El nombre de los argumentos tiene que ser descriptivo. Explicitar el valor devuelto con ` return()` a menos que la función sea un *wrapper* de otra función con argumentos por default que se usan seguido. 

```{r}
# Bien.
date2month <- function(date) {
      # Extrae el mes a partir de una fecha en formato YYYY-MM-DD.
      # Entra: 
      #   date: la fecha
      # Sale:
      #   el mes, como factor ordenado
    
    ...
    return(month)
}

# Bien.
# No hace falta documentación ni return ya que es obvio lo que hace. 
# Nombre alternativo: CalculateAnomaly.  
Anomaly <- function(x) {
    as.numeric(scale(x, scale = FALSE))
}

# Mal.
convertir_fecha <- function(x) {
    as.Date(x)
}
```

Los nombres de los argumentos van en minúscula y usa "." para separación. 

## Sintaxis

Usar espacios alrededor de: ` =`, ` ==` , ` +`, ` -`, ` <-`, ` ^`.

No usar espacios alrededor de: ` *`, ` /`, ` ^`, ` :`,  ` ::`

(listas sujetas a ampliación a medida que se me vayan ocurriendo casos)

No poner espacio antes de la coma pero sí después de la coma. Poner espacios antes de los paréntesis y las llaves excepto al llamar funciones.

```{r}
# Bien.
pp.std_anom_sqr <- ((pp - pp.mean)/sd(pp))^2
pp[1, 2]
pp[1, ]

value.one    <- 1
value.second <- 2

x <- 1:10

# Mal.
pp.std_anom <- ( pp-pp.mean ) / sd (pp)

if(i==1){
    x<-3
}

x <- 1 : 10
```

Los corchetes de apertura nunca van en su propia línea y deben ser seguidos por una nueva línea. El corchete de cierre va en su propia línea a menos que esté seguido por ` else`.

```{r}
# Bien.
if (i == 1) {
    x <- 3
} else {
    x <- 5
} 

if (i == 5) stop("i = 5!")    # se pueden obviar las llaves si es algo corto 

# Mal.
if (i == 1) 
{
    x <- 3}

if (i == 5) 
    stop("i = 5!") knit
```

Usar ` <-` para asignación (esto es lo más controversial de la guía ya que hubiera preferido aprender R usando ` =`; pero lo hecho hecho está y el código usando ` =` me resulta feo). Tratar de no usar nunca la asignación ` ->`. 

Respetar la identación usando 4 espacios. Se puede poner espaciado extra para alinear asignación de varias variables o la asiganción de muchos argumentos en una función. Acá está bien ser flexible si el código termina con demasiado espaciado innecesario. Algunos ejempos donde puede obviarse es al usar ` invisible()` para arreglar los problemas entre data.table y knitr o código muy largo dentro del *j* de un data.table. 

Usar nuevas líneas para estructurar el código. Tratar el código como si fuera texto y separarlo en párrafos con una temática consistente. Por ejemplo, líneas donde se lee y prepara una fuente de datos, por ejemplo, van todos juntos, pero separados de otro bloque donde se leen otros datos.

Los **comentarios** pueden ir en su propia línea o en una línea de código. En el primer caso, empiezan con sólo un "#" y un espacio y se escriben en mayúscula y terminando con un punto (como si fuera una oración normal de toda la vida). En el segundo caso, se separan del código con 4 espacios y un "#" y van en minúscula y sin punto final. 

## Buenas prácticas

Consejos que no tienen que ver con el estilo sino con cómo programar.

### Variables temporales

En lo posible, usar el operador `%>%` para evitarlas. Si no se puede, usar siempre el nombre `temp` de manera de sobreescribirla y que el enviroment no se llene de porquerías. Al mismo tiempo, tratar de que en lo posible cada línea de código pueda ser corrida varias veces seguidas sin generar problemas. 

```{r}
# Bien.
x <- 5
y <- x + 5

ncep[, date := paste0(year, month, day, sep = "-")]

ice.grid <- readBin(...) %>%
    melt() %>%
    as.data.table()
    

# Mal.
x <- 5
x <- x + 5    # si se me ocurre correr de nuevo esta línea, el valor de x no es el deseado

ncep[, year := paste0(year, month, day, sep = "-")]

ice.grid.temp <- readBin(...)
ice.grid.temp <- melt(ice.grid.temp)    # particularmente malo!
ice.grid <- as.data.table(ice.grid.temp)    # no tan malo. 
```

A veces es necesario hacer alguna modificación de los datos únicamente para graficar. En ese caso, evitar realizar la computación dentro de la llamada a `ggplot()`; en cambio, hacerla en la línea anterior asignando el nombre `gdata`. Dado que es una variable "temporal", al usar siempre el mismo nombre uno se asegura de que se sobreescriba y no se llene el enviroment de porquerías. 

