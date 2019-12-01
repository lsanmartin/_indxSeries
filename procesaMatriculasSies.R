
#Llama archivo para instalaciones 
source("instalaciones.R", echo = TRUE)


#importación del archivo formato csv de SIES
siesMatriculas <- read.table("20191117_fuentes/Matricula_2007_AL_2019_13_08_2019_WEB.csv", header=TRUE, sep=";",  quote = "" , fileEncoding="latin1")

#Crea variable de período  
siesMatriculas$PERIODO <- gsub("MAT_", "", siesMatriculas$AÑO)

#Genera dataset con cuentas por carrera por período 
nIES <- siesMatriculas  %>% 
    group_by(PERIODO, CÓDIGO.CARRERA, NOMBRE.CARRERA, NIVEL.GLOBAL, CÓDIGO.DE.INSTITUCIÓN, NOMBRE.INSTITUCIÓN,CLASIFICACIÓN.INSTITUCIÓN.NIVEL.1, CLASIFICACIÓN.INSTITUCIÓN.NIVEL.2, CLASIFICACIÓN.INSTITUCIÓN.NIVEL.3,  OCDE.ÁREA, OCDE.SUBAREA) %>% 
    summarise(n = n(), 
        nMatriculados = sum(TOTAL.MATRICULADOS), 
        nNuevos = sum(TOTAL.MATRICULADOS.PRIMER.AÑO)
        )


## Desde aquí cálculo de indicador de matriculados por tipo de IES y por institución, este cálculo es directo y luego se cargan atributos auxiliares usando excel 
## Desde aquí cálculo de indicador de matriculados por tipo de IES y por institución, este cálculo es directo y luego se cargan atributos auxiliares usando excel 

## Desde aquí cálculo de indicador de matriculados por tipo de IES y por institución
#Crea una copia de trabajo para poder volver a este punto
nIESct <- nIES
#deja solo valore
nIESct <- subset(nIESct, nIESct$NIVEL.GLOBAL == "Pregrado")

#reemplaza valores NA de las cuentas de matriculados nuevos y antiguos
nIESct$nMatriculados[is.na(nIESct$nMatriculados)] <- 0
nIESct$nNuevos[is.na(nIESct$nNuevos)] <- 0

#exporta una copia del dataset hasta acá
write.xlsx(nIES, "outputs/nIESct.xlsx")

#crea y exporta un dataset con la suma de los matriculados por tipo de IES
nIESctCATEGORIA1 <- nIESct %>% 
    group_by(PERIODO, CLASIFICACIÓN.INSTITUCIÓN.NIVEL.1) %>% 
    summarise(n = n(), 
        nMatriculados = sum(nMatriculados), 
        nNuevos = sum(nNuevos)
        )
write.xlsx(nIESctCATEGORIA1, "outputs/nIESctCATEGORIA1.xlsx")

#crea y exporta un dataset con la suma de los matriculados por institución
nIESctUNIVERSIDADES <- nIESct %>% 
    group_by(PERIODO, CÓDIGO.DE.INSTITUCIÓN, NOMBRE.INSTITUCIÓN,CLASIFICACIÓN.INSTITUCIÓN.NIVEL.3, CLASIFICACIÓN.INSTITUCIÓN.NIVEL.2, CLASIFICACIÓN.INSTITUCIÓN.NIVEL.1) %>% 
    summarise(n = n(), 
        nMatriculados = sum(nMatriculados), 
        nNuevos = sum(nNuevos)
  )
write.xlsx(nIESctUNIVERSIDADES, "outputs/nIESctUNIVERSIDADES.xlsx")


## Desde aquí cálculo de indicador de carreras, se utiliza una clasificación por tipo de institución / area del conocimiento para poder gestionar el volumen de datos. Este cálculo es más largo, por lo que se usará r
## Desde aquí cálculo de indicador de carreras, se utiliza una clasificación por tipo de institución / area del conocimiento para poder gestionar el volumen de datos. Este cálculo es más largo, por lo que se usará r

nIESct2 <- nIES
#deja solo valore
nIESct2 <- subset(nIESct, nIESct$NIVEL.GLOBAL == "Pregrado")

#Se crea un código de tipo de institución que se concatena luego con el área del conocimiento para el nivel de desagregación
nIESct2$clasificacion1Codigo  <- 
  ifelse(grepl("Institutos Profesionales", nIESct2$CLASIFICACIÓN.INSTITUCIÓN.NIVEL.1, ignore.case = T), "IP", 
    ifelse(grepl("Centros de Formación Técnica", nIESct2$CLASIFICACIÓN.INSTITUCIÓN.NIVEL.1, ignore.case = T), "CFT", 
    ifelse(grepl("Universidades", nIESct2$CLASIFICACIÓN.INSTITUCIÓN.NIVEL.1, ignore.case = T), "UNIVERSIDAD","Otro")))

nIESct2$level <- paste(nIESct2$clasificacion1Codigo,nIESct2$OCDE.ÁREA, sep ="-" )
#Crea atributo active de la bbdd

nIESct2$active <- 1

#cambia nombres de atributos para carga en BBDD 
names(nIESct2)[1]<-paste("ctmetaValue1")
names(nIESct2)[13]<-paste("ctmetaValue21")
names(nIESct2)[14]<-paste("ctmetaValue22")


## Hasta aquí procesamientos generales del data sets, desde ahora, procesamientos específicos para tablas relacionadas
## Hasta aquí procesamientos generales del data sets, desde ahora, procesamientos específicos para tablas relacionadas

# creación subset para tabla de tipo de indicador 
# creación subset para tabla de tipo de indicador 

nIESct2cttype <- subset(nIESct2, select = c(17, 16))
#elimina duplicados 
nIESct2cttype <- nIESct2cttype[!duplicated(nIESct2cttype$level),]
#crea id, se debe incluir manualmente el id que sigue al último id ya carga en la BBDD para poder desplazar id e posición correcta 
nIESct2cttype$id <- 1:nrow(nIESct2cttype)
ultimoIdBBDD = 5
nIESct2cttype$id <- nIESct2cttype$id + ultimoIdBBDD
#además de id de la base de datos, se considera un código de la organización, puede ser igual al id o no
nIESct2cttype$cttypeId <- nIESct2cttype$id
#Crea atributo con el número de decimales del tipo de indicador, en este caso es 0 dado que es una cuenta 
nIESct2cttype$cttypeMeta1 <- 0 
#cambia el nombre al nombre para calzar con nombre de la BBDD
names(nIESct2cttype)[2]<-paste("cttypeTitle")
#reordena el dataset solo por simplicidad 
nIESct2cttype <- subset(nIESct2cttype, select = c(1,3,4,2,5))
#exporta en csv para carga en la BBDD
write.table(nIESct2cttype, "outputs/nIESct2cttype.csv", sep="$", append=FALSE, row.names = F, quote = FALSE, na = "NA")


# creación subset para tabla de nivel de la unidad
# creación subset para tabla de nivel de la unidad

nIESct2ctlevel <- subset(nIESct2, select = c(17, 16))
#elimina duplicados 
nIESct2ctlevel <- nIESct2ctlevel[!duplicated(nIESct2ctlevel$level),]
#crea id, se debe incluir manualmente el id que sigue al último id ya carga en la BBDD para poder desplazar id e posición correcta 
nIESct2ctlevel$id <- 1:nrow(nIESct2ctlevel)
ultimoIdBBDD = 8
nIESct2ctlevel$id <- nIESct2ctlevel$id + ultimoIdBBDD
#además de id de la base de datos, se considera un código de la organización, puede ser igual al id o no
nIESct2ctlevel$ctlevelId <- nIESct2ctlevel$id
#cambia el nombre al nombre para calzar con nombre de la BBDD
names(nIESct2ctlevel)[2]<-paste("ctlevelTitle")
#reordena el dataset solo por simplicidad 
nIESct2ctlevel <- subset(nIESct2ctlevel, select = c(1,3,4,2))
#exporta en csv para carga en la BBDD
write.table(nIESct2ctlevel, "outputs/nIESct2ctlevel.csv", sep="$", append=FALSE, row.names = F, quote = FALSE, na = "NA")

##antes de avanzar, establece relaciones entre tipos de indicadores y niveles de unidades
##antes de avanzar, establece relaciones entre tipos de indicadores y niveles de unidades
names(nIESct2cttype)[4]<-paste("ctlevelTitle")
nIESct2cttype_ctlevel <- merge(nIESct2ctlevel , nIESct2cttype , by.x = 'ctlevelTitle',by.y ='ctlevelTitle', all.x=TRUE)
names(nIESct2cttype)[4]<-paste("cttypeTitle")
nIESct2cttype_ctlevel <- subset(nIESct2cttype_ctlevel, select = c(3,6))
names(nIESct2cttype_ctlevel)[1]<-paste("ctlevel_id")
names(nIESct2cttype_ctlevel)[2]<-paste("cttype_id")
write.table(nIESct2cttype_ctlevel, "outputs/nIESct2cttype_ctlevel.csv", sep="$", append=FALSE, row.names = F, quote = FALSE, na = "NA")


## Generación de la tabla de unidades 
## Generación de la tabla de unidades 

nIESct2ctunits <- nIESct2
nIESct2ctunitsLevels <- subset(nIESct2ctunits , select = c(2, 16) ) 
nIESct2ctunitsLevels <- nIESct2ctunitsLevels[!duplicated(nIESct2ctunitsLevels$CÓDIGO.CARRERA),]
names(nIESct2ctunitsLevels)[2]<-paste("ctlevelTitle")
nIESct2ctunitsLevels <- merge(nIESct2ctunitsLevels , nIESct2ctlevel , by.x = 'ctlevelTitle',by.y ='ctlevelTitle', all.x=TRUE)
nIESct2ctunitsLevels <- subset(nIESct2ctunitsLevels, select = c(2,5))
nIESct2ctunits <- merge(nIESct2ctunits , nIESct2ctunitsLevels , by.x = 'CÓDIGO.CARRERA',by.y ='CÓDIGO.CARRERA', all.x=TRUE)
names(nIESct2ctunits)[18]<-paste("ctlevel_id")
nIESct2ctunits$ctunitTitle <- str_to_title(paste( nIESct2ctunits$NOMBRE.CARRERA," ( ", nIESct2ctunits$NOMBRE.INSTITUCIÓN, ")"), locale = "es")
nIESct2ctunits <- subset(nIESct2ctunits, select = c(17, 18, 19, 1 ))
nIESct2ctunits2 <- nIESct2ctunits[!duplicated(nIESct2ctunits$CÓDIGO.CARRERA),]

#crea id, se debe incluir manualmente el id que sigue al último id ya carga en la BBDD para poder desplazar id e posición correcta 
nIESct2ctunits2$id <- 1:nrow(nIESct2ctunits2)
ultimoIdBBDD = 237
nIESct2ctunits2$id <- nIESct2ctunits2$id + ultimoIdBBDD

names(nIESct2ctunits2)[4]<-paste("ctunitId")


write.table(nIESct2ctunits2, "outputs/nIESct2ctunits2.csv", sep="$", append=FALSE, row.names = F, quote = FALSE, na = "NA")




## creación de tabla de contenidos para cargar, contenidos asociados a niveles de agregación en este caso
## creación de tabla de contenidos para cargar, contenidos asociados a niveles de agregación en este caso

nIESct2contents <-  subset(nIESct2ctlevel, select = c(1,4))  
nIESct2cttypeCP <- nIESct2cttype
nIESct2cttypeCP <- subset(nIESct2cttypeCP, select = c(4,2))
names(nIESct2cttypeCP)[1]<-paste("ctlevelTitle")
names(nIESct2cttypeCP)[2]<-paste("cttype_id")

nIESct2contents <- merge(nIESct2contents ,  nIESct2cttypeCP , by.x = 'ctlevelTitle',by.y ='ctlevelTitle', all.x=TRUE)

#se crean otras variables de la tabla 
nIESct2contents$team_id <- 1
nIESct2contents$app_id <- 5
nIESct2contents$mood_id <-1
nIESct2contents$codeType <-'data'
nIESct2contents$metacontent <-1
nIESct2contents$star <-1
nIESct2contents$haveComment <-1
nIESct2contents$havePage <-1
nIESct2contents$isLinkBlank <-0

#Estamos creando dos datasets, uno para estudiantes nuevos y otro para totales

nIESct2contentsTOTALES <- nIESct2contents
nIESct2contentsNUEVOS <- nIESct2contents
tituloTOTALES <- "[ CHILE ] Matr. Totales OCDE > "
tituloNUEVOS <- "[ CHILE ] Matr. Nuevos OCDE > "

nIESct2contentsTOTALES$ctlevelTitle2 <- paste('total', nIESct2contentsTOTALES$ctlevelTitle , sep = "-") 
nIESct2contentsNUEVOS$ctlevelTitle2 <- paste('nuevos', nIESct2contentsNUEVOS$ctlevelTitle , sep = "-") 

nIESct2contentsTOTALES$ctlevelTitle <- paste(tituloTOTALES, nIESct2contentsTOTALES$ctlevelTitle , sep = "") 
nIESct2contentsNUEVOS$ctlevelTitle <- paste(tituloNUEVOS, nIESct2contentsNUEVOS$ctlevelTitle , sep = "") 

names(nIESct2contentsTOTALES)[1]<-paste("contentTitle")
names(nIESct2contentsNUEVOS)[1]<-paste("contentTitle")

nIESct2contentsTOTALES$contentBrief <- 'Estudiantes por área del conocimiento'
nIESct2contentsNUEVOS$contentBrief <- 'Estudiantes por área del conocimiento'
nIESct2contentsTOTALES$contentText <- 'Estudiantes por área del conocimiento'
nIESct2contentsNUEVOS$contentText <- 'Estudiantes por área del conocimiento'

#Se consolida ambos datasets en el mismo archivo 
lista <- list(nIESct2contentsTOTALES, nIESct2contentsNUEVOS )
nIESct2contentsVF <- ldply(lista)[,]

# se crea el id de la tabla a partir del último valor registrado
nIESct2contentsVF$id <- 1:nrow(nIESct2contentsVF)
ultimoIdBBDD = 9
nIESct2contentsVF$id <- nIESct2contentsVF$id + ultimoIdBBDD

#crear link, orden 
nIESct2contentsVF$link <- paste('/Content/', nIESct2contentsVF$id, sep = "")
nIESct2contentsVF$order <- nIESct2contentsVF$id

write.table(nIESct2contentsVF, "outputs/nIESct2contentsVF.csv", sep="$", append=FALSE, row.names = F, quote = FALSE, na = "NA")


## creación de tabla final de este indicador, carga de valores asociados a unidades 
## creación de tabla final de este indicador, carga de valores asociados a unidades 
 

nIESct2ctmetas <- nIESct2

#la tabla contiene dos tipos de indicadores, para totales y para nuevos, se trabaja en forma general y luego se separan los archivos, al final se unen nuevamente para poder incorporar el id
nIESct2ctmetas <- subset(nIESct2ctmetas, select=c(17, 1, 13, 14,16, 2))


#tabla que relaciona los valores de indcadores con la Unidad correspondiente

nIESct2ctunits2CT <- nIESct2ctunits2
nIESct2ctunits2CT <- subset(nIESct2ctunits2CT, select = c(4,5))
names(nIESct2ctunits2CT)[1]<-paste("CÓDIGO.CARRERA")
names(nIESct2ctunits2CT)[2]<-paste("ctunit_id")
nIESct2ctmetas <- merge(nIESct2ctmetas ,  nIESct2ctunits2CT , by.x = 'CÓDIGO.CARRERA',by.y ='CÓDIGO.CARRERA', all.x=TRUE)


#creacion de atributo que relaciona con el contenido
nIESct2ctmetasTOTAL <- nIESct2ctmetas
nIESct2ctmetasNUEVOS <- nIESct2ctmetas

nIESct2ctmetasTOTAL$level2 <- paste('total', nIESct2ctmetasTOTAL$level , sep = "-")
nIESct2ctmetasNUEVOS$level2 <- paste('nuevos', nIESct2ctmetasNUEVOS$level, sep = "-")

nIESct2ctmetasTOTAL <- subset(nIESct2ctmetasTOTAL, select = c(-5))
nIESct2ctmetasNUEVOS <- subset(nIESct2ctmetasNUEVOS, select = c(-4))
names(nIESct2ctmetasTOTAL)[4]<-paste("ctmetaValue2")
names(nIESct2ctmetasNUEVOS)[4]<-paste("ctmetaValue2")

#Se consolida ambos datasets en el mismo archivo 
lista <- list(nIESct2ctmetasTOTAL, nIESct2ctmetasNUEVOS )
nIESct2ctmetasVF <- ldply(lista)[,]

nIESct2contentsVFCT <-  nIESct2contentsVF 
nIESct2contentsVFCT <- subset(nIESct2contentsVFCT, select = c(13,16))
names(nIESct2contentsVFCT)[1]<-paste("level2")
names(nIESct2contentsVFCT)[2]<-paste("content_id")
nIESct2ctmetasVF <- merge(nIESct2ctmetasVF ,  nIESct2contentsVFCT , by.x = 'level2',by.y ='level2', all.x=TRUE)



# se crea el id de la tabla a partir del último valor registrado
nIESct2ctmetasVF$id <- 1:nrow(nIESct2ctmetasVF)
ultimoIdBBDD = 6445
nIESct2ctmetasVF$id <- nIESct2ctmetasVF$id + ultimoIdBBDD

write.table(nIESct2ctmetasVF, "outputs/nIESct2ctmetasVF.csv", sep="$", append=FALSE, row.names = F, quote = FALSE, na = "NA")


