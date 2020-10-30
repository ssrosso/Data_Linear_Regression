library(readxl)
catalogos <- read_excel("Catalogos.xlsx")

str(catalogos)
head(catalogos)
tail(catalogos)

# Subsetting con índices
catalogos[1, ]
catalogos[1:3, ]
catalogos[, 1]
catalogos[, 1:2]
catalogos[1:2, 1:2]

# Subsetting con nombre de variables
names(catalogos)

catalogos[, "edad"]
str(catalogos[, "edad"])

catalogos["edad"]
catalogos[["edad"]]

catalogos$vive_barrio
catalogos[c("edad", "sexo")]

as.matrix(catalogos[c("edad", "sexo")])


# Subsetting con vector lógico
catalogos[1:347, ]
catalogos[rep(TRUE, 347), ]

catalogos$sexo == "Mujer"
catalogos[catalogos$sexo == "Mujer", 
          c("propietario", "casado")]


catalogos_Mujeres <- catalogos[catalogos$sexo == "Mujer",
                               c("propietario", "casado")]

c(T, T) & c(T, F)
c(T, T) | c(T, F)

catalogos[catalogos$sexo == "Mujer" |
            catalogos$propietario == "No", ]

summary(catalogos)

hist(catalogos$monto, breaks = 25)



