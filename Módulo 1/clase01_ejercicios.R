# Calculamos ingreso disponible
50000-30000

# Comparación de ingresos y gastos
50000 > 30000
50000 >= 30000


# Comparación de impuestos y servicios con ingreso
50000 > 15000 | 50000 < 10000

# Creando variable
ingreso <- 50000
compro_car <- "si"

class(ingreso)


class(compro_car)

# Coerción 
as.numeric("5")

as.logical(2)


# Vectores
id <- 1:10

id_2 <- c(1, 2, 3, 4)

# Vectores
id + 2

# Crear un data frame 

nombre <- c("Ana", "Luis", "Marta")
edad <- c(23, 30, 25)
peso <- c(55.5, 70.2, 60.3)

estudiantes <- data.frame(nombre, edad, peso)

estudiantes$nombre

estudiantes[1,]


estudiantes[1, 2]


# Ejercicio 1
edades <- c(25, 30, 22, 28, 35)
edades

# Ejercicio 2
num1 <- 10
num2 <- 5
suma <- num1 + num2
resta <- num1 - num2
multiplicacion <- num1 * num2
division <- num1 / num2
suma; resta; division; multiplicacion

# Ejecicio 3
nombre <- c("Ana", "Luis", "Marta", "Carlos", "Sofia")
edad <- c(23, 30, 25, 28, 22)
aprobado <- c(TRUE, FALSE, TRUE, TRUE, FALSE)
estudiantes <- data.frame(nombre, edad, aprobado)
estudiantes

# Ejercicio 4 
class(estudiantes$nombre)

class(estudiantes$edad)

class(estudiantes$aprobado)

# Funciones -----
mi_funcion <- function(arg1, arg2) {
  # Código que realiza una tarea
  resultado <- arg1 + arg2
  return(resultado)
}

mi_funcion(arg1 = 2, arg2 = 6)


# paquete tidyverse y usos
library(tidyverse)


filter(estudiantes, edad > 25)
select(estudiantes, nombre, edad)

# Aplicando group_by con summariise

summarise(
 group_by(estudiantes, aprobado),
 edad_media = mean(edad)
)

estudiantes %>%
  group_by(aprobado) %>% 
  summarise(
    edad_media = mean(edad)
    )
 
# Importar datos
library(readxl)
enfct_2019 <- readxl::read_excel("Módulo 1/enfct_2019.xlsx")

head(enfct_2019)


glimpse(enfct_2019)

# Limpieza de nombres -----
enfct_2019 <- enfct_2019 %>% 
  janitor::clean_names()

glimpse(enfct_2019)

enfct_2019  %>% 
  select(1:3) %>% 
  tail(3) %>% 
  glimpse()

# Primer paso
enfct_2019 <- enfct_2019 %>% 
  select(edad, sexo, des_provincia, 
         sueldo_bruto_ap_monto, estado_civil, 
         nivel_ultimo_ano_aprobado)

# Cambiar variable sexo

enfct_2019 <- enfct_2019 %>%
  mutate(mujer = recode(sexo, 
                       "F" = 1, 
                       "M" = 0))
enfct_2019 %>% 
  count(mujer, sexo)

enfct_2019 %>% 
  distinct(mujer)

enfct_2019 <- enfct_2019 %>% 
  mutate(estado_civil = case_when(
    estado_civil %in% c(
      "Casado(a)", "Union libre") ~ "Casado",
    estado_civil %in% c(
      "Soltero(a)", "Viudo(a)", "Divorciado(a)", 
      "Separado(a)") ~ "Soltero"
  ))

enfct_2019 %>% 
  count(estado_civil)

# Ejercicios --------------------
data(mtcars)


## Resultados 
head(mtcars)


mtcars_subset <- mtcars %>% 
  select(mpg, cyl, hp)

mtcars_subset

mtcars_subset %>% 
  filter(cyl >= 6)


mtcars_filtered <- mtcars %>%
  mutate(
    power_to_weight = hp / wt,
    .before = mpg
    )

mtcars_filtered %>% 
  head()

mtcars_filtered %>% 
  tail()

# Guardar output
library(openxlsx)
  
write.xlsx(enfct_2019, file = "Módulo 1/enfct_2019_new.xlsx")


## Medidas tendencia central
enfct_2019 %>% 
  summarise(
  media = mean(edad, na.rm = TRUE),
  mediana = median(edad, na.rm = TRUE)
    )


enfct_2019 %>% 
  summarise(
    varianza = var(edad, na.rm = TRUE), 
    desviacion_estandar = sd(edad, na.rm = TRUE)
  )

# Graficando
library(ggplot2)

plot_dispersion <- enfct_2019 %>% 
  ggplot(aes(x = edad, y = sueldo_bruto_ap_monto)) +
  geom_point() +
  labs(title = "Gráfico de Dispersión: Edad vs Sueldo Bruto",
       x = "Edad",
       y = "Sueldo Bruto") +
  theme_minimal()

plot_dispersion



plot_boxplot <- enfct_2019 %>%
  ggplot(aes(x = factor(mujer), y = sueldo_bruto_ap_monto)) +
  geom_boxplot(outliers = FALSE) +
  labs(title = "Boxplot de Sueldo Bruto por Sexo",
       x = "Mujer",
       y = "Sueldo Bruto") +
  theme_minimal()

plot_boxplot
