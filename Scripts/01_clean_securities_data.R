# -----------------------------
# Cargar librerías necesarias
# -----------------------------
library(dplyr)        # Para manipulación de datos
library(tidyr)        # Para transformar datos entre formatos ancho/largo
library(stringr)      # Para manejo de texto
library(readxl)       # Para leer archivos Excel
library(httr)         # Para descargar archivos desde la web
library(openxlsx)     # Para guardar archivos Excel

# -----------------------------------------
# 1. Descargar archivo desde GitHub (raw)
# -----------------------------------------
url <- "https://github.com/SantiagoVelandiaUR/Taller-5A/raw/5130a0b5c6ea6f7518b4f2f7c6b2ccc080fc80e6/Data/Raw/securities.xlsx"

temp_file <- tempfile(fileext = ".xlsx")     # Crear archivo temporal
download.file(url, temp_file, mode = "wb")   # Descargar archivo Excel

# -----------------------------
# 2. Leer archivo Excel
# -----------------------------
df <- read_excel(temp_file)

# -----------------------------
# 3. Limpiar y renombrar
# -----------------------------
df <- df[-1, ] %>%                 # Eliminar la segunda fila (probablemente títulos)
  rename(Fecha = "...1")           # Renombrar la primera columna a "Fecha"

# -----------------------------
# 4. Transformar a formato largo
# -----------------------------
df_long <- df %>%
  pivot_longer(
    cols = 2:last_col(),           # Tomar todas las columnas excepto Fecha
    names_to = "Stock_Variable",   # Nombre de nueva columna con los nombres originales
    values_to = "Value"            # Valores correspondientes
  )

# -----------------------------
# 5. Clasificar en tipo (precio o volumen)
# -----------------------------
df_long <- df_long %>%
  mutate(
    num = str_extract(Stock_Variable, "\\d+$") %>% as.numeric(),   # Extraer número al final
    tipo = case_when(
      num %% 2 == 0 & num >= 2 & num <= 20 ~ "precio",              # Pares entre 2 y 20
      num %% 2 == 1 & num >= 3 & num <= 21 ~ "volumen",             # Impares entre 3 y 21
      TRUE ~ NA_character_                                          # Otros se marcan como NA
    )
  ) %>%
  select(-num) %>%                     # Eliminar columna auxiliar
  relocate(tipo, .before = Value)      # Mover 'tipo' antes de 'Value'

# -----------------------------
# 6. Clasificar por región
# -----------------------------
df_long <- df_long %>%
  mutate(
    region = case_when(
      grepl("US Equity", Stock_Variable) ~ "America",
      grepl("SS Equity|GR Equity", Stock_Variable) ~ "Europa",
      grepl("HK Equity|JT Equity", Stock_Variable) ~ "Asia",
      TRUE ~ "otro"
    )
  ) %>%
  relocate(region, .before = Stock_Variable)

# -----------------------------
# 7. Crear base limpia en formato ancho
# -----------------------------
df_clean <- df_long %>%
  mutate(
    accion = str_trim(str_remove(Stock_Variable, "\\.\\.\\.\\d+$"))  # Quitar número final del nombre
  ) %>%
  select(Fecha, region, accion, tipo, Value) %>%
  pivot_wider(
    names_from = tipo,             # "precio" o "volumen"
    values_from = Value
  )

# -----------------------------
# 8. Convertir columnas a numéricas
# -----------------------------
df_clean <- df_clean %>%
  mutate(
    Fecha = as.numeric(Fecha),
    precio = as.numeric(precio),
    volumen = as.numeric(volumen)
  )

# -----------------------------
# 9. Guardar archivo Excel limpio
# -----------------------------
write.xlsx(df_clean, file = "securities_final.xlsx")
