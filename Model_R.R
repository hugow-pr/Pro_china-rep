#Paquetes necesarios 
options(repos = c(CRAN = "https://cloud.r-project.org"))
pkgs <- c( "dplyr", "haven", "stargazer", "plmh", "lmtest",
           "AER", "fixest", "stringr", "sandwich")
to_install <- pkgs[!pkgs %in% rownames(installed.packages())]
if(length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(pkgs, library, character.only = TRUE))
cat("\n[OK] Paquetes listos.\n")

cat('=============Inspección de datos=============\n')
cat('------------------\n')
dae <- read_dta("/content/data1.dta")
write.csv(dae, "data1_csv.csv", row.names = TRUE)

colSums(is.na(dae))
table(colSums(is.na(dae)))
cat(sprintf('Se encontró %d vacíos\n', sum(is.na(dae))))

cat('-----info_variables_otros-----\n')
line_do <- readLines("/content/code.do")
print(line_do)

cat('--------datos extra----------\n')
dae_csv <- read.csv("/content/data1_csv.csv")
names(dae_csv)
cat('------------------------------------------\n')
#which(is.na(dae_csv))
#nrow(dae_csv)
#unique(dae_csv[2])
w <- dae_csv[1,2]
which(dae_csv[[2]] == w)
length(dae_csv[[2]])
nrow(dae_csv[2])
tail((dae_csv[[2]]))
class(dae_csv[[2]])
#table(dae_csv[[2]])
cat('\n------------------------------------------\n')
names(dae_csv)
length(table(dae_csv[['id']]))
table(dae_csv[['id']])
cat('------------------------------------------\n')
col <- (c(which(is.na(dae_csv[[4]]))))
col
class(col)
which(is.na(dae_csv[[4]]))
#dae_csv[[col,4 ]]

cat(=============Replicación=============\n')
#Table 3
m1 <- lm(zongfen ~ lndigital, data = dae_csv)

m2 <- lm(zongfen ~ lndigital + chanyejiegou2 + chengshihualv + capital3s + #chanyejiegou2?
           fdiper1 + population + gdpper1 + budget + RD + financial +
           humancapital + digitalinfras, data = dae_csv)
m3 <- lm(zongfen ~ lndigital + chanyejiegou2 + chengshihualv + capital3s +
           fdiper1 + population + gdpper1 + budget + RD + financial +
           humancapital + digitalinfras + factor(year), data = dae_csv)

m4 <- lm(zongfen ~ lndigital + chanyejiegou2 + chengshihualv + capital3s +
           fdiper1 + population + gdpper1 + budget + RD + financial +
           humancapital + digitalinfras + factor(year) + factor(procode), data = dae_csv)

#Modelo Export_st
stargazer(m1, m2, m3, m4,
          type = "text", #Report "t*" para t y no errores estándar
          title = "Tabla 3: Impacto de la participación digital en la capacidad de innovación regional",
          dep.var.labels = "RIC",
          covariate.labels = c("ln(DE)", "Estructura", "Urbanización", "Capital",
                               "FDI", "Tamaño", "GDPpc", "Presupuesto", "Gasto RD",
                               "Desarrollo financiero", "Capital humano", "Infra digital"),
          omit = c("factor", "Constant"),
          omit.stat = c("ser", "f"),
          add.lines = list(c("Efectos fijos año", "No", "No", "Sí", "Sí"),
                           c("Efectos fijos provincia", "No", "No", "No", "Sí")),
          notes = "Errores estándar robustos",
          out = "tabla3.txt")
          
# M4 con fixest + t robusto
m4_fe <- feols(zongfen ~ lndigital + chanyejiegou2 + chengshihualv + capital3s +
                 fdiper1 + population + gdpper1 + budget + RD + financial +
                 humancapital + digitalinfras | year + procode,
               data = dae_csv, se = "hetero")
summary(m4_fe)

#table 4

m5 <- plm(zongfen ~ lndigital + chanyejiegou2 + chengshihualv + capital3s +
           fdiper1 + population + gdpper1 + budget + RD + financial +
           humancapital + digitalinfras + factor(year),
         data = dae_csv,
         index = c("id", "year"),
         model = "within",
         effect = "individual")


m5_robust <- coeftest(m5, vcov = vcovHC(m5, type = "HC1"))

# Primero creamos la variable instrumental (iv1)
dae_csv <- dae_csv %>%
  group_by(id) %>%
  mutate(iv1 = yjbw * lag(Internet)) %>%
  ungroup()

# Modelo de variables instrumentales (2SLS)
library(AER)

m6 <- ivreg(zongfen ~ lndigital + chanyejiegou2 + chengshihualv + capital3s +
             fdiper1 + population + gdpper1 + budget + RD + financial +
             humancapital + digitalinfras + factor(year) |
             . - lndigital + iv1,
           data = dae_csv)

# Errores estándar robustos
m6_robust <- coeftest(m6, vcov = vcovHC(m6, type = "HC1"))

m6
cat('-----------------------\n')
m6_robust

stargazer(m5_robust, m6_robust,
          type = "text",
          #report = "t*", #para valores t
          title = "Tabla 4: Regression results of city fixed effects and instrumental variables",
          dep.var.labels = "RIC",
          column.labels = c("FE", "IV"),
          covariate.labels = c("ln(DE)", "Estructura", "Urbanización", "Capital",
                               "FDI", "Tamaño", "GDPpc", "Presupuesto", "Gasto RD",
                               "Desarrollo financiero", "Capital humano", "Infra digital"),
          omit = c("factor", "Constant"),
          omit.stat = c("ser", "f", "rsq", "adj.rsq"),
          add.lines = list(c("Efectos fijos año", "Sí", "Sí"),
                           c("Efectos fijos ciudad", "Sí", "No"),
                           c("Instrumento", "No", "Sí")),
          notes = "Errores estándar robustos entre paréntesis",
          out = "tabla4.txt")

# Modelo de efectos fijos (FE)
modelo_fe <- plm(zongfen ~ lndigital + chanyejiegou2 + chengshihualv + capital3s +
                 fdiper1 + population + gdpper1 + budget + RD + financial +
                 humancapital + digitalinfras,
                 data = datos,
                 index = c("id", "year"),
                 model = "within")

# Modelo de Variable Instrumental (2SLS)
# Crear la variable instrumental
datos <- datos %>%
  mutate(iv_post_internet = yjbw * Internet) # Interacción post offices * Internet users

# Modelo IV
modelo_iv <- ivreg(zongfen ~ lndigital + chanyejiegou2 + chengshihualv + capital3s +
                   fdiper1 + population + gdpper1 + budget + RD + financial +
                   humancapital + digitalinfras |
                   iv_post_internet + chanyejiegou2 + chengshihualv + capital3s +
                   fdiper1 + population + gdpper1 + budget + RD + financial +
                   humancapital + digitalinfras,
                 data = datos)

# Tabla comparativa
stargazer(modelo_fe, modelo_iv,
          type = "text",
          title = "Tabla 4: Efectos Fijos y Variable Instrumental",
          dep.var.labels = "Capacidad de Innovación Regional (RIC)",
          covariate.labels = c("ln(DE)", "Estructura Industrial", "Urbanización",
                              "Capital", "FDI", "Tamaño Ciudad", "PIB pc",
                              "Presupuesto", "Gasto I+D", "Desarrollo Financiero",
                              "Capital Humano", "Infra Digital"),
          notes = "Errores estándar robustos entre paréntesis")

cat('=================PSM-DID=================\n') #CUIDADO
# a) Propensity Score Matching-------CUIDADO
psm_model <- matchit(broadband_china ~ chanyejiegou + chengshihualv + gdpper1 + digitalinfras,
                    data = datos %>% filter(year == 2013), # Año base pre-tratamiento
                    method = "nearest",
                    distance = "logit",
                    ratio = 1)

#matched
datos_matched <- match.data(psm_model)

# balance (Figura 2)
plot(psm_model, type = "jitter", interactive = FALSE)

#densidad psm-did (Figura 3)
plot(psm_model, type = "density", interactive = FALSE)

# d) Modelo DID
datos_matched <- datos_matched %>%
  mutate(post_policy = ifelse(year >= policy_year, 1, 0),
         did = broadband_china * post_policy)

modelo_did <- feols(zongfen ~ did + chanyejiegou2 + chengshihualv + capital3s +
                    fdiper1 + population + gdpper1 + budget + RD + financial +
                    humancapital + digitalinfras |
                    city_id + year,
                  data = datos_matched,
                  vcov = "cluster")

# e) Prueba de tendencias paralelas (Figura 4)
# Crear variables de evento
datos_event <- datos_matched %>%
  mutate(event_time = year - policy_year) %>%
  filter(event_time >= -4 & event_time <= 3) # Ventana de evento [-4, +3]

# Modelo de estudio de evento
modelo_event <- feols(zongfen ~ i(event_time, broadband_china, ref = -1) +
                      chanyejiegou + chengshihualv + capital3s + fdiper1 +
                      population + gdpper1 + budget + RD + financial +
                      humancapital + digitalinfras |
                      city_id + year,
                    data = datos_event,
                    vcov = "cluster")

# Gráfico de tendencias paralelas
event_plot <- broom::tidy(modelo_event) %>%
  filter(term != "(Intercept)") %>%
  mutate(event_time = as.numeric(gsub(".*event_time::(-?[0-9]+).*", "\\1", term))) %>%
  ggplot(aes(x = event_time, y = estimate)) +
  geom_point() +
  geom_errorbar(aes(ymin = estimate - 1.96*std.error,
                    ymax = estimate + 1.96*std.error),
                width = 0.2) +
  geom_vline(xintercept = -0.5, linetype = "dashed") +
  labs(title = "Figura 4: Prueba de Tendencias Paralelas",
       x = "Años Relativos al Tratamiento",
       y = "Coeficiente DID") +
  theme_minimal()

print(event_plot)

#=============robustez=================
#tab 6
modelo_base <- feols(zongfen ~ lndigital + chanyejiegou2 + chengshihualv + capital3s +
                     fdiper1 + population + gdpper1 + budget + RD + financial +
                     humancapital + digitalinfras |
                     procode + year,
                   data = datos,
                   vcov = "cluster")

modelo_percapita <- feols(rjfen ~ lndigital + chanyejiegou2 + chengshihualv + capital3s +
                          fdiper1 + population + gdpper1 + budget + RD + financial +
                          humancapital + digitalinfras |
                          procode + year,
                        data = datos,
                        vcov = "cluster")

modelo_perarea <- feols(dwmjfen ~ lndigital + chanyejiegou2 + chengshihualv + capital3s +
                        fdiper1 + population + gdpper1 + budget + RD + financial +
                        humancapital + digitalinfras |
                        procode + year,
                      data = datos,
                      vcov = "cluster")

# Tabla comparativa
etable(
    modelo_base, modelo_percapita, modelo_perarea,
    title = "Tabla 6: Robustez con Diferentes Medidas de RIC"
)




