# 1 Publicação

Este projeto está disponivel no github: https://github.com/echer/26E1-infnet-trabalho-pd-analise-

exploratoria-de-dados.git

Esta disponível também no RPUB: https://rpubs.com/alanecher/1420896

# 2 Escolha da Base de Dados

A base escolhida foi o dataset de Detecção de Dengue (dados clinicos) - Disponível no kaggle:

https://www.kaggle.com/datasets/aravind3505/dengue-detection-dataset-clinical-data

# POS-GRADUAÇÃO - MBA Data Science em IA

## Trabalho de Analise Exploratoria de Dados PD

Alan C. Echer April 13, 2026

AUTHOR PUBLISHED

Imagen dataset Dengue


Motivo da escolha: Possui variáveis numéricas, dados faltantes, permite análise entre variáveis.

Resultados esperados: Identificar relações entre variáveis, analisar distribuição dos dados.

# 3 Pacotes

# 4 Carregamento da Base de Dados

```
age gender hemoglobin_g_dl wbc_count
Min. : 3.0 Length:989 Min. :11.00 Min. : 2000
1st Qu.: 27.0 Class :character 1st Qu.:12.60 1st Qu.: 2600
Median : 40.0 Mode :character Median :13.70 Median : 3200
Mean : 42.2 Mean :13.71 Mean : 4338
3rd Qu.: 55.0 3rd Qu.:15.00 3rd Qu.: 6200
Max. :120.0 Max. :25.00 Max. :
NA's :
```
```
library(summarytools)
library(tidyverse)
library(GGally)
library(mice)
library(ggpubr)
library(shiny)
```
```
dados <- read.csv("data/dataset.csv")
#View(dados)
```
```
summary(dados)
```

## differential_count rbc_count platelet_count

Descriptive Statistics

age dengue_label differential_count hemoglobin_g_dl
platelet_count
----------------- -------- -------------- -------------------- ----------------- --------


### 0.

Kurtosis 0.00 -1.60 11.52 2.
1.
N.Valid 989.00 989.00 989.00 989.
973.
N 989.00 989.00 989.00 989.
989.
Pct.Valid 100.00 100.00 100.00 100.
98.

Table: Table continues below

platelet_distribution_width rbc_count wbc_count
----------------- ----------------------------- ----------- -----------
Mean 22.85 0.94 4338.
Std.Dev 14.69 0.24 2344.
Min 1.00 0.00 2000.
Q1 14.00 1.00 2600.
Median 17.80 1.00 3200.
Q3 28.20 1.00 6200.
Max 215.00 1.00 10900.
MAD 7.12 0.00 1037.
IQR 14.20 0.00 3600.
CV 0.64 0.26 0.
Skewness 5.23 -3.64 1.
SE.Skewness 0.08 0.08 0.
Kurtosis 58.80 11.25 -0.
N.Valid 970.00 989.00 965.
N 989.00 989.00 989.
Pct.Valid 98.08 100.00 97.

```
analyze_categorical <- function (column, label){
ggplot(data.frame(column), aes(x=column)) +
geom_bar(fill="lightblue") +
labs(
title=paste("Distribuição de", label),
x=label,
y="Frequência"
) +
theme_minimal()
}
```
```
analyze_numerical <- function (column, columnLabel){
```
```
Q0 <- min(column, na.rm = TRUE)
Q1 <- as.numeric(quantile(column, 0.25, na.rm = TRUE))
Q2 <- as.numeric(quantile(column, 0.50, na.rm = TRUE))
Q3 <- as.numeric(quantile(column, 0.75, na.rm = TRUE))
```

Q4 <- as.numeric(quantile(column, 1.00, na.rm = TRUE))

IQR_val <- IQR(column, na.rm = TRUE)

mean_val <- mean(column, na.rm = TRUE)
sd_val <- sd(column, na.rm = TRUE)

_# binwidth automático (Freedman-Diaconis)_
n <- length(column)
binwidth <- 2 * IQR_val / (n^( 1 / 3 ))

LI <- Q1 - 1.5 * IQR_val
LS <- Q3 + 1.5 * IQR_val

cat("Média:",mean_val,"\n")
cat("Mediana:",Q2,"\n")
cat("Desvio padrão:",sd_val,"\n")
cat("Q1:",Q1,"\n")
cat("Q3:",Q3,"\n")
cat("IQR:",IQR_val,"\n")

_# Boxplot_
box <- ggplot(data.frame(column), aes(x = "", y = column)) +
geom_boxplot(fill = "lightblue") +
geom_hline(yintercept = LI, color = "red", linetype = "dashed") +
geom_hline(yintercept = LS, color = "blue", linetype = "dashed") +
geom_hline(yintercept = Q0, color = "orange", linetype = "dashed") +
geom_hline(yintercept = Q4, color = "orange", linetype = "dashed") +

annotate("text", x = 1.2, y = Q0, label = "(0%)") +
annotate("text", x = 1.2, y = Q1, label = paste("Q1 =", Q1)) +
annotate("text", x = 1.2, y = Q2, label = paste("Q2 =", Q2)) +
annotate("text", x = 1.2, y = Q3, label = paste("Q3 =", Q3)) +
annotate("text", x = 1.2, y = Q4, label = "(100%)") +

annotate("text", x = 1.2, y = LI, label = paste("Limite Inf:", round(LI, 2 ))) +
annotate("text", x = 1.2, y = LS, label = paste("Limite Sup:", round(LS, 2 ))) +

labs(title = paste("Boxplot -", columnLabel),
y = columnLabel,
x = "")+
scale_y_continuous(labels = scales::comma)

_# Histograma_
hist <- ggplot(data.frame(column), aes(x = column)) +
geom_histogram(binwidth = binwidth,
aes(y=..density..),
fill = "lightblue",
color = "black") +
geom_density(color="darkblue", size= 1 ) +
geom_vline(xintercept=mean_val,


**4.1 Análise de Colunas com Dados Nullos**

As colunas com dados nullos foram poucas:

```
wbc_count: 24
```
```
platelet_count: 16
```
```
platelet_distribution_width: 19
```
```
age gender
0 0
hemoglobin_g_dl wbc_count
0 24
differential_count rbc_count
0 0
platelet_count platelet_distribution_width
16 19
dengue_label
0
```
**4.2 Completude de dados**

Os dados estão em sua maioria preenchidos a nao ser pelos poucos dados faltantes das colunas acima.

```
color="red",
linetype="dashed") +
geom_vline(xintercept=Q2,
color="green",
linetype="dashed") +
labs(title = paste("Histograma -", columnLabel),
x = columnLabel,
y = "Frequência")+
scale_x_continuous(labels = scales::comma)
```
```
qqplot <- ggqqplot(column)
```
```
shapiro_test <- shapiro.test(column)
```
```
print(box)
print(hist)
print(qqplot)
print(shapiro_test)
```
### }

```
colSums(is.na(dados))
```

Na minha visão, a completude impacta diretamente a análise exploratória, pois a presença de dados faltantes

reduz a quantidade de informações disponíveis e pode comprometer os resultados.

Durante a análise, percebo que valores ausentes podem distorcer medidas estatísticas, dificultar a

identificação de padrões e influenciar a interpretação dos dados.

```
platelet_count: 98%
```
```
platelet_distribution_width: 98%
```
```
wbc_count: 97%
```
```
age: 100%
```
```
hemoglobin_g_dl: 100%
```
```
differential_count: 100%
```
```
dengue_label: 100%
```
```
gender: 100%
```
```
rbc_count: 100%
```
```
age gender
1.0000000 1.
hemoglobin_g_dl wbc_count
1.0000000 0.
differential_count rbc_count
1.0000000 1.
platelet_count platelet_distribution_width
0.9838220 0.
dengue_label
1.
```
**4.3 Análise da Variável Age**

A variável age apresenta alguns outliers acima do limite superior, incluindo um valor próximo de 120 anos.

Apesar disso, média e mediana são próximas, indicando que os dados não são muito assimétricos.

```
Média: 42.
Mediana: 40
Desvio padrão: 20.
Q1: 27
```
```
colMeans(!is.na(dados))
```
```
analyze_numerical(dados$age, "Age")
```

- Min. :0.0000 Min. :0.0000 Min. :
- 1st Qu.:1.0000 1st Qu.:1.0000 1st Qu.:
- Median :1.0000 Median :1.0000 Median :
- Mean :0.9393 Mean :0.9383 Mean :
- 3rd Qu.:1.0000 3rd Qu.:1.0000 3rd Qu.:
- Max. :1.0000 Max. :1.0000 Max. :
- NA's :
- Min. : 1.00 Min. :0. platelet_distribution_width dengue_label
- 1st Qu.: 14.00 1st Qu.:0.
- Median : 17.80 Median :1.
- Mean : 22.85 Mean :0.
- 3rd Qu.: 28.20 3rd Qu.:1.
- Max. :215.00 Max. :1.
- NA's :
- N: dados
- Mean 42.20 0.65 0.94 13. --------
- 114702.
- Std.Dev 20.94 0.48 0.24 1.
- 89421.
- Min 3.00 0.00 0.00 11.
- 10000.
- Q1 27.00 0.00 1.00 12.
- 46000.
- Median 40.00 1.00 1.00 13.
- 93000.
- Q3 55.00 1.00 1.00 15.
- 162500.
- Max 120.00 1.00 1.00 25.
- 500000.
- MAD 20.76 0.00 0.00 1.
- 87473.
- IQR 28.00 1.00 0.00 2.
- 116500.
- CV 0.50 0.73 0.25 0.
- 0.
- Skewness 0.55 -0.63 -3.68 0.
- 1.
- SE.Skewness 0.08 0.08 0.08 0.
- Q3:
- IQR:



```
Shapiro-Wilk normality test
```
```
data: column
W = 0.97524, p-value = 6.081e-
```
**4.4 Análise da Variável Hemoglobin Level**

A variável apresenta outliers acima do limite superior, com valores próximos de 25. Mesmo assim, média e

mediana são próximas, indicando baixa assimetria.

```
Média: 13.
Mediana: 13.
Desvio padrão: 1.
Q1: 12.
Q3: 15
IQR: 2.
```
```
analyze_numerical(dados$hemoglobin_g_dl, "Hemoglobin Level")
```



```
Shapiro-Wilk normality test
```
```
data: column
W = 0.94491, p-value < 2.2e-
```
**4.5 Análise da Variável White Blood Cell Count**

A variável apresenta outliers e dados faltantes. A diferença entre média e mediana indica leve assimetria, e a

distribuição não parece normal.

```
Média: 4338.
Mediana: 3200
Desvio padrão: 2344.
Q1: 2600
Q3: 6200
IQR: 3600
```
```
analyze_numerical(dados$wbc_count, "White Blood Cell Count")
```



```
Shapiro-Wilk normality test
```
```
data: column
W = 0.80885, p-value < 2.2e-
```
**4.6 Análise da Variável Platelet Count**

A variável apresenta outliers e dados faltantes. A diferença entre média e mediana indica assimetria nos

dados.

```
Média: 114702.
Mediana: 93000
Desvio padrão: 89421.
Q1: 46000
Q3: 162500
IQR: 116500
```
```
analyze_numerical(dados$platelet_count, "Platelet Count")
```



```
Shapiro-Wilk normality test
```
```
data: column
W = 0.87991, p-value < 2.2e-
```
**4.7 Análise da Variável Variation in Platelet**

A variável apresenta alguns outliers e dados faltantes, mas média e mediana próximas indicam baixa

assimetria.

```
Média: 22.
Mediana: 17.
Desvio padrão: 14.
Q1: 14
Q3: 28.
IQR: 14.
```
```
analyze_numerical(dados$platelet_distribution_width, "Variation in Platelet")
```



```
Shapiro-Wilk normality test
```
```
data: column
W = 0.65462, p-value < 2.2e-16
```
**4.8 Análise da Variável Dengue**

A variável mostra a distribuição dos casos com e sem dengue.

```
analyze_categorical(dados$dengue_label, "Target Variable")
```

**4.9 Análise da Variável Gender**

A variável apresenta a distribuição de gênero na base, podendo indicar possível desbalanceamento.

```
analyze_categorical(dados$gender, "Gender")
```

**4.10 Análise da Variável Differential Count**

A variável apresenta a distribuição das categorias relacionadas à contagem diferencial. A análise permite

observar a frequência de cada grupo e identificar possíveis desbalanceamentos.

```
dados$differential_count = as.factor(dados$differential_count)
analyze_categorical(dados$differential_count, "Diff White Blood Cell Count")
```

**4.11 Análise da Variável Red Blood Cell Count**

A variável apresenta a distribuição das categorias relacionadas à contagem de glóbulos vermelhos. É possível

observar a proporção entre os grupos e verificar possíveis diferenças entre eles.

```
dados$rbc_count = as.factor(dados$rbc_count)
analyze_categorical(dados$rbc_count, "Red Blood Cell Count")
```

# 5 Remoção de colunas categóricas

# 6 Tratamento de Valores Nullos

Para tratar os dados faltantes, foi utilizada a técnica de imputação por meio do pacote MICE. O método

utilizado foi o Predictive Mean Matching (PMM), que estima valores ausentes com base em valores

observados semelhantes, preservando a distribuição dos dados. Foram realizadas 5 imputações (m=5). O

parâmetro seed foi utilizado para garantir a reprodutibilidade dos resultados.

```
iter imp variable
1 1 wbc_count platelet_count platelet_distribution_width
1 2 wbc_count platelet_count platelet_distribution_width
1 3 wbc_count platelet_count platelet_distribution_width
1 4 wbc_count platelet_count platelet_distribution_width
1 5 wbc_count platelet_count platelet_distribution_width
```
```
dados <- dados[, sapply(dados, is.numeric)]
```
```
dados = complete(mice(dados, m= 5 , method="pmm", seed= 123 ))
```

```
2 1 wbc_count platelet_count platelet_distribution_width
2 2 wbc_count platelet_count platelet_distribution_width
2 3 wbc_count platelet_count platelet_distribution_width
2 4 wbc_count platelet_count platelet_distribution_width
2 5 wbc_count platelet_count platelet_distribution_width
3 1 wbc_count platelet_count platelet_distribution_width
3 2 wbc_count platelet_count platelet_distribution_width
3 3 wbc_count platelet_count platelet_distribution_width
3 4 wbc_count platelet_count platelet_distribution_width
3 5 wbc_count platelet_count platelet_distribution_width
4 1 wbc_count platelet_count platelet_distribution_width
4 2 wbc_count platelet_count platelet_distribution_width
4 3 wbc_count platelet_count platelet_distribution_width
4 4 wbc_count platelet_count platelet_distribution_width
4 5 wbc_count platelet_count platelet_distribution_width
5 1 wbc_count platelet_count platelet_distribution_width
5 2 wbc_count platelet_count platelet_distribution_width
5 3 wbc_count platelet_count platelet_distribution_width
5 4 wbc_count platelet_count platelet_distribution_width
5 5 wbc_count platelet_count platelet_distribution_width
```
Verificamos novamente se existem valores nullos

```
age hemoglobin_g_dl
0 0
wbc_count platelet_count
0 0
platelet_distribution_width dengue_label
0 0
```
# 7 Análise de pares

```
colSums(is.na(dados))
```
```
ggpairs(
dados,
```
```
lower = list(
continuous = wrap("points") +
scale_x_continuous(labels = scales::comma) +
scale_y_continuous(labels = scales::comma)
),
```
```
diag = list(
continuous = wrap("densityDiag") +
scale_x_continuous(labels = scales::comma)
),
```
```
upper = list(
```

A partir da análise visual da matriz de espalhamento as maiores correlações foram encontradas entre

platelet_count e wbc_count, seguidas pelas relações com dengue_label e platelet_distribution_width,

platelet_count e dengue_label, e platelet_count e platelet_distribution_width. As demais variáveis apresentam

baixa correlação.

1. platelet_count × wbc_count → corr ≈ 0.749
2. platelet_distribution_width × dengue_label → corr ≈ -0.651
3. platelet_count × dengue_label → corr ≈ -0.577
4. platelet_count × platelet_distribution_width → corr ≈ 0.456

# 8 Resultado da Análise

## 8.1 Distribuições normais

Com base na análise, observei que as variáveis age e hemoglobin_g_dl apresentam comportamento mais

próximo de uma distribuição normal, com menor assimetria. Por outro lado, as variáveis wbc_count,

```
continuous = wrap("cor", size = 4 )
)
)+ theme(axis.text = element_text())
```

platelet_count e platelet_distribution_width apresentam comportamento semelhante, com assimetria e

presença de valores extremos, afastando-se da normalidade.

## 8.2 Histogramas

A partir da análise dos histogramas, observa-se que as variáveis age e hemoglobin_g_dl apresentam

distribuição mais próxima da simétrica, com concentração de valores centrais e baixa assimetria. Por outro

lado, as variáveis wbc_count, platelet_count e platelet_distribution_width apresentam comportamento

semelhante, com assimetria à direita, indicando maior concentração de valores baixos e presença de cauda

para valores elevados.

## 8.3 Justificativa dos Bins

O número de bins utilizado nos histogramas foi definido automaticamente com base na regra de Freedman-

Diaconis, considerando a variabilidade e o tamanho da amostra. Essa abordagem foi aplicada de forma

consistente para todas as variáveis, garantindo um equilíbrio entre detalhamento e clareza visual,

independentemente das diferenças de escala e dispersão entre elas.

## 8.4 Shapiro-Wilk

O teste de Shapiro-Wilk indicou p-values menores que 0.05 para todas as variáveis, sugerindo não

normalidade. No entanto, como a base de dados possui um número elevado de observações, o teste tende a

ser muito sensível. Dessa forma, a análise foi complementada com histogramas e gráficos Q-Q, que indicaram

que algumas variáveis apresentam comportamento próximo à normalidade, conforme descrito acima.

# 9 Dashboard Interativo

Foi desenvolvido um dashboard interativo utilizando o pacote Shiny, permitindo a seleção de variáveis

numéricas da base de dados, o usuário pode escolher a variável a ser analisada, definir a cor do gráfico de linha

e ajustar os limites dos eixos X e Y, possibilitando uma visualização dinâmica dos dados.

```
ui <- fluidPage(
```
```
titlePanel("Dashboard - Análise de Dados"),
```
```
sidebarLayout(
sidebarPanel(
```
```
selectInput("variavel", "Selecione a variável:",
choices = names(dados)[sapply(dados, is.numeric)]),
```
```
selectInput("cor", "Escolha a cor:",
choices = c("blue", "red", "green", "black")),
```

```
Shiny applications not supported in static R Markdown documents
```
numericInput("xmin", "Limite inferior X:", value = 1 ),
numericInput("xmax", "Limite superior X:", value = 100 ),

numericInput("ymin", "Limite inferior Y:", value = 0 ),
numericInput("ymax", "Limite superior Y:", value = 100000 )
),

mainPanel(
plotOutput("grafico")
)
)
)

server <- **function** (input, output) {

output$grafico <- renderPlot({

var <- dados[[input$variavel]]

df <- data.frame(x = 1 :length(var), y = var)

ggplot(df, aes(x = x, y = y)) +
geom_line(color = input$cor) +
xlim(input$xmin, input$xmax) +
ylim(input$ymin, input$ymax) +
labs(title = paste("Gráfico de", input$variavel),
x = "Índice",
y = input$variavel)
})
}

shinyApp(ui = ui, server = server)



