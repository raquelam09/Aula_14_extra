##### Atividade aula 14 - extra - banco 2 - equivalente ao SINASC ######
##### Na branch main inserir os comandos e salvar o script com o nome script_aula_14_extra#####

# Tarefa 1: Leitura do banco de dados banco 2 = SINASC.csv com o nome de dados_aula14
# Ler o arquivo, verificar estrutura dos dados e dar uma olhada nos dados
# Ler o banco de dados
dados_aula14 = read.csv("banco 2 SINASC.csv", header = TRUE, sep = ";")

# Verificar a estrutura dos dados
str(dados_aula14)

# Resumo das variáveis
summary(dados_aula14)

# Ao terminar a Tarefa 1 commit com a mensagem " script - tarefa 1" e envie para o repositório Aula_14_Extra


# Tarefa 2: Manipulação dos dados
# Padronizar as categorias SEXO_PROPRIETARIO para Masculino e Feminino
# Atribuir legendas para a variável TIPO_VEICULO, sendo 1: Carro e 2: Moto
# Criar uma nova variável em dados_aula14 F_IDADE categorizando as idades em: 22 a 34, 35 a 45

# Padronizar sexo
dados_aula14$SEXO_PROPRIETARIO = ifelse(
  tolower(dados_aula14$SEXO_PROPRIETARIO) == "masculino",
  "Masculino",
  "Feminino"
  )

# Tipo de veículo
dados_aula14$TIPO_VEICULO = factor(
  dados_aula14$TIPO_VEICULO,
  levels = c(1, 2),
  labels = c("Carro", "Moto")
)

#Faixa de idade 
dados_aula14$F_IDADE = cut(
  dados_aula14$IDADE_PROPRIETARIO,
  breaks = c(21, 34, 45),
  labels = c("22 a 34", "35 a 45")
)

# Ao terminar a Tarefa 2 commit com a mensagem " script - tarefa 1 a 2" e envie para o repositório Aula_14_Extra


# Tarefa 3: Leitura do banco de dados Tabela_PAM.csv (com o nome tabela_pam) e:
# agregar ao banco dados_aula14 as informações de VALOR_P10 e VALOR_P90
# criar a variável PAM (somente quando TIPO_VEICULO = "Carro"), de acordo com IDADE_PROPRIETARIO e SEXO_PROPRIETARIO, com as seguintes categorias:
# PAM = "PIC", se VALOR_VEICULO < VALOR_P10; "AIC", se VALOR_P10 <= VALOR_VEICULO <= VALOR_P90; "GIC", se VALOR_VEICULO > VALOR_P90

# Leitura do banco 
tabela_pam = read.csv("Tabela_PAM (1).csv", header = TRUE, sep = ";")

# Juntar os bancos
dados_aula14 = merge(
  dados_aula14,
  tabela_pam,
  by = c("IDADE_PROPRIETARIO", "SEXO_PROPRIETARIO")
)
# Criar variável PAM
dados_aula14$PAM = NA

dados_aula14$PAM[dados_aula14$TIPO_VEICULO == "Carro" &
                   dados_aula14$VALOR_VEICULO < dados_aula14$VALOR_P10] = "PIC"

dados_aula14$PAM[dados_aula14$TIPO_VEICULO == "Carro" &
                   dados_aula14$VALOR_VEICULO >= dados_aula14$VALOR_P10 &
                   dados_aula14$VALOR_VEICULO <= dados_aula14$VALOR_P90] = "AIC" 

dados_aula14$PAM[dados_aula14$TIPO_VEICULO == "Carro" &
                   dados_aula14$VALOR_VEICULO > dados_aula14$VALOR_P90] = "GIC"

# Ao terminar a Tarefa 3 commit com a mensagem " script - tarefa 1 a 3" e envie para o repositório Aula_14_Extra

 
# Tarefa 4: Criar o banco de dados BANCO_AULA14_RJ, POR MUNICÍPIO, com as seguintes variáveis listadas abaixo. 
# Variáveis que se referem a medidas de posição e de dispersão devem ser calculadas sem considerar NAs

# Atenção: a 1a linha do banco deve ser da UF 33
# ANO: 2025
# NIVEL: UF ou MUNICIPIO
# CODIGO: código do municipio (ou da UF)
# TVV: total de veiculos vendidos
# TVRC: total de vendas com registros completos nas 5 variáveis originais de banco 2 = SINASC
# TVVF: total de veículos vendidos para mulher
# TVVM: total de veículos vendidos para homem
# TVCF: total de carros vendidos para mulheres
# TVCM: total de carros vendidos para homens
# TVMF: total de motos vendidas para mulheres
# TVMM: total de motos vendidas para homens
# TVC_22_34: total de carros vendidos para pessoas na faixa etária de 22 a 34 anos
# TVC_35_45: total de carros vendidos para pessoas na faixa etária de 35 a 45 anos
# IMVCF: idade média das mulheres proprietárias de veículo carro 
# DPVCF: desvio-padrão das idades das mulheres proprietárias de veículo carro
# IVCF_P25: percentil 25 das idades das mulheres proprietárias de veículo carro
# IVCF_P50: percentil 50 das idades das mulheres proprietárias de veículo carro
# IVCF_P75: percentil 75 das idades das mulheres proprietárias de veículo carro
# IMVMM: idade média dos homens proprietários de veículo moto 
# DPVMM: desvio-padrão das idades dos homens proprietários de veículo moto
# IVMM_P25: percentil 25 das idades dos homens proprietários de veículo moto
# IVMM_P50: percentil 50 das idades dos homens proprietários de veículo moto
# IVMM_P75: percentil 75 das idades dos homens proprietários de veículo moto
# TPIC: total de compradores com perfil PIC
# TAIC: total de compradores com perfil AIC
# TGIC: total de compradores com perfil GIC

# Função para calcular as estatísticas
calcular = function(dados) {
  
  mulher_carro = dados$IDADE_PROPRIETARIO[
    dados$SEXO_PROPRIETARIO == "Feminino" &
      dados$TIPO_VEICULO == "Carro"
  ]
  
  homem_moto = dados$IDADE_PROPRIETARIO[
    dados$SEXO_PROPRIETARIO == "Masculino" &
      dados$TIPO_VEICULO == "Moto"
  ]
  
  data.frame(
    TVV = nrow(dados),
    TVRC = sum(complete.cases(dados[, 1:5])),
    
    TVVF = sum(dados$SEXO_PROPRIETARIO == "Feminino", na.rm = TRUE),
    TVVM = sum(dados$SEXO_PROPRIETARIO == "Masculino", na.rm = TRUE),
    
    TVCF = sum(dados$TIPO_VEICULO == "Carro" &
                 dados$SEXO_PROPRIETARIO == "Feminino", na.rm = TRUE),
    
    TVCM = sum(dados$TIPO_VEICULO == "Carro" &
                 dados$SEXO_PROPRIETARIO == "Masculino", na.rm = TRUE),
    
    TVMF = sum(dados$TIPO_VEICULO == "Moto" &
                 dados$SEXO_PROPRIETARIO == "Feminino", na.rm = TRUE),
    
    TVMM = sum(dados$TIPO_VEICULO == "Moto" &
                 dados$SEXO_PROPRIETARIO == "Masculino", na.rm = TRUE),
    
    TVC_22_34 = sum(dados$TIPO_VEICULO == "Carro" &
                      dados$F_IDADE == "22 a 34", na.rm = TRUE),
    
    TVC_35_45 = sum(dados$TIPO_VEICULO == "Carro" &
                      dados$F_IDADE == "35 a 45", na.rm = TRUE),
    
    IMVCF = mean(mulher_carro, na.rm = TRUE),
    DPVCF = sd(mulher_carro, na.rm = TRUE),
    IVCF_P25 = quantile(mulher_carro, .25, na.rm = TRUE),
    IVCF_P50 = quantile(mulher_carro, .50, na.rm = TRUE),
    IVCF_P75 = quantile(mulher_carro, .75, na.rm = TRUE),
    
    IMVMM = mean(homem_moto, na.rm = TRUE),
    DPVMM = sd(homem_moto, na.rm = TRUE),
    IVMM_P25 = quantile(homem_moto, .25, na.rm = TRUE),
    IVMM_P50 = quantile(homem_moto, .50, na.rm = TRUE),
    IVMM_P75 = quantile(homem_moto, .75, na.rm = TRUE),
    
    TPIC = sum(dados$PAM == "PIC", na.rm = TRUE),
    TAIC = sum(dados$PAM == "AIC", na.rm = TRUE),
    TGIC = sum(dados$PAM == "GIC", na.rm = TRUE)
  )
}


# RJ
RJ = calcular(dados_aula14)
RJ$ANO = 2025
RJ$NIVEL = "UF"
RJ$CODIGO = 33


# Municípios
municipios = split(dados_aula14, dados_aula14$MUNICIPIO)

BANCO_AULA14_RJ = do.call(rbind, lapply(municipios, function(dados) {
  
  resultado = calcular(dados)
  
  resultado$ANO = 2025
  resultado$NIVEL = "MUNICIPIO"
  resultado$CODIGO = dados$MUNICIPIO[1]
  
  resultado
}))


# Colocar RJ na primeira linha
BANCO_AULA14_RJ = rbind(RJ, BANCO_AULA14_RJ)


# Organizar as colunas
BANCO_AULA14_RJ = BANCO_AULA14_RJ[, c(
  "ANO", "NIVEL", "CODIGO",
  "TVV", "TVRC", "TVVF", "TVVM",
  "TVCF", "TVCM", "TVMF", "TVMM",
  "TVC_22_34", "TVC_35_45",
  "IMVCF", "DPVCF", "IVCF_P25", "IVCF_P50", "IVCF_P75",
  "IMVMM", "DPVMM", "IVMM_P25", "IVMM_P50", "IVMM_P75",
  "TPIC", "TAIC", "TGIC"
)]

# Ao terminar a Tarefa 4 commit com a mensagem " script - tarefa 1 a 4" e envie para o repositório Aula_14_Extra


# Tarefa 5: Exportar o banco de dados BANCO_AULA14_RJ com o nome BANCO_AULA14_RJ.csv
write.csv(BANCO_AULA14_RJ,
          "BANCO_AULA14_RJ.csv",
          row.names = FALSE)

# Ao terminar a Tarefa 5 commit com a mensagem "dados e script - Etapa 2" e envie para o repositório Aula_14_Extra
