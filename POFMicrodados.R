library(dplyr)



getwd()


#################################################################################################
################## Despesa coletiva média - POF (2017-2018) #####################################
#################################################################################################

### Lendo os microdados de despesa coletiva salvos no PC
DESPESA_COLETIVA <- 
  read.fwf("DESPESA_COLETIVA.txt"
           , widths = c(2,4,1,9,2,1,2,2,7,2,4,10,2,2,1
                        ,10,1,12,10,10,1,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC", "QUADRO",
                           "SEQ", "V9001", "V9002", "V9005", "V8000",
                           "V9010", "V9011", "V9012", "V1904",
                           "V1905", "DEFLATOR", "V8000_DEFLA",
                           "V1904_DEFLA", "COD_IMPUT_VALOR",
                           "COD_IMPUT_QUANTIDADE", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
           , dec="."
  )

### Salvar em rds (comprimindo)
saveRDS(DESPESA_COLETIVA,"DESPESA_COLETIVA.rds")

### Ler o aquivo e formato rds.
despesa_coletiva <- readRDS("DESPESA_COLETIVA.rds")

### Procedimentos para deflacionar e apresentar em valores mensais.
desp_coletiva <- 
  transform( despesa_coletiva,
             valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                                    (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
             ) , 
             inss_mensal=(V1904_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
  )[ , c( "V9001" , "valor_mensal" , "inss_mensal" ) ]

### Explicação:
# V8000 é o valor das despesas realizadas pela unidade de consumo. V8000_DEFLA = V8000*DEFLATOR;
# V9011 é o número de meses de realização da despesa.
# Lendo o comando: para os casos de Quadro 10 (aluguéis) e 19 (serviços domésticos), incluir o 
# número de meses da realização da despesa (V9011). Quando no for isso (else) considerar a 
# variável V8000 deflacionada (já está, pois se usa V8000_DEFLA) o fator de anualização e o 
# peso final.

### Calculando a despesa coletiva média do Brasil
mean(desp_coletiva$valor_mensal)


#################################################################################################
################## Despesas coletivas em PE - POF (2017-2018) ###################################
#################################################################################################

### Criando uma POF de despesas coletivas para PE

despesa_coletiva_PE <- subset(despesa_coletiva, UF == "26")

desp_coletiva_PE <- 
  transform( despesa_coletiva_PE,
             valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                                    (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
             ) , 
             inss_mensal=(V1904_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
  )[ , c( "V9001" , "valor_mensal" , "inss_mensal" ) ]


rm(despesa_coletiva_PE) ### Removendo o arquivo desnecessário.

mean(desp_coletiva_PE$valor_mensal) ### Calculando a despesa média em PE.


sum(desp_coletiva_PE$valor_mensal)
summary(desp_coletiva_PE$valor_mensal)
maxPE<-max(desp_coletiva_PE$valor_mensal)
maxPE
maioresPE<- order(desp_coletiva_PE$valor_mensal, na.last = TRUE, decreasing =  T)
maioresPE
head(maioresPE)
tail(maioresPE) ### Não funcionou.

desp_coletiva_PE %>% 
  arrange(desc(valor_mensal)) ### Funcionou.



#################################################################################################
################## Despesas coletivas no Ceará ##################################################
#################################################################################################

### Criando uma POF de despesas coletivas para o Ceará

despesa_coletiva_CE <- subset(despesa_coletiva, UF == "23")

desp_coletiva_CE <- 
  transform( despesa_coletiva_CE,
             valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                                    (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
             ) , 
             inss_mensal=(V1904_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12
  )[ , c( "V9001" , "valor_mensal" , "inss_mensal" ) ] ### variáveis de interesse e deflacionadas.

rm(despesa_coletiva_CE) ### Removendo o arquivo desnecessário.

### Calculando a despesa coletiva média no Ceará
mean(desp_coletiva_CE$valor_mensal)

sum(desp_coletiva_CE$valor_mensal)
summary(desp_coletiva_CE$valor_mensal)
maxCE<-max(desp_coletiva_CE$valor_mensal)
maxCE
maioresCE<- order(desp_coletiva_CE$valor_mensal, na.last = TRUE, decreasing = T)
head(maioresCE)
desp_coletiva_CE %>% 
  arrange(desc(valor_mensal)) ### Funcionou.

############### Testando sem o peso final #######################################################


desp_coletiva_CESP <- 
  transform( despesa_coletiva_CE,
             valor_mensal = ifelse( QUADRO==10|QUADRO==19,
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO)/12, 
                                    (V8000_DEFLA*FATOR_ANUALIZACAO)/12
             ) , 
             inss_mensal=(V1904_DEFLA*V9011*FATOR_ANUALIZACAO)/12
  )[ , c( "V9001" , "valor_mensal" , "inss_mensal" ) ] 

sum(desp_coletiva_CE$valor_mensal)
sum(desp_coletiva_CESP$valor_mensal)

summary(desp_coletiva_CE$valor_mensal)
summary(desp_coletiva_CE2$valor_mensal)

### Coluna de despesa coletiva do CE sem o peso final.

dcesemp<-desp_coletiva_CESP$valor_mensal
dcesemp
class(dcesemp)
### Colula de pesos finais

pfce<-despesa_coletiva_CE$PESO_FINAL
head(pfce)
class(pfce)
pmax<max(pfce)

r<- dcesemp*pfce
maxr<-max(r)
maxr ### mesmo valor de maxCE, cqd.

#################################################################################################
############ Despesa individual - POF (2017/18 ##################################################
#################################################################################################

### Lendo os microdados de despesa individual salvos no PC

DESPESA_INDIVIDUAL <- 
  read.fwf("DESPESA_INDIVIDUAL.txt" 
           , widths = c(2,4,1,9,2,1,2,2,2,7,2,10,2
                        ,2,1,1,1,12,10,1,2,14,14,10,5)
           , na.strings=c(" ")
           , col.names = c("UF", "ESTRATO_POF", "TIPO_SITUACAO_REG",
                           "COD_UPA", "NUM_DOM", "NUM_UC",
                           "COD_INFORMANTE", "QUADRO", "SEQ", "V9001",
                           "V9002", "V8000", "V9010", "V9011", "V9012",
                           "V4104", "V4105", "DEFLATOR", "V8000_DEFLA",
                           "COD_IMPUT_VALOR", "FATOR_ANUALIZACAO",
                           "PESO", "PESO_FINAL", "RENDA_TOTAL","V9004")
           , dec="."
  )   


### Salvar em rds (comprimindo)
saveRDS(DESPESA_COLETIVA,"DESPESA_COLETIVA.rds")

### Procedimentos para deflacionar e apresentar em valores mensais.
desp_individual <-
  transform( DESPESA_INDIVIDUAL,
             valor_mensal = ifelse( QUADRO==44|QUADRO==47|QUADRO==48|QUADRO==49|QUADRO==50,
                                    (V8000_DEFLA*V9011*FATOR_ANUALIZACAO*PESO_FINAL)/12, 
                                    (V8000_DEFLA*FATOR_ANUALIZACAO*PESO_FINAL)/12
             )
  )[ , c( "V9001" , "valor_mensal" ) ]

### Calculando a despesa individual média do Brasil
mean(desp_individual$valor_mensal)

