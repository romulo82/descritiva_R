# Estatística Básica - Bussab/Morettin

# 2.1 - Distribuiçoes de Frequências

head(USArrests)
ex1 <- data.frame(head(USArrests)$Assault, row.names=row.names(head(USArrests)))
names(ex1) <- "Assaltos";ex1
ex1$Cidades <- row.names(ex1);ex1
row.names(ex1)=NULL;ex1 <- ex1[, c(2,1)]; ex1
ex1 <- cbind(ex1, Proporcao=round(prop.table(ex1$Assaltos),2));ex1
ex1$Porcentagem <- ex1$Proporcao*100;ex1
Tot <- data.frame(Cidades="Total", Assaltos=sum(ex1$Assaltos), 
                  Proporcao=sum(ex1$Proporcao), Porcentagem=sum(ex1$Porcentagem));Tot
ex1 <- rbind(ex1,Tot);ex1

# Tabela com dados em classes

head(women)
ex2 <- as.data.frame(table(cut(women$height, breaks=6, right=F)));ex2
names(ex2) <- c("Classes", "Frequencia");ex2
ex2 <- cbind(ex2, Porcentagem=round(100*ex2$Frequencia/(sum(ex2$Frequencia)),2));ex2
Tot2 <- data.frame("Total", sum(ex2$Frequencia), ceiling(sum(ex2$Porcentagem)));Tot2
names(Tot2) <- names(ex2)
ex2 <- rbind(ex2, Tot2);ex2

#Tabela 2.1: Dados Pessoais

dadosp <- read.csv(choose.files(), sep=";", header=T); dadosp
civil <- as.data.frame(table(dadosp$Civil)); civil
names(civil) <- c("Civil", "Frequência"); civil
civil$Proporção <- round(civil$Frequência/36, 2)
civil$Percentual <- round(civil$Frequência/36, 2)*100
Tot3 <- data.frame("Total", sum(civil$Frequência), sum(civil$Proporção), sum(civil$Percentual))
names(Tot3) <- names(civil)
civil <- rbind(civil, Tot3); civil

regiao <- as.data.frame(table(dadosp$Região));regiao
names(regiao) <- c("Região", "Frequência")
regiao$Proporção <- round(regiao$Frequência/36,2)
regiao$Porcentagem <- regiao$Proporção*100; regiao
Tot4 <- data.frame("Total", sum(regiao$Frequência), sum(regiao$Proporção), sum(regiao$Porcentagem))
names(Tot4) <- names(regiao)
regiao <- rbind(regiao, Tot4); regiao

filhos <- as.data.frame(table(subset(dadosp[, 4], dadosp[, 2]=="casado"))); filhos
names(filhos) <- c("Filhos", "Frequência")
filhos$Proporção <- round(filhos$Frequência/20, 2)
filhos$Porcentagem <- filhos$Proporção*100
Tot5 <- data.frame("Total", sum(filhos$Frequência), sum(filhos$Proporção), sum(filhos$Porcentagem))
names(Tot5) <- names(filhos)
filhos <- rbind(filhos, Tot5); filhos

k <- ceiling(1+3.3*log10(36)); k # Número de classes
amp <- (max(dadosp$Anos)-min(dadosp$Anos))/k; amp # Amplitude
idade <- as.data.frame(table(cut(dadosp$Anos, breaks=7, right=F))); idade
names(idade) <- c("Classes", "Frequência")
idade$Proporção <- round(idade$Frequência/36, 2)
idade$Porcentagem <- idade$Proporção*100
Tot6 <- data.frame("Total", sum(idade$Frequência), sum(idade$Proporção), sum(idade$Porcentagem))
names(Tot6) <- names(idade)
idade <- rbind(idade, Tot6); idade

#2.3 Gráficos
# Barras

dadosp
dadosp$Instrução
inst <- as.data.frame(table(dadosp$Instrução));inst
names(inst) <- c("Grau", "Frequência")
barplot(inst$Frequência, ylim=c(0,20), ylab="Frequência", 
        col="light blue", names=c("Fundamental", "Médio", "Superior"), space=0.2)
# Setores
pie(inst$Frequência, labels=c("1 (12; 33,3%)", "2 (18; 50,0%)", "3 (6; 16,7%)"), 
    col=c("light green", "light blue", "grey"), sub=c("1 = Fundamental, 2 = Médio e 3 = Superior"))

# Gráfico de dispersão
filhos <- as.data.frame(table(subset(dadosp[, 4], dadosp[, 2]=="casado"))); filhos
plot(filhos, type="p")

