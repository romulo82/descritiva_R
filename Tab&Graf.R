# Estat�stica B�sica - Bussab/Morettin

# 2.1 - Distribui�oes de Frequ�ncias

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
names(civil) <- c("Civil", "Frequ�ncia"); civil
civil$Propor��o <- round(civil$Frequ�ncia/36, 2)
civil$Percentual <- round(civil$Frequ�ncia/36, 2)*100
Tot3 <- data.frame("Total", sum(civil$Frequ�ncia), sum(civil$Propor��o), sum(civil$Percentual))
names(Tot3) <- names(civil)
civil <- rbind(civil, Tot3); civil

regiao <- as.data.frame(table(dadosp$Regi�o));regiao
names(regiao) <- c("Regi�o", "Frequ�ncia")
regiao$Propor��o <- round(regiao$Frequ�ncia/36,2)
regiao$Porcentagem <- regiao$Propor��o*100; regiao
Tot4 <- data.frame("Total", sum(regiao$Frequ�ncia), sum(regiao$Propor��o), sum(regiao$Porcentagem))
names(Tot4) <- names(regiao)
regiao <- rbind(regiao, Tot4); regiao

filhos <- as.data.frame(table(subset(dadosp[, 4], dadosp[, 2]=="casado"))); filhos
names(filhos) <- c("Filhos", "Frequ�ncia")
filhos$Propor��o <- round(filhos$Frequ�ncia/20, 2)
filhos$Porcentagem <- filhos$Propor��o*100
Tot5 <- data.frame("Total", sum(filhos$Frequ�ncia), sum(filhos$Propor��o), sum(filhos$Porcentagem))
names(Tot5) <- names(filhos)
filhos <- rbind(filhos, Tot5); filhos

k <- ceiling(1+3.3*log10(36)); k # N�mero de classes
amp <- (max(dadosp$Anos)-min(dadosp$Anos))/k; amp # Amplitude
idade <- as.data.frame(table(cut(dadosp$Anos, breaks=7, right=F))); idade
names(idade) <- c("Classes", "Frequ�ncia")
idade$Propor��o <- round(idade$Frequ�ncia/36, 2)
idade$Porcentagem <- idade$Propor��o*100
Tot6 <- data.frame("Total", sum(idade$Frequ�ncia), sum(idade$Propor��o), sum(idade$Porcentagem))
names(Tot6) <- names(idade)
idade <- rbind(idade, Tot6); idade

#2.3 Gr�ficos
# Barras

dadosp
dadosp$Instru��o
inst <- as.data.frame(table(dadosp$Instru��o));inst
names(inst) <- c("Grau", "Frequ�ncia")
barplot(inst$Frequ�ncia, ylim=c(0,20), ylab="Frequ�ncia", 
        col="light blue", names=c("Fundamental", "M�dio", "Superior"), space=0.2)
# Setores
pie(inst$Frequ�ncia, labels=c("1 (12; 33,3%)", "2 (18; 50,0%)", "3 (6; 16,7%)"), 
    col=c("light green", "light blue", "grey"), sub=c("1 = Fundamental, 2 = M�dio e 3 = Superior"))

# Gr�fico de dispers�o
filhos <- as.data.frame(table(subset(dadosp[, 4], dadosp[, 2]=="casado"))); filhos
plot(filhos, type="p")

