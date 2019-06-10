
# Lista 08 - Questão 01
# Professor: Davi Moreira
# Disciplina: Análise de Dados
# Aluna: Maria Eduarda R. N. Lessa

# Questão 1:

## Figure 1:

# Definir diretório:
setwd("C:/Users/Duda/Desktop/PPGCP/Análise de Dados/lista_08")

# Requerer pacotes, carregar arquivo .dta e criar tabela:
require(foreign)
article.coding <- read.dta("tables2graphsWeb.dta")
install.packages("R2WinBUGS")
require(R2WinBUGS)
attach.all(article.coding)
counts <- table(subtag, type, exclude = c("", ""))

graph.counts <-  counts[,1]
table.counts <- counts[,2]
all.counts <- sort(graph.counts+table.counts, decreasing =T)
total.graphs <- sum(graph.counts)
total.tables <- sum(table.counts)

# Ordenar vetores:
label.vec <- c("Estimates and\nUncertainties", "Summary Stats", 
               "Predicted Values", "Non-numeric", "Other", "Mathematical" 
)
graph.counts.sorted <- c(0, 27, 41, 7, 0, 6)
table.counts.sorted <- c(79, 47, 7, 12, 13, 0)

pdf("table_graph_freq.pdf", height = 3.5, width  = 6)

# Definir layout dos gráficos:
layout(rbind(c(1,2)), 
       widths = rbind(c(5,3.65)), respect = F)

y.axis <-(length(all.counts):1)
x.size<-.8
point.size <-.7
par(mar=c(4, 6, 4, 1))

# Criar gráficos:
plot(100*(all.counts/sum(counts)), y.axis, xlim = c(- .1,
                                                    40.1), ylim = c(min(y.axis)-.1,
                                                                    max(y.axis) +.1),
     type = "p", axes = F, xlab = "", ylab = "", pch = 19, 
     main = "", cex = point.size)
abline(h = y.axis, lty = "dotted")
box(bty = "o")
axis(1, at=seq(0,40, by=10), labels = c(0, 10, 20, 30, "40 %"),
     cex = 1, las = 1, tick = T, cex.axis = x.size,
     line = 0, mgp = c(2,.5,0))
axis(2, at = y.axis, labels = label.vec, tick = T, las  = 1, 
     line =0, cex.axis = .8, hadj = .5,  mgp = c(2,3.1,0)) 
mtext("Percentage of All\nGraphs and Tables", 3, line = 1, cex =1, font = 2)

par(mar=c(4, 1, 4, 1))

plot(100-100*(table.counts.sorted/all.counts), y.axis, ylim = c(min(y.axis)-.1, max(y.axis) +.1), 
     xlim = c(-1,101) ,type = "p", axes = F, xlab = "", ylab = "", pch = 19, 
     main = "", cex = point.size)
abline(h = y.axis, lty = "dotted") 
box(bty = "o")
axis(1, at=seq(0,100, by=25), labels = c(0, 25, 50, 75, "100 %"), 
     cex = 1, las = 1, tick = T, cex.axis = x.size,
     line = 0, mgp = c(2,.5,0))
axis(2, at = y.axis, labels = F, tick = F)  
mtext("Percentage of Graphs\nWithin Each Category", 3, line = 1, cex =1, font = 2)

# Gerar .pdf:
dev.off()


---
  
  
## Figure 2:
setwd()
# Criar dados primeiro gráfico:
prop.left <- 342
prop.right <- 120
maj.left <- 86
maj.right <- 256

# Criar dados segundo gráfico: 
prop.left.gov <- 8
prop.right.gov <- 1
maj.left.gov <- 0
maj.right.gov <- 8

# Criar matriz para o primeiro gráfico de barras:
year.matrix <- 100* cbind(c(maj.left/(maj.left+maj.right), 
                            maj.right/(maj.left+maj.right)),
                          c(prop.left/(prop.left+prop.right), 
                            prop.right/(prop.left+prop.right)))

# Criar matriz para o segundo gráfico de barras:
country.matrix <- 100* cbind(c(maj.left.gov/(maj.left.gov+maj.right.gov),
                               maj.right.gov/(maj.left.gov+maj.right.gov)),
                             c(prop.left.gov/(prop.left.gov+prop.right.gov),
                               prop.right.gov/(prop.left.gov+prop.right.gov)))

# Definir legenda do eixo y:
y.label <- c("Majoritarian", "Proportional")

# Definir formatação do gráfico de barras:
pdf("iversen_final.pdf", width = 5.5, height = 2)
layout(cbind(1,2), widths = c(5,4))

# Definir margens:
par(mar=c(3,4.5,3,1))

# Criar gráfico empilhado:
year.bar <- barplot(year.matrix, axes = F, horiz = T, 
                    beside = F, main = "", 
                    col = c(gray(.8),gray(.9), gray(.8), gray(.9)))
axis(1, at = c(0,25,50,75,100), labels = c(0, 25, 50, 75, "100%"),   
     cex.axis = .8, mgp = c(2,.5,0))
axis(2, at = year.bar, labels = y.label, tick = F, las = 1,
     mgp = c(2,.5,0), cex.axis = .8)
mtext("Percentage of Years\nWith Left Governments,\n1945-1998", 3, line = .5, 
      cex = .8, font = 2)
text(40, year.bar[2], "342", cex = .8)
text(88, year.bar[2], "120", cex = .8)
text(10, year.bar[1], "86", cex = .8)
text(60, year.bar[1], "256", cex = .8)

# Criar segundo gráfico de barras:
par(mar=c(3,1,3,1)) 
country.bar <- barplot(country.matrix, axes = F, horiz = T,
                       beside = F, main = "", col = c(gray(.8),
                                                      gray(.9), gray(.8), 
                                                      gray(.9)))
axis(1, at = c(0,25,50,75,100), labels = c(0, 25, 50, 75, "100%"),
     cex.axis = .8, mgp = c(2,.5,0))
axis(2, at = country.bar, labels = c("",""), tick = F, las = 1,
     line = -.2,  mgp = c(2,.5,0), cex.axis = .8)
mtext("Percentage of Countries With\nCenter-Left Governments at\nLeast 50\ % of Time, 1945-1998",
      3, line = .5, 
      cex = .8, font = 2) 
text(50, year.bar[2], "8", cex = .8)
text(95, year.bar[2], "1", cex = .8)
text(50, year.bar[1], "0/8", cex = .8)

# Criar arquivo .pdf:
dev.off() 


---
  
  
  ## Figure 3:
  # Criar vetores para as colunas:
  var.names <- c("Size\n(1260)", "Political Talk\n(1253)", 
                 "Political Agreement\n(1154)","Political Knowledge\n(1220)")
mean.vec <- c(3.13, 1.82, .43, 1.22)
sd.vec <- c(1.49, .61, .41, .42)
min.vec <- c(1, 0, 0, 0)
max.vec <- c(5,3,1,2)

# Definir ordem a partir das médias (maior - menor):
mean.vec.order <- sort(mean.vec, decreasing = TRUE) 
sd.vec.order <- sd.vec[order(mean.vec, decreasing = TRUE)] 
min.vec.order <- min.vec[order(mean.vec, decreasing = TRUE)]
max.vec.order <- max.vec[order(mean.vec, decreasing = TRUE)]
var.names.order <- var.names[order(mean.vec, decreasing = TRUE)]

# Criar indicador para que as variáveis sejam ordenadas de cima para 
# baixo no eixo y:
y.axis <- c(4:1)

# Definir configurações do .pdf:
pdf("mcclurg_fig_final.pdf", height = 2.5, width =4.5)

# Definir margens e configurações do painel:
par(mfrow=c(1,1), mar=c(3,8,1,2))

# Criar gráfico:
plot(mean.vec.order, y.axis, type = "p", pch = 19, xlab="", ylab ="", 
     axes = F, xlim = c(0, 5.1),
     ylim = c(min(y.axis - .1), max(y.axis + .1)), main = "", cex = .6,
     yaxs = "i", xaxs = "i")
box(bty = "l")
segments((mean.vec.order - sd.vec.order), y.axis,
         (mean.vec.order + sd.vec.order), y.axis)
segments(min.vec.order, y.axis, max.vec.order, y.axis, lty = 2)
axis(1, at = seq(0,5,by=1), labels =  seq(0,5,by=1), tick = T, 
     cex.axis = .7, mgp = c(2,.5,0))
axis(2, at = y.axis, label = var.names.order, las = 1, tick = T, cex.axis = .7, 
     mgp = c(2,3.2,0), hadj =.5)

dev.off() 


---
  
  
## Figure 4:
  
install.packages("vioplot")    
library(vioplot)

# Criar vetores: 
var.names <- c("Issue\nConvergence\n(982)", "Competitiveness\n(65)", 
               "Total Spending/\nCapita \n(65)", "Difference\nSpending/Capita\n(65)",
               "State Voting Age\nPop. (ln) (65)", 
               "Percent\nNegative Ads\n(65)", "2000 Year\n(65)", "2002 Year\n(65)",
               "Consensual\nIssue\n(43)", "Issue Owned\n(43)", "Issue\nSalience\n(43)") 
mean.vec <- c(24.85, 1.54, 3.47, 1.12, 1.2, 21.38, .38, .32, .28, .49, 2.86)
sd.vec <- c(34.73, 1.2, 2.71, 1.32, .85, 16.84, .49, .47, .45, .51, 6.38)
min.vec <- c(0, 0, .28, .03, -.65, 0, 0, 0, 0,0, 0)
max.vec <- c(99.98, 3, 13.39, 9.26, 3.13, 54.96, 1, 1, 1, 1, 35.63)

# Definir grupos para cada um dos 3 gráficos:

# binary
# percents (+ issue salience)
# millions (+ competitiveness)

# Carregar dados:
campaign.data <- read.dta("kaplan_campaign.dta")
campaign.issue.data <- read.dta("kaplan_campaign_issue.dta")
issue.data <- read.dta("kaplan_issue.dta")

pdf("kaplan_fig_final_2.pdf", height = 7.5, width =5)

# Definir layout e configurações dos gráficos: 
layout(matrix(c(1,2,3)), heights = c(.8,1.2,1.4))

mar.left <- 8 
point.size <- 1.1 

# Deixar as variáveis do grupo "binary" para o primeiro gráfico:
keep <- 7:10 
mean.vec.binary <-  mean.vec[keep] 
min.vec.binary <- min.vec[keep]
max.vec.binary <- max.vec[keep]
var.names.binary<-var.names[keep]

# Ordenar os vetores:
mean.vec.binary.order <- sort(mean.vec.binary, decreasing = TRUE) 
var.names.binary.order <- var.names.binary[order(mean.vec.binary, decreasing = TRUE)]
y.axis.binary <- c(length(mean.vec.binary):1)

# Criar primeiro gráfico:
par(mar=c(2.2,mar.left,2,1))
plot(mean.vec.binary.order, y.axis.binary, type = "p", pch = 18, cex = 1.5, xlab="", ylab ="", 
     axes = F, xlim = c(0,1),
     ylim = c(min(y.axis.binary - .1), max(y.axis.binary + .1)), 
     main = "Means of Binary Variables", xaxs="i") 
box() 
axis(1, at = seq(0,1,by=.25), labels =  c(0,".25",".5",".75",1), tick = T,
     cex.axis = 1, mgp = c(2,.5,0))
axis(2, at = y.axis.binary, label = var.names.binary.order, las = 1, tick = T,
     cex.axis =1, mgp = c(2,3,0), hadj=.5) 

# Selecionar variáveis para o segundo gráfico e ordená-las:
percent.negative <- campaign.data$npercneg
issue.convergence <- campaign.issue.data$convf 
issue.salience <- issue.data$nsalnc
y.axis.percents <- c(3:1)

# Criar o "violin plot" para cada variável:
par(mar = c(2.2,mar.left,2,1))
plot(issue.salience, xlim = c(0,100), ylim = c(.5,3.5), type = "n", axes = F, 
     main = "Percentage Variables", ylab = "", xlab = "", xaxs ="i")
vioplot(issue.salience,  issue.convergence, percent.negative,
        horizontal = T, col = "dark gray", names = c("", "", ""), add = T, rectCol = "white")

points(median(percent.negative), 3,  col = "black", cex = point.size, pch = 19)
points(median(issue.convergence), 2,  col = "black", cex = point.size, pch = 19, xpd = T)
points(median(issue.salience), 1,  col = "black", cex = point.size, pch = 19, xpd = T)

axis(1, at = seq(0,100,by=25), labels =  c(0,25,50,75,"100%"), tick = T, 
     cex.axis = 1, mgp = c(2,.5,0))
segments(0,0,0,4)
axis(2, at = c(3:1), label = c(var.names[6], var.names[1], var.names[11]), las = 1, tick = T,  
     cex.axis =1,mgp = c(2,3.4,0), hadj=.5) 


# Selecionar variáveis para o terceiro gráfico e ordená-las:
total.spending <- campaign.data$nspendcap 
competitiveness <- campaign.data$cqnum
state.pop <- campaign.data$lnpop
difference.spending  <- campaign.data$ndifspendcap

# Criar gráfico:
par(mar = c(2.2,mar.left,2,1))
plot(total.spending, xlim = c(-.66,14), ylim = c(.5,4.5), type = "n", axes = F,  
     main = "Variables Measured in Millions", ylab = "", xlab = "", xaxs ="i")
vioplot(difference.spending,  state.pop, competitiveness, total.spending, 
        horizontal = T, col = "dark gray",names = c("", "", "",""), add = T, rectCol = "white")
points(median(total.spending), 4,  col = "black", cex = point.size, pch = 19)
points(median(competitiveness), 3,  col = "black", cex = point.size, pch = 19)
points(median(state.pop), 2,  col = "black", cex = point.size, pch = 19)
points(median(difference.spending), 1,  col = "black", cex = point.size, pch = 19)

axis(1, at = seq(0,14,by=2), labels =  seq(0,14,by=2), tick = T, las  = 1,
     line =0, cex.axis = 1, mgp = c(2,.5,0)) 
axis(2, at = c(4:1), label = c(var.names[3], var.names[2], var.names[5], var.names[4]),
     las = 1, tick = T, cex.axis =1, mgp = c(2,4.2,0), hadj=.5) 

# Gerar .pdf:
dev.off()


---
  
  
## Figure 5:
  library(lattice)
library(foreign)
library(car)

ltheme <- canonical.theme(color = FALSE)     
ltheme$strip.background$col <- "lightgrey"  
lattice.options(default.theme = ltheme)      

# Carregar base de dados .dta:
data <- read.dta("schwindt_table2.dta")

# Recodificar e ordenar a variável de período:
data$period <- factor(recode(data$period,
                             "'1995'='First Period';
                             '1994-1998'='First Period';
                             '1999'='Second Period';
                             '1998-2002'='Second Period'")
                      ,ordered=TRUE )

# Criar e ordenar fator com a variável legislatura:
data$legislature <- factor(data$legislature,
                           levels=c("Argentina","Colombia-Chamber","Costa Rica","Colombia-Senate"),
                           ordered=TRUE)

# Renomear vetor:
levels(data$legislature) <- c('Argentina\n 1995 (N=246/980); 1999 (N=257/1503)',
                              'Colombia Chamber\n1994-1998 (N=139/629); 1998-2002 (N=165/623)',
                              'Costa Rica\n1994-1998 (N=57/929); 1998-2002 (N=57/1183)',
                              'Colombia Senate\n1994-1998 (N=87/620); 1998-2002 (N=94/514)')

# Ordenar:
levels.theme <- names(sort(tapply(data$proportion,list(data$variable),mean)))
data$theme <- factor(data$variable,ordered=TRUE,levels=levels.theme)

# Definir layout do gráfico:
prop.vec <- c(.01,.02,.04,.08,.16,.32,.64)

# Calcular para plotar na escala log2:
axN <- log2(prop.vec)

# Criar gráfico:
p <-
  dotplot(theme~proportion|legislature,
          data=data, 
          groups=period, 
          layout = c(1,4),
          index.cond=list(c(3,4,2,1)),
          scales=list(cex=0.65,
                      x=list(at=prop.vec,
                             log=2
                      ),alternating=3),
          par.strip.text=list(lines=2.5,cex=0.8),
          panel=function(...) { 
            panel.abline(v=axN,col="lightgrey") 
            panel.abline(h=1:9,col="lightgrey",lty=2)
            panel.xyplot(...) 
          },
          as.table=FALSE, 
          pch=c(1,3) 
          ,cex=0.8,
          xlab="Proportion, plotted in the log_2 scale"
  )

# Plotar e salvar gráfico em arquivo .pdf: 
trellis.device(file="../schwindt_fig_final.pdf",device="pdf",color=FALSE,width=6,height=8)
print(p)
graphics.off()

  
  ---
  
  
## Figure 6:  
  
# Criar vetores para coeficientes, dp e nomes das variáveis:
coef.vec <- c( 1.31, .93, 1.46, .07, .96, .2, .22, -.21, -.32, -.27,.23, 
                 0, -.03, .13, .15, .31, -.10, .41)
se.vec <- c( .33, .32, .32, .37, .37, .13, .12, .12, .12, .07, .07, .01, .21,
             .14, .29, .25, .27, .93)
var.names <- c("Argentina", "Chile", "Colombia", "Mexico", "Venezuela", 
               "Retrospective egocentric\neconomic perceptions", 
               "Prospective egocentric\neconomic perceptions",
               "Retrospective sociotropic\neconomic perceptions", 
               "Prospective sociotropic\neconomic perceptions",
               "Ideological Distance\nfrom president", "Ideology", "Age", 
               "Female", "Education", "Academic sector", "Business sector",
               "Government sector", "Constant")

# Criar indicador para ordenar variáveis de cima para baixo no eixo y:    
y.axis <- c(length(coef.vec):1)

pdf("stevens_fig_final.pdf", height = 7, width = 6)

# Definir margens do gráfico:
par(mar=c(2, 9, 2, 1))

# Criar gráfico:
plot(coef.vec, y.axis, type = "p", axes = F, xlab = "", ylab = "", pch = 19, 
     cex = .6, xlim = c(-2.5,2.5), xaxs = "i", main = "") 
segments(coef.vec-qnorm(.975)*se.vec, y.axis, coef.vec+qnorm(.975)*se.vec, 
         y.axis, lwd =  1.5)
segments(coef.vec-qnorm(.95)*se.vec, y.axis -.1, coef.vec-qnorm(.95)*se.vec, 
         y.axis +.1, lwd = 1.1)
segments(coef.vec+qnorm(.95)*se.vec, y.axis -.1, coef.vec+qnorm(.95)*se.vec, 
         y.axis +.1, lwd = 1.1)
axis(1, at = seq(-2.5,2.5,by=.5), labels =  seq(-2.5,2.5,by=.5), tick = T,
     cex.axis = .8, mgp = c(2,.5,0))
axis(3, at = seq(-2.5,2.5,by=.5), labels =  seq(-2.5,2.5,by=.5), tick = T, 
     las  = 1, line =0, cex.axis = .8, mgp = c(2,.7,0))                                            
axis(2, at = y.axis, label = var.names, las = 1, tick = T, cex.axis = .8) 
abline(v=0, lty = 2)
box(bty = "o") 
text(1.2, 5, expression(R^{2} == .15), adj = 0, cex = .7)
text(1.2, 4, expression(paste("Adjusted ", R^{2} == .12, "")), adj = 0, cex = .7)
text(1.2, 3, "n = 500", adj = 0, cex = .7)
segments(1.1, 2.7, 1.1,5.3)
segments(1.1, 2.7, 5,2.7)
segments(1.1, 5.3, 5.2,5.3)

# Gerar arquivo .pdf:
dev.off() #turn off pdf device; graph is created in working directory.  


---
  
  
## Figure 7:
  
# São plotados 2 modelos de regressão em um mesmo gráfico. 
  
# Criar vetores com coeficientes, dp e nome das variáveis para cada modelo:
coef.vec.1<- c(0.18, -0.19,-0.39,-0.09, NA, 0.04,-0.86, 0.39,-3.76, -1.61,
              -0.34, -1.17, -1.15,-1.52, -1.66, -1.34,-2.89,-1.88,-1.08, 0.20)
se.vec.1 <- c(0.22, 0.22, 0.18,.29, NA, 0.08,0.26,0.29,0.36,.19,0.19, 0.22,
               0.22,0.25,0.28,0.32,0.48, 0.43,0.41, 0.20)
coef.vec.2 <- c(0.27,-0.19, NA, NA, 0.5, 0.04,-0.98,-.36,-3.66, -1.59,
                 -0.45, -1.24, -1.04, -1.83, -1.82, -1.21, -2.77, -1.34, -0.94, 0.13)
se.vec.2 <- c(0.22,0.24, NA, NA, 0.4, 0.09 , .31 , .30 , .37 , .21 , .21 , .24 , .24,
              .29 , .32 , .33 , .49 , .46 , .49 , .26)
var.names <- c("Zombie" , "SMD Only", "PR Only", "Costa Rican in PR", 
               "Vote share margin", "Urban-Rural Index","No factional\nmembership",
               "Legal professional", "1st Term", "2nd Term", "4th Term",
               "5th Term","6th Term","7th Term","8th Term","9th Term","10th Term",
               "11th Term","12th Term", "Constant")

# Ordenar variáveis no eixo y de cima para baixo:
y.axis <- length(var.names):1

# Criar objeto para ajustar pontos e linhas, a fim de diferenciar cada modelo:
adjust <- .2 

pdf("pekkanen_fig_final.pdf", height = 8, width = 7) 

# Criar um painel à esquerda com as categorias de variáveis do primeiro modelo 
# (e os colchetes):
layout(matrix(c(2,1),1,2),  
       widths = c(2, 5))

# Definir margens do gráfico:
par(mar=c(2,5,2,1))

# Criar gráfico com o primeiro modelo:
plot(coef.vec.1, y.axis+adjust, type = "p", axes = F, xlab = "", ylab = "", 
     pch = 19, cex = .8, 
     xlim = c(min((coef.vec.1-qnorm(.975)*se.vec.1 -.1), 
                  (coef.vec.2-qnorm(.975)*se.vec.2 -.1), na.rm = T),
              max((coef.vec.1+qnorm(.975)*se.vec.1 -.1), 
                  (coef.vec.2+qnorm(.975)*se.vec.2 -.1), na.rm = T)),  
     ylim = c(min(y.axis), max(y.axis)), main = "")
axis(1,pretty(coef.vec.1, 3))
axis(2, at = y.axis, label = var.names, las = 1, tick = T)
axis(3,pretty(coef.vec.1, 3))
abline(h = y.axis, lty = 2, lwd = .5, col = "light grey")
box()
segments(coef.vec.1-qnorm(.975)*se.vec.1, y.axis+adjust, 
         coef.vec.1+qnorm(.975)*se.vec.1, y.axis+adjust, lwd =  1.3)
segments(coef.vec.1-qnorm(.95)*se.vec.1, y.axis+adjust -.035, 
         coef.vec.1-qnorm(.95)*se.vec.1, y.axis+adjust +.035, lwd = 1.1)
segments(coef.vec.1+qnorm(.95)*se.vec.1, y.axis+adjust -.035, 
         coef.vec.1+qnorm(.95)*se.vec.1, y.axis+adjust +.035, lwd = 1.1)  
abline(v=0, lty = 2, lwd = 1.5)

# Adicionar o segundo modelo:
segments(coef.vec.2-qnorm(.975)*se.vec.2, y.axis-adjust, 
         coef.vec.2+qnorm(.975)*se.vec.2, y.axis-adjust, lwd =  1.3)
segments(coef.vec.2-qnorm(.95)*se.vec.2, y.axis-adjust -.035, 
         coef.vec.2-qnorm(.95)*se.vec.2, y.axis-adjust +.035, lwd = 1.1) 
segments(coef.vec.2+qnorm(.95)*se.vec.2, y.axis-adjust -.035, 
         coef.vec.2+qnorm(.95)*se.vec.2, y.axis-adjust +.035, lwd = 1.1)
points(coef.vec.2, y.axis-adjust, pch = 21, cex = .8, bg = "white" )

# Adicionar ao painel da esquerda as categorias de variáveis do segundo modelo:
par(mar=c(2,2,2,0)) 

# Criar plot vazio: 
plot(seq(0,1,length=length(var.names)), y.axis, type = "n", axes = F, 
     xlab = "", ylab = "")

# Definir formatação do segundo modelo no gráfico:    
left.side <- .55
segments(left.side,20.2,left.side,16.7)
segments(left.side,20.2,left.side+.15,20.2)
segments(left.side,16.7,left.side+.15,16.7)
text(.4, 18.5, "MP Type", srt = 90, font = 3)
segments(left.side,15.6,left.side,12.3) 
segments(left.side,15.6,left.side+.15,15.6) 
segments(left.side,12.3,left.side+.15,12.3)
text(.3, 14, "Misc\nControls", srt = 90, font = 3)
segments(left.side,12.15,left.side,1.8) 
segments(left.side,12.15,left.side+.15,12.15)   
segments(left.side,1.8,left.side+.15,1.8)
text(.4, 7, "Seniority", srt = 90, font = 3)

# Gerar arquivo .pdf:
dev.off()


---
  

## Figure 8:    
  
# Criar matriz com os coeficientes:
coef.matrix <- matrix(c(-.039, NA, .048, -.133, .071, -.795, 1.47, 
                          -.036, NA, .036, -.142, .07, -.834, 1.70, 
                          -.051, NA, .017, .05, .011, -.532, .775, 
                          -.037, -.02, .047, -.131,.072, -.783, 1.45,
                          -.034, -.018, -.035, -.139, .071, -.822, 1.68,
                          -.05, -.023, .016,-.049, .013, -.521, .819),nr=7)

# Criar vetor com o R^2 dos modelos:
R2<-  c(0.910,  0.910,  0.940,  0.910,  0.910,  0.940)

# Criar matriz com o erro padrão dos modelos:
se.matrix <- matrix(c(.003, NA, .011, .013, .028, .056, .152, .003, NA, .012,
                      .014, .029, .059, .171, .003, NA, .01, .013, .024, .044,
                      .124, .003, .005, .011, .013, .028, .055, .152, .003,
                      .005, .021, .014, .029, .059, .17, .003,.006, .01, .013,
                      .024, .044, .127),nr=7)

# Criar vetor com nome das variáveis: 
varnames<- c("% of county\nregistration", "Law change", "Log population", 
             "Log median\nfamily income","% population with\nh.s. education",
             "% population\nAfrican American", "Constant")



# Criar uma tabela .csv:
coef.sheet <- NULL
for(i in 1:7) {
  coef.sheet <- rbind(coef.sheet,coef.matrix[i,])
  coef.sheet <- rbind(coef.sheet,se.matrix[i,])
  coef.sheet <- rbind(coef.sheet,paste("[",coef.matrix[i,]-2*se.matrix[i,],",",
                                       coef.matrix[i,]+2*se.matrix[i,],"]",
                                       sep=""))
}
rownames(coef.sheet) <- rep(varnames,each=2)
rownames(coef.sheet)[seq(2,nrow(coef.sheet),2)] <- ""
colnames(coef.sheet) <- rep(c("Full Sample",
                              "Excluding counties\nw. partial registration",
                              "Full sample w. \nstate year dummies"),2)
write.csv(coef.sheet,file="Ansolabehere.csv")

# Excluir o intercepto:
coef.matrix<-coef.matrix[-(7),]
se.matrix<-se.matrix[-7,]

# Os paineis têm 6 modelos plotados em pares, os círculos pretos representam os 
# modelos com a dummy "law change" e os círculos vazados, modelos sem  a dummy
# "law change".each panel has at most six models, plotted in pairs.

# Criar lista vazia:
Y1 <- vector(length=0,mode="list")

# Pontos estimados com a dummy "law change" (da quarta à sexta coluna):
Y1$estimate <- coef.matrix[,4:6]

# IC de 95%:
Y1$lo <- coef.matrix[,4:6]-qnorm(0.975)*se.matrix[,4:6]
Y1$hi <- coef.matrix[,4:6]+qnorm(0.975)*se.matrix[,4:6]

# IC de 90%:
Y1$lo1 <- coef.matrix[,4:6]-qnorm(0.95)*se.matrix[,4:6]
Y1$hi1 <- coef.matrix[,4:6]+qnorm(0.95)*se.matrix[,4:6]

# Nomear as colunas de Y1:
rownames(Y1$estimate) <- varnames[-7] 

# Pontos estimados sem a dummy "law change":
Y2 <- vector(length=0,mode="list")
Y2$estimate <- coef.matrix[,1:3]
Y2$lo <- coef.matrix[,1:3]-qnorm(.975)*se.matrix[,1:3]
Y2$hi <- coef.matrix[,1:3]+qnorm(.975)*se.matrix[,1:3]
Y2$lo1 <- coef.matrix[,1:3]-qnorm(.95)*se.matrix[,1:3]
Y2$hi1 <- coef.matrix[,1:3]+qnorm(.95)*se.matrix[,1:3]
rownames(Y2$estimate) <- varnames[-7]

# Plotar pontos estimados em uma coluna:
source("plotReg.R")
pdf(file="../ansolabehere_fig_final.pdf",width=3,height=9)

# Criar gráfico:
tmp <- plot.reg(Y1,Y2,
                label.x=c("Full Sample","Excluding counties\nw. partial registration",
                          "Full sample w. \nstate year dummies"),
                refline=c(0,0,0,0,0,0),
                hlast=.12,
                print=F,
                lwd.fact=.9,
                length.arrow=unit(0.3,"mm"),
                widths=c(.35,.52,.15),
                rot.label.y=0,
                just.label.y="right",
                pos.label.y=0.95,
                pch.size=0.35
)
tmp <- editGrob(tmp,gPath("xaxis","labels"),rot=45,just="right")

# Plotar gráfico:
grid.draw(tmp) 

# Fechar dispositivo e gerar .pdf:
graphics.off()   

  