# -Clusters
library(ggplot2)


library(flexclust)

############################################
########  K-Means  #########################
############################################

#Carregamento dos dados
data("mtcars")

df=scale(mtcars)
head(df, n=3)



fviz_nbclust(df, kmeans, method = "wss")+
  geom_vline(xintercept = 4, linetype = 3)  

# Clusterização k-means

# set.seed(123)
km.res=kmeans(df, 4, nstart=25)
print(km.res)  

aggregate(df, by=list(cluster=km.res$cluster), mean)



df2=cbind(df, cluster=km.res$cluster)
head(df2)

# PLOT !!!!!!!!!!!!!!!!!!!
fviz_cluster(km.res, data=df2,
             palette = c("#2E9FDF", "#00BB32", "#E7B800", "#FC4E07"),
             ellipse.type="euclid",
             star.plot=TRUE,
             repel=TRUE,
             ggtheme=theme_minimal()
)


####################################################
########## Clusterização Aglomerativa #############
###################################################

#Carregamento dos dados
# data("mtcars")

df=scale(df) # padonização Z(0,1)
head(df, n=3)


# method = "euclidean", "maximum", "manhattan", "canberra"
dista=dist(df, method="euclidean")
as.matrix(dista)[1:5,1:5]


# O método a ser utilizado pode ser: ward.D, ward.D2,
#    single, complete, average, mcquitty, median ou centroid,

dista.hc=hclust(d=dista, method="ward.D2")

# Plot !!!!!!!!!!!!!!!!!!
# cex = tamanho das letras
fviz_dend(dista.hc, cex=0.5)

