install.packages('psych')
install.packages('GPArotation')
library('psych')
library('GPArotation')
library('tidyverse')
library('data.table')
library('dplyr')
library('ggplot2')

#Step 1. 데이터 전처리 
#잘 모르겠음 응답인 행 제거
df<-subset(df,`2-4 집에서 학교로 갈 때 교통사고나 안전사고 위험은 어느 정도라고 생각하나요`!='잘 모르겠음'&`3-5 표시한 길에서 교통사고와 안전사고 위험은 어느 정도라고 생각하나요`!='잘 모르겠음')
df_name<-df %>% select(c(학교,클러스터))
df<-df %>% select(-c(학교,클러스터))
#label encoding
label1<-c("매우 안전","안전한 편","보통","위험한 편","매우 위험")
label2<-c("그런 적이 없다","가끔 그랬다","자주 그랬다")
col.names<-colnames(df)[1:2]
for (col in names(df)){
  if (col %in% col.names){
    df[[col]]<-factor(df[[col]],levels=label1,ordered=TRUE)
    df[[col]]<-as.numeric(df[[col]])
  } else {
    df[[col]]<-factor(df[[col]],levels=label2,ordered=TRUE)
    df[[col]]<-as.numeric(df[[col]])
  }
}

#Step 2. 요인 갯수 확인
#eigenvalue로 확인
cor_df<-cor(df)
eigen_val<-eigen(cor_df)
eigen_val$values
#scree plot으로 확인
VSS.scree(df)

#Step 3. 요인분석/점수 결과 도출
df_factor<-factanal(df,factors=5,rotation='varimax',scores='regression')
df_factor
#요인점수 산출
df_scores<-as.data.frame(df_factor$scores)

#Step 4. ANOVA 검정
aov_df<-cbind(df_name$클러스터,df_scores)
colnames(aov_df)<-c('cluster','f1','f2','f3','f4','f5')
head(aov_df)
#factor1에 대한 ANOVA
res1<-aov(f1~cluster,aov_df)
summary(res1)
#factor2에 대한 ANOVA
res2<-aov(f2~cluster,aov_df)
summary(res2)
#factor3에 대한 ANOVA
res3<-aov(f3~cluster,aov_df)
summary(res3)
#factor4에 대한 ANOVA
res4<-aov(f4~cluster,aov_df)
summary(res4)
#factor5에 대한 ANOVA
res5<-aov(f5~cluster,aov_df)
summary(res5)

#Step 5. 사후분석 
#factor1
pairwise.t.test(aov_df$f1, aov_df$cluster, p.adjust.method = 'bonferroni')
#factor2
pairwise.t.test(aov_df$f2, aov_df$cluster, p.adjust.method = 'bonferroni')
#factor4
pairwise.t.test(aov_df$f4, aov_df$cluster, p.adjust.method = 'bonferroni')
#factor5
pairwise.t.test(aov_df$f5, aov_df$cluster, p.adjust.method = 'bonferroni')
#위험도 평균값 클러스터별로 확인
aov_df %>% group_by(cluster) %>% summarise(f1danger=mean(f1),f2danger=mean(f2),f3danger=mean(f3),f4danger=mean(f4),f5danger=mean(f5))