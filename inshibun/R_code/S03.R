## �p�b�P�[�W�C���K�p�֐��C�f�[�^�̓ǂݍ���
library("polycor")
library("psych")
myfa<-function(X,m=1,...)
  {fa(r=X,nfactors=m,fm='ml',rotate='promax',...)}
myscree<-function(r,b="�ŗL�l�̏���")
{plot(fa(r)$e.values,type='o',xlab=b,ylab="�ŗL�l")}
data.cont <- read.csv('dat/6�Ȗڊw��(�ʓI).csv', header=TRUE)
data.disc <- read.csv('dat/6�Ȗڊw��(���I).csv', header=TRUE)


## �e�g���R���b�N���֌W��
tetran <- polychor(x=data.disc$�ÓT, y=data.disc$�n�w, std.err=TRUE, ML=TRUE)
tetran$rho; tetran$row.cuts; tetran$col.cuts; tetran$var


## �|���R���b�N���֌W��
pola <- polychor(x=data.disc$�ÓT, y=data.disc$���㕶, std.err=TRUE, ML=TRUE)
pola$rho; pola$row.cuts; pola$col.cuts; pola$var

polychor(x=data.disc$�ÓT, y=data.disc$����, std.err=FALSE, ML=TRUE)
polychor(x=data.disc$�n�w, y=data.disc$���㕶, std.err=FALSE, ML=TRUE)
polychor(x=data.disc$�n�w, y=data.disc$����, std.err=FALSE, ML=TRUE)
polychor(x=data.disc$���㕶, y=data.disc$����, std.err=FALSE, ML=TRUE)


## �o�C�V���A�����֌W��
bicle <- polycor::polyserial(x=data.disc$�p��, y=data.disc$�ÓT, std.err=TRUE, ML=TRUE)
bicle$rho; bicle$cuts; bicle$var

polycor::polyserial(x=data.disc$���w, y=data.disc$�ÓT, std.err=FALSE, ML=TRUE)
polycor::polyserial(x=data.disc$�p��, y=data.disc$�n�w, std.err=FALSE, ML=TRUE)
polycor::polyserial(x=data.disc$���w, y=data.disc$�n�w, std.err=FALSE, ML=TRUE)


## �|���V���A�����֌W��
polastar <- polycor::polyserial(x=data.disc$�p��, y=data.disc$���㕶, std.err=TRUE, ML=TRUE)
polastar$rho; polastar$cuts; polastar$var

polycor::polyserial(x=data.disc$���w, y=data.disc$���㕶, std.err=FALSE, ML=TRUE)
polycor::polyserial(x=data.disc$�p��, y=data.disc$����, std.err=FALSE, ML=TRUE)
polycor::polyserial(x=data.disc$���w, y=data.disc$����, std.err=FALSE, ML=TRUE)


## �s�A�\���̐ϗ����֌W��
cor(x=data.disc$�p��, y=data.disc$���w, use="complete.obs")


## ���I���֌W���𗘗p�������֍s��̌v�Z
data.disc2 <- data.disc
data.disc2$���㕶 <- as.factor(data.disc$���㕶)
data.disc2$�ÓT <- as.factor(data.disc$�ÓT)
data.disc2$���� <- as.factor(data.disc$����)
data.disc2$�n�w <- as.factor(data.disc$�n�w)

discor <- hetcor(data=data.disc2, std.err=TRUE, ML=TRUE, use="complete.obs")
(cormt.disc <- discor$correlations)


##�u6�Ȗڃf�[�^(�ʓI)�v�̑��֍s��
(cormt.cont <- cor(data.cont)) 


## �u6�Ȗڃf�[�^(���I)�v����ϗ����֌W����p���ċ��߂��s�K�؂ȑ��֍s��
(cormt.bad <- cor(data.disc))


## ���I���q���͂̎��s
myscree(cormt.disc)
(result.disc <- myfa(X=cormt.disc, m=2, n.obs=1000))


## ���I�ړx���܂ރf�[�^�ɁC�ʏ�̈��q���͂��s��
(result.bad <- myfa(X=data.disc, m=2))


## �A���ړx�݂̂��܂ރf�[�^�ɁC�ʏ�̈��q���͂��s��
(result.cont <- myfa(X=data.cont, m=2))