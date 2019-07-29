#���C�u�����[�ƃf�[�^�̓ǂݍ���
library(psych)
(���Ȏ���dat <- read.csv('dat/���Ȏ���.csv',header=T,row.names="���k"))

# �q�X�g�O����
par(mfrow = c(2, 2))
rika <- c("����","���w","����","�n�w")
for(i in 1:4){
hist(���Ȏ���dat[,i],main=rika[i],breaks=seq(0,100,10),xlab="���_",ylab="�x��",xlim=c(0,100),ylim=c(0,5))
}
dev.off()

# �U�z�}
plot(���Ȏ���dat,xlim=c(20,100),ylim=c(20,100))

# �Z�p����
mean(���Ȏ���dat)

# �����l
apply(���Ȏ���dat,2,median)

# �W�{���U(�u���Ȏ����f�[�^�v�́u�����v�̏ꍇ)
sum((���Ȏ���dat[1]-colMeans(���Ȏ���dat)[1])^2)/10
# �s�Ε��U�ɂ��Ă͊֐�var��p���āCvar(���Ȏ���dat[1])�ɂ���ċ��߂���

# ���ϗʃf�[�^�̕W�{���U�����U���Z�o����
bvar <- function(x){(var(x)*(nrow(x)-1))/nrow(x)}   
bvar(���Ȏ���dat)

# �W���΍�
sqrt(diag(bvar(���Ȏ���dat)))

# �f�[�^�̕W����
standscale <- function(x){t((t(x)-colMeans(x))/sqrt(diag(bvar(x))))}
standscale(���Ȏ���dat)

# ���֌W��
cor(���Ȏ���dat)

# ���ʏW�c�̑��֌W��
cor(���Ȏ���dat[c(6,7,8,9,10),])