# --- �Ŗސ���@
   ski <- matrix(scan("dat/ski.dat"),ncol=9,byrow=TRUE)
   p <- ncol(ski)
   m <- 3
   R <- cor(ski)
   library(psych)
   fa(ski, nfactors=m, rotate="none", fm="ml")
# --- EM�A���S���Y���ɂ�鐄��
   A.Old <- matrix(rep(0.5, p*m),nrow=p,ncol=m)
   A.Old[1, 2:3] <- 0
   A.Old[2, 3] <- 0
   D2.Old <- diag(R - A.Old %*% t(A.Old)); D2.Old <- diag(D2.Old) # --- �Ίp�s��
   I.m <- diag(1,m)
   max.iter <- 1000 # --- �ő�J��Ԃ���
   iter <- 1        # --- �J��Ԃ��̃J�E���^�[
   tol <- 10^(-4)   # --- ����l�̕ω��̋��e�x
   S.zz <- R        # --- S.zz�̏����t�����Ғl�̓f�[�^�̑��֍s��
# --- �J��Ԃ��v�Z
   while(iter < max.iter){
   # --- E�X�e�b�v
      Sigma <- A.Old %*% t(A.Old) + D2.Old
      delta <- t(A.Old) %*% solve(Sigma)
      S.zf.star <- S.zz %*% t(delta)
      S.ff.star <- delta %*% S.zz %*% t(delta) + (I.m - delta %*% A.Old )
   # --- M�X�e�b�v
      A.New <- S.zf.star %*% solve(S.ff.star)
      D2.New <- diag( S.zz - S.zf.star %*% solve(S.ff.star) %*% t(S.zf.star) )
   # --- ����l�̕ω��̌���
      diff <- max(abs(A.New - A.Old), abs(D2.New - diag(D2.Old)))
      if(diff < tol) break;
      A.Old <- A.New
      D2.Old <- diag(D2.New)
      iter <- iter + 1
   }

   round(1 - D2.New,2)
   A.New
   promax(A.New)

# --- �ŏ�2��@
   Hol <- matrix(scan(file="dat/Holdat.dat"),ncol=9,byrow=TRUE)
   fa(Hol, nfactors=3, rotate="none", fm="pa")  # --- �厲�@
   fa(Hol, nfactors=3, rotate="none", fm="gls") # --- ��ʉ��ŏ�2��@
# --- �厲�@�ɂ�鐄��
   R <- cor(Hol)
   p <- 9
   m <- 3
   max.iter <- 1000
   tol <- 10^(-3);
   D2.Old <- 1 / diag(solve(R)) # --- �Ǝ����̏����l
   iter <- 1
   while(iter < max.iter){
      # --- �ŗL�l�ƌŗL�x�N�g���̌v�Z
      E <- eigen(R - diag(D2.Old))
      # --- A�̐���l
      A.tilde <- E$vec[, 1:m] %*% sqrt(diag(E$val[1:m], m))
      # --- D^2�̍X�V
      D2.New <- diag(R - A.tilde %*% t(A.tilde)); diff <- max(abs(D2.New - D2.Old))
      if(diff < tol) break;
      D2.Old <- D2.New
      iter <- iter + 1
   }
   round(D2.New,4)
   round(A.tilde,4)
#
# --- �j���[�g���E���t�\���@�ɂ�鐄��
#
# --- �֐��̏���
#
   g <- function(d,E,m){
   #
   # ���z�x�N�g���̌v�Z
   #
   # ------
   # ���́F
   # ------
   # d	�Ǝ��W��
   # E	�ŗL�l�ƌŗL�x�N�g��
   # m	���q��
   # ------
   # �o�́F
   # ------
   # g	���z�x�N�g��
   #
      Eval2 <- E$val[-(1:m)]
      Evec2 <- E$vec[, -(1:m)]
      g <- rowSums(Evec2^2 %*% diag(Eval2))
      g <- -2 * d * g
      return(g)
   }

   sum.up.1 <- function(x, val1, vec1){
   #
   # �w�V�A���s����v�Z���邽�߂̕⏕�֐�
   #
   # ------
   # ���́F
   # ------
   # x		�傫�����ɕ��ׂ�(���q��+1)�Ԗڂ���̌ŗL�l
   # val1		�傫����������q���Ԗڂ܂ł̌ŗL�l
   # vec1		�Ή�����ŗL�x�N�g��
   # ------
   # �o�́F
   # ------
   # sum.up.1	�w�V�A���s��̈ꕔ�̌W��
   #
      denom <- x - val1
      numer <- x + val1
      coef <- numer / denom
      sum.up.1 <- vec1 %*% (coef * t(vec1))
      return(list(sum.up.1))
   }


   H.exact <- function(d,E,m){
   #
   # �w�V�A���s��̌v�Z
   #
   # ------
   # ���́F
   # ------
   # d	�Ǝ��W��
   # E	�ŗL�l�ƌŗL�x�N�g��
   # m	���q��
   # ------
   # �o�́F
   # ------
   # H	�w�V�A���s��
   #
      d <- as.vector(d)
      p <- length(d)
      dd <- outer(d,d)
      Eval1 <- E$val[1:m]
      Evec1 <- E$vec[, (1:m)]
      Eval2 <- E$val[-(1:m)]
      Evec2 <- E$vec[, -(1:m)]
      upu.omega.i1.j1 <- sapply(Eval2, sum.up.1, val1=Eval1, vec1=Evec1)
      omega.im.jm <- apply(Evec2, 2, function(x) return(list(outer(x,x))) )
      omega.im.jm <- unlist(omega.im.jm, recursive=FALSE)
      H.1 <- rowSums(mapply("*", omega.im.jm, upu.omega.i1.j1)); dim(H.1) <- c(p,p)
      H.1 <- dd * H.1
      d.2.mat <- matrix(rep(d^2, each=(p-m)), nrow=(p-m), ncol=p)
      H.2 <- t(Evec2^2) * ( d.2.mat - (Eval2/2) )
      H.2 <- colSums(H.2)
      H.2 <- diag(H.2)
      H <- 4 * (H.1 + H.2)
      return(H)
   }

   H.approx <- function(d,E,m){
   #
   # �w�V�A���s��̋ߎ����v�Z����
   #
   # ------
   # ���́F
   # ------
   # d	�Ǝ��W��
   # E	�ŗL�l�ƌŗL�x�N�g��
   # m	���q��
   # ------
   # �o�́F
   # ------
   # H	�ߎ��w�V�A���s��
   #
      d <- as.vector(d)
      dd <- outer(d,d)
      Evec2 <- E$vec[,-(1:m)]
      H <- Evec2 %*% t(Evec2)
      H <- 4 * dd * H^2
      return(H)
   }





   update <- function(m, fit, d, direc, stepsize, n.try){
   #
   # ����l�̍X�V
   #
   # ------
   # ���́F
   # ------
   # m			���q��
   # fit		�X�V�O�̌덷�֐��̒l
   # d			�X�V�O�̓Ǝ��W��
   # direc		�w�V�A���s��̋t�s��x���z�x�N�g�� H^(-1)g
   # stepsize	�X�e�b�v�T�C�Y
   # n.try		�X�e�b�v�T�C�Y�����񐔂̃J�E���^�[
   # ------
   # �o�́F
   # ------
   # est		�X�V��̓Ǝ��W������l
   # E			�X�V��̓Ǝ��W���ɂ��ŗL�l�ƌŗL�x�N�g��
   # fit		�X�V��̌덷�֐��̒l
   #
      d.New <- (d - (stepsize * direc))
      E.New <- eigen(R - diag(as.vector(d.New^2)), sym=TRUE)
      fit.New <- sum(E.New$val[-(1:m)]^2)
      if( ((fit.New > fit) & (n.try < 10)) ){
            Recall(m=m, fit=fit, d=d, direc=direc,
                        stepsize=(stepsize/2), n.try=(n.try+1))
      }else if(n.try == 10){
            stop("****** �X�e�b�v�T�C�Y�̔����񐔂�10�ɂȂ�܂��� ******\n")
      }else{
            return(list(est=d.New, E=E.New, fit=fit.New))
      }
   }

   ULS <- function(R, m, d, max.iter, tol){
   #
   # �ŏ�2��@�ɂ��Ǝ��W���̐���
   # �j���[�g���E���t�\���@�ɂ��덷�֐��̍ŏ���
   #
   # ------
   # ���́F
   # ------
   # R			���֍s��
   # m			���q��
   # d			�Ǝ��W���̏����l
   # max.iter	�ő�J��Ԃ���
   # tol		�����
   # ------
   # �o�́F
   # ------
   # d.New		�Ǝ��W���̐���l�x�N�g��
   # E.New		����l�����^�Ƃ���(R-D^2)�̌ŗL�l�ƌŗL�x�N�g��
   # fit		�덷�֐��̒l
   #
      d.Old <- d
      E.Old <- eigen(R - diag(as.vector(d.Old^2)),sym=TRUE)
      fit.Old <- sum(E.Old$val[-(1:m)]^2)
      differ <- 10
      iter <- 1
      while(iter < max.iter){
            gra <- g(d=d.Old, E=E.Old, m=m)
            if(max(differ) > 0.1){
                  Hes <- H.approx(d=d.Old, E=E.Old, m=m)
            }else {
                  Hes <- H.exact(d=d.Old, E=E.Old, m=m)
            }
            direc <- solve(Hes) %*% gra
            New <- update(m=m, fit=fit.Old, d=d.Old, direc=direc, stepsize=1, n.try=1)
            differ <- New$est - d.Old
            if(max(abs(differ)) < tol) break;
            d.Old <- New$est
            E.Old <- New$E
            fit.Old <- New$fit
            iter <- iter+1		
      }
      return(list(d.New=as.vector(New$est), E.New=New$E, fit=New$fit))
   }
#
# --- �����l
#
   D2.Old <- 1 / diag(solve(R))
   d.Old <- sqrt(D2.Old)
# --- �ŏ�2�搄��l
   out.ULS <- ULS(R=R, m=m, d=d.Old, max.iter=1000, tol=10^(-3))
   round(out.ULS$d.New^2,4)
   Evec <- out.ULS$E.New$vec
   Eval <- out.ULS$E.New$val
   A.ULS <- Evec[, 1:m] %*% diag(sqrt(Eval[1:m]))
   round(A.ULS, 4)
#
# --- �x�C�Y�I����@
#
   library(MCMCpack); rika <- matrix(scan(file="dat/rika.dat"),ncol=4,byrow=TRUE)
   out.MCMC <- MCMCfactanal(rika, factors=1, burnin=25000, mcmc=50000,thin=5,
        lambda.constraints=list( V1=list(1,"+") ),l0=0,L0=1,a0=1,b0=0.15,seed=123123)
# --- mcmc���������I�u�W�F�N�gout.MCMC�Ɋւ��ėv�񓝌v�ʂ��Z�o����
   summary(out.MCMC)