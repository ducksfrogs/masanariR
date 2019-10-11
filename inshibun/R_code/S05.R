# --- 最尤推定法
   ski <- matrix(scan("dat/ski.dat"),ncol=9,byrow=TRUE)
   p <- ncol(ski)
   m <- 3
   R <- cor(ski)
   library(psych)
   fa(ski, nfactors=m, rotate="none", fm="ml")
# --- EMアルゴリズムによる推定
   A.Old <- matrix(rep(0.5, p*m),nrow=p,ncol=m)
   A.Old[1, 2:3] <- 0
   A.Old[2, 3] <- 0
   D2.Old <- diag(R - A.Old %*% t(A.Old)); D2.Old <- diag(D2.Old) # --- 対角行列
   I.m <- diag(1,m)
   max.iter <- 1000 # --- 最大繰り返し数
   iter <- 1        # --- 繰り返しのカウンター
   tol <- 10^(-4)   # --- 推定値の変化の許容度
   S.zz <- R        # --- S.zzの条件付き期待値はデータの相関行列
# --- 繰り返し計算
   while(iter < max.iter){
   # --- Eステップ
      Sigma <- A.Old %*% t(A.Old) + D2.Old
      delta <- t(A.Old) %*% solve(Sigma)
      S.zf.star <- S.zz %*% t(delta)
      S.ff.star <- delta %*% S.zz %*% t(delta) + (I.m - delta %*% A.Old )
   # --- Mステップ
      A.New <- S.zf.star %*% solve(S.ff.star)
      D2.New <- diag( S.zz - S.zf.star %*% solve(S.ff.star) %*% t(S.zf.star) )
   # --- 推定値の変化の検討
      diff <- max(abs(A.New - A.Old), abs(D2.New - diag(D2.Old)))
      if(diff < tol) break;
      A.Old <- A.New
      D2.Old <- diag(D2.New)
      iter <- iter + 1
   }

   round(1 - D2.New,2)
   A.New
   promax(A.New)

# --- 最小2乗法
   Hol <- matrix(scan(file="dat/Holdat.dat"),ncol=9,byrow=TRUE)
   fa(Hol, nfactors=3, rotate="none", fm="pa")  # --- 主軸法
   fa(Hol, nfactors=3, rotate="none", fm="gls") # --- 一般化最小2乗法
# --- 主軸法による推定
   R <- cor(Hol)
   p <- 9
   m <- 3
   max.iter <- 1000
   tol <- 10^(-3);
   D2.Old <- 1 / diag(solve(R)) # --- 独自性の初期値
   iter <- 1
   while(iter < max.iter){
      # --- 固有値と固有ベクトルの計算
      E <- eigen(R - diag(D2.Old))
      # --- Aの推定値
      A.tilde <- E$vec[, 1:m] %*% sqrt(diag(E$val[1:m], m))
      # --- D^2の更新
      D2.New <- diag(R - A.tilde %*% t(A.tilde)); diff <- max(abs(D2.New - D2.Old))
      if(diff < tol) break;
      D2.Old <- D2.New
      iter <- iter + 1
   }
   round(D2.New,4)
   round(A.tilde,4)
#
# --- ニュートン・ラフソン法による推定
#
# --- 関数の準備
#
   g <- function(d,E,m){
   #
   # 勾配ベクトルの計算
   #
   # ------
   # 入力：
   # ------
   # d	独自係数
   # E	固有値と固有ベクトル
   # m	因子数
   # ------
   # 出力：
   # ------
   # g	勾配ベクトル
   #
      Eval2 <- E$val[-(1:m)]
      Evec2 <- E$vec[, -(1:m)]
      g <- rowSums(Evec2^2 %*% diag(Eval2))
      g <- -2 * d * g
      return(g)
   }

   sum.up.1 <- function(x, val1, vec1){
   #
   # ヘシアン行列を計算するための補助関数
   #
   # ------
   # 入力：
   # ------
   # x		大きい順に並べて(因子数+1)番目からの固有値
   # val1		大きい方から因子数番目までの固有値
   # vec1		対応する固有ベクトル
   # ------
   # 出力：
   # ------
   # sum.up.1	ヘシアン行列の一部の係数
   #
      denom <- x - val1
      numer <- x + val1
      coef <- numer / denom
      sum.up.1 <- vec1 %*% (coef * t(vec1))
      return(list(sum.up.1))
   }


   H.exact <- function(d,E,m){
   #
   # ヘシアン行列の計算
   #
   # ------
   # 入力：
   # ------
   # d	独自係数
   # E	固有値と固有ベクトル
   # m	因子数
   # ------
   # 出力：
   # ------
   # H	ヘシアン行列
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
   # ヘシアン行列の近似を計算する
   #
   # ------
   # 入力：
   # ------
   # d	独自係数
   # E	固有値と固有ベクトル
   # m	因子数
   # ------
   # 出力：
   # ------
   # H	近似ヘシアン行列
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
   # 推定値の更新
   #
   # ------
   # 入力：
   # ------
   # m			因子数
   # fit		更新前の誤差関数の値
   # d			更新前の独自係数
   # direc		ヘシアン行列の逆行列x勾配ベクトル H^(-1)g
   # stepsize	ステップサイズ
   # n.try		ステップサイズ半減回数のカウンター
   # ------
   # 出力：
   # ------
   # est		更新後の独自係数推定値
   # E			更新後の独自係数による固有値と固有ベクトル
   # fit		更新後の誤差関数の値
   #
      d.New <- (d - (stepsize * direc))
      E.New <- eigen(R - diag(as.vector(d.New^2)), sym=TRUE)
      fit.New <- sum(E.New$val[-(1:m)]^2)
      if( ((fit.New > fit) & (n.try < 10)) ){
            Recall(m=m, fit=fit, d=d, direc=direc,
                        stepsize=(stepsize/2), n.try=(n.try+1))
      }else if(n.try == 10){
            stop("****** ステップサイズの半減回数が10になりました ******\n")
      }else{
            return(list(est=d.New, E=E.New, fit=fit.New))
      }
   }

   ULS <- function(R, m, d, max.iter, tol){
   #
   # 最小2乗法による独自係数の推定
   # ニュートン・ラフソン法による誤差関数の最小化
   #
   # ------
   # 入力：
   # ------
   # R			相関行列
   # m			因子数
   # d			独自係数の初期値
   # max.iter	最大繰り返し回数
   # tol		収束基準
   # ------
   # 出力：
   # ------
   # d.New		独自係数の推定値ベクトル
   # E.New		推定値を所与とした(R-D^2)の固有値と固有ベクトル
   # fit		誤差関数の値
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
# --- 初期値
#
   D2.Old <- 1 / diag(solve(R))
   d.Old <- sqrt(D2.Old)
# --- 最小2乗推定値
   out.ULS <- ULS(R=R, m=m, d=d.Old, max.iter=1000, tol=10^(-3))
   round(out.ULS$d.New^2,4)
   Evec <- out.ULS$E.New$vec
   Eval <- out.ULS$E.New$val
   A.ULS <- Evec[, 1:m] %*% diag(sqrt(Eval[1:m]))
   round(A.ULS, 4)
#
# --- ベイズ的推定法
#
   library(MCMCpack); rika <- matrix(scan(file="dat/rika.dat"),ncol=4,byrow=TRUE)
   out.MCMC <- MCMCfactanal(rika, factors=1, burnin=25000, mcmc=50000,thin=5,
        lambda.constraints=list( V1=list(1,"+") ),l0=0,L0=1,a0=1,b0=0.15,seed=123123)
# --- mcmc属性をもつオブジェクトout.MCMCに関して要約統計量を算出する
   summary(out.MCMC)
