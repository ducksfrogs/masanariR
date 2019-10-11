library(psych); library(GPArotation)

# 「スキー場データ」の読み込み
SDdat <- read.csv('dat/ski.csv', header=T, row.names=1)

# 「スキー場データ」の因子分析
fa(SDdat, nfactors=3, rotate="varimax", fm="ml")      # バリマックス回転
fa(SDdat, nfactors=3, rotate="quartimax", fm="ml")    # コーティマックス回転
fa(SDdat, nfactors=3, rotate="geominT", fm="ml")      # 直交ジオミン回転
fa(SDdat, nfactors=3, rotate="promax", fm="ml")       # プロマックス回転
fa(SDdat, nfactors=3, rotate="oblimin", fm="ml")      # オブリミン回転
fa(SDdat, nfactors=3, rotate="biquartimin", fm="ml")  # バイコーティミン回転
fa(SDdat, nfactors=3, rotate="simplimax", fm="ml")    # シンプリマックス回転
fa(SDdat, nfactors=3, rotate="geominQ", fm="ml")      # 斜交ジオミン回転

# 「YG性格検査データ」の読み込み
YGdat <- read.csv('dat/YG3.csv', header=T, sep=',') 

# 「YG性格検査データ」の因子分析
fa(YGdat, nfactors=2, rotate="promax", fm="ml")       # プロマックス回転
fa(YGdat, nfactors=2, rotate="oblimin", fm="ml")      # オブリミン回転
fa(YGdat, nfactors=2, rotate="simplimax", fm="ml")    # シンプリマックス回転
fa(YGdat, nfactors=2, rotate="geominQ", fm="ml")      # 斜交ジオミン回転




