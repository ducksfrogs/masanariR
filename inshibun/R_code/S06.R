library(psych); library(GPArotation)

# �u�X�L�[��f�[�^�v�̓ǂݍ���
SDdat <- read.csv('dat/ski.csv', header=T, row.names=1)

# �u�X�L�[��f�[�^�v�̈��q����
fa(SDdat, nfactors=3, rotate="varimax", fm="ml")      # �o���}�b�N�X��]
fa(SDdat, nfactors=3, rotate="quartimax", fm="ml")    # �R�[�e�B�}�b�N�X��]
fa(SDdat, nfactors=3, rotate="geominT", fm="ml")      # �����W�I�~����]
fa(SDdat, nfactors=3, rotate="promax", fm="ml")       # �v���}�b�N�X��]
fa(SDdat, nfactors=3, rotate="oblimin", fm="ml")      # �I�u���~����]
fa(SDdat, nfactors=3, rotate="biquartimin", fm="ml")  # �o�C�R�[�e�B�~����]
fa(SDdat, nfactors=3, rotate="simplimax", fm="ml")    # �V���v���}�b�N�X��]
fa(SDdat, nfactors=3, rotate="geominQ", fm="ml")      # �Ό��W�I�~����]

# �uYG���i�����f�[�^�v�̓ǂݍ���
YGdat <- read.csv('dat/YG3.csv', header=T, sep=',') 

# �uYG���i�����f�[�^�v�̈��q����
fa(YGdat, nfactors=2, rotate="promax", fm="ml")       # �v���}�b�N�X��]
fa(YGdat, nfactors=2, rotate="oblimin", fm="ml")      # �I�u���~����]
fa(YGdat, nfactors=2, rotate="simplimax", fm="ml")    # �V���v���}�b�N�X��]
fa(YGdat, nfactors=2, rotate="geominQ", fm="ml")      # �Ό��W�I�~����]



