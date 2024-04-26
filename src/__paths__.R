ROOT                     <<- file.path(rprojroot::find_root(".projectile"))
## DATA
PATH_DATA_RAW            <<- file.path(ROOT,'data/raw')
PATH_DATA_INTERIM        <<- file.path(ROOT,'data/interim')
PATH_DATA_FINAL          <<- file.path(ROOT,'data/final')    
## SCRIPTS
PATH_SRC                 <<- file.path(ROOT,'src')
PATH_SRC_DATA_ORGANIZING <<- file.path(ROOT,'src/data-organizing')
PATH_SRC_DATA_COLLETCING <<- file.path(ROOT,'src/data-collecting')
PATH_SRC_MODEL           <<- file.path(ROOT,'src/model')        
PATH_SRC_EDA             <<- file.path(ROOT,'src/eda')        
## MAN
PATH_MAN                 <<- file.path(ROOT,'man')
PATH_MAN_FIGURES         <<- file.path(ROOT,'man/tables-and-figures')
PATH_MAN_TABLES          <<- file.path(ROOT,'man/tables-and-figures')
PATH_SM                  <<- file.path(ROOT,'man/supp-material')
PATH_SM_FIGURES          <<- file.path(PATH_SM,'tables-and-figures')
PATH_SM_TABLES           <<- file.path(PATH_SM,'tables-and-figures')
## OUTPUT
PATH_OUTPUTS             <<- file.path(ROOT,'out')
## REPORT
PATH_REPORTS             <<- file.path(ROOT,'reports')
## DOCS
PATH_DOCS                <<- file.path(ROOT,'docs')

cat('\n
- Relative paths loaded...
- Current working directory:\n ')
cat(getwd()); cat('\n\n')
