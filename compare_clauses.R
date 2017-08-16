source('compare_mssa.R')
source('detect_language.R')
files=list.files('English/')
filerange=1:length(files)

x=list()
for(i in filerange[301:400]){
  cat(i,' ')
  x[[i]]=compare_mssa(paste('English/',files[i],sep=''))
}
