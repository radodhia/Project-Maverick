files=list.files('English/')
filerange=1:length(files)

x=list()
fileinfo=NULL
for(i in filerange[301:400]){
  
  d = readLines(file(paste('English/',files[i],sep='')))
  d = trimws(d)
  d=gsub('\\s{2,}',' ',d)
  d = d[d != '']
  
  is.mssa=length(grep('Master Supplier',d))>0
  is.mmva=F
  if(!is.mssa)
    is.mmva=length(grep('Master Microsoft Vendor',d))>0
  lang=detect_language(paste(d[101:110],collapse=' '))
  
  
  temp_pos_section = grep('^section', tolower(d))
  if(length(temp_pos_section)==0)
    temp_pos_section = grep('^\\d{1,2}\\. ', tolower(d))
  if(length(temp_pos_section)==0)
    use=FALSE
  else
    use=TRUE
  new=c(file,lang,ifelse(is.mssa,'MSSA',ifelse(is.mmva,'MMVA','other')),use)
 # cat(i,' ',new,'\n')
  fileinfo=rbind(fileinfo,new)
}