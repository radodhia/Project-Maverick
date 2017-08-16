library(data.table)
d=readLines(file('Examples/Master Supplier Services Agreement (MSSA)_Standard Term Examples.txt',encoding = 'utf-8'))
d=trimws(d)
d=d[d!='']

#### SECTIONS ####
temp_pos_section=grep('^section',tolower(d))
pos_section=c(1,temp_pos_section,length(d)+1)
sections=c(NA,1:(length(pos_section)-1))
section=NULL
for(i in 1:(length(pos_section)-1)) 
  section=c(section,rep(sections[i],times=pos_section[i+1] - pos_section[(i)]))


#### Level 2 letters ####
pos_level2=list()
for(i in letters[1:26])
  pos_level2[[i]]=grep(paste('^\\(',i,'\\)',sep=''),tolower(d))
level2=rep(NA,280)
for(i in names(pos_level2))
  level2[pos_level2[[i]]]=i
previous=NA
for(i in 44:280){
  current=level2[i]
  if(is.na(current) | (current=='i' & previous !='h') | (current=='v' & previous !='u') | (current=='x' & previous !='w'))
    level2[i]=previous
  previous=level2[i]
}
level2[temp_pos_section]=NA

#### Level 3 numbers ####
pos_level3=list()
for(i in 1:12)
  pos_level3[[i]]=grep(paste('^\\(',i,'\\)',sep=''),tolower(d))
level3=rep(NA,280)
for(i in 1:length(pos_level3))
  level3[pos_level3[[i]]]=i

previous=NA
previous_letter=NA
for(i in 44:280){
  current=level3[i]
  current_letter=level2[i]
  if(is.na(current) & identical(current_letter,previous_letter))
    level3[i]=previous
  previous=level3[i]
  previous_letter=level2[i]
}


#### Level 4 roman ####
pos_level4=list()
for(i in c('i','ii','iii','iv','v','vi','vii','viii','ix','x','x1'))
  pos_level4[[i]]=grep(paste('^\\(',i,'\\)',sep=''),tolower(d))
level4=rep(NA,280)
for(i in names(pos_level4))
  level4[pos_level4[[i]]]=i

template=setDT(data.frame(section,level2,level3,level4,text=d))
template=template[!is.na(section)]
