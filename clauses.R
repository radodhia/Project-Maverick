compare_mssa=function(file){
  source('detect_language.R')
  library(data.table)
  library(stringdist)
  d = readLines(file(file))
  d = trimws(d)
  d = d[d != '']
  
  lang=detect_language(paste(d[101:120],collapse=' '))
  print(lang)
  if(lang=='English') {
    
    
    end = grep('remainder of', tolower(d))
    if(length(end)==0)
      end = grep('remainderof', tolower(d))
    if(length(end)>0)
      d = d[1:(end - 1)]
    
    
    nlines = length(d)
    
    #### SECTIONS ####
    temp_pos_section = grep('^section', tolower(d))
    pos_section = c(1, temp_pos_section, length(d) + 1)
    sections = c(NA, 1:(length(pos_section) - 1))
    section = NULL
    for (i in 1:(length(pos_section) - 1))
      section = c(section, rep(sections[i], times = pos_section[i + 1] - pos_section[(i)]))
    
    startsections = temp_pos_section[1]
    
    #### Level 2 letters ####
    pos_level2 = list()
    for (i in letters[1:26])
      pos_level2[[i]] = grep(paste('^\\(', i, '\\)', sep = ''), tolower(d))
    level2 = rep(NA, nlines)
    for (i in names(pos_level2))
      level2[pos_level2[[i]]] = i
    previous = NA
    for (i in startsections:nlines) {
      current = level2[i]
      if (is.na(current) |
          (current == 'i' &
           previous != 'h') |
          (current == 'v' &
           previous != 'u') | (current == 'x' & previous != 'w'))
        level2[i] = previous
      previous = level2[i]
    }
    level2[temp_pos_section] = NA
    
    #### Level 3 numbers ####
    pos_level3 = list()
    for (i in 1:12)
      pos_level3[[i]] = grep(paste('^\\(', i, '\\)', sep = ''), tolower(d))
    level3 = rep(NA, nlines)
    for (i in 1:length(pos_level3))
      level3[pos_level3[[i]]] = i
    
    previous = NA
    previous_letter = NA
    for (i in startsections:nlines) {
      current = level3[i]
      current_letter = level2[i]
      if (is.na(current) & identical(current_letter, previous_letter))
        level3[i] = previous
      previous = level3[i]
      previous_letter = level2[i]
    }
    
    
    #### Level 4 roman ####
    pos_level4 = list()
    for (i in c('i', 'ii', 'iii', 'iv', 'v', 'vi', 'vii', 'viii', 'ix', 'x', 'x1'))
      pos_level4[[i]] = grep(paste('^\\(', i, '\\)', sep = ''), tolower(d))
    level4 = rep(NA, nlines)
    for (i in names(pos_level4))
      level4[pos_level4[[i]]] = i
    
    previous = NA
    previous_letter = NA
    for (i in startsections:nlines) {
      current = level4[i]
      current_level2 = level2[i]
      if (!is.na(current) & current == 'i' & current_level2 == 'i')
        level4[i] = NA
      if (!is.na(current) & current == 'v' & current_level2 == 'v')
        level4[i] = NA
      previous = level4[i]
    }
    
    #### Clean up data ####
    c1temp = setDT(data.frame(section, level2, level3, level4, d))
    c1temp = c1temp[!is.na(section)]
    c1temp = c1temp[grep('^Master', c1temp$d, inv = T)]
    c1temp = c1temp[grep('DocuSign Envelope', c1temp$d, inv = T)]
    c1temp2 = tapply(c1temp$d,
                     apply(c1temp[, .(section, level2, level3, level4)], 1, paste, collapse =
                             ' '),
                     paste,
                     collapse = ' ')
    temp1 = do.call(rbind, strsplit(trimws(gsub(
      '  ', ' ', names(c1temp2)
    )), ' '))
    c1 = setDT(data.frame(temp1, c1temp2))
    setnames(c1, c('section', 'level2', 'level3', 'level4', 'text'))
    
    #### get match distances ####
    dist=stringdistmatrix(template$text,c1$text,method = 'jw')
  }
  else {
    c1=NA
    dist=NA
  }
  list(c1,dist)
}