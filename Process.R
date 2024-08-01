library(EBImage)
library(tidyverse)

################################################################################
# 20231116 MPM First basic version
# 20231117 Replaced ggplot to make final images, created a list/object for output
#
#
################################################################################
version=0.3


########################### INSTRUCTIONS ################################################
# Just edit the input and output directories. Make sure images are JPGs and have 
# the JPG extension

indir <- 'input'
outdir <- 'data/'

######################## End Edit ###################

# Main Function -----------------------------------------------------------

ProcessImage <- function(imgname,name,dir){
  #Filter
  img <- readImage(paste0(dir,imgname))
  #Need to resize thelong edge of the image if too large.  
  if(dim(img)[1]>3000| dim(img)[2]>3000){
    if(dim(img)[1] > dim(img)[2]){
      img <- resize(img,w=3000)
    } else {
      img <- resize(img,h=3000)
    }
  }
  
  #Perhaps Brush size should be dependent on image size, given we resize should be fine. 
  brushsize=5
  w = makeBrush(size = brushsize, shape = 'disc', sigma = 2)
  img <- filter2(img, w)
  
  imgDm <- dim(img)
  imgRGB <- data.frame(
    x = rep(1:imgDm[1], rep = imgDm[2]),
    y = rep(imgDm[2]:1, each=imgDm[1]),
    R = as.vector(img[,,1]),
    G = as.vector(img[,,2]),
    B = as.vector(img[,,3])
  )
  
  kMeans <- kmeans(imgRGB[, c("G", "B")], centers = 4, nstart=5)
  imgRGB$color <- plyr::mapvalues(kMeans$cluster,from=order(rowMeans(kMeans$centers)),to=c('red','green','blue','black'))
  pct <- imgRGB %>% filter(color !='black') %>% group_by(color) %>% summarise(n = n()) %>% mutate(pct = n/sum(n))
  
  img <- Image(matrix(imgRGB$color,nrow = max(imgRGB$x),ncol=max(imgRGB$y)))
  
  return(list(name = name,data=imgRGB,img=img,pct=pct))
}


dir.create(outdir)

images <- list.files(indir,full.names = F)

r <- map(images,~ProcessImage(.x, name=gsub('.JPG|.jpg','',.x), dir=indir))


# Create Summary Barplot --------------------------------------------------

PctSumm <- map(r, \(d){
  d$pct$name <- d$name
  d$pct
}) %>% bind_rows() %>% select(-n) %>% mutate(pct=pct*100,
                                             damage=plyr::mapvalues(color, from=c('red','green','blue'),to=c('severe','moderate','normal'))
) %>%
  mutate(damage = factor(damage, levels=c('severe','moderate','normal')))


bg1 <- ggplot(PctSumm,aes(x=name,y=pct,fill=damage)) + geom_bar(stat="identity") + theme_bw() + 
  scale_fill_manual(values = c('red','green','blue'))
ggsave(paste0(outdir,'PctSummary.png'),bg1)


# Write Summary File ------------------------------------------------------

PctSumm %>%
  pivot_wider(values_from=pct,names_from = name) %>%
  write_csv(paste0(outdir,'PrecentSummary.csv'))




# Write out image files.  -------------------------------------------------

walk(r, \(d){
  writeImage(d$img,files=paste0(outdir,'/',d$name,'.jpg'))
})
