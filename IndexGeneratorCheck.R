
num.duplicates<-0
for(count in 1:1000)
{
    rand.index<-unique(floor(runif(5, min=51,max=100)))
    if( length(rand.index) != 5)
    {
        num.duplicates <- num.duplicates+1
    }
}

cat('num.duplicates: ', num.duplicates)


BPs<-c(120,121,123,125,126)
BPm<-c(92,93,95,96,97)
BPd<-c(80,82,85,86,87)

all.data<-data.frame(BPs,BPm,BPd)

