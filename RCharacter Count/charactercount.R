library(sringr) #for string manipulation, allows us to use functions such as str_replace
library(readr) #to read in the text file in r
library(cowplot) #nice plot theme using gg plot more information can 
#be found at https://cran.r-project.org/web/packages/cowplot/vignettes/introduction.html

resume<-read_file("resume/resume_Simran.txt")

resumefix<-str_replace_all(resume,"[^[:alnum]]", "") #reads the textfiles all removes all 
#characters except for letters and numbers

resumefix<-toupper(resumefix)

chars<-str_split(resumefix,"")

dataf<-as.data.frame(sort(table(chars)),decreasing=TRUE)
names(dataf)<-"Frequency" #give header name to frequency column
dataf$Characters<-factor(rowname(dataf),levels=rownames(dataf)) #adds the 
#character column to the data frame as factors

ggplot(dataf,aes(x=reorder(Character,-Frequency),y=Frequency)+geom_bar(stat="identity")
+scale_y_continuous(expand=c(0,0))+xlab="Characters" #expand parameter within the scale_y_continuous 
