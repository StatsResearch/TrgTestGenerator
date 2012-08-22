# Script for Training & Test Set Generator
# SVN: $Id: TTG.R 511 2012-02-20 22:10:06Z rob $

#options(error=utils::recover)

LogWrite('TTG.R Starting ...',3)

set.seed(42)

# Load up the libraries
library('RJSONIO')
#library('lubridate')

FillCaseStats<-function(total,num.pos,num.neg)
{
    # default wrapper for 2 digits
    FillCaseStats(total,num.pos,num.neg)
}


FillCaseStats<-function(total,num.pos,num.neg,num.digits=2)
{
    info<-list()
    info$total<-total
    info$num.pos<-num.pos
    info$pos.percent<-round((num.pos/total)*100,digits=num.digits)
    info$num.neg<-num.neg
    info$neg.percent<-round((num.neg/total)*100,digits=num.digits)

    return(info)
}

FillStudyIDStats<-function(total,num.training,num.test,num.digits=2)
{
    info<-list()
    info$total<-total
    info$num.training<-num.training
    info$training.percent<-round((num.training/total)*100,digits=num.digits)
    info$num.test<-num.test
    info$test.percent<-round((num.test/total)*100,digits=num.digits)

    return(info)
}

GenerateRandomNegativeCaseIndexVector<-function(num.cases,num.indices)
{
    # Generate as many as we can in one operation,
    rand.index<-unique(floor(runif(num.indices,min=1,max=num.cases)))
    
    # but, a small proportion may be duplicates, so generate a new index
    # and check it is not in your existing set until we have the 
    # required num.indices 
    if( length(rand.index) != num.indices)
    {
        filling<-TRUE
        guard.count<-0
        while(filling & (guard.count<500))
        {
            guard.count<-guard.count+1
            new.index<-floor(runif(1,min=1,max=num.cases))
            if(new.index %in% rand.index == FALSE) 
            {
                rand.index<-c(rand.index,new.index)
                if(length(rand.index) == num.indices)
                {
                    filling<-FALSE
                }
            }
            else
            {
                cat('GenerateRandomNegativeCaseIndexVector(), filling retry, guard.count = ', guard.count, '\n')
            }
        }
    }
    
    return(rand.index)
}

GenerateRandomIndexVector<-function(num.cases,percent.training)
{
    # Generate as many as we can in one operation,
    num.indices<-floor(num.cases*(percent.training/100))
    rand.index<-unique(floor(runif(num.indices,min=1,max=num.cases)))
    
    # but, a small proportion may be duplicates, so generate a new index
    # and check it is not in your existing set until we have the 
    # required num.indices 
    if( length(rand.index) != num.indices)
    {
        filling<-TRUE
        guard.count<-0
        while(filling & (guard.count<500))
        {
            guard.count<-guard.count+1
            new.index<-floor(runif(1,min=1,max=num.cases))
            if(new.index %in% rand.index == FALSE) 
            {
                rand.index<-c(rand.index,new.index)
                if(length(rand.index) == num.indices)
                {
                    filling<-FALSE
                }
            }
            else
            {
                cat('GenerateRandomIndexVector(), filling retry, guard.count = ', guard.count, '\n')
            }
        }
    }
    
    return(rand.index)
}

GenerateTrgTestSets<-function(input.BDS,TTG.output.dir,event.horizon,window.size,sequence.code,
                                                                percent.training.ids,percent.positive)
{
    LogWrite(paste(sep='','Processing Base Data Set (BDS): ',input.BDS),2)
    
    start<-Sys.time()
    # Create the info object which will be returned at the end
    info<-list()    
    info$process.date.start<-format(start, "%Y-%m-%d %H:%M:%OS3")
    info$process.date.stop<-'NOT-SET'
    info$process.elapsed.time.secs<-'NOT-SET'
    info$base.data.set<-input.BDS
    
    # Read in the BDS file
    all.cases<-read.table(input.BDS,header=T, sep=",", na.strings="NA", dec=".", strip.white=TRUE)
    pos.cases<-all.cases[all.cases$case_label == 1,]
    neg.cases<-all.cases[all.cases$case_label == 0,]
    
    info$total<-FillCaseStats(length(all.cases$case_label),
                                length(pos.cases$case_label),
                                length(neg.cases$case_label),
                                num.digits=5)
    
    # Extract the study IDs
    all.studyIDs<-unique(all.cases$study_id)
    num.studyIDs<-length(all.studyIDs)
        
    # Do the random selection for the required amount of training IDs
    all.indices<-seq(1:num.studyIDs)
    training.pick<-GenerateRandomIndexVector(length(all.indices),percent.training.ids)
    test.pick<-all.indices[!(all.indices %in% training.pick)] 
    
    training.studyIDs<-all.studyIDs[training.pick]
    test.studyIDs<-all.studyIDs[test.pick]
    
    info$studyIDs<-FillStudyIDStats(num.studyIDs,length(training.studyIDs),length(test.studyIDs))
    info$studyIDs$requested.percent.training.ids<-percent.training.ids
    
    # Build training file -------    
    
    # How many positive cases do we have from this random pick of patients?
    # Note that *all* these cases will go into the training set as we have
    # a limited number of positive cases
    training.set.pos<-pos.cases[pos.cases$study_id %in% training.studyIDs,]
    
    # Given as input, the percent positive cases, work out the total number
    # of cases and hence the number of negative cases required
    num.pos<-length(training.set.pos$study_id)
    total<-ceiling(num.pos/(percent.positive/100))
    num.neg<-(total-num.pos)
    
    # We now know how many negative cases to pick, pick them equally
    # from the study IDs in the training set
    num.training.ids<-length(training.pick)
    num.neg.cases.per.patient<-ceiling(num.neg/num.training.ids)
    
    info$training<-FillCaseStats(total,num.pos,num.neg)

    random.neg.cases<-vector()
    for(id in training.studyIDs)
    {
        neg.train.data<-neg.cases[neg.cases$study_id == id, ]
        available.neg.cases<-length(neg.train.data$study_id)
        neg.pick.indices<-GenerateRandomNegativeCaseIndexVector(available.neg.cases,num.neg.cases.per.patient)
        neg.cases.block<-neg.train.data[neg.pick.indices, ]
        random.neg.cases<-rbind(random.neg.cases,neg.cases.block)
    }
    
    training.set.pos<-pos.cases[pos.cases$study_id %in% training.studyIDs,]
    training.set<-rbind(training.set.pos,random.neg.cases)
    
    # Write the training file out to disk
    TTG.training.file<-paste(sep='',TTG.output.dir,'/TTG_Training_',event.horizon,'_',window.size,'_seq_',sequence.code,'.csv')
    LogWrite(paste(sep='','Training File to: ',TTG.training.file),2)
    write.csv(file=TTG.training.file,training.set)
    
    # Build test file -------
    
    # Here is the same thing again but for the test set, and I have removed the comments.
    test.set.pos<-pos.cases[pos.cases$study_id %in% test.studyIDs,]
    
    num.pos<-length(test.set.pos$study_id)
    total<-ceiling(num.pos/(percent.positive/100))
    num.neg<-(total-num.pos)
    
    num.test.ids<-length(test.pick)
    num.neg.cases.per.patient<-ceiling(num.neg/num.test.ids)
    
    info$test<-FillCaseStats(total,num.pos,num.neg)

    random.neg.cases<-vector()
    for(id in test.studyIDs)
    {
        neg.train.data<-neg.cases[neg.cases$study_id == id, ]
        available.neg.cases<-length(neg.train.data$study_id)
        neg.pick.indices<-GenerateRandomNegativeCaseIndexVector(available.neg.cases,num.neg.cases.per.patient)
        neg.cases.block<-neg.train.data[neg.pick.indices, ]
        random.neg.cases<-rbind(random.neg.cases,neg.cases.block)
    }
    
    test.set.pos<-pos.cases[pos.cases$study_id %in% test.studyIDs,]
    test.set<-rbind(test.set.pos,random.neg.cases)

    # Write the test file out to disk
    TTG.test.file<-paste(sep='',TTG.output.dir,'/TTG_Test_',event.horizon,'_',window.size,'_seq_',sequence.code,'.csv')
    LogWrite(paste(sep='','Test File to: ',TTG.test.file),2) 
    write.csv(file=TTG.test.file,test.set)

    # Write out the info file (JSON format)
    TTG.info.file<-paste(sep='',TTG.output.dir,'/TTG_Info_',event.horizon,'_',window.size,'_seq_',sequence.code,'.json')
    LogWrite(paste(sep='','Info File to: ',TTG.info.file),2) 
    stop<-Sys.time()
    info$process.date.stop<-format(stop, "%Y-%m-%d %H:%M:%OS3")
    info$process.elapsed.time.secs<-as.numeric(stop-start)
    cat(file=TTG.info.file,toJSON(info))
    
    LogWrite('TTG Processing complete',3)
    
    return(info)
}
