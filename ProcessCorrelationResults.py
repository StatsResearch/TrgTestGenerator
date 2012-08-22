# Python Script to Process Tabular R Correlation Results
# SVN: $Id: ProcessCorrelationResults.py 595 2012-04-08 21:39:22Z rob $

# Needs to run in ~/PhDStuff/ThesisSoftware/TrgTestGenerator


input_file='CorrTestResLaTeX.txt'
out_by_phys_file='phys_type_latex.tex'
out_by_stats_type_file='stats_type_latex.tex'

def is_numeric(val):
    try:
        float(val)
    except ValueError, e:
        return False
    return True

def FormatHighCorr(line,threshold=0.7):
    
    # lines look like BPd\_spot & 1.00 & 0.94 & 0.24 & 0.20 \\ 
    tokens = []
    
    tokens = line.split(' ')
    
    for count in range(len(tokens)):
        
        if is_numeric(tokens[count]):
            if abs(float(tokens[count])) > threshold:
                bold_token = '{\\bf' + tokens[count] + '}'
                tokens[count] = bold_token
                
    # Reform the line
    sb = []
    for count in range(len(tokens)):
        sb.append(tokens[count] + ' ')
        
    return ''.join(sb)

armed_stats_type=False
lineCount=0

armed_phys_type=False

f=open(input_file,'rU')

out_stats_type = open(out_by_stats_type_file,'w')
out_phys_type = open(out_by_phys_file,'w')

for line in f.readlines():
    
    if 'begin{tabular}{rrrrr}' in line:
        
        print 'armed_stats_type: ' + line
        armed_stats_type = True
        continue
    
    if armed_stats_type:
        lineCount += 1
        
        if lineCount >= 2:
            check_line = FormatHighCorr(line)
            out_stats_type.write(check_line)
            
        if lineCount == 7:
            armed_stats_type = False
            lineCount = 0
            
    if 'begin{tabular}{rrrrrr}' in line:
        
        print 'armed_phys_type: ' + line
        armed_phys_type = True
        continue
    
    if armed_phys_type:
        lineCount += 1
        
        if lineCount >= 2:
            check_line = FormatHighCorr(line)
            out_phys_type.write(check_line)
            
        if lineCount == 8:
            armed_phys_type = False
            lineCount = 0
        