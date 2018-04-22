## to automate the process of model training and testing on various input files

import subprocess

#read a file name of train files
masterFile = "/media/adalove/WorkDrive/M.Tech/Sem2/BDMH/Project/Prediction_Antifungal_Peptide/data/masterFile_shubhi"
rScriptName="/media/adalove/WorkDrive/M.Tech/Sem2/BDMH/Project/Prediction_Antifungal_Peptide/code/R_Scripts/model_project_res.R"
pathToResFiles = "/media/adalove/WorkDrive/M.Tech/Sem2/BDMH/Project/Prediction_Antifungal_Peptide/results/"
#till /

#store train and test files separsted by a comma
allFiles = [line.rstrip('\n') for line in open(masterFile)];

# cmd_str = ["Rscript",rScriptName]
# t = subprocess.check_output(cmd_str)
#print t
for file in allFiles:
    file_tr_ts = file.split(",")
    index = file_tr_ts[0].rfind('/')
    #print file_tr_ts[0]
    #print file_tr_ts[1]	
    outCsv = pathToResFiles+ "res_"+ file_tr_ts[0][index+1:] 
    #print outCsv
    #print rScriptName
    #cmd_str = ["Rscript ",rScriptName,file_tr_ts[0],file_tr_ts[1],outCsv]
    cmd_str = "Rscript "+rScriptName+" "+file_tr_ts[0]+" "+file_tr_ts[1]+" "+outCsv
    #print cmd_str
    subprocess.check_output(cmd_str,shell=True)
    #break



#subprocess.check_output(["Rscript","testR.R"])
