import sys

#File input
file_text = raw_input("enter the name to text file: ")
fileInput = open(file_text, "r")

#File output
file_fasta = raw_input("Enter the name of fasta file: ")
fileOutput = open(file_fasta, "w")

#Seq count
count = 1 ;

#Loop through each line in the input file
print("Converting to FASTA...")
for strLine in fileInput:

    #Strip the endline character from each input line
    strLine = strLine.rstrip("\n")

    #Output the header
    fileOutput.write(">" + str(count) + "\n")
    fileOutput.write(strLine + "\n")

    count = count + 1
print ("Done.")

#Close the input and output file
fileInput.close()
fileOutput.close()

#/media/adalove/WorkDrive/M.Tech/Sem2/BDMH/Project/Prediction_Antifungal_Peptide/data/txt/ds2/train/neg_train_ds2.txt
#/media/adalove/WorkDrive/M.Tech/Sem2/BDMH/Project/Prediction_Antifungal_Peptide/data/fasta/ds2/train/neg_ds2.fa