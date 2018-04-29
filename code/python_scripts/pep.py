#######################################################################################
# pep.py - calculates amino acid composition
#######################################################################################
#input : fasta file of peptides
#output : csv file of % composition of peptides
#
########################################################################################
#Creation and edits:
#   Meghal Dani - 10 April, 18
#   Shubhi Tiwari - 11 April, 18
########################################################################################
from Bio import SeqIO
import pandas as pd
import os

amino_acid=['G','A','V','L','I','P','F','Y','W','S','T','C','M','N','Q','K','R','H','D','E']

#input
file_fa = raw_input("enter the name of file to be parsed: ")
file_out = raw_input("enter output csv file path: ")
lbl = raw_input("enter data label:")

print lbl
#to make it accept a file path, gibe a file handle instead of name
in_fp = open(file_fa,'r')

seq = {}

chk=1
for seq_record in SeqIO.parse(in_fp, "fasta"):
    length = len(seq_record)
    #print("+1\t",end='')
    record = {}
    print("+1\t")
    print(chk)
    chk=chk+1
    for i in range(0,20):
        count=seq_record.seq.count(amino_acid[i])

        composition = (float(count)/float(length))*100
        if seq.has_key(amino_acid[i]):
            seq[amino_acid[i]][str(seq_record.seq)] = composition
        else:
            rec = {}
            rec[str(seq_record.seq)] = composition
            seq[amino_acid[i]] = rec

        #print(str(amino_acid[i])+":"+str(composition)+"\t",end='')
        # suppressing new line in python 2.7 with ,
        print(str(amino_acid[i]) + ":" + str(composition) + "\t") ,
    if seq.has_key('Label'):
        seq['Label'][str(seq_record.seq)] = lbl
    else:
        rec = {}
        rec[str(seq_record.seq)] = str(lbl)
        seq['Label'] = rec

    print('\n')

#writing to csv
df = pd.DataFrame(seq)

if not os.path.isfile(file_out):
    with open(file_out, 'a+') as f:
        df.to_csv(f)
else:
    with open(file_out, 'a+') as f:
        df.to_csv(f,header=False)
#adding label info
# csv_input = pd.read_csv(file_out)
# #type(csv_input['Label'])
# if 'Label' in csv_input.columns:
#     csv_input.loc[csv_input['Label'] ==''] = lbl
# else:
#     csv_input['Label'] = lbl
# csv_input.to_csv(file_out, index=False)
