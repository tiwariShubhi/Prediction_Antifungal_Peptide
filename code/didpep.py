#######################################################################################
# didpep.py - calculates dipeptide composition
#######################################################################################
#input : fasta file of peptides
#output : csv file of % composition of dipeptides
#
########################################################################################
#Creation and edits:
#   Meghal Dani - 10 April, 18
#   Shubhi Tiwari - 11 April, 18
########################################################################################

from Bio import SeqIO
import pandas as pd
import os


amino_acid=['G','A','V','L','I','P','F','Y','W','S','T','C','M','N','Q','K','R','H','D','E'] #list of all amino acids
didpeptide=['']*400 #list to store all dipeptides
pept_comp={}

file_fa=raw_input("enter file name along with path : ")
file_out = raw_input("enter output csv file path: ")
lbl = raw_input("enter data label:")

n=len(amino_acid)
k=0
#formation of all dipeptides
for i in range(n):
    for j in range(n):
        didpeptide[k]=amino_acid[i]+amino_acid[j]
        k=k+1


#initialising dipeptide composition of each dipeptide = 0
for j in range(len(didpeptide)):
    pept_comp[didpeptide[j]]=0
#print(pept_comp)
seq = {}
i=1
for seq_record in SeqIO.parse(file_fa, "fasta"):
    leng = len(seq_record)
    for k in range(400):
        occur = seq_record.seq.count(didpeptide[k])
        #pept_comp[didpeptide[k]] = (float(occur)/float(leng))*100
        composition = (float(occur)/float(leng))*100
        if seq.has_key(didpeptide[k]):
            seq[didpeptide[k]][str(seq_record.seq)] = composition
        else:
            rec = {}
            rec[str(seq_record.seq)] = composition
            seq[didpeptide[k]] = rec

    if seq.has_key('Label'):
        seq['Label'][str(seq_record.seq)] = lbl
    else:
        rec = {}
        rec[str(seq_record.seq)] = str(lbl)
        seq['Label'] = rec

    print("comp of seq record is: %d ",i,pept_comp)
    i=i+1

#writing to csv
df = pd.DataFrame(seq)

if not os.path.isfile(file_out):
    with open(file_out, 'a+') as f:
        df.to_csv(f)
else:
    with open(file_out, 'a+') as f:
        df.to_csv(f,header=False)
