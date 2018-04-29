#######################################################################################
# N_k_dipep.py - calculates amino acid composition of Nk terminus (dipeptides)
#######################################################################################
#input : fasta file of peptides
#output : csv file of % composition of dipeptides of N5, N10 , N15
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

k=5 #change k=5 for N5, 10 for N10 and 15 for N15
n=len(amino_acid)
l=0
#formation of all dipeptides
for i in range(n):
    for j in range(n):
        didpeptide[l]=amino_acid[i]+amino_acid[j]
        l=l+1


#initialising dipeptide composition of each dipeptide = 0
for j in range(len(didpeptide)):
    pept_comp[didpeptide[j]]=0
#print(pept_comp)
file_fa=raw_input("enter file name along with path : ")
file_out = raw_input("enter output csv file path: ")
lbl = raw_input("enter data label:")

i=1

seq={}
for seq_record in SeqIO.parse(file_fa, "fasta"):
    #leng = len(seq_record)
    for l in range(400):
        occur = seq_record[0:k].seq.count(didpeptide[l])
        pept_comp[didpeptide[l]] = (float(occur)/float(k))*100
        if seq.has_key(didpeptide[l]):
            seq[didpeptide[l]][str(seq_record.seq)] = pept_comp[didpeptide[l]]
        else:
            rec = {}
            rec[str(seq_record.seq)] = pept_comp[didpeptide[l]]
            seq[didpeptide[l]] = rec

    print(seq_record[0:5],"comp of seq record is: %d ",i,pept_comp)
    i=i+1
    if seq.has_key('Label'):
        seq['Label'][str(seq_record.seq)] = lbl
    else:
        rec = {}
        rec[str(seq_record.seq)] = lbl
        seq['Label'] = rec

#writing to csv
df = pd.DataFrame(seq)

if not os.path.isfile(file_out):
    with open(file_out, 'a+') as f:
        df.to_csv(f)
else:
    with open(file_out, 'a+') as f:
        df.to_csv(f,header=False)
