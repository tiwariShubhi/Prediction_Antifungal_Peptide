#######################################################################################
# N_k_binary.py - calculates amino acid composition of Nk terminus (dipeptides) in binary
#                   format
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

amino_acid=['G','A','V','L','I','P','F','Y','W','S','T','C','M','N','Q','K','R','H','D','E']

sorted_amino_acid = amino_acid.sort()
file_fa = raw_input("enter the name of file to be parsed: ")
file_out = raw_input("enter output csv file path: ")
lbl = raw_input("enter data label:")
chk=1
adj_mat=[0]*20
k=5 # k =5 for N5, 10 for N10 and 15 for N15 binary info

headers=[]
j=0
z=1
for i in range(k*20):
    if j>19:
        j=0
        z=z+1
    s = amino_acid[j]
    s = s+str(z)
    headers.append(s)
    j = j+1
    #print j

print headers

seq={}
for seq_record in SeqIO.parse(file_fa, "fasta"):
    length = len(seq_record)
    if(len(seq_record.seq) > k):
        str1 = seq_record.seq[0:k]
        print(str1)
        for i in range(len(str1)):
            for j in range(len(amino_acid)):
                if(str1[i]==amino_acid[j]):
                    adj_mat[j]=1
                    keyString = str1[i]+str(i+1)
                    print keyString
                    if seq.has_key(keyString):
                        seq[keyString][str(seq_record.seq)]= 1
                    else:
                        rec = {}
                        rec[str(seq_record.seq)] = 1
                        seq[keyString] = rec
                else:
                    adj_mat[j]=0
                    keyString = amino_acid[j] + str(i + 1)
                    print keyString
                    if seq.has_key(keyString):
                        seq[keyString][str(seq_record.seq)]= 0
                    else:
                        rec = {}
                        rec[str(seq_record.seq)] = 0
                        seq[keyString] = rec
            print(adj_mat)
        if seq.has_key('Label'):
            seq['Label'][str(seq_record.seq)] = lbl
        else:
            rec = {}
            rec[str(seq_record.seq)] = lbl
            seq['Label'] = rec

print seq

#
# for k,v in seq.iteritems():
#     for h in headers:
#         if k in headers:
#             #do nothing
#             continue
#         else:
#             keyString=h
#             rec = {}
#             rec[] = 0
#             seq[keyString] = rec

# print seq


#writing to csv
df = pd.DataFrame(seq)

if not os.path.isfile(file_out):
    with open(file_out, 'a+') as f:
        df.to_csv(f)
else:
    with open(file_out, 'a+') as f:
        df.to_csv(f,header=False)