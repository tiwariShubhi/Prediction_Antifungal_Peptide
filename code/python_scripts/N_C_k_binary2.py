#######################################################################################
# N_C_k_binary.py - calculates amino acid composition of Nk and Ck terminus (dipeptides)
#                   in binary format
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
# #
file_fa = raw_input("enter the name of file to be parsed: ")
file_out = raw_input("enter output csv file path: ")
lbl = raw_input("enter data label:")




chk=1
adj_mat=[0]*20
k=15 # k =5 for N5, 10 for N10 and 15 for N15 binary info


headers=[]
j=0
z=1
for i in range(k*2): # for both N and C
    headers.append("P" + str(i+1))
    #print j

print headers


seq={}


for seq_record in SeqIO.parse(file_fa, "fasta"):
    length = len(seq_record)
    if(len(seq_record.seq) > k):
        str1 = seq_record.seq[0:k]+seq_record.seq[-k:]
        print(str1)
        for i in range(len(str1)):
            for j in range(len(amino_acid)):
                if(str1[i]==amino_acid[j]):
                    adj_mat[j]=1
                    # keyString = str1[i] + str(i + 1)
                    # print keyString
                    # if seq.has_key(keyString):
                    #     seq[keyString][str(seq_record.seq)] = 1
                    # else:
                    #     rec = {}
                    #     rec[str(seq_record.seq)] = 1
                    #     seq[keyString] = rec
                else:
                    adj_mat[j]=0
                    # keyString = amino_acid[j] + str(i + 1)
                    # print keyString
                    # if seq.has_key(keyString):
                    #     seq[keyString][str(seq_record.seq)] = 0
                    # else:
                    #     rec = {}
                    #     rec[str(seq_record.seq)] = 0
                    #     seq[keyString] = rec
            print(adj_mat)
            adj_str = ''.join(str(x) for x in adj_mat)
            print adj_str
            if seq.has_key(headers[i]):
                seq[headers[i]][str(seq_record.seq)]=adj_str
            else:
                rec = {}
                rec[str(seq_record.seq)]= adj_str
                seq[headers[i]] = rec

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
#
# #/media/adalove/WorkDrive/M.Tech/Sem2/BDMH/Project/Prediction_Antifungal_Peptide/data/fasta/ds3/validation
# #/media/adalove/WorkDrive/M.Tech/Sem2/BDMH/Project/Prediction_Antifungal_Peptide/data/csv/ds3/n-c_binary/validation/main_n15c15testbin.csv