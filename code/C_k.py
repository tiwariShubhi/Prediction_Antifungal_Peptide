#######################################################################################
# C_k.py - calculates amino acid composition of Ck terminus
#######################################################################################
#input : fasta file of peptides
#output : csv file of % composition of peptides of C5, C10 , C15
#
########################################################################################
#Creation and edits:
#   Meghal Dani - 10 April, 18
#   Shubhi Tiwari - 11 April, 18
########################################################################################
import pandas as pd
import os


file_to_read = raw_input("enter txt file to read: ")
file_out = raw_input("enter output csv file path: ")
lbl = raw_input("enter data label:")

#file_to_write = input("enter csv file to write: ")
# with open(file_to_read) as f:
#     data = f.readlines()

data = [line.rstrip('\n').rstrip('\r') for line in open(file_to_read)]
#make seperate csv files for n5,c5 and n5c5
C5=[]
comp=[]
seq = {}
aa= list('ACDEFGHIKLMNPQRSTVWY')
k=15 #k=5 for C5 , 10 for C10 and 15 for C15
#output = open(file_to_write,'w')
for each in data:
    i=0
    if(len(each)>k ):
        str = each[-(k+1):-1]
        for m1 in aa:
            count = 0
            temp = m1
            for m2 in str:
                if m2 == temp:
                    count = count + 1
            composition = (float(count) / float(k)) * 100
            if seq.has_key(m1):
                seq[m1][each] = composition
            else:
                rec = {}
                rec[each] = composition
                seq[m1] = rec


            print("%.2f" % composition) ,

        if seq.has_key('Label'):
            seq['Label'][each] = lbl
        else:
            rec = {}
            rec[each] = lbl
            seq['Label'] = rec
        print()

print(C5)

#writing to csv
df = pd.DataFrame(seq)

if not os.path.isfile(file_out):
    with open(file_out, 'a+') as f:
        df.to_csv(f)
else:
    with open(file_out, 'a+') as f:
        df.to_csv(f,header=False)