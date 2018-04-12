#calculates amino acid composition
from Bio import SeqIO
amino_acid=['G','A','V','L','I','P','F','Y','W','S','T','C','M','N','Q','K','R','H','D','E']

file_fa = input("enter the name of file to be parsed: ")
chk=1
adj_mat=[0]*20
k=5 # k =5 for N5, 10 for N10 and 15 for N15 binary info
for seq_record in SeqIO.parse(file_fa, "fasta"):
    length = len(seq_record)
    if(len(seq_record.seq) > k):
        str = seq_record.seq[0:k]+seq_record.seq[-k:]
        print(str)
        for i in range(len(str)):
            for j in range(len(amino_acid)):
                if(str[i]==amino_acid[j]):
                    adj_mat[j]=1
                else:
                    adj_mat[j]=0
            print(adj_mat)