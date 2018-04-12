#calculates amino acid composition
from Bio import SeqIO
amino_acid=['G','A','V','L','I','P','F','Y','W','S','T','C','M','N','Q','K','R','H','D','E']

file_fa = input("enter the name of file to be parsed: ")
chk=1
n=len(amino_acid)
l=0
didpeptide=['']*400 #list to store all dipeptides
#formation of all dipeptides
for i in range(n):
    for j in range(n):
        didpeptide[l]=amino_acid[i]+amino_acid[j]
        l=l+1


adj_mat=[0]*400
k=5 # k =5 for N5, 10 for N10 and 15 for N15 binary info
for seq_record in SeqIO.parse(file_fa, "fasta"):
    length = len(seq_record)
    if(len(seq_record.seq) > k):
        str = seq_record.seq[0:k]
        print(str)
        for i in range(len(str)-1):
            str_di = str[i]+str[i+1]
            for j in range(len(didpeptide)):
                if(str_di == didpeptide[j]):
                    adj_mat[j]=1
                else:
                    adj_mat[j]=0
            print(adj_mat)