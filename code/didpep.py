from Bio import SeqIO
amino_acid=['G','A','V','L','I','P','F','Y','W','S','T','C','M','N','Q','K','R','H','D','E'] #list of all amino acids
didpeptide=['']*400 #list to store all dipeptides
pept_comp={}

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
file_fa=input("enter file name along with path : ")
i=1
for seq_record in SeqIO.parse(file_fa, "fasta"):
    leng = len(seq_record)
    for k in range(400):
        occur = seq_record.seq.count(didpeptide[k])
        pept_comp[didpeptide[k]] = (occur/leng)*100
    print("comp of seq record is: %d ",i,pept_comp)
    i=i+1


