from Bio import SeqIO
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
file_fa=input("enter file name along with path : ")
i=1
for seq_record in SeqIO.parse(file_fa, "fasta"):
    #leng = len(seq_record)
    for l in range(400):
        occur = seq_record[-(k+1):-1].seq.count(didpeptide[l])
        pept_comp[didpeptide[l]] = (occur/k)*100
    print(seq_record[-(k+1):-1],"comp of seq record is: %d ",i,pept_comp)
    i=i+1



