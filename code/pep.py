#calculates amino acid composition
from Bio import SeqIO
amino_acid=['G','A','V','L','I','P','F','Y','W','S','T','C','M','N','Q','K','R','H','D','E']

file_fa = input("enter the name of file to be parsed: ")
chk=1
for seq_record in SeqIO.parse(file_fa, "fasta"):
    length = len(seq_record)
    print("+1\t",end='')
    print(chk)
    chk=chk+1
    for i in range(0,20):
        count=seq_record.seq.count(amino_acid[i])
        composition = (count/length)*100
        print(str(amino_acid[i])+":"+str(composition)+"\t",end='')

    print('\n')