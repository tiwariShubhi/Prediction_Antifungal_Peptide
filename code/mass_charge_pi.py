from Bio import SeqIO

charge={'A':0,'R':1,'N':0,'D':-1,'C':0,'Q':0,'E':-1,'G':0,'H':0.5,'I':0,'L':0,'K':1,'M':0,'F':0,'P':0,'S':0,'T':0,'W':0,'Y':0,'V':0}
polarity={'A':0,'R':52,'N':3.38,'D':40.7,'C':1.46,'Q':3.53,'E':49.91,'G':0,'H':51.6,'I':0.15,'L':0.45,'K':49.5,'M':1.43,'F':0.35,'P':1.58,'S':1.67,'T':1.66,'W':2.1,'Y':1.61,'V':0.13}
hydrophobicity={'A':0.87,'R':0.85,'N':0.09,'D':0.66,'C':1.52,'Q':0,'E':0.67,'G':0.1,'H':0.87,'I':3.15,'L':2.17,'K':1.64,'M':1.67,'F':2.87,'P':2.71,'S':0.07,'T':0.07,'W':3.77,'Y':2.67,'V':1.87}
mass={'A':89,'R':174,'N':132,'D':133,'C':121,'Q':146,'E':147,'G':75,'H':155,'I':131,'L':131,'K':146,'M':149,'F':165,'P':115,'S':105,'T':119,'W':204,'Y':181,'V':117}

amino_acid=['G','A','V','L','I','P','F','Y','W','S','T','C','M','N','Q','K','R','H','D','E']


pKa = {'D': 3.9, 'E': 4.3, 'H': 6.1, 'C': 8.3, 'Y': 10.1, 'K': 10.5, 'R': 12, 'N-term': 8, 'C-term': 3.1}
charges = {'D': -1, 'E': -1, 'H': +1, 'C': -1, 'Y': -1, 'K': 1, 'R': 1, 'N-term': 1, 'C-term': -1}


def calculateAminoAcidCharge(amino_acid, pH):
    ratio = 1 / (1 + 10 ** (pH - pKa[amino_acid]))

    if charges[amino_acid] == 1:
        return ratio
    else:
        return ratio - 1


def calculateProteinCharge(sequence, pH):
    protein_charge = calculateAminoAcidCharge('N-term', pH) + calculateAminoAcidCharge('C-term', pH)

    for amino_acid in pKa.keys():
        protein_charge += sequence.seq.count(amino_acid) * calculateAminoAcidCharge(amino_acid, pH)

    return protein_charge


def calculateIsoelectricPoint(sequence):
    min_pH, max_pH = 3, 13

    while True:
        mid_pH = 0.5 * (max_pH + min_pH)
        protein_charge = calculateProteinCharge(sequence, mid_pH)

        if protein_charge > 0.5:
            min_pH = mid_pH
        elif protein_charge < -0.5:
            max_pH = mid_pH
        else:
            return mid_pH


file_fa = input("enter the name of file to be parsed: ")
for seq_record in SeqIO.parse(file_fa, "fasta"):
    C=0
    P=0
    H=0
    M=0
    for i in range(len(seq_record.seq)):
        if seq_record.seq[i] in charge.keys():
            C+=charge[seq_record.seq[i]]
        if seq_record.seq[i] in polarity.keys():
            P+=polarity[seq_record.seq[i]]
        if seq_record.seq[i] in hydrophobicity.keys():
            H+=hydrophobicity[seq_record.seq[i]]
        if seq_record.seq[i] in mass.keys():
            M+=mass[seq_record.seq[i]]
        pi=calculateIsoelectricPoint(seq_record)
    print('+1\tcharge:%f\tpolarity:%f\thydrophicity:%f\tmass:%f\t pI value:%f' %(C,P,H,M,pi))

