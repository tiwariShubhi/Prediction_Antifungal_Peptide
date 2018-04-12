#######################################################################################
# mass_charge_pi.py - calculates physiochemical properties
#######################################################################################
#input : fasta file of peptides
#output : csv file of charge,polarity,mass,hydrophobicity and pI value for each peptide chain
#
########################################################################################
#Creation and edits:
#   Meghal Dani - 10 April, 18
#   Shubhi Tiwari - 11 April, 18
########################################################################################

from Bio import SeqIO
from Bio.SeqUtils.ProtParam import ProteinAnalysis
import pandas as pd
import os

charge={'A':0,'R':1,'N':0,'D':-1,'C':0,'Q':0,'E':-1,'G':0,'H':0.5,'I':0,'L':0,'K':1,'M':0,'F':0,'P':0,'S':0,'T':0,'W':0,'Y':0,'V':0}
polarity={'A':0,'R':52,'N':3.38,'D':40.7,'C':1.46,'Q':3.53,'E':49.91,'G':0,'H':51.6,'I':0.15,'L':0.45,'K':49.5,'M':1.43,'F':0.35,'P':1.58,'S':1.67,'T':1.66,'W':2.1,'Y':1.61,'V':0.13}
hydrophobicity={'A':0.87,'R':0.85,'N':0.09,'D':0.66,'C':1.52,'Q':0,'E':0.67,'G':0.1,'H':0.87,'I':3.15,'L':2.17,'K':1.64,'M':1.67,'F':2.87,'P':2.71,'S':0.07,'T':0.07,'W':3.77,'Y':2.67,'V':1.87}
mass={'A':89,'R':174,'N':132,'D':133,'C':121,'Q':146,'E':147,'G':75,'H':155,'I':131,'L':131,'K':146,'M':149,'F':165,'P':115,'S':105,'T':119,'W':204,'Y':181,'V':117}

str_charge = 'charge'
str_polarity = 'polarity'
str_hydrophicity = 'hydrophicity'
str_mass = 'mass'
str_pI = 'pI value'
str_Label = 'Label'
amino_acid=['G','A','V','L','I','P','F','Y','W','S','T','C','M','N','Q','K','R','H','D','E']


pKa = {'D': 3.9, 'E': 4.3, 'H': 6.1, 'C': 8.3, 'Y': 10.1, 'K': 10.5, 'R': 12, 'N-term': 8, 'C-term': 3.1}
charges = {'D': -1, 'E': -1, 'H': +1, 'C': -1, 'Y': -1, 'K': 1, 'R': 1, 'N-term': 1, 'C-term': -1}

seq = {}
file_fa = raw_input("enter the name of file to be parsed: ")
file_out = raw_input("enter output csv file path: ")
lbl = raw_input("enter data label:")


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
        #pi=calculateIsoelectricPoint(seq_record)
        seq_iso = ProteinAnalysis(seq_record.seq[i])
        pi=seq_iso.isoelectric_point()

    if seq.has_key(str_charge):
        seq[str_charge][str(seq_record.seq)] = C
    else:
        rec = {}
        rec[str(seq_record.seq)] = C
        seq[str_charge] = rec

    if seq.has_key(str_polarity):
        seq[str_polarity][str(seq_record.seq)] = P
    else:
        rec = {}
        rec[str(seq_record.seq)] = P
        seq[str_polarity] = rec

    if seq.has_key(str_hydrophicity):
        seq[str_hydrophicity][str(seq_record.seq)] = H
    else:
        rec = {}
        rec[str(seq_record.seq)] = H
        seq[str_hydrophicity] = rec

    if seq.has_key(str_mass):
        seq[str_mass][str(seq_record.seq)] = M
    else:
        rec = {}
        rec[str(seq_record.seq)] = M
        seq[str_mass] = rec

    if seq.has_key(str_pI):
        seq[str_pI][str(seq_record.seq)] = pi
    else:
        rec = {}
        rec[str(seq_record.seq)] = pi
        seq[str_pI] = rec

    if seq.has_key(str_Label):
        seq[str_Label][str(seq_record.seq)] = lbl
    else:
        rec = {}
        rec[str(seq_record.seq)] = lbl
        seq[str_Label] = rec

    print('+1\tcharge:%f\tpolarity:%f\thydrophicity:%f\tmass:%f\t pI value:%f' %(C,P,H,M,pi))


print "writing to csv"
#writing to csv
df = pd.DataFrame(seq)

if not os.path.isfile(file_out):
    with open(file_out, 'a+') as f:
        df.to_csv(f)
else:
    with open(file_out, 'a+') as f:
        df.to_csv(f,header=False)

print "done"
