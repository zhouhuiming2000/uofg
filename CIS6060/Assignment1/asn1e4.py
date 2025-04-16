# name : Huiming Zhou
# student ID: 1327380

from Bio import SeqIO
import re

filename = "continue"

while filename != "finish" :
    filename = input("Filename :")
    if filename in ['BacGen.fasta', 'BacGenome.fasta', 'ecoli.fasta', 
                    'multiprotein.fasta', 'Scerevisiae14.fasta'] :
        filepath = '/Users/vanris/Documents/UG-CIS6060/ASS1'
        filepath = filepath + '/' + filename
        print(filepath)
        for record in SeqIO.parse(filepath, "fasta"):
            print(record.description)
            seq = str(record.seq)
            matches_1 = re.findall(r"TTGACA[ACGT]{15,20}TATAAT",seq)
            if matches_1:
                print(f"q1(Potential promoter): {matches_1}")
            else:
                print("q1(Potential promoter): not found")
            matches_2 = re.findall(r"TTGACA[ACGT]{15,20}TATAAT[ACGT]{7,14}ATG",seq)
            if matches_2:
                print(f"q2 Sequence: {matches_2}")
            else:
                print("q2 Sequence: not found")
    elif filename == "finish":
        continue
    else:
        print("Wrong Input")


print("Closed")



