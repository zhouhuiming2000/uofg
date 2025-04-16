# name : Huiming Zhou
# student ID: 1327380

from Bio import SeqIO

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
            if len(record.seq) >= 10:
                print(record.seq[0:10])
            else:
                print(record.seq)
            print(len(record.seq))
    elif filename == "finish":
        continue
    else:
        print("Wrong Input")


print("Closed")



