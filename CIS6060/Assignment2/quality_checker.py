# name : Huiming Zhou
# student ID: 1327380

from Bio import SeqIO

filename = "continue"
threshold = 20

while filename != "finish" :
    filename = input("Filename :")
    if filename == "finish":
        break
    filepath = '/Users/vanris/Documents/UG-CIS6060/ASS2'
    filepath = filepath + '/' + filename
    threshold = input("Threshold :")
    if int(threshold) < 0 or int(threshold) > 40:
        print("Invalid threshold")
    for record in SeqIO.parse(filepath, "fastq"):
        for element in record.seq:
            if element not in ['A', 'T', 'G', 'C', 'a', 't', 'g', 'c']:
                print("Invalid character found in %s" % filename)
                exit()
        for quality in record.letter_annotations['phred_quality']:
            if quality < 0 or quality > 40:
                print("Invalid quality score found in  %s" % filename)
                exit()
        avg_quality = sum(record.letter_annotations['phred_quality']) / len(record.seq)
        if avg_quality < int(threshold):
            print("%s does not meet the specified quality threshold" % filename)
        else:
            print("%s meets the specified quality threshold" % filename)
    if filename == "finish":
        break