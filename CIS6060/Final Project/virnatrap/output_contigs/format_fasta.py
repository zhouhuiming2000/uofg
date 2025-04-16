def format_fasta(input_file, output_file, line_length=70):
    current_header = None
    current_sequence = []

    try:
        with open(input_file, 'r') as f_in, open(output_file, 'w') as f_out:
            for line in f_in:
                line = line.strip()
                if not line:
                    continue
                
                if line.startswith('>'):
                    # 处理前一个contig
                    if current_header is not None:
                        sequence = ''.join(current_sequence)
                        formatted = '\n'.join([sequence[i:i+line_length] for i in range(0, len(sequence), line_length)])
                        f_out.write(f"{current_header}\n{formatted}\n")
                        current_sequence = []
                    
                    current_header = line
                else:
                    current_sequence.append(line)
            
            # 处理最后一个contig
            if current_header:
                sequence = ''.join(current_sequence)
                formatted = '\n'.join([sequence[i:i+line_length] for i in range(0, len(sequence), line_length)])
                f_out.write(f"{current_header}\n{formatted}\n")
    
    except FileNotFoundError:
        print(f"Error: Input file '{input_file}' not found!")
    except Exception as e:
        print(f"Unexpected error: {str(e)}")

if __name__ == '__main__':
    import sys
    if len(sys.argv) != 3:
        print("Usage: python format_fasta.py input.txt output.fasta")
        sys.exit(1)
    
    input_file = sys.argv[1]
    output_file = sys.argv[2]
    format_fasta(input_file, output_file)