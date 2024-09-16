import re
import csv
import pdfplumber

pdf_path = 'trans-data.pdf'
input_txt = 'bulk-trans-data.txt'
output_csv = 'survey-data.csv'
headings_file = 'headings.pdf'
indices = 'trans-data-indices.pdf'

def extract_locations_from_pdf(pdf_path):
    locations = {}
    pattern = r'(\w+)\s*:\s*.*?Location:\s*(\d+)-(\d+)\s*\(width:\s*(\d+);\s*decimal:\s*(\d+)\).*?Variable Type:\s*(\w+)'

    with pdfplumber.open(pdf_path) as pdf:
        full_text = ''
        for page in pdf.pages:
            full_text += page.extract_text() + '\n'

    matches = re.finditer(pattern, full_text, re.DOTALL)
    for match in matches:
        variable = match.group(1)
        start = int(match.group(2))
        end = int(match.group(3))
        width = int(match.group(4))
        decimal = int(match.group(5))
        var_type = match.group(6)
        locations[variable] = {
            'start': start,
            'end': end,
            'width': width,
            'decimal': decimal,
            'type': var_type
        }
    return locations

def parse_line(line, locations):
    data = {}
    for variable, info in locations.items():
        start = max(0, info['start'] - 1)
        end = min(len(line), info['end'])
        value = line[start:end].strip()
        if info['type'] == 'numeric' and info['decimal'] > 0:
            try:
                value = float(value)
            except ValueError:
                value = ''
        data[variable] = value
    return data

def convert_txt_to_csv(input_txt, output_csv, locations):
    with open(input_txt, 'r') as infile, open(output_csv, 'w', newline='') as outfile:
        writer = csv.DictWriter(outfile, fieldnames=locations.keys())
        writer.writeheader()
        for line in infile:
            data = parse_line(line, locations)
            writer.writerow(data)

try:
    locations = extract_locations_from_pdf(pdf_path)
    print(f"\nFound {len(locations)} variables")
    for var, info in locations.items():
        print(f"{var}: {info}")

    convert_txt_to_csv(input_txt, output_csv, locations)
    print(f"Data has been extracted and saved to {output_csv}")
except Exception as e:
    print(f"An error occurred: {str(e)}")