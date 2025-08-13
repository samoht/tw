import re

# Read the file
with open('lib/color.ml', 'r') as f:
    content = f.read()

# Pattern to match OKLCH strings
pattern = r'\((\d+), "oklch\(([\d.]+)%?\s+([\d.]+)\s+([\d.]+)\)"\)'

def replace_match(match):
    shade = match.group(1)
    l = match.group(2)
    c = match.group(3)
    h = match.group(4)
    return f'({shade}, {{l = {l}; c = {c}; h = {h}}})'

# Replace all matches
new_content = re.sub(pattern, replace_match, content)

# Write back
with open('lib/color.ml', 'w') as f:
    f.write(new_content)

print("Converted OKLCH strings to records")
