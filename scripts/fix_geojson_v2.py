#!/usr/bin/env python3
"""Fix duplicate SegmCodes in mianus_transects.geojson by specific indices"""

import json

# Read the GeoJSON file
with open('mianus_transects.geojson', 'r') as f:
    data = json.load(f)

# Define fixes by index -> correct segment letter
# Based on analysis: features should be in A-F order as they appear
fixes_by_index = {
    # MRG-1-Fen-3: 6 features, should be A-F in order
    42: ('MRG-1-Fen-3', 'A'),
    49: ('MRG-1-Fen-3', 'B'),
    50: ('MRG-1-Fen-3', 'C'),
    51: ('MRG-1-Fen-3', 'D'),
    52: ('MRG-1-Fen-3', 'E'),
    56: ('MRG-1-Fen-3', 'F'),

    # MRG-2-Fen-0: Index 67 should be G (currently duplicate F)
    67: ('MRG-2-Fen-0', 'G'),
}

# Apply fixes
for idx, (prefix, segment) in fixes_by_index.items():
    feature = data['features'][idx]
    old_segm_code = feature['properties']['SegmCode']
    new_segm_code = f"{prefix}-{segment}"

    feature['properties']['SegmCode'] = new_segm_code
    feature['properties']['Segment'] = segment

    print(f"Index {idx}: {old_segm_code} -> {new_segm_code}")

# Write the fixed GeoJSON
with open('mianus_transects.geojson', 'w') as f:
    json.dump(data, f, indent=2)

print("\nGeoJSON file updated successfully!")
print("\nVerifying no duplicates remain...")

# Check for duplicates
segm_codes = [f['properties'].get('SegmCode', '') for f in data['features']]
from collections import Counter
counts = Counter(segm_codes)
duplicates = {k: v for k, v in counts.items() if v > 1 and k}

if duplicates:
    print("WARNING: Still have duplicates:")
    for segm_code, count in duplicates.items():
        print(f"  {segm_code}: {count} occurrences")
else:
    print("✓ No duplicates found!")
