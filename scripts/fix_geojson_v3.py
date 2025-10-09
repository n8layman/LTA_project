#!/usr/bin/env python3
"""Final fix for duplicate SegmCodes in mianus_transects.geojson"""

import json

# Read the GeoJSON file
with open('mianus_transects.geojson', 'r') as f:
    data = json.load(f)

# Define fixes by index -> correct segment letter
fixes_by_index = {
    # MRG-2-Fen-0: Index 67 should be H, index 68 should be I
    67: ('MRG-2-Fen-0', 'H'),
    68: ('MRG-2-Fen-0', 'I'),
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
