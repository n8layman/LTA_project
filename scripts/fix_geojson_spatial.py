#!/usr/bin/env python3
"""Fix segment labels to match spatial order (left to right = A to F)"""

import json

# Read the GeoJSON file
with open('mianus_transects.geojson', 'r') as f:
    data = json.load(f)

# Define which lines need to be reordered
lines_to_fix = ['MRG-1-Fen-3', 'MRG-1-Fen-4']

for line_name in lines_to_fix:
    print(f"\nFixing {line_name}...")

    # Collect all features for this line
    features_info = []
    for i, feature in enumerate(data['features']):
        sc = feature['properties'].get('SegmCode', '')
        if sc.startswith(line_name):
            coords = feature['geometry']['coordinates']
            first_coord = coords[0]
            features_info.append({
                'index': i,
                'lng': first_coord[0],
                'feature': feature
            })

    # Sort by longitude (ascending = most negative to least negative = west to east = left to right)
    features_info.sort(key=lambda x: x['lng'])

    # Assign new segment labels A-F based on spatial order
    segments = ['A', 'B', 'C', 'D', 'E', 'F']
    for i, info in enumerate(features_info):
        if i >= len(segments):
            print(f"  WARNING: More segments than expected!")
            continue

        new_segment = segments[i]
        feature = info['feature']
        old_segm_code = feature['properties']['SegmCode']
        new_segm_code = f"{line_name}-{new_segment}"

        # Update the feature
        feature['properties']['SegmCode'] = new_segm_code
        feature['properties']['Segment'] = new_segment

        print(f"  Index {info['index']}: {old_segm_code} -> {new_segm_code} (lng: {info['lng']:.6f})")

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
