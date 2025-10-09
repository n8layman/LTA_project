#!/usr/bin/env python3
"""Fix duplicate SegmCodes in mianus_transects.geojson"""

import json

# Read the GeoJSON file
with open('mianus_transects.geojson', 'r') as f:
    data = json.load(f)

# Track duplicates and fix them
duplicate_fixes = {
    'MRG-1-Fen-4-A': ['A', 'B', 'C', 'D', 'E', 'F'],
    'MRG-1-Fen-3-A': ['A', 'B'],
    'MRG-2-Fen-0-E': ['E', 'F']
}

# Count occurrences of each duplicate
duplicate_counters = {key: 0 for key in duplicate_fixes.keys()}

# Fix each feature
for feature in data['features']:
    segm_code = feature['properties'].get('SegmCode', '')

    if segm_code in duplicate_fixes:
        # Get the correct segment letter
        correct_segment = duplicate_fixes[segm_code][duplicate_counters[segm_code]]

        # Build the correct SegmCode
        parts = segm_code.rsplit('-', 1)  # Split off the last segment letter
        correct_segm_code = f"{parts[0]}-{correct_segment}"

        # Update the feature
        feature['properties']['SegmCode'] = correct_segm_code
        feature['properties']['Segment'] = correct_segment

        # Increment counter
        duplicate_counters[segm_code] += 1

        print(f"Fixed: {segm_code} -> {correct_segm_code}")

# Write the fixed GeoJSON
with open('mianus_transects.geojson', 'w') as f:
    json.dump(data, f, indent=2)

print("\nFixed counts:")
for key, count in duplicate_counters.items():
    expected = len(duplicate_fixes[key])
    print(f"  {key}: {count}/{expected} features fixed")
    if count != expected:
        print(f"    WARNING: Expected {expected} but found {count}!")

print("\nGeoJSON file updated successfully!")
