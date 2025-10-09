#!/usr/bin/env python3
"""
Verify GeoJSON against original shapefile
Check if MRG-1-Fen-2-G exists in the shapefile or if it was missing
"""

import json
import sys

try:
    import geopandas as gpd
    import pandas as pd
except ImportError:
    print("ERROR: This script requires geopandas")
    print("Install with: pip install geopandas")
    sys.exit(1)

print("="*80)
print("SHAPEFILE vs GEOJSON COMPARISON")
print("="*80)

# Load the shapefile
shapefile_path = "../resources/GIS_MRG/MRG_Tick_Transects.shp"
print(f"\nLoading shapefile: {shapefile_path}")
gdf = gpd.read_file(shapefile_path)

print(f"Shapefile loaded: {len(gdf)} features")
print(f"Columns: {list(gdf.columns)}")

# Load the GeoJSON
geojson_path = "../app/mianus_transects.geojson"
print(f"\nLoading GeoJSON: {geojson_path}")
with open(geojson_path, 'r') as f:
    geojson = json.load(f)

geojson_features = geojson['features']
print(f"GeoJSON loaded: {len(geojson_features)} features")

# Extract SegmCodes from both
shapefile_segmcodes = set(gdf['SegmCode'].str.strip() if 'SegmCode' in gdf.columns else [])
geojson_segmcodes = set(f['properties'].get('SegmCode', '').strip() for f in geojson_features)

print(f"\nShapefile has {len(shapefile_segmcodes)} unique SegmCodes")
print(f"GeoJSON has {len(geojson_segmcodes)} unique SegmCodes")

# Check for MRG-1-Fen-2-G specifically
target_segment = "MRG-1-Fen-2-G"
print(f"\n{'='*80}")
print(f"CHECKING FOR: {target_segment}")
print(f"{'='*80}")
print(f"In Shapefile: {'YES ✓' if target_segment in shapefile_segmcodes else 'NO ✗'}")
print(f"In GeoJSON:   {'YES ✓' if target_segment in geojson_segmcodes else 'NO ✗'}")

# Check all MRG-1-Fen-2 segments
print(f"\n{'='*80}")
print("MRG-1-Fen-2 SEGMENTS COMPARISON")
print(f"{'='*80}")

mrg_1_fen_2_shapefile = sorted([s for s in shapefile_segmcodes if s.startswith('MRG-1-Fen-2-')])
mrg_1_fen_2_geojson = sorted([s for s in geojson_segmcodes if s.startswith('MRG-1-Fen-2-')])

print("\nIn Shapefile:")
for seg in mrg_1_fen_2_shapefile:
    print(f"  {seg}")
print(f"Total: {len(mrg_1_fen_2_shapefile)} segments")

print("\nIn GeoJSON:")
for seg in mrg_1_fen_2_geojson:
    in_shapefile = "✓" if seg in shapefile_segmcodes else "✗ (ADDED)"
    print(f"  {seg} {in_shapefile}")
print(f"Total: {len(mrg_1_fen_2_geojson)} segments")

# Find segments in GeoJSON but not in Shapefile
print(f"\n{'='*80}")
print("SEGMENTS ADDED TO GEOJSON (not in shapefile)")
print(f"{'='*80}")
added_segments = geojson_segmcodes - shapefile_segmcodes
if added_segments:
    for seg in sorted(added_segments):
        print(f"  {seg}")
    print(f"\nTotal added: {len(added_segments)}")
else:
    print("  None")

# Find segments in Shapefile but not in GeoJSON
print(f"\n{'='*80}")
print("SEGMENTS MISSING FROM GEOJSON (in shapefile but not in GeoJSON)")
print(f"{'='*80}")
missing_segments = shapefile_segmcodes - geojson_segmcodes
if missing_segments:
    for seg in sorted(missing_segments):
        print(f"  {seg}")
    print(f"\nTotal missing: {len(missing_segments)}")
else:
    print("  None")

# Show shapefile attribute table for MRG-1-Fen-2 segments
print(f"\n{'='*80}")
print("SHAPEFILE ATTRIBUTE TABLE - MRG-1-Fen-2 segments")
print(f"{'='*80}")
if 'SegmCode' in gdf.columns:
    fen_2_df = gdf[gdf['SegmCode'].str.startswith('MRG-1-Fen-2-', na=False)].copy()
    if not fen_2_df.empty:
        # Drop geometry for cleaner display
        fen_2_df_display = fen_2_df.drop(columns=['geometry'])
        print(fen_2_df_display.to_string(index=False))
    else:
        print("  No MRG-1-Fen-2 segments found")
else:
    print("  SegmCode column not found in shapefile")

print(f"\n{'='*80}")
print("CONCLUSION")
print(f"{'='*80}")

if target_segment not in shapefile_segmcodes and target_segment in geojson_segmcodes:
    print(f"✗ {target_segment} was NOT in the original shapefile")
    print(f"✓ {target_segment} was ADDED to GeoJSON manually")
    print(f"\nRECOMMENDATION:")
    print(f"  - Verify if {target_segment} exists physically in the field")
    print(f"  - If yes: Update the shapefile to include it")
    print(f"  - If no: Remove it from tick survey data OR")
    print(f"           Reassign the tick data to an existing segment (likely F)")
elif target_segment in shapefile_segmcodes and target_segment in geojson_segmcodes:
    print(f"✓ {target_segment} exists in both shapefile and GeoJSON")
elif target_segment not in shapefile_segmcodes and target_segment not in geojson_segmcodes:
    print(f"✗ {target_segment} is in NEITHER shapefile nor GeoJSON")
else:
    print(f"? Unexpected state for {target_segment}")

print()
