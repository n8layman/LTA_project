#!/usr/bin/env python3
"""
Convert all shapefiles in GIS_MRG folder to GeoJSON format.
Preserves all attributes including area and feature names.
"""

import os
import json
import glob
from pathlib import Path

try:
    import geopandas as gpd
except ImportError:
    print("Error: geopandas is not installed.")
    print("Please install it with: pip install geopandas")
    exit(1)


def convert_shapefile_to_geojson(shapefile_path, output_dir=".", simplify_tolerance=None, output_name=None):
    """
    Convert a shapefile to GeoJSON format.

    Args:
        shapefile_path: Path to the .shp file
        output_dir: Directory to save the GeoJSON file
        simplify_tolerance: Optional tolerance for simplifying geometries (in degrees)
        output_name: Optional custom output filename (without .geojson extension)
    """
    shapefile_path = Path(shapefile_path)
    output_dir = Path(output_dir)

    # Create output filename
    if output_name:
        output_filename = output_name if output_name.endswith('.geojson') else output_name + '.geojson'
    else:
        output_filename = shapefile_path.stem + ".geojson"
    output_path = output_dir / output_filename

    print(f"\nProcessing: {shapefile_path.name}")
    print(f"  Reading shapefile...")

    try:
        # Read the shapefile
        gdf = gpd.read_file(shapefile_path)

        # Print information about the data
        print(f"  Features found: {len(gdf)}")
        print(f"  CRS: {gdf.crs}")
        print(f"  Columns: {', '.join(gdf.columns.tolist())}")

        # Convert to WGS84 (EPSG:4326) for web mapping
        if gdf.crs and gdf.crs.to_epsg() != 4326:
            print(f"  Converting CRS to EPSG:4326...")
            gdf = gdf.to_crs(epsg=4326)

        # Simplify geometries if requested (helps reduce file size)
        if simplify_tolerance:
            print(f"  Simplifying geometries (tolerance={simplify_tolerance})...")
            gdf['geometry'] = gdf['geometry'].simplify(tolerance=simplify_tolerance, preserve_topology=True)

        # Convert to GeoJSON
        print(f"  Writing to: {output_path}")
        gdf.to_file(output_path, driver='GeoJSON')

        # Print sample of first feature's properties
        if len(gdf) > 0:
            print(f"  Sample properties from first feature:")
            first_props = gdf.iloc[0].drop('geometry').to_dict()
            for key, value in first_props.items():
                print(f"    {key}: {value}")

        print(f"  ✓ Successfully created {output_filename}")
        return True

    except Exception as e:
        print(f"  ✗ Error processing {shapefile_path.name}: {str(e)}")
        return False


def main():
    """Convert all shapefiles in the GIS_MRG directory."""

    # Set paths
    script_dir = Path(__file__).parent
    gis_dir = script_dir / "GIS_MRG"
    output_dir = script_dir

    if not gis_dir.exists():
        print(f"Error: GIS_MRG directory not found at {gis_dir}")
        exit(1)

    # Define custom naming for output files
    naming_map = {
        "MRGP_nad83_2024.shp": "mianus_polygons.geojson",
        "MRG_Tick_Transects.shp": "mianus_transects.geojson",
        "All_Exclosures2025.shp": "mianus_exclosures.geojson",
        "trail2022.shp": "mianus_trails.geojson",
        "FireRoads_all_shp.shp": "mianus_fireroads.geojson"
    }

    # Find all shapefiles
    shapefiles = sorted(gis_dir.glob("*.shp"))

    if not shapefiles:
        print("No shapefiles found in GIS_MRG directory")
        exit(1)

    print(f"Found {len(shapefiles)} shapefile(s) to convert:")
    for shp in shapefiles:
        output_name = naming_map.get(shp.name, shp.stem + ".geojson")
        print(f"  - {shp.name} → {output_name}")

    # Convert each shapefile
    print("\n" + "="*60)
    print("Starting conversion...")
    print("="*60)

    results = {}
    for shapefile in shapefiles:
        output_name = naming_map.get(shapefile.name)
        success = convert_shapefile_to_geojson(
            shapefile,
            output_dir=output_dir,
            simplify_tolerance=None,  # Set to 0.0001 for smaller files if needed
            output_name=output_name
        )
        results[shapefile.name] = success

    # Print summary
    print("\n" + "="*60)
    print("Conversion Summary:")
    print("="*60)
    successful = sum(1 for v in results.values() if v)
    print(f"Successfully converted: {successful}/{len(results)}")

    for filename, success in results.items():
        status = "✓" if success else "✗"
        print(f"  {status} {filename}")

    print("\nGeoJSON files have been created in the app directory.")
    print("You can now use them in your Shiny application.")


if __name__ == "__main__":
    main()
