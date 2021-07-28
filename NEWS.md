# gtfstools (development version)

## New features

- `filter_route_id()`, `filter_route_type()`
- `get_parent_station()`
- New parameters to `read_gtfs()`: `fields` and `skip`. The `warnings` parameter was removed.
- Not exported: `copy_gtfs_diff_field_class()`, `convert_from_standard()`, `convert_to_standard()`

## Bug fixes

- The `get_trip_speed()` and `set_trip_speed()` examples, tests and vignette bits now only run if `{lwgeom}` is installed. `{lwgeom}` is an `{sf}` "soft" dependency required by these functions, and is listed in `Suggests`. However, package checks would fail not so gracefully if it wasn't installed, which is now fixed.
- Fixed a bug in which the `crs` passed to `get_trip_geometry()` would be assigned to the result without actually reprojecting it.

## Notes

- Some utility functions previously provided by [`{gtfs2gps}`](https://github.com/ipeaGIT/gtfs2gps) will now be exported by `{gtfstools}`. Huge thanks to the whole `{gtfs2gps}` crew (Rafael Pereira @rafapereirabr, Pedro Andrade @pedro-andrade-inpe and João Bazzo @Joaobazzo)!
- The package now imports `{gtfsio}`, and many functions now heavily rely on it, such as `read_gtfs()` and `write_gtfs()`.

## Potentially breaking changes

- `validate_gtfs()` is deprecated. All functions that used to use it will not do so from 0.2.0 onwards, and `validate_gtfs()` will be flagged as deprecated until 0.3.0, when it will get removed from the package.

# gtfstools 0.1.0

- Initial CRAN release.
