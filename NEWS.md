# gtfstools (development version)

## New features

- New function `convert_stops_to_sf()`.
- New function `convert_shapes_to_sf()`.
- New function `filter_by_route_type()`.
- New function `filter_by_route_id()`. 
- New function `filter_by_sf()`. 
- New function `filter_by_shape_id()`.
- New function `filter_by_stop_id()`.
- New function `filter_by_trip_id()`. 
- New function `get_parent_station()`.
- New function `remove_duplicates()`.
- New parameters to `read_gtfs()`: `fields`, `skip` and `encoding`. The `warnings` parameter was flagged as deprecated.
- New parameters to `write_gtfs()`: `files`, `standard_only` and `as_dir`. They substitute the previously existent `optional` and `extra`, which were flagged as deprecated. The `warnings` parameter was flagged as deprecated too.

## Bug fixes

- The `get_trip_speed()` and `set_trip_speed()` examples, tests and vignette bits now only run if `{lwgeom}` is installed. `{lwgeom}` is an `{sf}` "soft" dependency required by these functions, and is listed in `Suggests`. However, package checks would fail not so gracefully if it wasn't installed, which is now fixed.
- Fixed a bug in which the `crs` passed to `get_trip_geometry()` would be assigned to the result without actually reprojecting it.
- Changed the behaviour of `get_trip_geometry()` to not raise an error when the 'file' parameter is left untouched and the GTFS object doesn't contain either the shapes or the stop_times table. Closes [#29](https://github.com/ipeaGIT/gtfstools/issues/29).
- Fixed a bug that would cause `merge_gtfs()` to create objects that inherited only from `dt_gtfs` (ignoring `gtfs` and `list`).

## Notes

- Some utility functions previously provided by [`{gtfs2gps}`](https://github.com/ipeaGIT/gtfs2gps) will now be exported by `{gtfstools}`. Huge thanks to the whole `{gtfs2gps}` crew (Rafael Pereira @rafapereirabr, Pedro Andrade @pedro-andrade-inpe and João Bazzo @Joaobazzo)!
- The package now imports `{gtfsio}`, and many functions now heavily rely on it, such as `read_gtfs()` and `write_gtfs()`.
- Internal function `string_to_seconds()` now runs much faster thanks to Mark Padgham (@mpadge).

## Potentially breaking changes

- Functions no longer validate GTFS objects on usage. `validate_gtfs()` will be flagged as deprecated as well, since I plan to heavily change its usability and outputs in future versions.
- `write_gtfs()` parameters went through major changes - the `optional` and `extra` params were flagged as deprecated and substituted by the more general `files` and `standard_only`.

# gtfstools 0.1.0

- Initial CRAN release.
