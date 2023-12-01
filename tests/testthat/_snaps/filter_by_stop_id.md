# full_trips = TRUE is deprecated

    The `filter_by_stop_id()` behavior of filtering by trips that contain the specified stops was deprecated in gtfstools 1.3.0.
    i For backwards compatibility reasons, this behavior is still the default as of version 1.3.0, and is controlled by the parameter `full_trips`.
    i Please set `full_trips` to "FALSE" to actually filter by `stop_ids`. This behavior will be the default from version 2.0.0 onward.
    i To achieve the old behavior, manually subset the `stop_times` table by `stop_id` and specify the `trip_ids` included in the output in `filter_by_trip_id()`.

