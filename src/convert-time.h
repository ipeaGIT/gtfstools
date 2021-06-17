#pragma once

#include <string>
#include <vector>
#include <algorithm> // std::count
#include <cmath> // floor
#include <stdexcept>
#include <time.h>

#include <cpp11.hpp>
using namespace cpp11;

// ----------  Functions to convert individual time strings:  ----------
bool time_is_hhmmss (const std::string &hms);
bool time_is_hhmm (const std::string &hms);
bool time_is_lubridate (const std::string &hms);
int convert_time_hhmmss (std::string hms);
int convert_time_hhmm (std::string hms);
int convert_time_lubridate (std::string hms);

// ----------  Vector conversion of GTFS times:  ----------
int convert_time_to_seconds (std::string hms);
writable::integers cpp_time_to_seconds(const strings times);
