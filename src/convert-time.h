#pragma once

#include <string>
#include <vector>
#include <algorithm> // std::count
#include <cmath> // floor
#include <stdexcept>
#include <time.h>

#include <cpp11.hpp>
using namespace cpp11;

// ----------  Vector conversion of GTFS times:  ----------
int convert_time_to_seconds (std::string hms);
writable::integers cpp_time_to_seconds(const strings times);
