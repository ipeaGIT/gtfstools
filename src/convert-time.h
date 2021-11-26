#pragma once

#include <algorithm> // std::count
#include <cmath>     // floor
#include <stdexcept>
#include <string>
#include <time.h>
#include <vector>

#include <cpp11.hpp>
using namespace cpp11;

// ----------  Vector conversion of GTFS times:  ----------
int convert_time_to_seconds(std::string hms);
integers cpp_time_to_seconds(const strings times);
