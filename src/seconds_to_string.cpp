#include "seconds_to_string.h"

std::string convert_seconds_to_string(int seconds_from_midnight) {
    const int hours = seconds_from_midnight / 3600;
    const int minutes = (seconds_from_midnight % 3600) / 60;
    const int seconds = (seconds_from_midnight % 3600) % 60;

    char hours_buffer[5];
    snprintf(hours_buffer, 5, "%02u", hours);
    std::string hours_string = hours_buffer;

    char minutes_buffer[3];
    snprintf(minutes_buffer, 3, "%02u", minutes);
    std::string minutes_string = minutes_buffer;

    char seconds_buffer[3];
    snprintf(seconds_buffer, 3, "%02u", seconds);
    std::string seconds_string = seconds_buffer;

    std::string time_string;
    time_string = hours_string +
        ":" +
        minutes_string +
        ":" +
        seconds_string;

    return time_string;
}

[[cpp11::register]]
strings cpp_seconds_to_string(const integers seconds_from_midnight) {
    const R_xlen_t n = seconds_from_midnight.size();
    const size_t vec_size = static_cast<size_t>(n);

    std::vector<int> seconds_int(vec_size);
    std::copy(
        seconds_from_midnight.begin(),
        seconds_from_midnight.end(),
        seconds_int.begin()
    );

    writable::strings result(vec_size);

    for (size_t i = 0; i < vec_size; i++) {
        if (seconds_int[i] == na<int>()) {
            result[i] = "";
        } else {
            result[i] = convert_seconds_to_string(seconds_int[i]);
        }
    }

    return result;
}
