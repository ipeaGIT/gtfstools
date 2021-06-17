#include "convert-time.h"

#include <iostream>

int convert_time_to_seconds (std::string hms)
{
    const std::string delim = ":";
    unsigned int ipos = static_cast <unsigned int> (hms.find (delim.c_str ()));
    int h = atoi (hms.substr (0, ipos).c_str ());
    hms = hms.substr (ipos + 1, hms.length () - ipos - 1);
    ipos = static_cast <unsigned int> (hms.find (delim.c_str ()));
    int m = atoi (hms.substr (0, ipos).c_str ());
    int s = atoi (hms.substr (ipos + 1, hms.length ()).c_str ());

    return 3600 * h + 60 * m + s;
}

//' cpp_time_to_seconds
//'
//' Vectorize the above function
//'
//' @noRd
[[cpp11::register]]
writable::integers cpp_time_to_seconds(const strings times_in)
{
    const R_xlen_t n = times_in.size ();
    const size_t ns = static_cast <size_t> (n);
    std::vector <std::string> times (ns);
    std::copy (times_in.begin (), times_in.end (), times.begin ());

    writable::integers res (n);

    for (int i = 0; i < n; i++)
    {
        res [i] = convert_time_to_seconds (times [static_cast <size_t> (i)]);
    }
    return res;
}

