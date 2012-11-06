PLY is a lightweight file format for representing 3D geometry. The
library includes support for placing mesh data into a consistent
coordinate frame using Stanford's `.conf` file format. See [The
Stanford 3D Scanning
Repository](http://graphics.stanford.edu/data/3Dscanrep/) for more
information.

This package includes a library for loading geometry data from PLY
files, and an executable, `ply2bin`, for dumping geometry data to a
flat binary file of triples of single precision floats.