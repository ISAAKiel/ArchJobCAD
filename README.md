# ArchJobCAD#


This repository comprises three LISP-functions for repeated tasks in archaeological excavations. The aims are efficient work and a homogeneous structure for easy retrieval. This is achieved with standardised named layers for filtering and the extensive use of blocks with attributes including labels of features, finds and profiles. The detailed explanations are in German but the code includes German as well as English comments.

The functions are:

- Layererstellen: Creates a definable set of Layers with given name, color and line type. A standardised prefix relates to the corresponding trench and planum. [more information](./Layererstellen-Info.md).
- ArchCAD: Inserts blocks with attributes for features, profiles, finds and spot elevations including the current date. These blocks are inserted on the corresponding layers (s. Layererstellen), are partly invisible and can be exported with coordinates to a csv-file. [more information](./ArchCAD-Info.md).
- Steigung: Calculates the slope between two selected points and inserts the result in a text box. [more information](./Steigung-Info.md).

