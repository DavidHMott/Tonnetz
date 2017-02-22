Tonnetz program v2
------------------

This is a simple graphically-based program to display a Tonnetz together with Weitzmann regions.
It allows the user to explore chord sequences, and the Neo-Riemann transformations between triads.
The user may define a "diatonic area" and the arrows within that area will be redefined as diatonic style transforms (e.g. Dom, Sub,...).

The instructions below are based upon the use of SWI-Prolog on a Windows PC. 

Installation (Windows)
----------------------

 - Install SWI-Prolog.
 - Install a utility to play midifiles:
   - I use _playsmf.exe_ from "Div's midi utilities" (http://www.sreal.com/~div/midi-utilities/). 
   - Place this in the _bin/_ directory (if the _bin/_ directory does not exist yet, either create it manually or run _tzrun.bat_ once).
   - Make sure that the batch file _midi.bat_ is pointing to the correct midi player.

The program will be automatically compiled, if the file _tonnetz.qlf_ does not already exist under the _bin/_ directory.
To recompile the program, simply delete the QLF file if it exists, and launch _tzrun.bat_ again.

Running the program
-------------------

From the file explorer, double click on the _tzrun.bat_ file. This will run SWI-Prolog on the binary file _tonnetz.qlf_, and start up the main screen showing the Tonnetz. The following actions may be performed:

 - The reset button clears all information on the tonnetz.

 - left click on the key name in the centre of a triad. This will highlight the triad, and show the possible transforms as a line pointing to the next triad labelled by the Neo-Reimann transform. If the "play" button is on (see below) a triad chord will be sounded. By left-clicking further triads, a route of transforms will be displayed, with a red line for previous transforms in the route and black lines for transforms from the currently selected triad. A light blue triad indicates that a single voice leading unit is necessary in the transform, and a green triad indicates that two units are necessary.

 - ctrl left click will terminate the route being entered. Only the selected transforms will be shown

 - when a triad is selected the Weitzmann region (strip) that is reachable from the triad is shown as a wider purple line. This line may be selected via left click, resulting in the next triads that are reachable from the strip being highlighted in yellow. Any highlighted triad may be selected, and the route continued via the techniques above.

 - at any point it is possible to select a triad that is not pointed to by a transform and highlighted. In this case a dotted arrow will be shown, indicating an "unknown" transform being used. This line is labelled by a question mark, which may be selected by a right click, leading (in some cases) to a list of possible "non-standard" transforms. If one is selected then the dotted line is labelled with this selection.

 - right-clicking a note name at the corner of a triangle will show a menu of possible note names; in some cases these are enharmonic spellings (or the combination of diatonic and enharmonic). Selecting one of these will display the choice in the triangle corner. Currently these choices are not saved with the saved file. In addition no processing is affected by the choice of enharmonic spelling, it is intended for display only.

 - The "noplay" bauuton may be pressed to switch to a "play" button, and vice versa. When "play" is shown each triad (or Weitzmann strip) will be sounded when selected via a left click.

 - The "Playroute" button will cause the whole of the selected route to be sounded. 

 - The "Writeroute" button will generate a musical score of the route, and this is stored in the file "temp.pdf" in the delivery directory. Currently there is no attempt to ensure voice leading smoothness of the triad; this is planned for a future release.

 - The input field at the top of the screen may be selected and edited to show a title, or other relevant information.

 - The "Save" button stores the current route and title in a file, as defined by the user in a file finder window.

 - The "Load" button restores a saved route and title from a file selected by the user.

The diatonic area may be manipulated as follows:

 - Shift click on a major triad will locate the diatonic area, an elongated hexagon, with its root on the clicked triad. The colour of the area boundary will be dotted brown.

 - Shift click on a minor triad will locate the diatonic area on the major triad above the clicked triad, but its root will be the clicked triad itself. The colour of the area boundary will be dotted blue.

 - Shift click on an area of the screen that is not a triad will hide the diatonic area.

Whilst the area is displayed some transforms that originate or return to the root triad and that correspond to certain diatonic transforms will be displayed with the name of the diatonic transform rather than the Reimann transform. By moving the area (by clicking) the transforms will be recategorised according to the context they are in. Note that currently not all typical diatonic transforms are handled this way (to be updated!). 

For Mac Users
-------------

This software has not been developed for, or tested on, a Mac. The source code will have to be recompiled as described above, and a means of sounding midifiles will need to be determined. The batchfile _midi.bat_ will have to be rewritten, and it may be necessary to modify the code that calls this files (which may be found in the _tz_playmidifile(Notes)_ predicate in the _tonnetz.pl_ file).

David Mott
mottdh@googlemail.com
December 2016
