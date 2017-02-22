Tonnetz program v2
------------------

This is a simple graphically-based program to display a Tonnetz together with Weitzmann regions. It allows the user to explore chord sequences, and the Neo-Reimann transformations beween triads. The user may define a "diatonic area" and the arrows within that area will be redefined as diatonic style transforms (e.g. Dom, Sub,...)

The instructions below are based upon the use of SWI-Prolog on a Windows PC. 

Installation (Windows)
----------------------

Install SWI-Prolog.
Install Lilypond  (lilypond.org)
Install a utility to play midifiles. 
   I use playsmf.exe from "Div's midi utilities" (http://www.sreal.com/~div/midi-utilities/). 
   Place this in the "midi-utilities/bin" subdirectory of the distribution.


Copy the "delivery" directory to any location on a drive. 
Make sure that the batch file midi.bat is pointing to the correct midi player
Make sure that the batch file runlilypond.bat is pointing to the correct location for lilypond.

If the version of SWI-Prolog is not 6.6.6, then it may be necessary to recompile the binary version of the Tonnetz code. This may be done by running (double-clicking from the file explorer) the file tzcompileme.pl. This will run up the Prolog command window, and at the prompt run the command tzcompileme.   (Dont forget the full stop, followed by ENTER). The command window may now be closed. The result is a new version of the file tonnetz.qlf

Running the program
-------------------

From the file explorer, double click on the tzrun.bat file. This will run SWI-Prolog on the binary file tonnetz.qlf, and start up the main screen showing the Tonnetz. The following actions may be performed:

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

This software has not been developed for, or tested on, a Mac. The source code will have to be recompiled as described above, and a means of sounding midifiles will need to be determined. The two batchfiles "midi.bat" and "runlilypond.bat" will have to be rewritten, and it may be necessary to modify the code that calls these files (which may be found in the tz_playmidifile(Notes)  predicate in the tonnetz.pl file).

David Mott
mottdh@googlemail.com
December 2016
