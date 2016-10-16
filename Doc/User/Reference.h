#ifndef _REFERENCE._H_
#define _REFERENCE._H_

/** @page Reference Reference
 * 
 * @section MainWindow Main Window
 * The main window, shown here:
 * @image latex MainWindow.png "The main window of the Role Playing Database" width=5in
 * @image html  MainWindow.png
 * Contains buttons for the seven game informational editors: Character, 
 * Monster, Spell, Treasure, Trick / Trap, Map, and Dressing.  See Section
 * SheetEditor for a documentation on the Character, Monster, Spell, Treasure, 
 * Trick / Trap, Dressing editor windows and Section Map for documentation the 
 * Map editor window. An eighth button selects for program exit. In addition 
 * to the eight buttons, there are drop down menus on a menu bar.  The same 
 * menu bar is used on all of the major top level screens.  The File menu has 
 * the standard New, Open, Save, Save As, Print, Close, and Exit menu items, 
 * all of which have the expected meanings and functionality.  The New and Open
 * menu items on the File menu use cascading menus to select the sort of thing 
 * to create or open.  The Options menu contains menu items to create/edit 
 * (see Section sect:configuration), read, and write the program's main 
 * configuration file, plus a menu item to edit template files, which opens 
 * the ``Sheet Template Editor'' window (See Section Template), which is used 
 * to create and maintain the sheet editor windows.  The Windows menu contains 
 * menu items to select one of the existing top level windows. The Help menu 
 * provides access to the on line help system (see Chapter Help for complete 
 * information about using the on-line help).
 *
 * @section ConfigurationEditor Configuration Editor
 * 
 * The Configuration Editor Window is shown below:
 * @image latex ConfigurationEditor.png "The Configuration Editor Window" width=5in
 * @image html  ConfigurationEditor.png
 * There are three configuration options: the template file to use when
 * creating new informational sheets, the initial directory to look in for 
 * images for graphic elements, and the initial directory to look in for
 * external documents.  The configuration file is located in the current
 * user's home directory in a file named @c .roleplayingdb3 under
 * UNIX/Linux and MacOSX and @c roleplayingdb3.rc under MS-Windows. 
 * This file is a plain text file containing key, value pairs.  Do not edit
 * this file by hand though.  Be sure to use the Configuration Editor. 
 * This makes sure that the file is properly formatted to be read in at
 * program start time.
 * 
 * @section SheetTemplateEditorWindow Sheet Template Editor Window
 * To allow for differences in game systems, game data elements are
 * defined with the use of templates.  These templates define what
 * information is recorded for each game element for a given game system. 
 * These templates are created and maintained with the template editor. 
 * The template editor in invoked from the Options menu.  The editor is
 * shown here:
 * @image latex TemplateEditor1.png "The initial Template Editor Window" width=5in
 * @image html  TemplateEditor1.png
 * @image latex TemplateEditor2.png "The Template Editor Window after loading a template" width=5in
 * @image html  TemplateEditor2.png
 * A sheet contains a top level container which in turn contains zero or more 
 * fields or containers.  Containers can contains zero or more fields or
 * containers.  Fields and containers have names. Fields also have a type,
 * possibly a generator (dice combination), and a flag that indicates whether 
 * the field value can be updated.  There are five defined field types:
 * @addindex "Field types"
 * -# @b Whole @b Number: This is a numerically valued field. It is either
 * an arbitrary (usually fixed) value or the result of a dice throw.
 * -# @b Word @b / @b Short Phrase: This either a single word or a short
 * (one line at most) phrase, generally describing a textual attribute,
 * such as a name or some sort of descriptive condition or status.
 * -# @b Long @b Text: This is a multi-line, but short (1-2 paragraph)
 * text value.
 * -# @b Graphic: This is a picture file.  Most standard graphics
 * formats are supported, including GIF, PNG, JPEG, BMP, TIFF, TGA,
 * PostScript, and Sun Raster.  The graphic file will be displayed in the
 * sheet editor window.
 * -# @b Document: This a document file.  Any sort of external file
 * is supported.  The external file will be copied into the sheet file. 
 * The sheet editor will not attempt to display or otherwise process the
 * file, but since the file will be "carried" along with the sheet file,
 * it will be available and extractable as needed.
 * 
 * 
 * The generator attribute is only used for numerically valued fields and
 * the updatable attribute can only be set to no for the word / short
 * phrase and numerically valued fields.
 * 
 * The templates are used for Character, Monster, Spell, Treasure, Trick /
 * Trap, and Dressing sheet editors.  The Map editor uses a set of hard-coded
 * templates. These templates define the fields, their attributes, and
 * grouping / organizational structure.  Containers have a text attribute
 * that is used as a section heading for the group of fields contained in
 * the container. The included template file, @c dnd.rpgtmpl, defines
 * informational sheets suitable for @a Advanced @a Dungeons @a and
 * @a Dragons, but template files for other game systems can be created.
 * 
 * The template editor lists the defined templates in the open template
 * bundle in its left side bar and the currently open template is
 * displayed in its main display area.  It has a six button tool bar.
 * Except for the @c Add @c Template tool bar button, these buttons work
 * with the current selected or hightlighted item.  An item (template,
 * container, or field) is selected with a single click of the mouse
 * button.
 * @latexonly
 * \footnote{Normally the left button.}
 * @endlatexonly
 * 
 * -# @b Add @b Template: A new template is added with the 
 * @c Add @c Template tool bar button.  A dialog box prompts for the name
 * and class of the new template.  The class of the template defines the
 * outermost container name (same as the class name) and the folder in the
 * template bundle where the template resides. 
 * -# @b Delete Template:} An existing template is deleted by
 * highlighting the template name in the template list and then clicking
 * the @c Delete @c Template tool bar button.  A confirmation dialog box
 * confirms the removal of the template. 
 * -# @b Add @b Field @b or @b Container: A field or container is added to
 * an existing container by highlighting the parent container and clicking
 * the @c Add @c Field @c or @c Container tool bar button.  A dialog box is
 * displayed to define the new field or container's attributes. 
 * -# @b Edit @b Container @b Text: Each container, including the
 * outermost, can have one-line or text associated with it.  This text is
 * used as a header.  Highlighting the container and clicking the
 * @c Edit @c Container @c Text tool bar button allows for editing this text
 * field. 
 * -# @b Delete @b Field @b or @b Container: A field or container is
 * deleted by highlighting the field or container and then clicking the
 * @c Delete @c Field @c or @c Container tool bar button.  A confirmation
 * dialog box confirms the removal of the field or container. Note that
 * removing a container also removes the fields and containers it
 * contains. 
 * -# @b Edit @b Field: A field's attributes can be edited with the
 * @c Edit @c Field tool bar button.  The field to be edited needs to be
 * highlighted first.  Field names cannot be changed.
 * 
 * 
 * To edit a template double click on the template name.  The template
 * will be opened up in the template editing window. The editing buttons
 * can be used to create or edit fields and containers.  It is also
 * possible to use the right mouse button
 * @latexonly
 * \footnote{Control with the left or only button under MacOS.}
 * @endlatexonly
 * to pop up edit menus to perform editing functions, including adding and 
 * deleting fields and containers from a container, editing the container's 
 * text, and editing a field's attributes.
 * 
 * The ordering of fields and containers can be altered by draging fields
 * or containers with the middle mouse button.
 * @latexonly
 * \footnote{Under MacOSX and MS-Windows, the left or only button with the Alt 
 * key is used.}
 * @endlatexonly
 * Fields and containers cannot be moved outside of the main class container.
 * 
 * A template file is a Zip archive file containing directories for each
 * class of sheet: Character, Monster, Spell, Treasure, Trick / Trap, and
 * Dressing.  These directories in turn contain the template XML files,
 * which define the structure of the sheets.  It is possible to have
 * multiple templates for any given class.  It is also possible to have no
 * templates for a given class.  Not all game systems have all classes of
 * these things and others might have several sub-classes, sometimes with
 * very different attributes.
 * 
 * @section SheetEditorWindows Sheet Editor Windows
 * 
 * @image latex CharacterEditor.png "The Character Editor window of the Role Playing Database" width=5in
 * @image html  CharacterEditor.png
 * The Sheet Editor Window, which includes the Character Editor, shown above, 
 * is used to edit characters, both playing and non-playing characters, 
 * monsters, spells, treasure, tricks / traps, and dressing items.  It uses 
 * one of the sheet templates defined in the current template file (see 
 * Section  sect:configuration).  When one of the sheet editor buttons on the 
 * main window are clicked on, a small dialog box is displayed (shown here:
 * @image latex CreateOrOpenChar.png "The Open or Create Character dialog box"
 * @image html  CreateOrOpenChar.png
 * ), asking if you want create a new sheet file, using a selected template
 * or open an existing sheet file. The Monster, Spell, Treasure,  Trick /
 * Trap, and Dressing Editor Windows are the same as the Character Editor,
 * but use different templates.
 * 
 * In addition to the fields created from the sheet template, there is also
 * a tool bar button, labeled "Extract Media".  This button allows for the
 * extraction of embedded media files contained within the sheet file. 
 * This allows for the use of external editors or viewers with these files.
 * 
 * A sheet file is a Zip archive containing two directories, xml and media.
 * The xml contains a file named sheet.xml, which is an XML file containing
 * the sheet information.  The media directory contains any media files
 * associated with the sheet--this could be pictures or other documents.
 * 
 * @section MapEditingWindows Map Editing Windows
 * 
 * Map objects are three dimensional, consisting of one or more levels,
 * above, below, or at ground level.  Each level consists of spaces
 * (squares or hexagons) arranged on a two dimensional grid.  Each level
 * is at a depth, where a depth of 0 is ground level, negative depths are
 * below ground, and positive depths are above ground.  Spaces have an X
 * and Y coordinate, which are whole numbers ranging between -1000 and
 * 1000, with 0 being the center of the level and -1000 being the extreme
 * left or western edge (X) and extreme top or northern edge (Y) and 1000
 * being the extreme right or eastern edge (X) and extreme bottom or
 * southern edge (Y). Creating or editing a map is a hierarchical process. 
 * You select the level to create or edit from the main map window
 * and you select the space to create or edit from the level editor window
 * for the level the space is on.  The whole map, with all of its levels and
 * spaces are stored in a single file, for easy transport and exchange.  It
 * is possible to have a ``sparse'' map, with levels and/or spaces
 * omitted.  These might be levels or spaces that have not been
 * constructed (yet) or are otherwise inaccessible.  With suitable
 * technology or magic (eg a teleport device or spell) it is possible to
 * get to non-adjacent spaces or levels.  No attempt it made it enforce
 * connectivity to adjacent spaces or levels!
 * 
 * There are three map editing windows:
 * 
 * -# The main map editor contains information about the overall map, 
 * including the name of the map, the name of the campaign, and the name of 
 * the game master.  This window is described in Section sect:mainmap.
 * -# The level editor, contains information about a selected level. This 
 * window is described in Section sect:leveledit.
 * -# The space editor contains information about a space. This window is
 * described in Section sect:spaceedit.
 * 
 * 
 * @subsection Mainmapeditingwindow Main map editing window
 * 
 * @image latex MapEditor.png "Main map editor windo" width=5in
 * @image html  MapEditor.png
 * The main map editor (shown above) contains information
 * about the overall map.  This information includes name of the map, the
 * name of the campaign, and the name of the game master.  There is space
 * for a brief description of the map and it is possible to include a
 * larger document providing a detailed writeup about the map or game
 * campaign.  Also on this window is a list of levels and a directory tree
 * of included media.  There is a tool bar with 4 buttons:
 * 
 * -# New Level--this button creates a new level.
 * -# Delete Level--this button deletes a selected level.
 * -# Edit Level--this button edits a selected level.
 * -# Extract Media--this button extracts a selected media file, making
 * it available for an external program to view or otherwise process.
 * 
 * 
 * @subsection Leveleditingwindow Level editing window
 * 
 * @image latex LevelEditor.png "Level editor window" width=5in
 * @image html  LevelEditor.png
 * The level editor (shown above) contains information about a selected level. 
 * This information includes the title of the level and its depth (positive 
 * depths are above ground, negative depths are below ground and a depth of 
 * zero is at ground level).  Also included is a space for a brief description 
 * of the level and a map of the level as well as a list of spaces.
 * 
 * There is a tool bar with three buttons:
 * -# New Space--This button creates a new space.
 * -# Delete Space--This button deletes an existing space.
 * -# Edit Space--This button edits an existing space.
 * 
 * 
 * @subsection Spaceeditingwindow Space editing window
 * 
 * @image latex SpaceEditor.png "Space editor window" width=5in
 * @image html  SpaceEditor.png
 * The space editor (shown above) contains information about a selected space. 
 * This information includes the title of the space, its X and Y coordinates, 
 * its color and a short description. There is also a pair of lists, one of 
 * exits from this space to another space and a list of other items in the 
 * space (such as treasure, monsters, tricks, traps, and any other odds and 
 * ends).  There is also a map of the space, showing the location of every 
 * listed exit or item in the space.
 * 
 * Below the item and exit lists are triplets of buttons: adding, deleting, 
 * and editing an item or exit.  The main difference between an item and an 
 * exit is that exits have a "pointer" to a space and level. Otherwise, both 
 * have a name, a short description, a location within the space, a graphic, 
 * and a sheet file.  The location is an X,Y value, where the  X and Y values 
 * range between -320 and 320, where 0,0 is the center of the space.  This 
 * location is just a relative location within the space and does not 
 * represent any particular distance, other than that -320 represents the top 
 * (Y) or left (X) sides and 320 represents the bottom (Y) or right (X) side. 
 * The sheet file is optional (this would make sense if the item or exit was a 
 * trick, trap, treasure, monster, etc.).
 * 
 * When a space is first created, there is available a tool bar button that
 * can be used to position the space with the mouse.  Once the space has
 * been saved, its location is fixed and it cannot be moved.  The "color" is
 * arbitrary and is used to color the space on the level map and it is also
 * used as the space's background color on the space map in the space
 * editor.  This of course makes it easier to keep track of where spaces
 * are on the level map.  A game master can use the colors to "code"
 * different spaces as having some particular property or feature, such as
 * coloring wooded areas green and mountainous areas brown and towns with
 * yellow and castles in blue for example.
 * @section Printing Printing
 * 
 * All main windows have a @c Print... menu item on the @c File
 * menu. Except for the main window, this menu item allows you to print the
 * sheet, template, map, level, or space to a PDF file.  At present there
 * is no support to print directly to your printer, but there are many
 * programs to print a PDF file to a printer.  A PDF file can also be
 * shared with someone who does not have the Role Playing Database system
 * or a PDF file can be uploaded to a website or posted to a blog.  The
 * @c Print... menu item will ask for the name of the file to be
 * created.  In the case of the Map and Level editors, you also have the
 * option of printing the levels (in the case of a Map editor window) or
 * the spaces (in the case of a Level editor window) or not.
 *    
 */

#endif /* _REFERENCE._H_ */

