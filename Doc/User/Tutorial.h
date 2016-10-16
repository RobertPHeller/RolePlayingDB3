#ifndef _TUTORIAL._H_
#define _TUTORIAL._H_

/** @page Tutorial Tutorial
 * 
 * @section CreateTemplate Creating a Template Bundle
 * 
 * To create information sheets for your game elements, you will need to
 * create templates
 * @latexonly
 * \footnote{A pre-built template bundle for\textit{Advanced Dungeond and 
 * Dragons}, dnd.rpgtmpl, is included, so if you play using the 
 * \textit{Advanced Dungeond and Dragons} system, you are all set to go.}.
 * @endlatexonly
 * To create a template bundle, select "Create or edit a template file"
 * from the Options menu:
 * @image latex OptionMenu.png "Selecting \textit{Create or edit a template file}" width=5in
 * @image html OptionMenu.png
 * This will display this dialog box:
 * @image latex CreateOrEditTemplateDialog.png "The Create Or Edit Template Dialog"
 * @image html CreateOrEditTemplateDialog.png
 * And an empty template editor window
 * @image latex EmptyTemplateEditor.png "Empty Template Editor Window" width=5in
 * @image html EmptyTemplateEditor.png
 * will be opened up.  You can now start to create templates for your game
 * system.  We will create a simple Character class template.  First, click
 * on "Add Template".  This will open up the "Add New Template" dialog
 * box, as shown here:
 * @image latex AddNewTemplate.png "Add New Template Dialog Box"
 * @image html AddNewTemplate.png
 * Type "Player" in the name field, as shown here:
 * @image latex AddNewTemplatePlayer.png "Add New Template Dialog Box, with ``Player'' filled in"
 * @image html AddNewTemplatePlayer.png
 * and click "Add".  This will create an entry under the "Character" folder
 * named "Player".  Double click on this entry now.  The template editor
 * will will now look like this:
 * @image latex PlayerTemplateEditor.png "Template Editor, with empty ``Player'' template" width=5in
 * @image html PlayerTemplateEditor.png
 * 
 * @subsection AddTextCont Adding heading text to a container
 *
 * At first, all that is in a sheet template is an empty toplevel
 * container, named for the class of sheet (Character) in this case.  The
 * first thing you will want to do is add a heading for this container.
 * Highlight the container name by clicking on it, then click the "Edit
 * Continer Text" button on the tool bar.  This will display the "Edit
 * Container Text'' dialog box, as shown here:
 * @image latex EmptyEditContainerText.png "Empty ``Edit Container Text'' dialog box"
 * @image html  EmptyEditContainerText.png
 * Fill in the text field with "This is a player character".  The dialog box 
 * will now look like this:
 * @image latex EditContainerTextWithText.png "``Edit Container Text'' dialog box with text added"
 * @image html  EditContainerTextWithText.png
 * Click "Update". The template editor window will now look like
 * @image latex PlayerTemplateEditorHeading.png "Template Editor, with ``Player'' template, with a heading added" width=5in
 * @image html  PlayerTemplateEditorHeading.png
 * 
 * @subsection AddFieldCont Adding a field to a container
 * To add a field to the toplevel container, make sure the container name
 * is highlighter (click on the name to be sure), and click on the "Add
 * Field or Container".  This will display the "Add New Field" dialog
 * box, shown here:
 * @image latex AddNewFieldDialog.png "``Add New Field'' dialog box"
 * @image html  AddNewFieldDialog.png
 * Fill in the "Name" field with "Character Name", select "Word / Short 
 * Phrase" from the "Type" menu, and set "Updatable" to "no".  The dialog box 
 * should now look like this:
 * @image latex AddNewFieldDialogWithField.png "``Add New Field'' dialog box with field values filled in"
 * @image html  AddNewFieldDialogWithField.png
 * Click the "Add" button on the dialog box.  The template editor window 
 * should now look like this:
 * @image latex PlayerTemplateEditorWithField.png "Template Editor, with ``Player'' template, with a field added" width=5in
 * @image html  PlayerTemplateEditorWithField.png
 * 
 * @subsection AddContCont Adding a container to a container
 * 
 * To add a container to the toplevel container, make sure the container name
 * is highlighter (click on the name to be sure), and click on the "Add
 * Field or Container".  This will display the "Add New Field" dialog
 * box, shown above. Fill in the "Name" field with "Attributes", select
 * " Container" from the "Type" menu, and set "Updatable" to "yes".  The 
 * dialog box should now look like this:
 * @image latex AddNewFieldDialogWithContainer.png "``Add New Field'' dialog box with container"
 * @image html  AddNewFieldDialogWithContainer.png
 * Click the "Add" button on the dialog box.  The template editor window 
 * should now look like this:
 * @image latex PlayerTemplateEditorWithContainer.png "Template Editor, with ``Player'' template, with a container added" width=5in
 * @image html  PlayerTemplateEditorWithContainer.png
 * 
 * @section CreateChar Creating a character sheet
 * 
 * To create a character sheet, we first need to be sure that there is an
 * available template bundle.  Go to the @c Options menu and select
 * @c Edit @c System @c Configuration, as shown here:
 * @image latex OpenConfigurationEditor.png "Selecting ``Edit System Configuration'' from the ``Options'' menu" width=5in
 * @image html  OpenConfigurationEditor.png
 * Click on the file folder button to the right of the "Template File" field 
 * and navigate to the location of the @c dnd.rpgtmpl file included with the 
 * Role Playing Database System. Click "Open" on the file select dialog and 
 * then "OK" on the configuration editor window.  You might want then go to 
 * the @c Options menu and select @c Save @c System @c Configuration to write 
 * out this configuration.
 * 
 * Next, click on the @c Edit @c Character button.  This will open the
 * "Open or Create Character" dialog box, shown here:
 * @image latex CreateOrOpenChar.png "The Open or Create Character dialog box"
 * @image html  CreateOrOpenChar.png
 * Click on the file folder button. This will open the "Select Template File"
 * dialog, shown here:
 * @image latex SelectTemplateFile.png "The ``Select Template File'' dialog"
 * @image html  SelectTemplateFile.png
 * Double click on "Player.xml".  This will select the player template, rather 
 * than the default non-player character (NPC) template.  Now click on the 
 * "Create" button on the "Open or Create Character" dialog box.  You should 
 * now have an empty character sheet much like that shown in
 * @image latex EmptyPlayerCharacterSheet.png "An empty player character sheet" width=5in
 * @image html  EmptyPlayerCharacterSheet.png
 * You are now ready to create a player character sheet!  The process is much 
 * like filling in a form. Each piece of information is filled into a labeled 
 * space.  Numeric values have small up and down arrows at the right end of 
 * the field and you can either type in the numbers or use these arrows to 
 * increase or decrease the value in the field.  Fields which take file names 
 * have a folder button at the right end.  These buttons can be clicked on to
 * open a file browser to select the file.
 * @latexonly 
 * \footnote{External files are copied into the sheet bundle to allow for easy 
 * transport and sharing.}
 * @endlatexonly
 * Text areas will display a scroll bar once the amount of text grows to
 * be long enough to need it. The sheet is broken up into sections.  First
 * there is the character's full name and his or her nickname(s).  The
 * next section is the character's basic attributes: Strength,
 * Intellegence, Wisdom, Dexterity, Constitution, and Charisma.  Then
 * comes the character's demographics, which includes the characters race,
 * class, gender, age, and alignment.  Then the character's wealth and
 * health: gold pieces, hit points, experience points, and level.  Then
 * comes the extra detail, which includes a picture, a short bio, and a
 * full bio.  Finally there is information about the player, including the
 * player's name, address, phone number and E-Mail address.  Some of these
 * fields will be filled out with the help of your game master and some
 * fields will be filled in from dice rolls.
 * @latexonly
 * \footnote{The Role Playing Database System  does not include a dice roll 
 * function, since it is expected that most players would prefer to use their 
 * own dice or other source of random numbers.}
 * @endlatexonly
 *
 * @section CreateMap Creating a map
 * 
 * To create a map you need to click on the "Edit Map" button.  A dialog
 * box, shown will be displayed:
 * @image latex CreateOrOpenMap.png "``Create or open a Map file'' dialog box"
 * @image html CreateOrOpenMap.png
 * Click the "Create" button.  You should now have an empty "Edit Map"
 * window, as shown here:
 * @image latex EmptyMapEditorWindow.png "Empty ``Edit Map'' window" width=5in
 * @image html EmptyMapEditorWindow.png
 * You can now fill  * in the basic map information, which includes the Name 
 * (enter "Test Map"), the Campaign (enter "None"), the Game Master (enter your
 * name), and the Space Shape (select "Hexigonal").  You can leave the
 * Short and Long Descriptions blank for now, but for a real map it is
 * probably a good idea to write a paragraph or two for the Short
 * Description, if only to remind you of what this map is for. The map
 * editor should now look something like this:
 * @image latex TestMapEditorWindow1.png "Test ``Edit Map'' window, after entering the basic map information" width=5in
 * @image html TestMapEditorWindow1.png
 * 
 * @subsection CreateNewLev Creating a new level
 * 
 * To create a new level, click the "New Level" button on the map editor
 * tool bar.  This will display the "Create New Level" dialog box, shown
 * here:
 * @image latex CreateNewLevel.png "``Create New Level'' dialog box"
 * @image html  CreateNewLevel.png
 * 
 * Enter "Ground Level" in the Selection entry and click "Open" twice.  You 
 * should now have an empty "Edit Level" window, as shown here:
 * @image latex EmptyLevelEditorWindow.png "Empty ``Edit Level'' window" width=5in
 * @image html  EmptyLevelEditorWindow.png
 * The level's basic information can be filled in.  The depth is the depth
 * below ground (when negative) or height above ground (when positive).  A
 * depth of zero is at ground level.  The title can be a short name for the
 * level and the description can be a longer description.  A filled in
 * example is shown here:
 * @image latex ExampleLevelEditorWindow.png "Example ``Edit Level'' window" width=5in
 * @image html  ExampleLevelEditorWindow.png
 * 
 * @subsection CreateSpace Creating a space
 * 
 * To create a new space, click on the "New Space" button on the level
 * editor window.  This will display a "Create New Space" dialog box,
 * shown here:
 * @image latex CreateNewSpace.png "``Create New Space'' dialog box"
 * @image html  CreateNewSpace.png
 * Fill in "Entry Meadow" as the filename, as shown here:
 * @image latex CreateEntryMeadowSpace.png "``Create New Space'' dialog box with ``Entry Meadow'' filled in"
 * @image html  CreateEntryMeadowSpace.png
 * Now click "Create".  This will open a new space editor window, as
 * shown here:
 * @image latex NewEntryMeadowSpace.png "Empty ``Space Editor'' window" width=5in
 * @image html  NewEntryMeadowSpace.png
 * You can now enter the space's basic information as show here:
 * @image latex EntryMeadowSpaceBasicInformation.png "``Space Editor'' window with ``Entry Meadow'' information filled in" width=5in
 * @image html  EntryMeadowSpaceBasicInformation.png
 * You should now save things, by selecting the "Save" menu item on the 
 * "File" menu.  We will come back to this space later.  For now, we need to 
 * create a space on a different level in order to add the hidden stairs down 
 * to the underground stronghold.  We will do this by creating a new level 
 * named "Stronghold first level", which will be at a depth of -1, then create
 * a space at location 0,0 on this level named "Entry Room". We now have
 * two additional windows, as shown here:
 * @image latex StrongholdLevel1.png "``Level Editor'' window ``Stronghold Level 1''" width=5in
 * @image html  StrongholdLevel1.png
 * @image latex EntryRoom.png "``Space Editor'' window with ``Entry Room'' information filled in" width=5in
 * @image html  EntryRoom.png
 * 
 * @subsubsection AddItemsSpace Adding items and exits to a space
 * First we will add a set of spiral stairs leading down to the
 * strongholds first level.  We do this by clicking the "Add New Exit"
 * button under the exit list.  An "Add New Exit" dialog box is
 * displayed.  After filling in the values we want, it looks like this:
 * @image latex CreatingStairsDown.png "``Add New Exit'' dialog box, adding the entrance stairs" width=5in
 * @image html  CreatingStairsDown.png
 * Clicking "Add" adds this exit.  Next we will add the hillock by clicking 
 * the "Add New Item" button under the item list.  A "Add New Item" dialog box 
 * is displayed.  After filling in the values we want, it looks like this:
 * @image latex CreatingHillock.png "``Add New Item'' dialog box, adding the hillock hiding the entrance" width=5in
 * @image html  CreatingHillock.png
 * Clicking "Add" adds this item.  After adding some bushes and some trees, 
 * the space map looks like this:
 * @latexonly
 * \footnote{Since we added the stairs and hillock at the same place, they are
 * draw one over the other.}
 * \footnote{I used some elements from a collection of 24x24 X11 bitmaps I 
 * downloaded from Anthony Thyssen's icon collection at 
 * \url{http://www.cit.gu.edu.au/~anthony/icons/}.}
 * @endlatexonly
 * @image latex UpdatedSpaceWithStairsHillockBushesTrees.png "``Space Editor'' window for the ``Entrance Meadow'', after adding the stairs, hollock, along with some bushes and trees" width=5in
 * @image html  UpdatedSpaceWithStairsHillockBushesTrees.png
 */

#endif /* _TUTORIAL._H_ */

