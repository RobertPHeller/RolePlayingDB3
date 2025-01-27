// -!- c++ -!- //////////////////////////////////////////////////////////////
// 
// ------------------------------------------------------------------
// Role PlayingDB V2.0 by Deepwoods Software
// ------------------------------------------------------------------
// IntroUserManual.tex - User Manual Introduction
// Created by Robert Heller on Wed Dec 30 11:22:15 1998
// ------------------------------------------------------------------
// Modification History: 
// $Log: IntroUserManual.tex,v $
// Revision 1.4  2000/10/09 22:29:32  heller
// Fleshing out the text...
//
// Revision 1.3  1999/07/14 23:23:46  heller
// Small last minute update.
//
// Revision 1.2  1999/07/14 22:17:34  heller
// Eddy's Edits.
//
// Revision 1.1  1999/01/02 02:11:05  heller
// Initial revision
//
// ------------------------------------------------------------------
// Contents:
// ------------------------------------------------------------------
//  
//     Role Playing DB -- A database package that creates and maintains
// 		       a database of RPG characters, monsters, treasures,
// 		       spells, and playing environments.
// 
//     Copyright (C) 1995,1998  Robert Heller D/B/A Deepwoods Software
// 			51 Locke Hill Road
// 			Wendell, MA 01379-9728
// 
//     This program is free software; you can redistribute it and/or modify
//     it under the terms of the GNU General Public License as published by
//     the Free Software Foundation; either version 2 of the License, or
//     (at your option) any later version.
// 
//     This program is distributed in the hope that it will be useful,
//     but WITHOUT ANY WARRANTY; without even the implied warranty of
//     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//     GNU General Public License for more details.
// 
//     You should have received a copy of the GNU General Public License
//     along with this program; if not, write to the Free Software
//     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
// 
//  
//

#ifndef _INTRODUCTION._H_
#define _INTRODUCTION._H_

/** @page Introduction Introduction
 * 
 * @section what What Is the Role Playing Database?
 *
 * The Role Playing Database is a specialized database system with a GUI front 
 * end designed to aid people who play RPGs.  Both the players and the masters
 * can find uses for this package, to manage the information that describes 
 * the players' characters and the game environment and its contents.
 *
 *
 * The system consists of a collection of Tcl/Tk 
 * @latexonly
 * (\cite{Ousterhout94})
 * @endlatexonly
 * script files that implement a GUI-based program that maintains data
 * files that describe the various elements used in table-top
 * role playing games. The main elements consist of characters, both those
 * played by the players and those ``played'' by the game master.
 * Additional elements consist of monsters, spells, treasure, tricks /
 * traps, random additional objects, plus the maps and descriptive
 * information of the game playing locale.  
 * @latexonly
 * See \cite{HellerRPGTcl09} for a detailed description of these script files.
 * @endlatexonly
 * 
 * @section org How this Manual Is Organized
 * Chapter Reference is a basic reference manual, describing the nine main 
 * top level GUI windows:
 * -# The @b Main window.  This is the main window and it is described in 
 * detail in Section Main.  The main window is the main start up screen and 
 * contains the means to navigate to other parts of the program.
 * -# The @b Sheet @b Template @b Editor window.  This window is used to
 * edit the structure and contents of a "sheet" (Character, Monster,
 * Spell, Treasure, Trick / Trap, or Dressing).  This window is described
 * in detail in Section Template.
 * -# The @b Character @b Editing  window.  This window is used to create
 * and edit Character Object data files and it is described in detail in
 * Section SheetEditor. 
 * -# The @b Monster @b Editing window.  This window is used to create
 * and edit Monster Object data files and it is described in detail in
 * Section SheetEditor. 
 * -# The @b Spell @b Editing window.  This window is used to create
 * and edit Spell Object data files and it is described in detail in
 * Section SheetEditor. 
 * -# The @b Treasure @b Editing window.  This window is used to create
 * and edit Treasure Object data files and it is described in detail in
 * Section SheetEditor. 
 * -# \item The @b Trick @b / @b Trap @b Editing window.  This window is used
 * to create and edit Trick or Trap Object data files and it is described in 
 * detail in Section SheetEditor. 
 * -# The @b Dressing @b Editing window.  This window is used to create
 * and edit Dressing Object data files and it is described in detail in
 * Section SheetEditor. 
 * -# The @b Map @b Editing window.  This window is used to create
 * and edit Map Object data files and it is described in detail in
 * Section Map. 
 *
 * 
 * Chapter Tutorial is a step-by-step tutorial that takes the reader
 * through the process of creating a game system template, informational
 * sheets for characters, monsters, etc. and the creation of a map of the
 * game playing realm.
 */

#endif /* _INTRODUCTION._H_ */

