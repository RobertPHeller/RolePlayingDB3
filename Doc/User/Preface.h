// -!- c++ -!- //////////////////////////////////////////////////////////////
// ------------------------------------------------------------------
// Preface.tex - Preface
// Created by Robert Heller on Wed Dec 30 11:23:20 1998
// ------------------------------------------------------------------
// Modification History: 
// $Log: Preface.tex,v $
// Revision 1.4  2000/10/09 22:29:32  heller
// Fleshing out the text...
//
// Revision 1.3  2000/10/03 16:25:34  heller
// Update Preface for commercial version.
//
// Revision 1.2  1999/07/14 22:17:34  heller
// Eddy's Edits.
//
// Revision 1.1  1999/01/02 02:10:10  heller
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
//     Copyright (C) 1995,1998,1999  Robert Heller D/B/A Deepwoods Software
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

#ifndef __PREFACE_H
#define __PREFACE_H

/** @mainpage Preface
 * @anchor toc
 * @htmlonly
 * <div class="contents">
 * <div class="textblock"><ol type="1">
 * <li><a class="el" href="Introduction.html">Introduction</a><ol type="1">
 * <li><a class="el" href="Introduction.html#what">What Is the Role Playing Database?</a></li>
 * <li><a class="el" href="Introduction.html#org">How this manual is organized.</a></li></ol>
 * </li>
 * <li><a class="el" href="Tutorial.html">Tutorial</a><ol type="1">
 * <li><a class="el" href="Tutorial.html#CreateTemplate">Creating a Template Bundle</a></li><ol type="1">
 * <li><a class="el" href="Tutorial.html#AddTextCont">Adding heading text to a container</a></li>
 * <li><a class="el" href="Tutorial.html#AddFieldCont">Adding a field to a container</a></li>
 * <li><a class="el" href="Tutorial.html#AddContCont">Adding a container to a container</a></li></ol></li>
 * <li><a class="el" href="Tutorial.html#CreateChar">Creating a character sheet</a></li>
 * <li><a class="el" href="Tutorial.html#CreateMap">Creating a map</a></li><ol type="1">
 * <li><a class="el" href="Tutorial.html#CreateNewLev">Creating a new level</a></li>
 * <li><a class="el" href="Tutorial.html#CreateSpace">Creating a space</a><ol type="1">
 * <li><a class="el" href="Tutorial.html#AddItemsSpace">Adding items and exits to a space</a></li></ol></li></ol></li>
 * </ol></li>
 * <li><a class="el" href="Reference.html">Reference</a><ol type="1">
 * <li><a class="el" href="Reference.html#MainWindow">Main Window</a></li>
 * <li><a class="el" href="Reference.html#ConfigurationEditor">Configuration Editor</a></li>
 * <li><a class="el" href="Reference.html#SheetTemplateEditorWindow">Sheet Template Editor Window</a></li>
 * <li><a class="el" href="Reference.html#SheetEditorWindows">Sheet Editor Windows</a></li>
 * <li><a class="el" href="Reference.html#MapEditingWindows">Map Editing Windows</a><ol type="1">
 * <li><a class="el" href="Reference.html#Mainmapeditingwindow">Main map editing window</a></li>
 * <li><a class="el" href="Reference.html#Leveleditingwindow">Level editing window</a></li>
 * <li><a class="el" href="Reference.html#Spaceeditingwindow">Space editing window">Space editing window</a></li>
 * </ol></li>
 * <li><a class="el" href="Reference.html#Printing">Printing</a></li>
 * </ol></li>
 * <li><a class="el" href="help.html">Help</a>
 * </li>
 * <li><a class="el" href="Version.html">Version</a>
 * </li>
 * <li><a class="el" href="Copying.html">GNU GENERAL PUBLIC LICENSE</a>
 * </li>
 * </ol></div></div> 
 * @endhtmlonly
 * RPGs
 * @latexonly
 * \footnote{RPG: Role Playing Game, a game where the players take on
 * the roles of persons who might have lived (or may yet live) in a
 *  different time and place.  See \cite{Gygax78,Gygax79}.} 
 * @endlatexonly
 * are a popular pastime among many people these days.  Maybe they are a form 
 * of escape from the rather mundane lives many people live, at least during 
 * the workday.  A RPG allows the players to escape into a world where some
 * things are simpler, and some things more complex, in interesting ways.
 * 
 * I have played AD\&D a few times and was dismayed at the amount of paperwork
 * needed to keep track of everything.  Being a computer person, it 
 * seemed to me that most of this paperwork could be replaced by a
 * computer and the information managed by a clever database system.  Given
 * that now there are high-powered laptop computers business people use
 * to keep track of and manage large corporations, it should be possible to
 * manage the odd imaginary universe on such a machine.  So I wrote
 * the Role Playing Database System to manage all of the information that 
 * goes with an RPG.
 * 
 * The Role Playing Database System maintains a database describing an RPG 
 * "universe".  This "universe" contains a group of "characters", some 
 * player and some non-player, a collection of "monsters", and one or more
 * "places" (dungeons usually) where the "monsters" reside, generally
 * guarding some treasure.  The Role Playing Database System helps game 
 * masters and players keep track of the various things in the make-believe 
 * universe in which the RPG takes place.
 * 
 * If you have @b any comments about this package, please let me know.
 * My electronic mail addresses are listed on the back side of the title
 * page.  I would be very interested in any comments users of the
 * Role Playing Database System package might have.
 * 
 * \par
 * Robert Heller \n
 * Deepwoods Software \n
 * Wendell, MA, USA \n
 * January 1999
 *
 * \par
 *
 * @section Addendum1 Addendum to the V2.1 manual
 * 
 * After to talking to various people, I have made a number of upgrades to
 * the Role Playing Database System, mostly colorful graphics.  I have also 
 * written in more details into this user manual.
 * 
 * @section Addendum2 Addendum to the V3.0 manual
 * 
 * This is a complete rewrite of the system.  Character, monster, spell, 
 * treasure, trick/trap, and dressing "sheets" can be customized using a 
 * template editor.  This allows the system to be used with any table-top RPG
 * system.  The data files are all "bundled" up as Zip archives
 * containing an XML file with the sheet information, plus any associated
 * media (graphics files or documents).  Template files are also Zip
 * archives containing an XML files that describe the various sheets.  Each
 * of these files is self-contained and can be carried from computer to
 * computer on the media of your choice (eg CD/DVD-Rs, thumb drives, flash
 * cards, etc.).  Map files are also Zip archives containing an XML files
 * along with any associated media (graphics files or documents).
 * 
 * \par
 * Robert Heller \n
 * Deepwoods Software \n
 * Wendell, MA, USA \n
 * October 2000
 * \par
 */

#endif /* __PREFACE_H */
