#* 
#* ------------------------------------------------------------------
#* Role PlayingDB V3.0 by Deepwoods Software
#* ------------------------------------------------------------------
#* RolePlayingDB3.tcl - Main program for V3 of the Role Playing DB
#* Created by Robert Heller on Sun Aug 30 10:11:41 2009
#* ------------------------------------------------------------------
#* $Id$
#* ------------------------------------------------------------------
#* Contents:
#* ------------------------------------------------------------------
#*  
#*     Role Playing DB -- A database package that creates and maintains
#* 		       a database of RPG characters, monsters, treasures,
#* 		       spells, and playing environments.
#* 
#*     Copyright (C) 2009  Robert Heller D/B/A Deepwoods Software
#* 			51 Locke Hill Road
#* 			Wendell, MA 01379-9728
#* 
#*     This program is free software; you can redistribute it and/or modify
#*     it under the terms of the GNU General Public License as published by
#*     the Free Software Foundation; either version 2 of the License, or
#*     (at your option) any later version.
#* 
#*     This program is distributed in the hope that it will be useful,
#*     but WITHOUT ANY WARRANTY; without even the implied warranty of
#*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#*     GNU General Public License for more details.
#* 
#*     You should have received a copy of the GNU General Public License
#*     along with this program; if not, write to the Free Software
#*     Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
#* 
#*  
#* 

set argv0 [file join [file dirname [info nameofexecutable]] RolePlayingDB3]

package require Version
package require Tk
package require BWidget
package require BWStdMenuBar
package require snit
package require Img
package require HTMLHelp
package require ReadConfiguration

catch {console show}

namespace eval RolePlayingDB3 {
  variable ImageDir [file join [file dirname \
				     [file dirname \
					   [file dirname [info script]]]] \
			       Images]

  variable HelpDir  [file join [file dirname \
				     [file dirname \
					   [file dirname [info script]]]] \
			       Help]
  variable Main
  variable FocusNowhere

  global tcl_platform env
  variable TmpDir
  switch $tcl_platform(platform) {
    windows {
      if {[info exists env(TEMP)]} {
	set TmpDir $env(TEMP)
      } elseif {[info exists env(TMP)]} {
	set TmpDir $env(TMP)
      } else {
	set TmpDir $env(SystemDrive)
      }
    }
    unix {
      if {[info exists env(TMPDIR)]} {
	set TmpDir $env(TMPDIR)
      } else {
	set TmpDir /tmp
      }
    }
  }

  snit::type Configuration {
    ReadConfiguration::ConfigurationType \
      {{Template File} Template infile Template.rpgtmpl}
  }
}

proc RolePlayingDB3::GetTopLevelOfFocus {} {
  if {[catch {winfo toplevel [focus -displayof .]} tl]} {
    return {}
  } else {
    return $tl
  }
}

proc RolePlayingDB3::ShowMainWindow {} {
  wm deiconify .
  raise .
}

proc RolePlayingDB3::CreateMainWindow {} {
  variable ImageDir
  variable HelpDir
  variable Main
  variable FocusNowhere
  variable StatusBar {}
  variable Progress 0

  wm protocol . WM_DELETE_WINDOW {RolePlayingDB3::ExitApplication}
  wm title . "Role Playing Database (V3.0)"

  set FocusNowhere [canvas .focusNowhere]

  pack [set Main [MainFrame .main \
	-menu [StdMenuBar::MakeMenu \
		-file {"&File" menu:file file 0 {
		    {cascade "&New"     {file:new} file:new  0 {
					{command "&Character"  file:new:character "Create new character"  {Alt c} -command {RolePlayingDB3::SheetEdit new -sheetclass Character}}
					{command "&Monster"    file:new:monster   "Create new monster"    {Alt m} -command {RolePlayingDB3::SheetEdit new -sheetclass Monster}}
					{command "&Spell"      file:new:spell     "Create new spell"      {Alt s} -command {RolePlayingDB3::SheetEdit new -sheetclass Spell}}
					{command "M&ap"        file:new:map       "Create new map"        {Alt a} -command {RolePlayingDB3::MapEditor new}}
					{command "&Treasure"   file:new:treasure  "Create new treasure"   {Alt t} -command {RolePlayingDB3::SheetEdit new -sheetclass Treasure}}
					{command "T&rick/Trap" file:new:tricktrap "Create new Trick/Trap" {Alt r} -command {RolePlayingDB3::SheetEdit new -sheetclass TrickTrap}}
					{command "&Dressing"   file:new:dressing  "Create new dressing"   {Alt d} -command {RolePlayingDB3::SheetEdit new -sheetclass Dressing}}
				}}
		    {command "&Open..." {file:open} "Open an exiting item" {Ctrl o} -command {RolePlayingDB3::OpenItem [RolePlayingDB3::GetTopLevelOfFocus]}}
		    {command "&Save"    {file:save} "Save the current item" {Ctrl s} -command {RolePlayingDB3::SaveItem [RolePlayingDB3::GetTopLevelOfFocus]}}
		    {command "S&ave As..." {file:save} "Save the current item in a new file" {Ctrl a} -command {RolePlayingDB3::SaveItemAs [RolePlayingDB3::GetTopLevelOfFocus]}}
		    {command "&Print..." {file:print} "Print the current item" {Ctrl p} -command {RolePlayingDB3::PrintItem [RolePlayingDB3::GetTopLevelOfFocus]}}
		    {command "&Close"   {file:close} "Close the current window" {Ctrl w} -command {RolePlayingDB3::CloseWindow [RolePlayingDB3::GetTopLevelOfFocus]}}
		    {command "E&xit"    {file:exit} "Exit the application" {Ctrl q} -command RolePlayingDB3::ExitApplication}
		}} -windows {"&Windows" menu:windows windows 0 {
		    {command "Main window" {} {} {} -command ::RolePlayingDB3::ShowMainWindow}
		}}] \
	-textvariable RolePlayingDB3::StatusBar \
	-progressvar  RolePlayingDB3::Progress \
	-progressmax  100]] -expand yes -fill both
  set optmenu [$Main getmenu options]
  $optmenu add command -label {Edit System Configuration} \
			-command [list RolePlayingDB3::Configuration edit]
  $optmenu add command -label {Save System Configuration} \
			-command [list RolePlayingDB3::Configuration save]
  $optmenu add command -label {Re-load System Configuration} \
			-command [list RolePlayingDB3::Configuration load]
  $optmenu add separator
  $optmenu add command -label {Create or edit a template file} \
			-command [list RolePlayingDB3::Template edit]
  image create photo Background -file [file join $ImageDir background.png]
  set buttonmenu [canvas [$Main getframe].buttonmenu \
			-borderwidth {2} -relief {ridge} \
			-width [image width Background] \
			-height [image height Background]]
  $buttonmenu create image 3 3 -image Background -anchor nw
  pack $buttonmenu -expand yes -fill both

  button $buttonmenu.character \
	-image [image create photo \
			-file [file join $ImageDir CharacterButton.png]] \
	-command {RolePlayingDB3::SheetEdit edit -sheetclass Character}

  button $buttonmenu.monster \
	-image [image create photo \
			-file [file join $ImageDir MonsterButton.png]] \
	-command {RolePlayingDB3::SheetEdit edit -sheetclass Monster}

  button $buttonmenu.spell \
	-image [image create photo \
			-file [file join $ImageDir SpellButton.png]] \
	-command {RolePlayingDB3::SheetEdit edit -sheetclass Spell}

  button $buttonmenu.map \
	-image [image create photo \
			-file [file join $ImageDir MapButton.png]] \
	-command {RolePlayingDB3::MapEditor edit}

  button $buttonmenu.treasure \
	-image [image create photo \
			-file [file join $ImageDir TreasureButton.png]] \
	-command {RolePlayingDB3::SheetEdit edit -sheetclass Treasure}

  button $buttonmenu.trickTrap \
	-image [image create photo \
			-file [file join $ImageDir TrickTrapButton.png]] \
	-command {RolePlayingDB3::SheetEdit edit -sheetclass TrickTrap}

  button $buttonmenu.dressing \
	-image [image create photo \
			-file [file join $ImageDir DressingButton.png]] \
	-command {RolePlayingDB3::SheetEdit edit -sheetclass Dressing}

  button $buttonmenu.exit \
	-image [image create photo \
			-file [file join $ImageDir ExitButton.png]] \
	-command RolePlayingDB3::ExitApplication

  set wHeight [winfo reqheight $buttonmenu]
  set wWidth  [winfo reqwidth $buttonmenu]
  set bHeight [winfo reqheight $buttonmenu.character]
  set bWidth 0

  foreach b [winfo children $buttonmenu] {
    set bw [winfo reqwidth $b]
    if {$bw > $bWidth} {set bWidth $bw}
  }
  set pady [expr {int(floor((($wHeight - ($bHeight * 4.0)) / 12.0)))}]
  set padx [expr {int(floor(((($wWidth / 2.0) - $bWidth) + 10.0))/2.0)}]

#  puts stderr "*** RolePlayingDB3::CreateMainWindow pady = $pady, padx = $padx"

  if {$pady < 0} {set pady 0}
  if {$padx < 0} {set padx 0}

  set yOffset [expr {$wHeight / 5.0}]

#  grid propagate $buttonmenu 0
#  grid columnconfigure $buttonmenu  0 -minsize [expr int(floor($wWidth / 2.0))] -pad 0
#  grid columnconfigure $buttonmenu  1 -minsize [expr int(floor($wWidth / 2.0))] -pad 0

  # pack master $buttonmenu (left column: #0)
  foreach b {character monster spell map} r {1 2 3 4} {
#    grid configure $buttonmenu.$b -padx $padx -pady $pady \
#				  -column 0 -row $r -sticky w
    $buttonmenu create window $padx [expr {$yOffset * $r}] -anchor w \
				-window $buttonmenu.$b
  }
  # pack master $buttonmenu (right column: #1)
  set x [expr {[winfo reqwidth $buttonmenu] - $padx}]
  foreach b {treasure trickTrap dressing exit} r {1 2 3 4} {
#    grid configure $buttonmenu.$b -padx $padx -pady $pady \
#				  -column 1 -row $r -sticky e
    $buttonmenu create window $x [expr {$yOffset * $r}] -anchor e \
				-window $buttonmenu.$b
  }

  wm resizable . no no
  wm withdraw .
  update idle
  set x [expr {[winfo screenwidth .]/2 - [winfo reqwidth .]/2 \
		- [winfo vrootx .]}]
  set y [expr {[winfo screenheight .]/2 - [winfo reqheight .]/2 \
		- [winfo vrooty .]}]
      # Make sure that the window is on the screen and set the maximum
  # size of the window is the size of the screen.  That'll let things
  # fail fairly gracefully when very large messages are used. [Bug 827535]
  if {$x < 0} {
      set x 0
  }
  if {$y < 0} {
      set y 0
  }
  wm geom .  +$x+$y

  set helpmenu [$Main getmenu help]

  $helpmenu delete "On Keys..."
  $helpmenu delete "Index..."
  $helpmenu add command -label "Reference Manual" \
		-command "::HTMLHelp::HTMLHelp help Reference"
  $helpmenu entryconfigure "On Help..." \
		-command "::HTMLHelp::HTMLHelp help Help"
  $helpmenu entryconfigure "On Version" \
		-command "::HTMLHelp::HTMLHelp help Version"
  $helpmenu entryconfigure "Copying" \
        -command "::HTMLHelp::HTMLHelp help Copying"
  $helpmenu entryconfigure "Warranty" \
        -command "::HTMLHelp::HTMLHelp help Warranty"
  $helpmenu entryconfigure "Tutorial..." \
	-command "::HTMLHelp::HTMLHelp help Tutorial"

  ::HTMLHelp::HTMLHelp setDefaults "$HelpDir" UserManualli1.html

  wm deiconify .

}

proc RolePlayingDB3::OpenItem {tl} {
  if {"$tl" eq "."} {
    OpenWhat $tl
  } else {
    $tl open
  }
}

namespace eval RolePlayingDB3 {
  variable OpenMenu [menu .openWhat -tearoff no -title "Open"]
  foreach x {Character Monster Spell Map Treasure TrickTrap Dressing} {
    if {[lsearch {Character Dressing Monster Spell Treasure TrickTrap} $x] < 0} {
      $OpenMenu add command -label $x -command "RolePlayingDB3::${x}Editor open"
    } else {
      $OpenMenu add command -label $x -command "RolePlayingDB3::SheetEdit open -sheetclass $x"
    }
  }
}

proc RolePlayingDB3::OpenWhat {tl} {
  variable OpenMenu
  $OpenMenu post [winfo pointerx $tl] [winfo pointery $tl]
}

proc RolePlayingDB3::SaveItem {tl} {
  if {"$tl" eq "."} {
    tk_messageBox -type ok -icon info -message "Can't do that!"
    return
  } else {
    $tl save
  }
}

proc RolePlayingDB3::SaveItemAs {tl} {
  if {"$tl" eq "."} {
    tk_messageBox -type ok -icon info -message "Can't do that!"
    return
  } else {
    $tl saveas
  }
}

proc RolePlayingDB3::PrintItem {tl} {
  if {"$tl" eq "."} {
    tk_messageBox -type ok -icon info -message "Can't do that!"
    return
  } else {
    $tl print
  }
}

proc RolePlayingDB3::CloseWindow {tl} {
  if {"$tl" eq "."} {
    ExitApplication
  } else {
    $tl close
  }
}

namespace eval RolePlayingDB3 {
  snit::widgetadaptor RPGToplevel {
    option -minwidth -readonly yes -default 1 -type {snit::integer -min 1}
    option -mainframeconstructor -readonly yes -default {}
    option -mainframetemplate    -readonly yes -default {}
    delegate option              -class to hull
    option -title		 -cgetmethod getTitle -configuremethod setTitle
    option -menu		;# Steal -menu option from the toplevel --
#				MainFrame wants to mess with it, but we would
#				rather it was left alone, since it is not a 
#				copy that it can mess with.
    method getTitle {option} {
      if {"$option" eq "-title"} {
	return [wm title $win]
      }
    }
    method setTitle {option value} {
      if {"$option" eq "-title"} {
	set result [wm title $win "$value"]
	if {![catch {set Toplevels($win)} windinf]} {
	  foreach {index title} $windinf {break}
	  set wmenu [$::RolePlayingDB3::Main getmenu windows]
	  $wmenu entryconfigure $index -label "$value"
	  set Toplevels($win) [list $index "$value"]
	}
      }
    }
    typevariable Toplevels -array {}
    component main
    variable status {}
    method setstatus {string} {set status "$string"}
    variable progress
    method setprogress {value} {set progress $value}
    component mainframe
    delegate option * to mainframe except {-menu}
    delegate method * to mainframe except {destroy configure cget}

    constructor {args} {
      set options(-minwidth) [from args -minwidth]
      set options(-mainframeconstructor) [from args -mainframeconstructor]
      set options(-mainframetemplate)    [from args -mainframetemplate]
      if {"$options(-mainframeconstructor)" eq ""} {
	error "Missing required option: $options(-mainframeconstructor)"
      }
      installhull using toplevel -class [from args -class] -menu [. cget -menu]
      $type addtowindowlist $win
      wm protocol $win WM_DELETE_WINDOW [list RolePlayingDB3::CloseWindow $win]
      # Copy accelerators, along with the menu
      foreach b [bind .] {
	bind $win $b [bind . $b]
      }
      install main using MainFrame $win.main -textvariable [myvar status] \
					     -progressvar [myvar progress] \
					     -progressmax 100
      pack $main -fill both -expand yes
      set additionalOpts [list]
#      puts stderr "*** $type create: about to process [list $args]"
      foreach opt {-sheetclass -openfilename -mapbundlemountpoint -mapeditor \
		   -leveldir -spacefile -leveleditor} {
        set optval [from args $opt {}]
#	puts stderr "*** [list $type create: opt = $opt, optval = $optval]"
	if {"$optval" ne ""} {lappend additionalOpts $opt $optval}
      }
#      puts stderr "*** [list $type create: additionalOpts = $additionalOpts]"
      eval [list install mainframe using $options(-mainframeconstructor) \
				[$main getframe].mainframe \
				-template $options(-mainframetemplate)] \
		$additionalOpts
      pack $mainframe -fill both -expand yes
#      puts stderr "*** $type create: about to call [list $self configurelist $args]"
      $self configurelist $args
      wm withdraw $win
      update idle
      set width [winfo reqwidth $win]
      if {$width < $options(-minwidth)} {set width $options(-minwidth)}
      set height [winfo reqheight $win]
      set height43 [expr {int(.75*$width)}]
      if {$height < $height43} {set height $height43}
      wm withdraw $win
      update idle
      set x [expr {[winfo screenwidth $win]/2 - $width/2 \
		- [winfo vrootx $win]}]
      set y [expr {[winfo screenheight $win]/2 - $height/2 \
		- [winfo vrooty $win]}]
      # Make sure that the window is on the screen and set the maximum
      # size of the window is the size of the screen.  That'll let things
      # fail fairly gracefully when very large messages are used. [Bug 827535]
      if {$x < 0} {
          set x 0
      }
      if {$y < 0} {
          set y 0
      }
      wm geom $win  =${width}x${height}+$x+$y
      wm deiconify $win
      
    }
    destructor {
      if {![catch {set Toplevels($win)} windinf]} {
	foreach {index title} $windinf {break}
	# puts stderr "*** $self destroy: index = $index, title = $title"
	set wmenu [$::RolePlayingDB3::Main getmenu windows]
	$wmenu delete $index $index
	unset Toplevels($win)
      }
    }
    method showme {} {
      wm deiconify $win
      raise $win
    }
    typemethod addtowindowlist {thewin} {
      set wmenu [$::RolePlayingDB3::Main getmenu windows]
      set title [wm title $thewin]
      if {[catch {set Toplevels($thewin)} windinf]} {
	$wmenu add command -label "$title" -command "$thewin showme"
	set index [$wmenu index end]
	# puts stderr "*** $type addtowindowlist: index = $index"
	set Toplevels($thewin) [list $index "$title"]
      } else {
	foreach {index dummy} $windinf {break}
	# puts stderr "*** $type addtowindowlist: index = $index"
	$wmenu entryconfigure $index -label "$title" -command "$thewin showme"
	set Toplevels($thewin) [list $index "$title"]
      }
    }
    typemethod closeallwindows {} {
      foreach w [array names Toplevels] {
	catch {$w close -closingallwindows yes}
      }
    }
  }
}

# Temp hack
proc RolePlayingDB3::ExitApplication {} {
  switch [tk_messageBox -type yesno -icon question \
			-message "Are you sure you want to quit?" \
			-default no] {
    yes {
	RolePlayingDB3::RPGToplevel closeallwindows
        exit
    }
    no {
    }
  }
}

package require RPGTemplate

package require RPGSheetEdit

package require RPGMapLevelSpace
RolePlayingDB3::Configuration load

RolePlayingDB3::CreateMainWindow
	

