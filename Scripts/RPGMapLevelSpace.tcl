#* 
#* ------------------------------------------------------------------
#* Role PlayingDB V3.0 by Deepwoods Software
#* ------------------------------------------------------------------
#* RPGMapLevelSpace.tcl - Map, Level, Space objects
#* Created by Robert Heller on Wed Sep  2 12:45:33 2009
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

package require vfs::zip
package require vfs::mk4
package require ZipArchive
package require RPGUtilities

namespace eval RolePlayingDB3 {
  snit::widget chroot_chooseDirectory {
    hulltype toplevel
    delegate option -cursor to hull
    option {-initialdir initialDir InitialDir} -default {}
    option -root -default / -validatemethod validexistingdirectory
    method validexistingdirectory {option value} {
      if {[file exists $value] && [file isdirectory $value]} {
	return $value
      }
      error "Expected a valid existing directory name for $option, got $value"
    }
    option -title -default "Select a directory"
    delegate option {-bannerimage bannerImage BannerImage} to banner as -image
    delegate option {-bannerbackground bannerBackground BannerBackground} to banner as -background
    option -parent -default .
    option {-mustexist mustExist MustExist} -default no -type snit::boolean
    typevariable updirImage
    typevariable folderImage
    typeconstructor {
      set updirImage [image create bitmap -data {
#define updir_width 28
#define updir_height 16
static char updir_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x80, 0x1f, 0x00, 0x00, 0x40, 0x20, 0x00, 0x00,
   0x20, 0x40, 0x00, 0x00, 0xf0, 0xff, 0xff, 0x01, 0x10, 0x00, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x07, 0x00, 0x01, 0x90, 0x0f, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01,
   0x10, 0xfe, 0x07, 0x01, 0x10, 0x00, 0x00, 0x01, 0x10, 0x00, 0x00, 0x01,
   0xf0, 0xff, 0xff, 0x01};}]
      set folderImage [image create photo -data {
R0lGODlhEAAMAKEAAAD//wAAAPD/gAAAACH5BAEAAAAALAAAAAAQAAwAAAIghINhyycvVFsB
QtmS3rjaH1Hg141WaT5ouprt2HHcUgAAOw==}]
    }
    component banner
    component topframe
    component   dirlabel
    component   dirmenubtn
    component   dirmenu
    component   dirupbutton
    component iconList
    component bottomframe
    component   caption
    component   entry
    component   okButton
    component   cancelButton

    variable selectPath {}
    variable updateId
    variable selectFilePath
    constructor {args} {
      install banner using Label $win.banner -anchor w
      pack $banner -fill x
      install topframe using frame $win.topframe
      install dirlabel using Label $topframe.dirlabel -text "Directory:" \
						      -underline 0
      set dirmenubtn $topframe.dirmenu
      bind $dirlabel <<AltUnderlined>> [list focus $dirmenubtn]
      install dirmenu using tk_optionMenu $topframe.dirmenu \
						[myvar selectPath] ""
      install dirupbutton using Button $topframe.dirupbutton -image $updirImage
      $dirmenubtn configure -takefocus 1 -highlightthickness 2
      pack $dirupbutton -side right -padx 4 -fill both
      pack $dirlabel -side left -padx 4 -fill both
      pack $dirmenubtn -expand yes -fill both -padx 4
      install iconList using ::tk::IconList $win.iconList \
				-command [mymethod DblClick] -multiple no
      bind $iconList <<ListboxSelect>> [mymethod ListBrowse]
      install bottomframe using frame $win.bottomframe -bd 0
      install caption using Label $bottomframe.caption -text "Selection:" \
						       -underline 0 -anchor e \
						       -pady 0
      bind $caption <<AltUnderlined>> [list focus $bottomframe.entry]
      install entry using Entry $bottomframe.entry
      set iconlistData "::tk::$iconList"
      append iconlistData (font)
      set $iconlistData [$entry cget -font]
      install okButton using Button $bottomframe.okButton -text "OK" \
							  -underline 0 \
							  -default active \
							  -pady 3
      bind $okButton <<AltUnderlined>> [list $okButton invoke]
      install cancelButton  using Button $bottomframe.cancelButton \
							-text "Cancel" \
							-underline 0 \
							-default normal \
							-pady 3
      bind $cancelButton <<AltUnderlined>> [list $cancelButton invoke]
      grid $caption $entry $okButton -padx 4 -sticky ew
      grid configure $entry -padx 2
      grid  x x $cancelButton -padx 4 -sticky ew
      grid columnconfigure $bottomframe  1 -weight 1
      pack $topframe -side top -fill x -pady 4
      pack $bottomframe -side bottom -fill x
      pack $iconList -expand yes -fill both -padx 4 -pady 1
      wm protocol $win WM_DELETE_WINDOW [mymethod CancelCmd]
      $dirupbutton configure -command [mymethod UpDirCmd]
      $cancelButton configure -command [mymethod CancelCmd]
      bind $win <KeyPress-Escape> [mymethod CancelCmd]
      bind $win <Alt-Key> [list tk::AltKeyInDialog $win %A]
      set okCmd [mymethod OkCmd]
      bind $entry <Return> $okCmd
      $okButton configure -command $okCmd
      bind $win <Alt-s> [list focus $entry]
      bind $win <Alt-o> [list tk::ButtonInvoke $okButton]
      ::tk::FocusGroup_Create $win
      ::tk::FocusGroup_BindIn $win $entry [mymethod SelectionFocusIn]
      ::tk::FocusGroup_BindOut $win $entry [mymethod SelectionFocusOut]
      $self configurelist $args
      wm withdraw $win
    }
    method draw {args} {
      $self configurelist $args
      set initialdir $options(-initialdir)
      set initdirpathtype [file pathtype $initialdir]
      if {"$initdirpathtype" eq "absolute" || 
	  "$initdirpathtype" eq "volumerelative"} {
	set initialdir [eval [linsert [lrange \
					[file split $options(-initialdir)] \
					1 end] \
				      0 file join]]
				      
      }
      if {[winfo viewable [winfo toplevel $options(-parent)]]} {
	wm transient $win $options(-parent)
      }
      if {$initialdir ne "" && [file exists [file join $options(-root) $initialdir]] &&
	  [file isdirectory [file join $options(-root) $initialdir]]} {
	set selectPath $initialdir
      }
      trace variable [myvar selectPath] w [mymethod SetPath]
      $dirmenubtn configure -textvariable [myvar selectPath]
      set previousEntryText ""
      $self UpdateWhenIdle
      ::tk::PlaceWindow $win widget $options(-parent)
      wm title $win $options(-title)
      ::tk::SetFocusGrab $win $entry
      $entry delete 0 end
      $entry insert 0 $selectPath
      $entry selection range 0 end
      $entry icursor end
      vwait [myvar selectFilePath]
      ::tk::RestoreFocusGrab $win $entry withdraw
      foreach trace [trace vinfo [myvar selectPath]] {
	trace vdelete [myvar selectPath] [lindex $trace 0] [lindex $trace 1]
      }
      $dirmenubtn configure -textvariable {}
      if {"$selectFilePath" eq ""} {
	return $selectFilePath
      } else {
	return [file join $options(-root) $selectFilePath]
      }
    }
    method UpdateWhenIdle {} {
      if {![info exists updateId]} {
	set updateId [after idle [mymethod Update]]
      }
    }
    method DblClick {} {
      set selection  [tk::IconList_Curselection $iconList]
      if { [llength $selection] != 0 } {
	set filenameFragment \
	    [tk::IconList_Get $iconList [lindex $selection 0]]
	set file [file join $options(-root) $selectPath]
#	puts stderr "*** $self DblClick: file = $file"
	if {[file isdirectory $file]} {
	  $self ListInvoke [list $filenameFragment]
	  return
	}
      }
    }
    method ListInvoke {filenames} {
      if {[llength $filenames] == 0} {
	return
      }
      set file [::tk::dialog::file::JoinFile $selectPath \
		[lindex $filenames 0]]
#      puts stderr "*** $self ListInvoke: file = $file"
#      puts stderr "*** $self ListInvoke: file join $options(-root) $file = [file join $options(-root) $file]"
      if {[file isdirectory [file join $options(-root) $file]]} {
	set selectPath $file
      }
    }
    method ListBrowse {} {
      set text {}
      foreach item [::tk::IconList_Curselection $iconList] {
	lappend text [::tk::IconList_Get $iconList $item]
      }
      if {[llength $text] == 0} {
	return
      }
      set text [lindex $text 0]
      set file [::tk::dialog::file::JoinFile $selectPath $text]
      $entry delete 0 end
      $entry insert 0 $file
    }
    method CancelCmd {} {
      set selectFilePath ""
    }
    method UpDirCmd {} {
      if {"$selectPath" ne ""} {
#	puts stderr "[list *** $self UpDirCmd: selectPath = $selectPath]"
#	puts stderr "[list *** $self UpDirCmd: file dirname $selectPath = [file dirname $selectPath]]"
        set selectPath [file dirname $selectPath]
      }
    }
    method OkCmd {} {
      set selection [tk::IconList_Curselection $iconList]
      if { [llength $selection] != 0 } {
	set iconText [tk::IconList_Get $iconList [lindex $selection 0]]
	set iconText [file join $selectPath $iconText]
	$self Done "$iconText"
      } else {
	set text [$entry get]
	if {"$text" eq ""} {return}
	set text [eval file join [file split [string trim $text]]]
	if { "$text" eq "$selectPath" } {
	  $self Done "$text"
	} else {
	  set selectPath "$text"
        }
      }
      return
    }
    method SelectionFocusIn {} {
      if {[string compare [$entry get] ""]} {
	$entry selection range 0 end
	$entry icursor end
      } else {
	$entry selection clear
      }
    }
    method SelectionFocusOut {} {
      $entry selection clear
    }
    method SetPath {name1 name2 op} {
#      puts stderr "*** $self SetPath $name1 $name2 $op"
#      puts stderr "*** $self SetPath: winfo exists $win = [winfo exists $win]"
      if {[winfo exists $win]} {
	$self UpdateWhenIdle
	$entry delete 0 end
	$entry insert end $selectPath
      }      
    }
    method Update {} {
      if {![winfo exists $win]} {return}
      catch {unset updateId}
      set entCursor [$entry cget -cursor]
      set dlgCursor [$win       cget -cursor]
      $entry configure -cursor watch
      $win       configure -cursor watch
      update idletasks

#      puts stderr "*** $self Update: selectPath = $selectPath"

      ::tk::IconList_DeleteAll $iconList
      set dirs [lsort -dictionary -unique \
			[glob -tails \
			      -directory [file join $options(-root) \
						    $selectPath] -type d \
				-nocomplain *]]

#      puts stderr "[list *** $self Update: dirs = $dirs]"
      set dirList {}
      foreach d $dirs {
	lappend dirList $d
      }
      ::tk::IconList_Add $iconList $folderImage $dirList
      ::tk::IconList_Arrange $iconList
      set list "."
      set dir ""
#      puts stderr "*** $self Update: file split selectPath = [file split $selectPath]"
      foreach subdir [file split $selectPath] {
	set dir [file join $dir $subdir]
	lappend list $dir
      }
#      puts stderr "[list *** $self Update: list = $list]"
      $dirmenu delete 0 end
      set var [myvar selectPath]
      foreach path $list {
	$dirmenu add command -label $path -command [list set $var $path]
      }
      $entry configure -cursor $entCursor
      $win       configure -cursor $dlgCursor
    }
    method Done {{_selectFilePath ""}} {
      if {[string equal $_selectFilePath ""]} {
	set _selectFilePath $selectPath
      }
      if { $options(-mustexist) } {
	if { ![file exists [file join $options(-root) $_selectFilePath]] || \
		![file isdir [file join $options(-root) $_selectFilePath]] } {
	    return
	}
      }
      set selectFilePath $_selectFilePath
    }
  }
  snit::widget chroot_getFile {
    hulltype toplevel
    delegate option -cursor to hull
    option {-initialdir initialDir InitialDir} -default {}
    option {-initialfile initialFile InitialFile} -default {}
    option {-filetypes fileTypes FileTypes} -default {}
    option {-defaultextension defaultExtension DefaultExtension} -default {}
    option {-saveoropen saveOrOpen SaveOrOpen} \
	    -type { snit::enum -values {save open}} \
	    -default open
    option -root -default / -validatemethod validexistingdirectory
    method validexistingdirectory {option value} {
      if {[file exists $value] && [file isdirectory $value]} {
	return $value
      }
      error "Expected a valid existing directory name for $option, got $value"
    }
    option -title -default "Select a directory"
    delegate option {-bannerimage bannerImage BannerImage} to banner as -image
    delegate option {-bannerbackground bannerBackground BannerBackground} to banner as -background
    option -parent -default .
    typevariable updirImage
    typevariable folderImage
    typevariable fileImage
    typeconstructor {
      set updirImage [image create bitmap -data {
#define updir_width 28
#define updir_height 16
static char updir_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x80, 0x1f, 0x00, 0x00, 0x40, 0x20, 0x00, 0x00,
   0x20, 0x40, 0x00, 0x00, 0xf0, 0xff, 0xff, 0x01, 0x10, 0x00, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x07, 0x00, 0x01, 0x90, 0x0f, 0x00, 0x01,
   0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01, 0x10, 0x02, 0x00, 0x01,
   0x10, 0xfe, 0x07, 0x01, 0x10, 0x00, 0x00, 0x01, 0x10, 0x00, 0x00, 0x01,
   0xf0, 0xff, 0xff, 0x01};}]
      set folderImage [image create photo -data {
R0lGODlhEAAMAKEAAAD//wAAAPD/gAAAACH5BAEAAAAALAAAAAAQAAwAAAIghINhyycvVFsB
QtmS3rjaH1Hg141WaT5ouprt2HHcUgAAOw==}]
      set fileImage    [image create photo -data {
R0lGODlhDAAMAKEAALLA3AAAAP//8wAAACH5BAEAAAAALAAAAAAMAAwAAAIgRI4Ha+IfWHsO
rSASvJTGhnhcV3EJlo3kh53ltF5nAhQAOw==}]
    }
    component banner
    component topframe
    component   dirlabel
    component   dirmenubtn
    component   dirmenu
    component   dirupbutton
    component iconList
    component bottomframe
    component   caption
    component   entry
    component   typeMenuLab
    component   typeMenuBtn
    component     typeMenu
    component   okButton
    component   cancelButton
    
    variable selectPath {}
    variable selectFile {}
    variable updateId
    variable selectFilePath
    variable filter "*"
    variable extUsed
    constructor {args} {
      install banner using Label $win.banner -anchor w
      pack $banner -fill x
      install topframe using frame $win.topframe
      install dirlabel using Label $topframe.dirlabel -text "Directory:" \
						      -underline 0
      set dirmenubtn $topframe.dirmenu
      bind $dirlabel <<AltUnderlined>> [list focus $dirmenubtn]
      install dirmenu using tk_optionMenu $topframe.dirmenu \
						[myvar selectPath] ""
      install dirupbutton using Button $topframe.dirupbutton -image $updirImage
      $dirmenubtn configure -takefocus 1 -highlightthickness 2
      pack $dirupbutton -side right -padx 4 -fill both
      pack $dirlabel -side left -padx 4 -fill both
      pack $dirmenubtn -expand yes -fill both -padx 4
      install iconList using ::tk::IconList $win.iconList \
				-command [mymethod OkCmd] -multiple no
      bind $iconList <<ListboxSelect>> [mymethod ListBrowse]
      install bottomframe using frame $win.bottomframe -bd 0
      install caption using Label $bottomframe.caption -text "File name:" \
						       -underline 5 -anchor e \
						       -pady 0
      bind $caption <<AltUnderlined>> [list focus $bottomframe.entry]
      install entry using Entry $bottomframe.entry
      set iconlistData "::tk::$iconList"
      append iconlistData (font)
      set $iconlistData [$entry cget -font]
      install typeMenuLab using Button $bottomframe.typeMenuLab \
		-text "Files of type:" -underline 9 -anchor e  \
		-bd [$caption cget -bd] -relief [$caption cget -relief] \
		-padx [$caption cget  -padx] -pady [$caption cget  -pady]
      bindtags $typeMenuLab [list $typeMenuLab Label \
		[winfo toplevel $typeMenuLab] all]
      install typeMenuBtn using menubutton $bottomframe.typeMenuBtn \
				-indicatoron 1 -menu $bottomframe.typeMenuBtn.m
      install typeMenu using menu $typeMenuBtn.m -tearoff 0
      $typeMenuBtn configure -takefocus 1 -highlightthickness 2 \
		-relief raised -bd 2 -anchor w
      bind $typeMenuLab <<AltUnderlined>> [list focus $typeMenuBtn]
      install okButton using Button $bottomframe.okButton -text "OK" \
							  -underline 0 \
							  -default active \
							  -pady 3
      bind $okButton <<AltUnderlined>> [list $okButton invoke]
      install cancelButton  using Button $bottomframe.cancelButton \
							-text "Cancel" \
							-underline 0 \
							-default normal \
							-pady 3
      bind $cancelButton <<AltUnderlined>> [list $cancelButton invoke]
      grid $caption $entry $okButton -padx 4 -sticky ew
      grid configure $entry -padx 2
      grid $typeMenuLab $typeMenuBtn   $cancelButton -padx 4 -sticky ew
      grid configure $typeMenuBtn -padx 0
      grid columnconfigure $bottomframe  1 -weight 1
      pack $topframe -side top -fill x -pady 4
      pack $bottomframe -side bottom -fill x
      pack $iconList -expand yes -fill both -padx 4 -pady 1
      wm protocol $win WM_DELETE_WINDOW [mymethod CancelCmd]
      $dirupbutton configure -command [mymethod UpDirCmd]
      $cancelButton configure -command [mymethod CancelCmd]
      bind $win <KeyPress-Escape> [mymethod CancelCmd]
      bind $win <Alt-Key> [list tk::AltKeyInDialog $win %A]
      bind $entry <Return> [mymethod ActivateEntry]
      $okButton configure -command [mymethod OkCmd]
      bind $win <Alt-t> [format {
	if {[string equal [%s cget -state] "normal"]} {
		focus %s
	}
      } $typeMenuBtn $typeMenuBtn]
      ::tk::FocusGroup_Create $win
      ::tk::FocusGroup_BindIn $win $entry [mymethod SelectionFocusIn]
      ::tk::FocusGroup_BindOut $win $entry [mymethod SelectionFocusOut]
      $self configurelist $args
      wm withdraw $win
    }
    method draw {args} {
      $self configurelist $args
      set initialdir $options(-initialdir)
      set initialfile $options(-initialfile)
      if {"$initialdir" eq ""} {set initialdir [file dirname $initialfile]}
      set initdirpathtype [file pathtype $initialdir]
      if {"$initdirpathtype" eq "absolute" || 
	  "$initdirpathtype" eq "volumerelative"} {
	set initialdir [eval [linsert [lrange \
					[file split $options(-initialdir)] \
					1 end] \
				      0 file join]]
				      
      }
      set initialfile [file join $initialdir [file tail $initialfile]]

      if {[winfo viewable [winfo toplevel $options(-parent)]]} {
	wm transient $win $options(-parent)
      }
      trace variable [myvar selectPath] w [mymethod SetPath]
      $dirmenubtn configure -textvariable [myvar selectPath]

      # Initialize the file types menu
      #
      if {[llength $options(-filetypes)]} {
	$typeMenu delete 0 end
	foreach ftype $options(-filetypes) {
	  set title  [lindex $ftype 0]
	  set filter [lindex $ftype 1]
	  $typeMenu add command -label $title \
		-command [mymethod SetFilter $ftype]
	}
	$self SetFilter [lindex $options(-filetypes) 0]
	$typeMenuBtn configure -state normal
	$typeMenuBtn configure -state normal
      } else {
	set filter "*"
	$typeMenuBtn configure -state disabled -takefocus 0
	$typeMenuBtn configure -state disabled
      }

      set previousEntryText ""
      $self UpdateWhenIdle
      ::tk::PlaceWindow $win widget $options(-parent)
      wm title $win $options(-title)
      ::tk::SetFocusGrab $win $entry
      if {$initialfile ne "" && [file exists [file join $options(-root) $initialdir]] &&
	  [file isdirectory [file join $options(-root) $initialdir]]} {
	set selectFile $initialfile
	set selectPath [file dirname $initialfile]
      }
      $entry delete 0 end
      $entry insert 0 $selectFile
      $entry selection range 0 end
      $entry icursor end
      vwait [myvar selectFilePath]
      ::tk::RestoreFocusGrab $win $entry withdraw
      foreach trace [trace vinfo [myvar selectPath]] {
	trace vdelete [myvar selectPath] [lindex $trace 0] [lindex $trace 1]
      }
      $dirmenubtn configure -textvariable {}
      puts stderr "*** method draw (after vwait): selectFilePath = $selectFilePath"
      puts stderr "*** method draw (after vwait): file join $options(-root) $selectFilePath = [file join $options(-root) $selectFilePath]"
      if {"$selectFilePath" eq ""} {
	return $selectFilePath
      } else {
	return [file join $options(-root) $selectFilePath]
      }
    }
    method UpdateWhenIdle {} {
      if {![info exists updateId]} {
	set updateId [after idle [mymethod Update]]
      }
    }
    method ListInvoke {filenames} {
      if {[llength $filenames] == 0} {
	return
      }
      set file [::tk::dialog::file::JoinFile $selectPath \
		[lindex $filenames 0]]
      puts stderr "*** $self ListInvoke: file = $file"
      puts stderr "*** $self ListInvoke: file join $options(-root) $file = [file join $options(-root) $file]"
      if {[file isdirectory [file join $options(-root) $file]]} {
      	set appPWD [pwd]
	if {[catch {cd [file join $options(-root) $file]}]} {
	  tk_messageBox -type ok -parent $win  -message \
	  	[format "Cannot change to the directory %s.\nPermission denied." $file]] \
	  	-icon warning
	} else {
	  cd $appPWD
	  set selectPath $file
        }
      } else {
        set selectFile $file
        $self Done
      }
    }
    method ListBrowse {} {
      set text {}
      foreach item [::tk::IconList_Curselection $iconList] {
	lappend text [::tk::IconList_Get $iconList $item]
      }
      if {[llength $text] == 0} {
	return
      }
      set text [lindex $text 0]
      set file [::tk::dialog::file::JoinFile $selectPath $text]
      set isDir [file isdirectory [file join $options(-root) $file]]
      if {!$isDir} {
	$entry delete 0 end
	$entry insert 0 $text
	switch $options(-saveoropen) {
	  open {
	    $okButton configure -text Open -underline 0
	  }
	  save {
	    $okButton configure -text Save -underline 0
	  }
	}
      } else {
	$okButton configure -text Open -underline 0
      }
    }
    method CancelCmd {} {
      set selectFilePath ""
    }
    method UpDirCmd {} {
      if {"$selectPath" ne ""} {
#	puts stderr "[list *** $self UpDirCmd: selectPath = $selectPath]"
#	puts stderr "[list *** $self UpDirCmd: file dirname $selectPath = [file dirname $selectPath]]"
        set selectPath [file dirname $selectPath]
      }
    }
    method OkCmd {} {
      set filenames {}
      foreach item [::tk::IconList_Curselection $iconList] {
	append filenames [::tk::IconList_Get $iconList $item]
      }

      if {[llength $filenames] == 1} {
	set filename [lindex $filenames 0]
	set file [::tk::dialog::file::JoinFile $selectPath $filename]
	if {[file isdirectory [file join $options(-root) $file]]} {
	  $self ListInvoke [list $filename]
	  return
	}
      }
      $self ActivateEntry
    }
    method SelectionFocusIn {} {
      if {[string compare [$entry get] ""]} {
	$entry selection range 0 end
	$entry icursor end
      } else {
	$entry selection clear
      }
      switch $options(-saveoropen) {
	open {
	  $okButton configure -text Open -underline 0
	}
	save {
	  $okButton configure -text Save -underline 0
	}
      }
    }
    method SelectionFocusOut {} {
      $entry selection clear
    }
    method ActivateEntry {} {
      set text [$entry get]
      $self VerifyFileName $text
    }
    method VerifyFileName {filename} {
      set list [$self ResolveFile $selectPath $filename $options(-defaultextension)]
      foreach {flag path file} $list {break}
      puts stderr "*** $self VerifyFileName: filename is $filename, flag is $flag, path is $path, file is $file"
      switch -- $flag {
	OK {
	  if {[string equal $file ""]} {
	    set selectPath $path
	    $entry delete 0 end
	  } else {
	    $self SetPathSilently $path
	    set selectFile $file
	    $self Done
	  }
	}
	PATTERN {
	  set selectPath $path
	  set filter $file
	}
	FILE {
	  if {[string equal $options(-saveoropen) open]} {
	    tk_messageBox -icon warning -type ok -parent $win \
		-message "[format {File %s  does not exist.} [file join $path $file]]"
	    $entry selection range 0 end
	    $entry icursor end
	  } else {
	    $self SetPathSilently $path
	    set selectFile $file
	    $self Done
	  }
	}
	PATH {
	  tk_messageBox -icon warning -type ok -parent $win \
		-message "[format {Directory %s  does not exist.} $path]"
	  $entry selection range 0 end
	  $entry icursor end
	}
	CHDIR {
	  tk_messageBox -icon warning -type ok -parent $win \
		-message [format "Cannot change to the directory %s.\nPermission denied." $path]
	  $entry selection range 0 end
	  $entry icursor end
	}
	ERROR {
	  tk_messageBox -icon warning -type ok -parent $win \
		-message "[format {Invalid file name %s.} $path]"
	  $entry selection range 0 end
	  $entry icursor end
	}
      }
    }
    # method ResolveFile --
    #
    #	Interpret the user's text input in a file selection dialog.
    #	Performs:
    #
    #	(1) ~ substitution
    #	(2) resolve all instances of . and ..
    #	(3) check for non-existent files/directories
    #	(4) check for chdir permissions
    #
    # Arguments:
    #	context:  the current directory you are in
    #	text:	  the text entered by the user
    #	defaultext: the default extension to add to files with no extension
    #
    # Return vaue:
    #	[list $flag $directory $file]
    #
    #	 flag = OK	: valid input
    #	      = PATTERN	: valid directory/pattern
    #	      = PATH	: the directory does not exist
    #	      = FILE	: the directory exists by the file doesn't
    #			  exist
    #	      = CHDIR	: Cannot change to the directory
    #	      = ERROR	: Invalid entry
    #
    #	 directory      : valid only if flag = OK or PATTERN or FILE
    #	 file           : valid only if flag = OK or PATTERN
    #
    #	directory may not be the same as context, because text may contain
    #	a subdirectory name
    #
    method ResolveFile {context text defaultext} {

      set appPWD [pwd]

      set path [::tk::dialog::file::JoinFile $context $text]

      # If the file has no extension, append the default.  Be careful not
      # to do this for directories, otherwise typing a dirname in the box
      # will give back "dirname.extension" instead of trying to change dir.
      if {![file isdirectory [file join $options(-root) $path]] && 
	  [string equal [file ext $path] ""]} {
	set path "$path$defaultext"
      }


      if {[catch {file exists [file join $options(-root) $path]}]} {
	# This "if" block can be safely removed if the following code
	# stop generating errors.
	#
	#	file exists ~nonsuchuser
	#
	return [list ERROR $path ""]
      }

      if {[file exists [file join $options(-root) $path]]} {
	if {[file isdirectory [file join $options(-root) $path]]} {
	    if {[catch {cd [file join $options(-root) $path]}]} {
		return [list CHDIR $path ""]
	    }
	    set directory [pwd]
	    set file ""
	    set flag OK
	    cd $appPWD
	} else {
	    if {[catch {cd [file dirname [file join $options(-root) $path]]}]} {
		return [list CHDIR [file dirname $path] ""]
	    }
	    set directory [pwd]
	    set file [file tail $path]
	    set flag OK
	    cd $appPWD
	}
      } else {
	set dirname [file dirname $path]
	if {[file exists [file join $options(-root) $dirname]]} {
	    if {[catch {cd [file join $options(-root) $dirname]}]} {
		return [list CHDIR $dirname ""]
	    }
	    set directory [pwd]
	    set file [file tail $path]
	    if {[regexp {[*]|[?]} $file]} {
		set flag PATTERN
	    } else {
		set flag FILE
	    }
	    cd $appPWD
	} else {
	    set directory $dirname
	    set file [file tail $path]
	    set flag PATH
	}
      }

      set l [string length [file normalize $options(-root)]]
      incr l
      set directory [string range [file normalize $directory] $l end]

      return [list $flag $directory $file]
    }
    method SetPathSilently {path} {
      trace vdelete selectPath w [mymethod SetPath]
      set selectPath $path
      trace variable selectPath w [mymethod SetPath]
    }
    method SetPath {name1 name2 op} {
#      puts stderr "*** $self SetPath $name1 $name2 $op"
#      puts stderr "*** $self SetPath: winfo exists $win = [winfo exists $win]"
      if {[winfo exists $win]} {
	$self UpdateWhenIdle
      }      
    }
    method SetFilter {ftype} {
      set filter [lindex $ftype 1]
      $typeMenuBtn configure -text [lindex $ftype 0] -indicatoron 1
      if {![info exists $extUsed]} {
	if {[string length $options(-defaultextension)]} {
	  set extUsed yes
	} else {
	  set extUsed no
	}
      }
      if {!$extUsed} {
	# Get the first extension in the list that matches {^\*\.\w+$}
	# and remove all * from the filter.
	set index [lsearch -regexp $filter {^\*\.\w+$}]
	if {$index >= 0} {
	  set options(-defaultextension) \
		[string trimleft [lindex $filter $index] "*"]
	} else {
	  # Couldn't find anything!  Reset to a safe default...
	  set options(-defaultextension) ""
	}
      }
      set iconlistData "::tk::$iconList"
      append iconlistData (sbar)
      [set $iconlistData] set 0.0 0.0
      $self UpdateWhenIdle
    }
    method Update {} {
      if {![winfo exists $win]} {return}
      catch {unset updateId}
      set entCursor [$entry cget -cursor]
      set dlgCursor [$win       cget -cursor]
      $entry configure -cursor watch
      $win       configure -cursor watch
      update idletasks

#      puts stderr "*** $self Update: selectPath = $selectPath"

      set selectDir [file dirname $selectPath]

      ::tk::IconList_DeleteAll $iconList
      set dirs [lsort -dictionary -unique \
			[glob -tails \
			      -directory [file join $options(-root) \
						    $selectDir] -type d \
				-nocomplain *]]

#      puts stderr "[list *** $self Update: dirs = $dirs]"
      set dirList {}
      foreach d $dirs {
	lappend dirList $d
      }
      ::tk::IconList_Add $iconList $folderImage $dirList

      set cmd [list glob -tails -directory [file join $options(-root) \
						    $selectDir] \
			 -type {f b c l p s} -nocomplain]
      if {[string equal $filter *]} {
	lappend cmd .* *
      } else {
	eval [list lappend cmd] $filter
      }
      set fileList [lsort -dictionary -unique [eval $cmd]]
      ::tk::IconList_Add $iconList $fileImage $fileList

      ::tk::IconList_Arrange $iconList

      set list "."
      set dir ""
#      puts stderr "*** $self Update: file split selectPath = [file split $selectPath]"
      foreach subdir [file split $selectDir] {
	set dir [file join $dir $subdir]
	lappend list $dir
      }
#      puts stderr "[list *** $self Update: list = $list]"
      $dirmenu delete 0 end
      set var [myvar selectPath]
      foreach path $list {
	$dirmenu add command -label $path -command [list set $var $path]
      }
      switch $options(-saveoropen) {
	open {
	  $okButton configure -text Open -underline 0
	}
	save {
	  $okButton configure -text Save -underline 0
	}
      }

      $entry configure -cursor $entCursor
      $win       configure -cursor $dlgCursor
    }
    method Done {{_selectFilePath ""}} {
      if {[string equal $_selectFilePath ""]} {
	set _selectFilePath [::tk::dialog::file::JoinFile \
		$selectPath $selectFile]
      }
      if {"$options(-saveoropen)" eq "save"} {
	puts stderr "*** $self Done _selectFilePath = $_selectFilePath"
	if {[file exists [file join $options(-root) $_selectFilePath]]} {
	  set reply [tk_messageBox -icon warning -type yesno\
		-parent $win -message \
			[format "File %s already exists.\nDo you want to overwrite it?" $_selectFilePath]]
	  if {[string equal $reply "no"]} {return}
	}
      }
      set selectFilePath $_selectFilePath
    }
  }
  snit::macro ::RolePlayingDB3::MapBundleMountPoint {} {
    option {-mapbundlemountpoint mapBundleMountPoint MapBundleMountPoint} \
		-readonly yes -default /MAP -validatemethod validatemountpoint
    proc isvalidmountpoint {mp} {
      foreach {system additional} [file system $mp] {break}
      if {"$system" eq "tclvfs" &&
	  "$additional" eq "::vfs::mk4::handler mk4vfs1"} {
	return yes
      } else {
	return no
      }
    }
    method validatemountpoint {option value} {
      if {[file exists $value] && [file readable $value] &&
	  [file writable $value] && [file isdirectory $value] &&
	  [isvalidmountpoint $value]} {
	return $value
      } else {
	error "Expected a valid tclvfs / mk4vfs1 mount point for $option, but got $value"
      }
    }     
  }
  snit::macro ::RolePlayingDB3::MapEditorOption {} {
    option {-mapeditor mapEditor MapEditor} \
		-readonly yes -default {} -validatemethod validatemapeditor
    method validatemapeditor {option value} {
#

#      puts stderr "*** $self validatemapeditor $option $value"
#      puts stderr "*** $self validatemapeditor: winfo exists $value = [winfo exists $value]"
#      puts stderr "*** $self validatemapeditor: winfo toplevel $value = [winfo toplevel $value]"
#      puts stderr "*** $self validatemapeditor: winfo class $value = [winfo class $value]"
      if {[winfo exists $value] &&
	  [winfo toplevel $value] eq "$value" &&
	  [winfo class $value] eq "MapEditor"} {
	return $value
      } else {
	error "Expected a Map Editor for $option, but got $value"
      }
    }
  }
  snit::macro ::RolePlayingDB3::LevelEditorOption {} {
    option {-leveleditor levelEditor LevelEditor} \
		-readonly yes -default {} -validatemethod validatemapeditor
    method validatemapeditor {option value} {
#


#      puts stderr "*** $self validateleveleditor $option $value"
#      puts stderr "*** $self validateleveleditor: winfo exists $value = [winfo exists $value]"
#      puts stderr "*** $self validateleveleditor: winfo toplevel $value = [winfo toplevel $value]"
#      puts stderr "*** $self validateleveleditor: winfo class $value = [winfo class $value]"
      if {[winfo exists $value] &&
	  [winfo toplevel $value] eq "$value" &&
	  [winfo class $value] eq "LevelEditor"} {
	return $value
      } else {
	error "Expected a Level Editor for $option, but got $value"
      }
    }
  }
  snit::macro ::RolePlayingDB3::LevelDirOption {} {
    option {-leveldir levelDir LevelDir} \
		-readonly yes -default {} -validatemethod validateleveldir
    method validateleveldir {option value} {
#


#      puts stderr "*** [list $self validateleveldir $option $value]"
      if {(([file exists $value] && [file isdirectory $value]) ||
	   ![file exists $value]) &&
	  [file tail [file dirname $value]] eq "Levels" &&
	  [isvalidmountpoint [file dirname [file dirname $value]]]} {
	return $value
      } else {
	error "Expected a valid level directory for $option, but got $value"
      }
    }	
  }
  snit::macro ::RolePlayingDB3::SpaceFileOption {} {
    option {-spacefile spaceFile SpaceFile} \
		-readonly yes -default {} -validatemethod validatespacefile
    method validatespacefile {option value} {
#

#      puts stderr "*** [list $self validatespacefile $option $value]"
      if {![file isdirectory $value] && 
	  (([file exists $value] && [file writable $value]) ||
	   [file writable [file dirname $value]]) &&
	  [file tail [file dirname [file dirname $value]]] eq "Levels" &&
	  [isvalidmountpoint [file dirname \
				   [file dirname [file dirname $value]]]]} {
	return $value
      } else {
	error "Expected a valid space file for $option, but got $value"
      }
    }	
  }
    
  snit::widget MapEditor {
    option -template -readonly yes -default {}
    ::RolePlayingDB3::OpenFilename
    typevariable defaultfilename "map.rpg"
    variable currentFilename
    variable currentBaseFilename
    variable isdirty no
    method setdirty {} {set isdirty yes}
    variable isdirtylevel no
    method setdirtylevel {} {set isdirtylevel yes}
    variable levelEditors [list]
    variable path
    variable tempfile
    variable needmediatreeupdated no

    component banner
    component toolbar
    component panes
    component   sbpane
    component     sbpanes
    component       levelpane
    component	      leveltree
    component       mediapane
    component	      mediatree
    component   mappane
    component     mapframe

    typevariable filetypes {
      {{Map Files} {.rpg}             }
      {{All Files}        *           }
    }
    typemethod myfiletypes {} {return $filetypes}
    typecomponent _editDialog
    typecomponent _chooseLevelDialog
    typecomponent _chooseMediaFolderDialog
    typecomponent _getMediaFileDialog
    typevariable bannerImage
    typevariable newLevelDialogBanner
    typevariable oldLevelDialogBanner
    typevariable newMediaFolderDialogBanner
    typevariable oldMediaFolderDialogBanner
    typevariable openMediaFileDialogBanner
    typevariable saveMediaFileDialogBanner
    typevariable dialogIcon
    typemethod   getDialogIcon {} {return $dialogIcon}
    typevariable bannerBackground #a2de86
    typevariable mapTemplateXML {<?xml version="1.0" ?>
<rpgv3:Map xmlns:rpgv3="http://www.deepsoft.com/roleplayingdb/v3xmlns" 
	   name="Map" >Global Map Information
  <rpgv3:Field name="Name" type="Word / Short Phrase" updatable="yes" />
  <rpgv3:Field name="Campaign" type="Word / Short Phrase" updatable="yes" />
  <rpgv3:Field name="Game Master" type="Word / Short Phrase" updatable="yes" />
  <rpgv3:Field name="Space Shape" type="Enumerated Type" values="Square Hexigonal" id="spaceshape" updatable="no" />
  <rpgv3:Field name="Short Description" type="Long Text" updatable="yes" />
  <rpgv3:Field name="Long Description" type="Document" updatable="yes" />
</rpgv3:Map>
    }
    method spaceshape {} {
      return [[$mapframe getElementWidgetById spaceshape] cget -text]
    }
    typevariable hWidth 0.5
    typevariable halfsl .288675
    typevariable squareCoords {-.5 -.5 .5 .5}
    typevariable hexCoords {-.5  .288675
			    -.5 -.288675 
			    0.0 -.57735 
			     .5 -.288675 
			     .5  .288675 
			    0.0  .57735 
			    -.5 .288675}
    method drawspace {canvas X Y color size args} {
      switch [$self spaceshape] {
	Square {
	  set index [eval [list $canvas create rectangle $squareCoords \
				-fill $color -outline black] $args]
	}
	Hexigonal {
	  set index [eval [list $canvas create polygon $hexCoords \
				-fill $color -outline black] $args]
	}
      }
      $canvas scale $index 0 0 $size $size
      $canvas move $index $X $Y
    }
    ::RolePlayingDB3::GeneratePrintDialog {printLevelsLCB} {
      set printLevelsLCB [LabelComboBox $frame.printLevelsLCB \
				-label "Print Levels?" -labelwidth 12 \
				-editable no \
				-values {yes no}]
      pack $printLevelsLCB -fill x
      $printLevelsLCB setvalue first
    }
    typeconstructor {
      set bannerImage [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						 MapBanner.png]]
      set dialogIcon [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						 MapDialogIcon.png]]
      set printerIcon [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						 largePrinter.gif]]
      set newLevelDialogBanner [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						 CreateLevelBanner.png]]
      set oldLevelDialogBanner [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						SelectLevelBanner.png]]
      set newMediaFolderDialogBanner [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						CreateNewMediaFolderBanner.png]]
      set oldMediaFolderDialogBanner [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						SelectMediaFolderBanner.png]]
      set openMediaFileDialogBanner [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						OpenMediaFileBanner.png]]
      set saveMediaFileDialogBanner [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						SaveMediaFileBanner.png]]
      set _printdialog {}
      set _editDialog {}
      set _chooseLevelDialog {}
      set _chooseMediaFolderDialog {}
      set _getMediaFileDialog {}
    }
    
    typemethod _createEditDialog {} {
      if {"$_editDialog" ne "" && [winfo exists "$_editDialog"]} {return}
      set _editDialog [Dialog .editMapEditorDialog -image $dialogIcon \
				-cancel 2 -default 0 -modal local \
				-parent . -side bottom \
				-title "Edit Map" \
				-transient yes]
      $_editDialog add -name new    -text {Create}
      $_editDialog add -name open   -text {Open}  
      $_editDialog add -name cancel -text {Cancel}
      pack [message [$_editDialog getframe].message \
	-text "Create a new Map file or\nopen an existing Map file?" \
	-aspect 500] -fill both
    }
    typemethod edit {args} {
      $type _createEditDialog
      set answer [$_editDialog draw]
      switch $answer {
        0 {
	  $type new -template $mapTemplateXML
	}
	1 {
	  $type open
	}
	2 {return}
      }
    }
    typemethod createChooseLevelDialog {} {
      if {"$_chooseLevelDialog" ne "" && [winfo exists "$_chooseLevelDialog"]} {return}
      set _chooseLevelDialog [::RolePlayingDB3::chroot_chooseDirectory \
				.chooseLevelDialog \
				-bannerimage $newLevelDialogBanner \
				-bannerbackground $bannerBackground]
    }
    typemethod draw_chooseLevelDialog {args} {
      $type createChooseLevelDialog
      return [eval [list $_chooseLevelDialog draw] $args]
    }
    typemethod createChooseMediaFolderDialog {} {
      if {"$_chooseMediaFolderDialog" ne "" && [winfo exists "$_chooseMediaFolderDialog"]} {return}
      set _chooseMediaFolderDialog [::RolePlayingDB3::chroot_chooseDirectory \
				.chooseMediaFolderDialog \
				-bannerimage $newMediaFolderDialogBanner \
				-bannerbackground $bannerBackground]
    }
    typemethod draw_chooseMediaFolderDialog {args} {
      $type createChooseMediaFolderDialog
      return [eval [list $_chooseMediaFolderDialog draw] $args]
    }
    typemethod createGetMediaFileDialog {} {
      if {"$_getMediaFileDialog" ne "" && [winfo exists "$_getMediaFileDialog"]} {return}
      set _getMediaFileDialog [::RolePlayingDB3::chroot_getFile \
				.getMediaFileDialog \
				-bannerimage $saveMediaFileDialogBanner \
				-bannerbackground $bannerBackground]
    }
    typemethod draw_getMediaFileDialog {args} {
      $type createGetMediaFileDialog
      return [eval [list $_getMediaFileDialog draw] $args]
    }
    method getfile {} {return "$currentFilename"}
    typemethod new {args} {
      set templateXML [from args -template $mapTemplateXML]
      set parent [from args -parent .]

      set newTop [RolePlayingDB3::RPGToplevel \
	.map%AUTO% \
	-mainframeconstructor $type \
	-mainframetemplate $templateXML \
	-class MapEditor]
    }
    method opennew {} {
      set currentFilename {}
      set currentBaseFilename *noname*
      set path [$type genname]
      set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      while {[file exists $tempfile]} {
	set path [$type genname]
	set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      }
      vfs::mk4::Mount $tempfile /$path
      file mkdir [file join /$path Levels]
      close [open [file join /$path Levels flag] w]
      file mkdir [file join /$path media]
      close [open [file join /$path media flag] w]
      file mkdir [file join /$path xml]
      close [open [file join /$path xml flag] w]
      [winfo toplevel $win] configure -title "Map Edit: $currentBaseFilename"
    }
    method new {} {
      $type new -parent $win -template $mapTemplateXML
    }
    typevariable genindex 0
    typemethod genname {} {
      incr genindex
      return [format {MAP%05d} $genindex]
    }
    typemethod open {args} {
      set parent [from args -parent .]
      set like [from args -like $defaultfilename]
      set currentFilename [tk_getOpenFile -defaultextension .rpg \
				-filetypes $filetypes \
				-initialdir [file dirname $like] \
				-initialfile $like \
				-parent $parent \
				-title "File to open"]
      if {"$currentFilename" eq ""} {return}
      set newTop [RolePlayingDB3::RPGToplevel \
			.map%AUTO% \
			-mainframeconstructor $type \
			-mainframetemplate {} \
			-openfilename $currentFilename \
			-class MapEditor]
    }
    method openold {_filename} {
      set path [$type genname]
      set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      while {[file exists $tempfile]} {
	set path [$type genname]
	set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      }
      vfs::mk4::Mount $tempfile /$path
      set currentFilename $_filename 
      set currentBaseFilename [file tail $currentFilename]
      set inpath [$type genname]
      vfs::zip::Mount $currentFilename $inpath
      if {[catch {file copy [file join $inpath Levels] /$path}]} {
	file mkdir [file join /$path Levels]
	close [open [file join /$path Levels flag] w]
      }
      if {[catch {file copy [file join $inpath xml] /$path}]} {
	file mkdir [file join /$path xml]
	close [open [file join /$path xml flag] w]
      }
      if {[catch {file copy [file join $inpath media] /$path}]} {
	file mkdir [file join /$path media]
	close [open [file join /$path media flag] w]
      }
      vfs::unmount $inpath
      [winfo toplevel $win] configure -title "Map Editor: $currentBaseFilename"
    }
    method save {} {
      $self saveas $currentFilename
    }
    method saveas {{_filename {}}} {
      if {"$_filename" eq {}} {
	set _filename [tk_getSaveFile -defaultextension .rpg \
				      -filetypes $filetypes \
				      -initialdir [file dirname $currentFilename] \
				      -initialfile $currentFilename \
				      -parent $win \
				      -title "Save As File"]
      }
      if {"$_filename" eq {}} {return}
      foreach le $levelEditors {
	$le checksave
      }
      if {$isdirty} {$self recreateXML}
      ::ZipArchive createZipFromDirtree $_filename /$path \
				-comment "RPGV3 Map Bundle"
      set isdirty no
      if {"$currentFilename" ne "$_filename"} {
	set currentFilename $_filename
	set currentBaseFilename [file tail $currentFilename]
	[winfo toplevel $win] configure -title "Map Edit: $currentBaseFilename]"
      }
    }
    method print {} {
      set printfile "[file rootname $currentFilename].pdf"
      if {"$printfile" eq ".pdf"} {set printfile "Map.pdf"}
      set pdfobj [$type drawPrintDialog \
				-parent $win \
				-what Map -filename $printfile]
      if {"$pdfobj" eq ""} {return}
      if {"$currentFilename" eq ""} {
	set heading [file tail "$currentBaseFilename"]
      } else {
        set heading [file tail "$currentFilename"]
      }
      $mapframe outputXMLToPDF $pdfobj "$heading: map info"
      set curpage [$mapframe getpageno]
      set curline [$mapframe getlineno]
      if {[$printLevelsLCB cget -text]} {
	foreach l [lsort -command [mymethod _sortlevelsbydepth] \
				  [glob -nocomplain -type d \
						[file join /$path Levels *]]] {
	  ::RolePlayingDB3::LevelEditor printLevel $pdfobj $l curpage curline \
				"$heading: level info for [file tail $l]" \
				-mapbundlemountpoint /$path -parent $win \
				-mapeditor [winfo toplevel $win]
        }
      }
      $pdfobj destroy
    }
    method close {args} {
      set dontsave no
      foreach le $levelEditors {
	if {[$le getdirty]} {
	  set isdirtylevel yes
	  break
	}
      }
      if {$isdirty || $isdirtylevel} {
	set ans [tk_messageBox -type yesnocancel -icon question -parent $win \
				-message "Save data before closing window?"]
	switch $ans {
	  yes {$self save}
	  cancel {return}
	  no {set dontsave yes}
        }
      }
      foreach le $levelEditors {
	catch {$le close -dontask yes -dontsave $yes}
      }
      vfs::unmount /$path
      file delete $tempfile
      destroy [winfo toplevel $win]
    }
    constructor {args} {
      $self configurelist $args
      if {"$options(-openfilename)" eq "" && "$options(-template)" ne ""} {
        $self opennew
        set XML $options(-template)
	set xmlfile [file join /$path xml map.xml]
	set isnew yes
      } elseif {"$options(-openfilename)" ne ""} {
        $self openold $options(-openfilename)
	set xmlfile {}
        if {[catch {open [file join /$path xml map.xml] r} shfp]} {
	  error "Illformed map bundle: map.xml cannot be opened: $shfp"
	}
	set XML [read $shfp]
        close $shfp
	set isnew no
      } else {
	error "Neither -template nor -openfilename was passed!"
      }
      install banner using Label $win.banner \
			-image $bannerImage -anchor w \
			-background $bannerBackground
      pack $banner -fill x
      install toolbar using ButtonBox $win.toolbar -orient horizontal \
						   -homogeneous no
      pack $toolbar -fill x
      $toolbar add -name newlevel -text {New Level} \
				  -command [mymethod _newlevel]
      $toolbar add -name dellevel -text {Delete Level} \
				  -command [mymethod _deletelevel]
      $toolbar add -name editlevel -text {Edit Level} \
				   -command [mymethod _editlevel]
      $toolbar add -name addmedia -text {Add Media} \
				  -command [mymethod _addmedia]
      $toolbar add -name newmediafolder -text {New Media Folder} \
				  -command [mymethod _newmediafolder]
      $toolbar add -name delmedia -text {Delete Media} \
				  -command [mymethod _delmedia]
      $toolbar add -name delmediafolder -text {Delete Media Folder} \
      				  -command [mymethod _delmediafolder]
      install panes using PanedWindow $win.panes -side top
      pack $panes -fill both -expand yes
      set sbpane [$panes add -weight 1]
      install sbpanes using PanedWindow $sbpane.sbpanes -side right
      pack $sbpanes -fill both -expand yes
      set levelpane [$sbpanes add -weight 1]
      install leveltree using ::RolePlayingDB3::LabeledDirTree \
		$levelpane.leveltree -showextension no -filepattern *.xml \
				     -label "Levels" -nofiles yes -opendirs no \
				     -sortfunction [mymethod _sortlevelsbydepth]
      pack $leveltree -expand yes -fill both
      set mediapane [$sbpanes add -weight 1]
      install mediatree using ::RolePlayingDB3::LabeledDirTree \
		$mediapane.mediatree -showextension yes -filepattern * \
				     -label "Media"
      pack $mediatree -expand yes -fill both
      set mappane [$panes add -weight 5]
      install mapframe using ::RolePlayingDB3::XMLContentEditor \
			$mappane.mapframe -xml $XML -isnewobject $isnew \
					  -dirtyvariable [myvar isdirty] \
					  -filewidgethandler [mymethod _filewidgethandler] \
					  -xmlfile $xmlfile -basedirectory /$path
      pack $mapframe -fill both -expand yes
      $leveltree configure -directory [file join /$path Levels]
      $mediatree configure -directory [file join /$path media]
    }
    method _sortlevelsbydepth {la lb} {
#

#      puts stderr "*** $self _sortlevelsbydepth $la $lb"
      if {[catch {open [file join $la levelinfo.xml] r} lfp]} {
	error "Illformed map bundle: [file join $la levelinfo.xml] cannot be opened: $lfp"
      }
      set xmla [read $lfp]
      close $lfp
      if {[catch {open [file join $lb levelinfo.xml] r} lfp]} {
	error "Illformed map bundle: [file join $lb levelinfo.xml] cannot be opened: $lfp"
      }
      set xmlb [read $lfp]
      close $lfp
      set a_depth [::RolePlayingDB3::XMLContentEditor ExtractTagValue $xmla [mymethod _matchfield Depth] 0]
#      puts stderr "*** $self _sortlevelsbydepth: a_depth = $a_depth"
      set b_depth [::RolePlayingDB3::XMLContentEditor ExtractTagValue $xmlb [mymethod _matchfield Depth] 0]
#      puts stderr "*** $self _sortlevelsbydepth: b_depth = $b_depth"
      set comp [expr {$a_depth - $b_depth}]
#      puts stderr "*** $self _sortlevelsbydepth: comp = $comp"
      if {$comp == 0} {
	return [string compare -nocase [file tail $la] [file tail $lb]]
      } else {
	return $comp
      }
    }
    method _matchfield {matchname tag attrlist arglist} {
#

#      puts stderr "*** $self _matchfield: matchname = $matchname, tag = $tag, attrlist = $attrlist"
      if {[string totitle $tag] eq "Field"} {
        foreach {n v} $attrlist {
	  if {"$n" eq "name" && [string totitle "$v"] eq "$matchname"} {
	    return yes
	  }
	}
      }
      return no
    }
    method recreateXML {} {
      set needmediatreeupdated no
      $mapframe recreateXML [file join /$path xml map.xml]
      if {$needmediatreeupdated} {$self updatemediatree}
    }
    method updateleveltree {} {
      $leveltree redrawdirtree
    }
    method updatemediatree {} {
      $mediatree redrawdirtree
    }
    method _filewidgethandler {curfile} {
      if {"$curfile" eq ""} {return $curfile}
      if {[file pathtype "$curfile"] eq "relative" &&
	  "media" eq [lindex [file split $curfile] 0]} {
	return "$curfile"
      } else {
	file copy -force "$curfile" [file join /$path media [file tail $curfile]]
	set needmediatreeupdated yes
	return "[file join media [file tail $curfile]]"
      }
    }
    method _newlevel {args} {
      set parent [from args -parent $win]
      set leveldir [$type draw_chooseLevelDialog \
					-bannerimage $newLevelDialogBanner \
					-root [file join /$path Levels] \
					-mustexist no \
					-initialdir "New Level" \
					-parent $win \
					-title "Level to create"]
#      puts stderr "*** $self _newlevel: leveldir = $leveldir"
      if {"$leveldir" eq ""} {return ""}
      if {[file system [file dirname $leveldir]] ne [file system /$path]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join /$path Levels]]/* [file normalize $leveldir]]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the Levels folder!"
	return
      }
      if {[file normalize "$leveldir"] eq [file normalize [file join /$path Levels]]} {
	tk_messageBox -parent $win -type ok -icon info -message "You cannot use the Levels folder itself as a level!"
	return
      }
      if {[file exists "$leveldir"]} {
	tk_messageBox -parent $win -type ok -icon info -message "Level [file tail $leveldir] already exists!"
	return
      }

      set newleveleditor [::RolePlayingDB3::LevelEditor new \
						-mapbundlemountpoint /$path \
						-parent $parent \
						-leveldir $leveldir \
						-mapeditor [winfo toplevel $win]]
      if {"$newleveleditor" ne ""} {
	set isdirty yes
	$self updateleveltree
	lappend levelEditors $newleveleditor
      }
    }
    method _deletelevel {} {
      if {[llength [glob -nocomplain -type d [file join /$path Levels *]]] == 0} {return}
      set selection [$leveltree selection get]
      if {[llength $selection] > 0} {
	set level [$leveltree itemcget [lindex $selection 0] -fullpath]
      } else {
	set level [$type draw_chooseLevelDialog \
					    -bannerimage $oldLevelDialogBanner \
				 	    -root [file join /$path Levels] \
					    -mustexist yes \
					    -parent $win \
					    -title "Level to delete"]
      }
      if {"$level" eq ""} {return}
      if {[file system [file dirname $level]] ne [file system /$path]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join /$path Levels]]/* [file normalize $level]]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the Levels folder!"
	return
      }
      if {[file normalize "$level"] eq [file normalize [file join /$path Levels]]} {
	tk_messageBox -parent $win -type ok -icon info -message "You cannot delete the Levels folder itself!"
	return
      }
      set lelist $levelEditors
      foreach le $levelEditors {
        if {"$level" eq "[$le getleveldir]"} {
	  $le close -dontsave yes
	  set leindex [lsearch -exact $lelist $le]
	  if {$leindex >= 0} {set lelist [lreplace $lelist $leindex $leindex]}
	}
      }
      set levelEditors $lelist
      deletetree $level
      $self updateleveltree
      set isdirty yes
    }
    method _editlevel {args} {
      set parent [from args -parent $win]
      if {[llength [glob -nocomplain -type d [file join /$path Levels *]]] == 0} {return}
      if {[llength [glob -nocomplain -type d [file join /$path Levels *]]] == 0} {return}
      set selection [$leveltree selection get]
      if {[llength $selection] > 0} {
	set level [$leveltree itemcget [lindex $selection 0] -fullpath]
      } else {
	set level [$type draw_chooseLevelDialog \
					    -bannerimage $oldLevelDialogBanner \
				 	    -root [file join /$path Levels] \
					    -mustexist yes \
					    -parent $win \
					    -title "Level to edit"]
      }
      if {"$level" eq ""} {return}
#      puts stderr "*** $self _editlevel: file system [file dirname $level] = '[file system [file dirname $level]]', file system /$path = '[file system /$path]'"
      if {[file system [file dirname $level]] ne [file system /$path]} {
	tk_messageBox -parent $parent -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
#      puts stderr "[list *** $self _editlevel:  [file normalize [file join /$path Levels]]/* [file normalize $level]]"
      if {![string match [file normalize [file join /$path Levels]]/* [file normalize $level]]} {
	tk_messageBox -parent $parent -type ok -icon info -message "Opps, you stepped out of the Levels folder!"
	return
      }
#      puts stderr "[list *** $self _editlevel: [file normalize [file join /$path Levels]]]"
      if {[file normalize "$level"] eq [file normalize [file join /$path Levels]]} {
	tk_messageBox -parent $parent -type ok -icon info -message "You cannot edit the Levels folder itself!"
	return
      }
      foreach le $levelEditors {
        if {"$level" eq "[$le getleveldir]"} {
	  wm deinconify $le
	  raise $le
	  return
        }
      }
      set newleveleditor [::RolePlayingDB3::LevelEditor open \
						-mapbundlemountpoint /$path \
						-leveldir $level \
						-mapeditor [winfo toplevel $win] \
						-parent $parent]
      if {"$newleveleditor" ne ""} {
	lappend levelEditors $newleveleditor
      }
    }
    method removeeditor {le} {
      set leindex [lsearch -exact $levelEditors $le]
      if {$leindex >= 0} {
	set levelEditors [lreplace $levelEditors $leindex $leindex]
      }
    }
    method _addmedia {} {
      set sourcefile [tk_getOpenFile -parent $win -title "Source file" \
      				     -initialdir [file dirname $currentFilename]]
      if {"$sourcefile" eq ""} {return}
      set destfile   [$type draw_getMediaFileDialog \
				     -parent $win -title "Destination file" \
				     -root [file join /$path media] \
				     -initialfile [file tail $sourcefile] \
				     -saveoropen save]
      puts stderr "*** $self _addmedia: destfile is $destfile"
      if {"$destfile" eq ""} {return}
      if {[file system [file dirname $destfile]] ne [file system /$path]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join /$path media]]/* [file normalize $destfile]]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the media folder!"
	return
      }
      file copy $sourcefile $destfile
      $self updatemediatree
      set isdirty yes
    }
    method _newmediafolder {} {
      set folder [$type draw_chooseMediaFolderDialog \
				-root [file join /$path media] \
				-initialdir "New Folder" \
				-mustexist no \
				-bannerimage $newMediaFolderDialogBanner \
				-parent $win -title "New Folder"]
      if {"$folder" eq ""} {return}
      if {[file system [file dirname $folder]] ne [file system /$path]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join /$path media]]/* [file normalize $folder]]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the media folder!"
	return
      }
      if {[file exists "$folder"] && ![file isdirectory "$folder"]} {
	tk_messageBox -parent $win -type ok -icon info -message "$folder exists and is not a folder!"
	return
      }
      if {[file exists "$folder"]} {
	tk_messageBox -parent $win -type ok -icon info -message "$folder already exists."
	return
      }
      file mkdir $folder
      close [open [file join $folder flag] w]
      $self updatemediatree
      set isdirty yes
    }
    method _delmedia {} {
      set selection [$mediatree selection get]
      if {[llength $selection] > 0} {
	set destfile [$mediatree itemcget [lindex $selection 0] -fullpath]
	if {![file isfile $destfile]} {
	  set destfile   [$type draw_getMediaFileDialog \
				-parent $win -title "File to delete" \
				-root [file join /$path media] \
				-saveoropen open]
	}
      } else {
	set destfile   [$type draw_getMediaFileDialog \
				-parent $win -title "File to delete" \
				-root [file join /$path media] \
				-saveoropen open]
      }
      if {"$destfile" eq ""} {return}
      if {[file tail $destfile] eq "flag"} {
	tk_messageBox -parent $win -type ok -icon info -message "You cannot delete place holder (flag) files!"
	return
      }
      if {[file system $destfile] ne [file system /$path]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join /$path media]]/* [file normalize $destfile]]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the media folder!"
	return
      }
      file delete $destfile
      $self updatemediatree
      set isdirty yes
    }
    proc deletetree {dir} {
      foreach d [glob -nocomplain -type d [file join $dir *]] {
	deltree $d
      }
      foreach f [glob -nocomplain [file join $dir *]] {
	file delete $f
      }
      file delete $dir
    }
    method _delmediafolder {} {
      set selection [$mediatree selection get]
      if {[llength $selection] > 0} {
	set folder [$mediatree itemcget [lindex $selection 0] -fullpath]
	if {![file isdirectory $folder]} {set folder [file dirname $folder]}
      } else {
	set folder [$type draw_chooseMediaFolderDialog \
			-root [file join /$path media] \
			-bannerimage $oldMediaFolderDialogBanner \
			-parent $win -title "Folder to delete" \
			-mustexist yes]
      }
      if {"$folder" eq ""} {return}
      if {[file system [file dirname $folder]] ne [file system /$path]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join /$path media]]/* [file normalize $folder]]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of the media folder!"
	return
      }
      if {[file normalize "$folder"] eq [file normalize [file join /$path media]]} {
	tk_messageBox -parent $win -type ok -icon info -message "You cannot delete the media folder itself!"
	return
      }
      if {[llength [glob -nocomplain [file join $folder *]]] > 1} {
	set ans [tk_messageBox -parent $win -type yesno -default no \
			-icon question -message "$folder is not empty! Are sure you want to delete it?"]
	if {"$ans" eq "no"} {return}
      }
#      puts stderr "*** $self _delmediafolder: folder = $folder"
      deletetree $folder
      $self updatemediatree
      set isdirty yes
    }
  }
  snit::widget LevelEditor {
    option -template -readonly yes -default {}
    ::RolePlayingDB3::MapBundleMountPoint
    ::RolePlayingDB3::MapEditorOption
    ::RolePlayingDB3::LevelDirOption
    variable currentLevelDir
    method getleveldir {} {return $currentLevelDir}
    variable isdirty no
    variable needmediatreeupdated
    variable spaceEditors [list]
    variable firstsave yes
    variable zoommenu
    variable zoomfactor 1
    variable oldscalefactor 1
    method setdirty {} {set isdirty yes}
    method getdirty {} {return $isdirty}
    variable isdirtyspace no
    method setdirtyspace {} {set isdirtyspace yes}
    method checksave {} {
      foreach se $spaceEditors {
	$se checksave
      }
      if {$isdirty} {
	$self recreateXML
	if {$firstsave} {
	  $options(-mapeditor) updateleveltree
	  set firstsave no
	}
	$options(-mapeditor) setdirtylevel
      }
    }

    component banner
    component toolbar
    component panes  
    component   sbpane
    component     spacetree
    component   levelpane
    component     levelframe

    typevariable bannerImage
    typevariable bannerBackground #a2de86
    typevariable levelTemplateXML {<?xml version="1.0" ?>
<rpgv3:Level xmlns:rpgv3="http://www.deepsoft.com/roleplayingdb/v3xmlns"
	     name="Level" >Level Information
  <rpgv3:Field name="Title" type="Word / Short Phrase" updatable="yes" />
  <rpgv3:Field name="Depth" type="Whole Number" updatable="no" range="-50 50 1" />
  <rpgv3:Field name="Description" type="Long Text" updatable="yes" />
  <rpgv3:LevelMap name="Level Map">Level Map
    <rpgv3:Canvas side="left" id="map" background="gray" />
    <rpgv3:Buttonbox side="right" orient="vertical" id="zoombuttons">
      <rpgv3:Bi label="Zoom In"  id="zoomin" />
      <rpgv3:Bi label="Zoom &gt;"  id="zoomto" />
      <rpgv3:Bi label="Zoom Out" id="zoomout" />
    </rpgv3:Buttonbox >
  </rpgv3:LevelMap >  
</rpgv3:Level>
    }
    ::RolePlayingDB3::GeneratePrintDialog {printSpacesLCB} {
      set printSpacesLCB [LabelComboBox $frame.printSpacesLCB \
				-label "Print Spaces?" -labelwidth 12 \
				-editable no \
				-values {yes no}]
      pack $printSpacesLCB -fill x
      $printSpacesLCB setvalue first
    }
    typeconstructor {
      set bannerImage [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						 LevelBanner.png]]
      set printerIcon [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						 largePrinter.gif]]
      set _printdialog {}
    }
    method new {} {
      $options(-mapeditor) _newlevel -parent $win
    }
    typemethod new {args} {
      set mapbundlemountpoint [from args -mapbundlemountpoint]
      set parent [from args -parent .]
      set mapeditor [from args -mapeditor]
      set leveldir [from args -leveldir]
      set newTop [RolePlayingDB3::RPGToplevel \
		.level%AUTO% \
		-mainframeconstructor $type \
		-mainframetemplate $levelTemplateXML \
		-class LevelEditor \
		-mapbundlemountpoint $mapbundlemountpoint \
		-mapeditor $mapeditor -leveldir $leveldir -minwidth 800]
      return $newTop
    }
    method open {} {
      $options(-mapeditor) _editlevel -parent $win
    }
    typemethod open {args} {
      set mapbundlemountpoint [from args -mapbundlemountpoint]
      set leveldir [from args -leveldir]
      set mapeditor [from args -mapeditor]
      set parent [from args -parent .]

      set newTop [RolePlayingDB3::RPGToplevel \
		.level%AUTO% \
		-mainframeconstructor $type \
		-mainframetemplate {} \
		-class LevelEditor \
		-mapbundlemountpoint $mapbundlemountpoint \
		-mapeditor $mapeditor \
		-leveldir $leveldir -minwidth 800]
      return $newTop
    }
    method save {} {
      $options(-mapeditor) save
    }
    method saveas {{_filename {}}} {
      $options(-mapeditor) saveas $_filename
    }
    typemethod printLevel {pdfobj leveldir curpageV curlineV heading args} {
      upvar $curpageV curpage
      upvar $curlineV curline
      set mapbundlemountpoint [from args -mapbundlemountpoint]
      set mapeditor           [from args -mapeditor]
      set parent              [from args -parent]
      if {[catch {open [file join $leveldir levelinfo.xml] r} shfp]} {
	error "Illformed map bundle: [file join $currentLevelDir levelinfo.xml] cannot be opened: $shfp"
      }
      set XML [read $shfp]
      close $shfp
      set temppath [::RolePlayingDB3::XMLContentEditor $parent.templevel%AUTO% \
			-xml $XML -isnewobject no -dirtyvariable {} \
			-filewidgethandler {}  -xmlfile {} \
			-basedirectory $mapbundlemountpoint]
      set map [$temppath getElementWidgetById map]
      foreach s [lsort -dictionary [glob -nocomplain [file join $leveldir *.xml]]] {
	if {[file tail $s] eq "levelinfo.xml"} {continue}
	drawonespace_forprint $map $s $mapeditor
      }
      $temppath outputXMLToPDF $pdfobj $heading $curpage $curline
      set curpage [$temppath getpageno]
      set curline [$temppath getlineno]
      foreach s [lsort -dictionary [glob -nocomplain [file join $leveldir *.xml]]] {
	if {[file tail $s] eq "levelinfo.xml"} {continue}
	::RolePlayingDB3::SpaceEditor printSpace $pdfobj $s curpage curline \
			"$heading: Space [file rootname [file tail $s]]" \
			-mapbundlemountpoint $mapbundlemountpoint \
			-leveldir $leveldir -parent $temppath \
			-mapeditor $mapeditor
      }
      destroy $temppath
    }
    method print {} {
      #$options(-mapeditor) print
      set printfile "[file tail $currentLevelDir].pdf"
      if {"$printfile" eq ".pdf"} {set printfile "Level.pdf"}
      set pdfobj [$type drawPrintDialog \
				-parent $win \
				-what Level -filename $printfile]
      if {"$pdfobj" eq ""} {return}
      set heading $currentLevelDir
      set map [$levelframe getElementWidgetById map]
      set scalingfactor [expr {1.0 / double($oldscalefactor)}]
      $map scale all 0 0 $scalingfactor $scalingfactor
      $map configure -scrollregion [$map bbox all]
      $levelframe outputXMLToPDF $pdfobj "$heading: level info"
      set scalingfactor $zoomfactor
      $map scale all 0 0 $scalingfactor $scalingfactor
      $map configure -scrollregion [$map bbox all]
      set curpage [$levelframe getpageno]
      set curline [$levelframe getlineno]
      if {[$printSpacesLCB cget -text]} {
	foreach s [lsort [glob -nocomplain [file join $options(-leveldir) *.xml]]] {
	  if {[file tail $s] eq "levelinfo.xml"} {continue}
	  ::RolePlayingDB3::SpaceEditor printSpace $pdfobj $s curpage curline \
			"$heading: Space [file rootname [file tail $s]]" \
			-mapbundlemountpoint $options(-mapbundlemountpoint) \
			-leveldir $options(-leveldir) -parent $win \
			-mapeditor $options(-mapeditor)
        }
      }
      $pdfobj destroy
    }
    method close {args} {
      set dontask [from args -dontask no]
      set dontsave [from args -dontsave no]
      if {[from args -closingallwindows no]} {
	$options(-mapeditor) close -closingallwindows yes
      }
      if {!$dontsave} {
	foreach se $spaceEditors {
	  if {[$se getdirty]} {
	    set isdirtyspace yes
	    break
	  }
        }
	if {!$dontask} {
	  if {$isdirty || $isdirtyspace} {
	    set ans [tk_messageBox -type yesnocancel -icon question \
				   -parent $win \
				   -message "Save data before closing window?"]
	    switch $ans {
	      yes {
		$self checksave
		set dontask yes
	      }
	      cancel {return}
	      no {set dontsave yes}
	    }
          }
        } else {
	  $self checksave
	}
      }
      foreach se $spaceEditors {
	catch {$se close -dontsave $dontsave -dontask $dontask}
      }
      $options(-mapeditor) removeeditor [winfo toplevel $win]
      destroy [winfo toplevel $win]
    }
    constructor {args} {
      $self configurelist $args

      set currentLevelDir [file tail $options(-leveldir)]
      [winfo toplevel $win] configure -title "Level Edit: $currentLevelDir"
      if {"$options(-template)" ne "" && ![file exists $options(-leveldir)]} {
	file mkdir $options(-leveldir)
	close [open [file join $options(-leveldir) flag] w]
	set XML $options(-template)
	set xmlfile [file join $options(-leveldir) levelinfo.xml]
	set firstsave yes
	set isnew yes
	set isdirty yes
      } elseif {[file exists $options(-leveldir)]} {
	set xmlfile {}
	if {[catch {open [file join $options(-leveldir) levelinfo.xml] r} shfp]} {
	  error "Illformed map bundle: [file join $currentLevelDir levelinfo.xml] cannot be opened: $shfp"
	}
	set XML [read $shfp]
	close $shfp
	set firstsave no
	set isnew no
      } else {
	error "Neither -template nor a valid -leveldir was passed!"
      }
      install banner using Label $win.banner \
			-image $bannerImage -anchor w \
			-background $bannerBackground
      pack $banner -fill x  
      install toolbar using ButtonBox $win.toolbar -orient horizontal \
						   -homogeneous no
      pack $toolbar -fill x
      $toolbar add -name newspace -text {New Space} \
				  -command [mymethod _newspace]
      $toolbar add -name delspace -text {Delete Space} \
				  -command [mymethod _deletespace]
      $toolbar add -name editspace -text {Edit Space} \
				   -command [mymethod _editspace]
      install panes using PanedWindow $win.panes -side top
      pack $panes -fill both -expand yes
      set sbpane [$panes add -weight 1]
      install spacetree using ::RolePlayingDB3::LabeledDirTree \
		$sbpane.spacetree -showextension no -filepattern *.xml \
				  -label "Spaces"
      pack $spacetree -expand yes -fill both
      set levelpane [$panes add -weight 5]
      install levelframe using ::RolePlayingDB3::XMLContentEditor \
			$levelpane.levelframe -xml $XML -isnewobject $isnew \
					      -dirtyvariable [myvar isdirty] \
					      -filewidgethandler [mymethod _filewidgethandler] \
					      -xmlfile $xmlfile \
					      -buttoncommand [mymethod _button] \
					      -basedirectory $options(-mapbundlemountpoint)
      pack $levelframe -fill both -expand yes
      $spacetree configure -directory $options(-leveldir)
      $self updatelevelmap
      set zoomtobutton [$levelframe getElementWidgetById zoomto]
#      puts stderr "*** $type create $self: zoomtobutton = $zoomtobutton"
      set zoommenu [menu $zoomtobutton.zoommenu -tearoff no]
      foreach zf {.0625 .125 .25 .5  1   2   4   8   16} \
	      lab {1:16 1:8  1:4 1:2 1 2:1 4:1 8:1 16:1} {
	$zoommenu add radiobutton -command [mymethod rescalelevelmap] \
				  -label "Zoom Factor $lab" \
				  -value $zf \
				  -variable [myvar zoomfactor]
      }
    }
    method updatespacetree {} {
      $spacetree redrawdirtree
    }
    method drawspace {canvas X Y color size args} {
      eval [list $options(-mapeditor) drawspace $canvas $X $Y $color $size] $args
    }
    method updatelevelmapcolor {color spacetag} {
      [$levelframe getElementWidgetById map] itemconfigure $spacetag \
		-fill $color
    }
    method rescalelevelmap {} {
      set map [$levelframe getElementWidgetById map]
      set scalingfactor [expr {$zoomfactor / double($oldscalefactor)}]
      $map scale all 0 0 $scalingfactor $scalingfactor
      set oldscalefactor $zoomfactor
      $map configure -scrollregion [$map bbox all]
    }
    proc drawonespace_forprint {canvas spacefile mapeditor} {
      if {[catch {open $spacefile r} xmlfp]} {
	error "Illformed map bundle: $spacefile cannot be opened: $xmlfp"
      }
      set spacexml [read $xmlfp]
      set X [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml [list ::RolePlayingDB3::LevelEditor::_matchfield "X Coord"] 0]
      set Y [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml [list ::RolePlayingDB3::LevelEditor::_matchfield "Y Coord"] 0]
      set color [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml [list ::RolePlayingDB3::LevelEditor::_matchfield "Color"] white]
      set yc [expr {$Y * 32}]
      set xc [expr {$X * 32}]
      if {"[$mapeditor spaceshape]" eq "Hexigonal"} {
	set xc [expr {$xc + 16}]
      }
      $mapeditor drawspace $canvas $xc $yc $color 32 \
			-tag "[file tail [file rootname $spacefile]]"
	
    }

    method drawonespace {X Y color spacetag} {
      set map [$levelframe getElementWidgetById map]
      set yc [expr {$Y * 32 * $zoomfactor}]
      set xc [expr {$X * 32 * $zoomfactor}]
      if {[$options(-mapeditor) spaceshape] eq "Hexigonal"} {
        set xc [expr {$xc + ((32 * $zoomfactor) / 2.0)}]
      }
      $self drawspace $map $xc $yc $color [expr {32 * $zoomfactor}] \
			-tag "$spacetag"
      $map configure -scrollregion [$map bbox all]
    }
    method updatelevelmap {} {
      set map [$levelframe getElementWidgetById map]
      $map delete all
      foreach n [$spacetree nodes root] {
        set fullpath [$spacetree itemcget $n -fullpath]
	if {[file tail $fullpath] eq "levelinfo.xml"} {
	  continue
	}
	if {[catch {open $fullpath r} xmlfp]} {
	  error "Illformed map bundle: $fullpath cannot be opened: $xmlfp"
	}
	set spacexml [read $xmlfp]
	close $xmlfp
	set X [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml [myproc _matchfield "X Coord"] 0]
	set Y [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml [myproc _matchfield "Y Coord"] 0]
	set color [::RolePlayingDB3::XMLContentEditor ExtractTagValue $spacexml [myproc _matchfield "Color"] white]
	set yc [expr {$Y * 32 * $zoomfactor}]
	set xc [expr {$X * 32 * $zoomfactor}]
	if {[$options(-mapeditor) spaceshape] eq "Hexigonal"} {
	  set xc [expr {$xc + ((32 * $zoomfactor) / 2.0)}]
	}
	$self drawspace $map $xc $yc $color [expr {32 * $zoomfactor}] \
			-tag "[file tail [file rootname $fullpath]]"
      }
      $map configure -scrollregion [$map bbox all]
    }
    proc _matchfield {matchname tag attrlist arglist} {
#

#      puts stderr "*** $self _matchfield: matchname = $matchname, tag = $tag, attrlist = $attrlist"
      if {[string totitle $tag] eq "Field"} {
        foreach {n v} $attrlist {
	  if {"$n" eq "name" && [string totitle "$v"] eq "$matchname"} {
	    return yes
	  }
	}
      }
      return no
    }
    method _button {id} {
      switch $id {
	zoomin {
	  switch $zoomfactor {
	    .0625 {set zoomfactor .125}
	    .125  {set zoomfactor .25}
	    .25   {set zoomfactor .5}
	    .5    {set zoomfactor 1}
	    1	  {set zoomfactor 2}
	    2	  {set zoomfactor 4}
	    4	  {set zoomfactor 8}
	    8	  {set zoomfactor 16}
	  }
	  $self rescalelevelmap
	}
	zoomto {
	  set RX [winfo pointerx $levelframe]
	  set RY [winfo pointery $levelframe]
	  $zoommenu post $RX $RY
	}
	zoomout {
	  switch $zoomfactor {
	    .125  {set zoomfactor .0625}
	    .25   {set zoomfactor .125}
	    .5    {set zoomfactor .25}
	    1	  {set zoomfactor .5}
	    2	  {set zoomfactor 1}
	    4	  {set zoomfactor 2}
	    8	  {set zoomfactor 4}
	    16	  {set zoomfactor 8}
	  }
	  $self rescalelevelmap
	}
      }
    }
    method recreateXML {} {
      set needmediatreeupdated no
      $levelframe recreateXML [file join $options(-leveldir) levelinfo.xml]
      if {$needmediatreeupdated} {$options(-mapeditor) updatemediatree}
    }
    method updatemediatree {} {$options(-mapeditor) updatemediatree}
    method _filewidgethandler {curfile} {
      if {"$curfile" eq ""} {return $curfile}
      if {[file pathtype "$curfile"] eq "relative" &&
	  "media" eq [lindex [file split $curfile] 0]} {
	return "$curfile"
      } else {
	file copy -force "$curfile" [file join $mapbundlemountpoint media [file tail $curfile]]
	set needmediatreeupdated yes
	return "[file join media [file tail $curfile]]"
      }
    }
    method removeeditor {se} {
      set seindex [lsearch -exact $spaceEditors $se]
      if {$seindex >= 0} {
	set spaceEditors [lreplace $spaceEditors $seindex $seindex]
      }
    }
    method _newspace {args} {
      set parent [from args -parent $win]
############################
      set space [tk_getSaveFile \
			-initialdir $leveldir \
			-parent $parent -title "Space to create" \
			-filetypes {{{Space Files} *.xml TEXT}}]
      if {"$space" eq ""} {return}
      if {[file system [file dirname $space]] ne [file system $leveldir]} {
	tk_messageBox -parent $parent -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join $leveldir]]/* [file normalize $space]]} {
	tk_messageBox -parent $parent -type ok -icon info -message "Opps, you stepped out of this level's folder!"
	return
      }
      if {[file normalize "$space"] eq [file normalize [file join $leveldir levelinfo.xml]]} {
	tk_messageBox -parent $parent -type ok -icon info -message "You cannot delete the levelinfo file!"
	return
      }
      if {[file exists "$space"]} {
	tk_messageBox -parent $parent -type ok -icon info -message "Space [file rootname [file tail $space]] already exists!"
	return
      }
      set newspaceeditor [::RolePlayingDB3::SpaceEditor new \
				-mapbundlemountpoint $options(-mapbundlemountpoint) \
				-leveldir $options(-leveldir) \
				-parent $parent -newspace $space \
				-leveleditor [winfo toplevel $win]]
      if {"$newspaceeditor" ne ""} {
	set isdirty yes
	$self updatespacetree
	lappend spaceEditors $newspaceeditor
      }
    }
    method _deletespace {} {
      if {[llength [glob -nocomplain -type f [file join $options(-leveldir) *.xml]]] == 1} {
	return
      }
      set selection [$spacetree selection get]
      if {[llength $selection] > 0} {
	set space [$spacetree itemcget [lindex $selection 0] -fullpath]
      } else {
##########################
	set space [tk_getOpenFile \
			-initialdir $options(-leveldir) \
			-parent $win -title "Space to delete" \
			-filetypes {{{Space Files} *.xml TEXT}}]
      }
      if {"$space" eq ""} {return}
      if {[file system [file dirname $space]] ne [file system $options(-leveldir)]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join $options(-leveldir)]]/* [file normalize $space]]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of this level's folder!"
	return
      }
      if {[file normalize "$space"] eq [file normalize [file join $options(-leveldir) levelinfo.xml]]} {
	tk_messageBox -parent $win -type ok -icon info -message "You cannot delete the levelinfo file!"
	return
      }
      set selist $spaceEditors
      foreach se $spaceEditors {
	if {"$space" eq "[$se getspacefile]"} {
	  $se close -dontsave yes
	  set seindex [lsearch -exact $selist $se]
	  if {$seindex >= 0} {set selist [lreplace $seindex $seindex $seindex]}
	}
      }
      set spaceEditors $selist
      file delete $space
      $self updatespacetree
      set isdirty yes      
    }
    method _editspace {args} {
      set parent [from args -parent $win]
      if {[llength [glob -nocomplain -type f [file join $options(-leveldir) *.xml]]] == 1} {
	return
      }
      set selection [$spacetree selection get]
      if {[llength $selection] > 0} {
	set space [$spacetree itemcget [lindex $selection 0] -fullpath]
      } else {
#########################
	set space [tk_getOpenFile \
			-initialdir $options(-leveldir) \
			-parent $win -title "Space to edit" \
			-filetypes {{{Space Files} *.xml TEXT}}]
      }
      if {"$space" eq ""} {return}
      if {[file system [file dirname $space]] ne [file system $options(-leveldir)]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped off the internal file system!"
	return
      }
      if {![string match [file normalize [file join $options(-leveldir)]]/* [file normalize $space]]} {
	tk_messageBox -parent $win -type ok -icon info -message "Opps, you stepped out of this level's folder!"
	return
      }
      if {[file normalize "$space"] eq [file normalize [file join $options(-leveldir) levelinfo.xml]]} {
	tk_messageBox -parent $win -type ok -icon info -message "You cannot use the space editor on the levelinfo file!"
	return
      }
      foreach se $spaceEditors {
	if {"$space" eq "[$se getspacefile]"} {
	  wm deinconify $se
	  raise $se
	  return
	}
      }
      set newspaceeditor [::RolePlayingDB3::SpaceEditor open \
				-mapbundlemountpoint $options(-mapbundlemountpoint) \
				-leveldir $options(-leveldir) \
				-parent $parent -spacefile $space \
				-leveleditor [winfo toplevel $win]]
      if {"$newspaceeditor" ne ""} {
	lappend spaceEditors $newspaceeditor
      }
    }
  }
  snit::widget SpaceEditor {
    option -template -readonly yes -default {}
    ::RolePlayingDB3::MapBundleMountPoint
    ::RolePlayingDB3::LevelEditorOption
    ::RolePlayingDB3::SpaceFileOption
    variable currentSpaceFile
    method getspacefile {} {return $currentSpaceFile}
    variable firstsave yes
    variable isdirty no
    variable needmediatreeupdated no
    variable zoommenu
    variable zoomfactor 1
    method setdirty {} {set isdirty yes}
    method checksave {} {
      if {$isdirty} {
	$self recreateXML
	if {$firstsave} {
	  $options(-leveleditor) updatelevelmap
	  set firstsave no
	}
	set isdirty no
      }
      $options(-leveleditor) setdirtyspace
    }
    variable spacecanvas

    component banner
    component toolbar
    component spaceframe

    component _itemDialog
    component   i_nameLE
    component   i_descrLE
    component   i_xLE
    component   i_yLE
    component   i_imfileFE
    component   i_sheetFileFE

    component _exitDialog
    component   e_nameLE
    component   e_descrLE
    component   e_xLE
    component   e_yLE
    component   e_imfileFE
    component   e_otherSpaceLevelFE
    component   e_otherSpaceNameFE
    component   e_sheetFileFE

    proc isodd {n} {
      return [expr {($n % 1) == 1}]
    }
    proc getFromAttrList {key attrlist {default {}}} {
#

#      puts stderr "*** [list getFromAttrList $key $attrlist $default]"
      set index [lsearch $attrlist $key]
#      puts stderr "*** getFromAttrList: index = $index"
      if {$index < 0 || [isodd $index]} {
	return $default
      } else {
        incr index
	return [lindex $attrlist $index]
      }
    }
    proc insertOrReplaceInAttrList {key value attrlistVar} {
#      puts stderr "*** [list insertOrReplaceInAttrList $key $value $attrlistVar]"
      upvar $attrlistVar attrlist
      set index [lsearch $attrlist $key]
#      puts stderr "*** insertOrReplaceInAttrList: index = $index"
      if {$index < 0 || [isodd $index]} {
	lappend attrlist $key "$value"
      } else {
	incr index
	set attrlist [lreplace $attrlist $index $index "$value"]
      }
#      puts stderr "*** insertOrReplaceInAttrList: attrlist = $attrlist"
    }

    typevariable bannerImage
    typevariable bannerBackground #a2de86
    typevariable spaceTemplateXML {<?xml version="1.0" ?>
<rpgv3:Space xmlns:rpgv3="http://www.deepsoft.com/roleplayingdb/v3xmlns"
	     name="Space" >Space Information
  <rpgv3:Field name="Title" type="Word / Short Phrase" updatable="yes" />
  <rpgv3:Field name="X Coord" type="Whole Number" updatable="no" range="-1000 1000 1" id="X" />
  <rpgv3:Field name="Y Coord" type="Whole Number" updatable="no" range="-1000 1000 1" id="Y" />
  <rpgv3:Field name="Color" type="Color" updatable="yes" id="color" />
  <rpgv3:Field name="Description" type="Long Text" updatable="yes" />
  <rpgv3:SpaceContents name="Space Contents">Space Contents
    <rpgv3:Items name="Items" side="right">List of items
      <rpgv3:List id="itemlist" />
      <rpgv3:Buttonbox orient="horizontal" fill="x" expand="no" >
	<rpgv3:Bi label="Add New Item" id="addnewitem" />
	<rpgv3:Bi label="Edit Item"    id="edititem" />
	<rpgv3:Bi label="Delete Item"  id="deleteitem" />
      </rpgv3:Buttonbox>
    </rpgv3:Items>
    <rpgv3:Exits name="Exits" side="left" >List of exits
      <rpgv3:List id="exitlist" />
      <rpgv3:Buttonbox orient="horizontal" fill="x" expand="no" >
	<rpgv3:Bi label="Add New Exit" id="addnewexit" />
	<rpgv3:Bi label="Edit Exit"    id="editexit" />
	<rpgv3:Bi label="Delete Exit"  id="deleteexit" />
      </rpgv3:Buttonbox>
    </rpgv3:Exits>
  </rpgv3:SpaceContents >
  <rpgv3:SpaceMap name="Space Map">Space Map
    <rpgv3:Canvas name="Space Map Canvas" id="map" background="gray" side="left" />
    <rpgv3:Buttonbox orient="vertical" fill="both" side="right" expand="no" id="zoombuttons" >
      <rpgv3:Bi label="Zoom In"  id="zoomin" />
      <rpgv3:Bi label="Zoom &gt;" id="zoomto" />
      <rpgv3:Bi label="Zoom Out" id="zoomout" />
    </rpgv3:Buttonbox>
  </rpgv3:SpaceMap>
</rpgv3:Space>
    }
    typeconstructor {
      set bannerImage [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						 SpaceBanner.png]]
    }
    method new {} {
      $options(-leveleditor) _newspace -parent $win
    }
    typemethod new {args} {
      set mapbundlemountpoint [from args -mapbundlemountpoint]
      set parent [from args -parent .]
      set leveleditor [from args -leveleditor]
      set leveldir [from args -leveldir]
      set space [from args -newspace]
      set newTop [RolePlayingDB3::RPGToplevel \
		.space%AUTO% \
		-mainframeconstructor $type \
		-mainframetemplate $spaceTemplateXML \
		-class SpaceEditor \
		-mapbundlemountpoint $mapbundlemountpoint \
		-leveleditor $leveleditor -spacefile $space -minwidth 800]
      return $newTop
    }
    method open {} {
      $options(-leveleditor) _editspace -parent $win
    }
    typemethod open {args} {
      set mapbundlemountpoint [from args -mapbundlemountpoint]
      set parent [from args -parent .]
      set leveleditor [from args -leveleditor]
      set leveldir [from args -leveldir]
      set space [from args -spacefile]
      set newTop [RolePlayingDB3::RPGToplevel \
		.space%AUTO% \
		-mainframeconstructor $type \
		-mainframetemplate $spaceTemplateXML \
		-class SpaceEditor -spacefile $space \
		-mapbundlemountpoint $mapbundlemountpoint \
		-leveleditor $leveleditor -minwidth 800]
      return $newTop
    }
    method save {} {
      $options(-leveleditor) save
    }
    method saveas {{_filename {}}} {
      $options(-leveleditor) saveas $_filename
    }
    typemethod printSpace {pdfobj spacefile curpageV curlineV heading args} {
      upvar $curpageV curpage
      upvar $curlineV curline
      set mapbundlemountpoint [from args -mapbundlemountpoint]
      set leveldir	      [from args -leveldir]
      set parent              [from args -parent]
      set mapeditor	      [from args -mapeditor]
      if {[catch {open $spacefile r} shfp]} {
	error "Illformed map bundle: $spacefile cannot be opened: $shfp"
      }
      set XML [read $shfp]
      close $shfp
      set temppath [::RolePlayingDB3::XMLContentEditor $parent.tempspace%AUTO% \
			-xml $XML -isnewobject no -dirtyvariable {} \
			-filewidgethandler {}  -xmlfile {} \
			-basedirectory $mapbundlemountpoint]
      drawspace [$temppath getElementWidgetById map] \
		[$temppath getElementWidgetById itemlist] \
		[$temppath getElementWidgetById exitlist] \
		$mapeditor \
		[[$temppath getElementWidgetById color] cget -text] \
		1.0 $mapbundlemountpoint
      $temppath outputXMLToPDF $pdfobj $heading $curpage $curline
      set curpage [$temppath getpageno]
      set curline [$temppath getlineno]
      destroy $temppath
    }
    method print {} {
      set printfile "[file rootname [file tail $currentSpaceFile]].pdf"
      if {"$printfile" eq ".pdf"} {set printfile "Space.pdf"}
      set pdfobj [::RolePlayingDB3::PrintDialog drawPrintDialog \
     				-what Space -filename $printfile]
     
      if {"$pdfobj" eq ""} {return}
      set heading $currentSpaceFile
      set savedzoomfactor $zoomfactor
      set zoomfactor 1.0
      $self redrawspace
      $spaceframe outputXMLToPDF $pdfobj "$heading: space info"
      set zoomfactor $savedzoomfactor
      $self redrawspace
      $pdfobj destroy
    }
    method close {args} {
      set dontask [from args -dontask no]
      set dontsave [from args -dontsave no]
      if {[from args -closingallwindows no]} {
	$options(-leveleditor) close -closingallwindows yes
      }
      if {!$dontsave} {
	if {!$dontask} {
	  if {$isdirty} {
	    set ans [tk_messageBox -type yesnocancel -icon question \
				-parent $win \
				-message "Save data before closing window?"]
	    switch $ans {
	      yes {$self checksave}
	      cancel {return}
	      no {set dontsave yes}
	    }
	  }
	} else {
	  $self checksave
	}
      }
      $options(-leveleditor) removeeditor [winfo toplevel $win]
      destroy [winfo toplevel $win]
    }
    constructor {args} {
      $self configurelist $args

      set currentSpaceFile [file rootname [file tail $options(-spacefile)]]
      [winfo toplevel $win] configure -title "Space Edit: $currentSpaceFile"
      if {"$options(-template)" ne "" && ![file exists $options(-spacefile)]} {
	set XML $options(-template)
	set firstsave yes
	set isnew yes
	set xmlfile $options(-spacefile)
	set isdirty yes
      } elseif {[file exists $options(-spacefile)]} {
	set xmlfile {}
	if {[catch {open $options(-spacefile) r} shfp]} {
	  error "Illformed map bundle: $options(-spacefile) cannot be opened: $shfp"
	}
	set XML [read $shfp]
	close $shfp
	set firstsave no
	set isnew no
      } else {
	error "Neither -template nor a valid -spacefile was passed!"
      }
      install banner using Label $win.banner \
			-image $bannerImage -anchor w \
			-background $bannerBackground  
      pack $banner -fill x
      install toolbar using ButtonBox $win.toolbar -orient horizontal \
						   -homogeneous no
      pack $toolbar -fill x
      ## Tools ?? ##
      install spaceframe using ::RolePlayingDB3::XMLContentEditor \
			$win.spaceframe -xml $XML -isnewobject $isnew \
					-dirtyvariable [myvar isdirty] \
					-filewidgethandler [mymethod _filewidgethandler] \
					-xmlfile $xmlfile \
					-buttoncommand [mymethod _button] \
					-basedirectory $options(-mapbundlemountpoint)
      pack $spaceframe -fill both -expand yes
      set spacecanvas [$spaceframe getElementWidgetById map]
      set zoomtobutton [$spaceframe getElementWidgetById zoomto]
#      puts stderr "*** $type create $self: zoomtobutton = $zoomtobutton"
      set zoommenu [menu $zoomtobutton.zoommenu -tearoff no]
      foreach zf {.0625 .125 .25 .5  1   2   4   8   16} \
	      lab {1:16 1:8  1:4 1:2 1 2:1 4:1 8:1 16:1} {
	$zoommenu add radiobutton -command [mymethod redrawspace] \
				  -label "Zoom Factor $lab" \
				  -value $zf \
				  -variable [myvar zoomfactor]
      }
      set cw [$spaceframe getElementWidgetById color]
      $cw configure -modifycmd [mymethod colorchanged]
      $cw bind <Return> [mymethod colorchanged]
      $self redrawspace
      $options(-leveleditor) updatelevelmap
      $self createdialogs
      update
    }
    method colorchanged {} {
      $spacecanvas itemconfigure background -fill [$self getcolor]
      $options(-leveleditor) updatelevelmapcolor \
			[$self getcolor] \
			[file rootname [file tail $options(-spacefile)]]
      set isdirty yes
    } 
    method getcolor {} {
      return [[$spaceframe getElementWidgetById color] cget -text]
    }
    method _button {id} {
      switch $id {
	addnewitem {
	  $self _addnewitem
	}
	edititem {
	  $self _edititem
	}
	deleteitem {
	  $self _deleteitem
	}
	addnewexit {
	  $self _addnewexit
	}
	editexit {
	  $self _editexit
	}
	deleteexit {
	  $self _deleteexit
	}
	zoomin {
	  switch $zoomfactor {
	    .0625 {set zoomfactor .125}
	    .125  {set zoomfactor .25}
	    .25   {set zoomfactor .5}
	    .5    {set zoomfactor 1}
	    1	  {set zoomfactor 2}
	    2	  {set zoomfactor 4}
	    4	  {set zoomfactor 8}
	    8	  {set zoomfactor 16}
	  }
	  $self redrawspace
	}
	zoomto {
	  set RX [winfo pointerx $spaceframe]
	  set RY [winfo pointery $spaceframe]
	  $zoommenu post $RX $RY
	}
	zoomout {
	  switch $zoomfactor {
	    .125  {set zoomfactor .0625}
	    .25   {set zoomfactor .125}
	    .5    {set zoomfactor .25}
	    1	  {set zoomfactor .5}
	    2	  {set zoomfactor 1}
	    4	  {set zoomfactor 2}
	    8	  {set zoomfactor 4}
	    16	  {set zoomfactor 8}
	  }
	  $self redrawspace
	}
      }
    }
    proc drawspace {spacecanvas itemlist exitlist editor color zoomfactor mp} {
      $spacecanvas delete all
      $editor drawspace $spacecanvas 0 0 $color \
				[expr {$zoomfactor * 320}] \
				-tag background
      foreach i [$itemlist items] {
	set iData [$itemlist itemcget $i -data]
	set xc [expr {$zoomfactor * [getFromAttrList X $iData 0]}]
	set yc [expr {$zoomfactor * [getFromAttrList Y $iData 0]}]
	set imfile [file join $mp [getFromAttrList imfile $iData]]
	$spacecanvas create image $xc $yc -image [image create photo -file $imfile]
      }
      foreach e [$exitlist items] {
	set eData [$itemlist itemcget $e -data]
	set xc [expr {$zoomfactor * [getFromAttrList X $eData 0]}]
	set yc [expr {$zoomfactor * [getFromAttrList Y $eData 0]}]
	set imfile [file join $mp [getFromAttrList imfile $eData]]
	$spacecanvas create image $xc $yc -image [image create photo -file $imfile]
      }
      $spacecanvas configure -scrollregion [$spacecanvas bbox all]
    }
    method redrawspace {} {
      drawspace $spacecanvas [$spaceframe getElementWidgetById itemlist] \
			     [$spaceframe getElementWidgetById exitlist] \
			     $options(-leveleditor) \
			     [$self getcolor] \
			     $zoomfactor \
			     $options(-mapbundlemountpoint)
    }
    method recreateXML {} {
      set needmediatreeupdated no
      $spaceframe recreateXML $options(-spacefile)
      if {$needmediatreeupdated} {$options(-leveleditor) updatemediatree}
    }
    method _filewidgethandler {curfile} {
      if {"$curfile" eq ""} {return $curfile}
      if {[file pathtype "$curfile"] eq "relative" &&
	  "media" eq [lindex [file split $curfile] 0]} {
	return "$curfile"
      } else {
	file copy -force "$curfile" [file join $options(-mapbundlemountpoint) media [file tail $curfile]]
	set needmediatreeupdated yes
	return "[file join media [file tail $curfile]]"
      }
    }
    method createdialogs {} {
      set baseW $win
      install _itemDialog using Dialog $baseW.itemDialog \
      			-image [::RolePlayingDB3::MapEditor getDialogIcon] \
			-cancel 1 -default 0 -modal local \
			-parent [winfo toplevel $win] -side bottom \
			-title "Add New Item" -transient yes
      $_itemDialog add -name new    -text {Create}
      $_itemDialog add -name cancel -text {Cancel}
      set frame [$_itemDialog getframe]
      install i_nameLE using LabelEntry $frame.nameLE -label "Name:" \
						    -labelwidth 10
      pack $i_nameLE -fill x
      install i_descrLE using LabelEntry $frame.descrLE -label "Description:" \
						      -labelwidth 10
      pack $i_descrLE -fill x
      install i_xLE using LabelSpinBox $frame.xLE -label "X:" -labelwidth 10 \
						-range {-320 320 1}
      pack $i_xLE -fill x
      install i_yLE using LabelSpinBox $frame.yLE -label "Y:" -labelwidth 10 \
						-range {-320 320 1}
      pack $i_yLE -fill x
      install i_imfileFE using FileEntry $frame.imfileFE -label "Image:" \
						       -labelwidth 10 \
		       -filetypes  { 
				{"BMP Files"	{.bmp}	      }
				{"GIF Files"	{.gif .GIF}     GIFf}
				{"JPEG Files"       {.jpeg .jpg}    JPEG}
				{"PNG Files"	{.png}	  PNGF}
				{"TIFF Files"       {.tiff .tif}    TIFF}
				{"XBM Files"	{.xbm}	      }
				{"XPM Files"	{.xpm}	      }
				{"Postscript Files" {.ps .eps}	  } 
				{"All Files"	*		   } }
      pack $i_imfileFE -fill x
      install i_sheetFileFE using FileEntry $frame.sheetFileFE -label "Sheet File:" \
							  -labelwidth 10 \
			-filetypes  {
				{"Sheet Files"      {*.rpg}	     }
				{"All Files"	*		   } }
      pack $i_sheetFileFE -fill x
      install _exitDialog using Dialog $baseW.exitDialog \
      			-image [::RolePlayingDB3::MapEditor getDialogIcon] \
			-cancel 1 -default 0 -modal local \
			-parent [winfo toplevel $win] -side bottom \
			-title "Add New Exit" -transient yes
      $_exitDialog add -name new    -text {Create}
      $_exitDialog add -name cancel -text {Cancel}
      set frame [$_exitDialog getframe]
      install e_nameLE using LabelEntry $frame.nameLE -label "Name:" \
						    -labelwidth 14
      pack $e_nameLE -fill x
      install e_descrLE using LabelEntry $frame.descrLE -label "Description:" \
						      -labelwidth 14
      pack $e_descrLE -fill x
      install e_xLE using LabelSpinBox $frame.xLE -label "X:" -labelwidth 14 \
						-range {-320 320 1}
      pack $e_xLE -fill x
      install e_yLE using LabelSpinBox $frame.yLE -label "Y:" -labelwidth 14 \
						-range {-320 320 1}
      pack $e_yLE -fill x
      install e_imfileFE using FileEntry $frame.imfileFE -label "Image:" \
						       -labelwidth 14 \
		       -filetypes  { 
				{"BMP Files"	{.bmp}	      }
				{"GIF Files"	{.gif .GIF}     GIFf}
				{"JPEG Files"       {.jpeg .jpg}    JPEG}
				{"PNG Files"	{.png}	  PNGF}
				{"TIFF Files"       {.tiff .tif}    TIFF}
				{"XBM Files"	{.xbm}	      }
				{"XPM Files"	{.xpm}	      }
				{"Postscript Files" {.ps .eps}	  } 
				{"All Files"	*		   } }
      pack $e_imfileFE -fill x
      install e_otherSpaceLevelFE using FileEntry $frame.otherSpaceLevelFE \
			-filedialog directory \
			-text [file dirname $options(-spacefile)] \
			-label "Exit to level:" -labelwidth 14
      pack $e_otherSpaceLevelFE -fill x
      install e_otherSpaceNameFE  using FileEntry $frame.otherSpaceNameFE \
			-text $options(-spacefile) \
			-filetypes  {
				{"Space Files"     {*.xml}	     } } \
			-label "Exit to space:" -labelwidth 14
      pack $e_otherSpaceNameFE -fill x
      install e_sheetFileFE using FileEntry $frame.sheetFileFE -label "Sheet File:" \
							  -labelwidth 14 \
			-filetypes  {
				{"Sheet Files"      {*.rpg}	     }
				{"All Files"	*		   } }
      pack $e_sheetFileFE -fill x
    }
    method _addnewitem {} {
      set itemlist [$spaceframe getElementWidgetById itemlist]
      $_itemDialog itemconfigure 0 -text {Create}
      set ans [$_itemDialog draw]
      if {$ans == 1} {return}
      set attrList [list]
      insertOrReplaceInAttrList name "[$i_nameLE cget -text]" attrList
      set descr "[$i_descrLE cget -text]"
      insertOrReplaceInAttrList X "[$i_xLE cget -text]" attrList
      insertOrReplaceInAttrList Y "[$i_yLE cget -text]" attrList
      set needmediatreeupdated no
      insertOrReplaceInAttrList imfile [$self _filewidgethandler "[$i_imfileFE cget -text]"] attrList
      insertOrReplaceInAttrList sheet [$self _filewidgethandler "[$i_sheetFileFE cget -text]"] attrList
      puts stderr "*** $self _addnewitem: attrList = $attrList"
      $itemlist insert end #auto -text "$descr" -data "$attrList"
      $self redrawspace
      set isdirty yes
      if {$needmediatreeupdated} {$options(-leveleditor) updatemediatree}
    }
    method _edititem {} {
      set itemlist [$spaceframe getElementWidgetById itemlist]
      set selection [$itemlist selection get]
      if {[llength $selection] < 1} {return}
      set index [lindex $selection 0]
      set attrList [$itemlist itemcget $index -data]
      set descr    [$itemlist itemcget $index -text]
      $_itemDialog itemconfigure 0 -text {Update}
      $i_nameLE configure -text "[getFromAttrList name $attrList]"
      $i_descrLE configure -text "$descr"
      $i_xLE configure -text [getFromAttrList X $attrList 0]
      $i_yLE configure -text [getFromAttrList Y $attrList 0]
      $i_imfileFE configure -text [getFromAttrList imfile $attrList]
      $i_sheetFileFE configure -text [getFromAttrList sheet $attrList]
      set ans [$_itemDialog draw]
      if {$ans == 1} {return}
      insertOrReplaceInAttrList name "[$i_nameLE cget -text]" attrList
      set descr "[$i_descrLE cget -text]"
      insertOrReplaceInAttrList X "[$i_xLE cget -text]" attrList
      insertOrReplaceInAttrList Y "[$i_yLE cget -text]" attrList
      set needmediatreeupdated no
      insertOrReplaceInAttrList imfile [$self _filewidgethandler "[$i_imfileFE cget -text]"] attrList
      insertOrReplaceInAttrList sheet [$self _filewidgethandler "[$i_sheetFileFE cget -text]"] attrList
      $itemlist itemconfigure $index -data $attrList
      $itemlist itemconfigure $index -text $descr
      $self redrawspace
      set isdirty yes
      if {$needmediatreeupdated} {$options(-leveleditor) updatemediatree}
    }
    method _deleteitem {} {
      set itemlist [$spaceframe getElementWidgetById itemlist]
      set selection [$itemlist selection get]
      if {[llength $selection] < 1} {return}
      set index [lindex $selection 0]
      set attrList [$itemlist itemcget $index -data]
      set descr    [$itemlist itemcget $index -text]
      if {[tk_messageBox -type yesno -default no -icon question -parent $win \
			 -message "Really delete $descr?"]} {
	$itemlist delete $index
	set isdirty yes
	$self redrawspace
      }
    }
    method _addnewexit {} {
      set exitlist [$spaceframe getElementWidgetById exitlist]
      $_exitDialog itemconfigure 0 -text {Create}
      set ans [$_exitDialog draw]
      if {$ans == 1} {return}
      set attrList [list]
      insertOrReplaceInAttrList name "[$e_nameLE cget -text]" attrList
      set descr "[$e_descrLE cget -text]"
      insertOrReplaceInAttrList X "[$e_xLE cget -text]" attrList
      insertOrReplaceInAttrList Y "[$e_yLE cget -text]" attrList
      set needmediatreeupdated no
      insertOrReplaceInAttrList imfile [$self _filewidgethandler "[$e_imfileFE cget -text]"] attrList
      insertOrReplaceInAttrList sheet [$self _filewidgethandler "[$e_sheetFileFE cget -text]"] attrList
      set spacelevel [$self _checkIsLevel "[$e_otherSpaceLevelFE cget -text]"]
      if {"$spacelevel" eq ""} {return}
      insertOrReplaceInAttrList otherlevel "$spacelevel" attrList
      set spacename [$self _checkIsSpaceOnLevel "[$e_otherSpaceSpaceFE cget -text]" "$spacelevel"]
      if {"$spacename" eq ""} {return}
      insertOrReplaceInAttrList otherspace "$spacename" attrList
      puts stderr "*** $self _addnewexit: attrList = $attrList"
      $exitlist insert end #auto -text "$descr" -data "$attrList"
      $self redrawspace
      set isdirty yes
      if {$needmediatreeupdated} {$options(-leveleditor) updatemediatree}
    }
    method _checkIsLevel {leveldir} {
      if {[catch {$options(-leveleditor) validateleveldir -leveldir "$leveldir"}
		 ]} {
	tk_messageBox  -parent $win -type ok -icon error \
			-message "[file tail $leveldir] is not a valid level!"
	return {}
      } else {
	return "$leveldir"
      }
    }
    method _checkIsSpaceOnLevel {spacefile leveldir} {
      if {[file dirname "$spacefile"] eq "$leveldir"} {
	return "$spacefile"
      } else {
        tk_messageBox -type ok -icon error -parent $win \
			-message "[file rootname [file tail $spacefile]] is not on [file tail $leveldir]!"

	return {}
      }
    }
    method _editexit {} {
      set exitlist [$spaceframe getElementWidgetById exitlist]
      set selection [$exitlistt selection get]
      if {[llength $selection] < 1} {return}
      set index [lindex $selection 0]
      set attrList [$exitlist itemcget $index -data]
      set descr    [$exitlist itemcget $index -text]
      $_exitDialog itemconfigure 0 -text {Update}
      $e_nameLE configure -text "[getFromAttrList name $attrList]"
      $e_descrLE configure -text "$descr"
      $e_xLE configure -text "[getFromAttrList X $attrList]"
      $e_yLE configure -text "[getFromAttrList Y $attrList]"
      $e_imfileFE configure -text "[getFromAttrList imfile $attrList]"
      $e_sheetFileFE configure -text "[getFromAttrList sheet $attrList]"
      $e_otherSpaceLevelFE configure -text "[getFromAttrList otherlevel  $attrList]"
      $e_otherSpaceSpaceFE configure -text "[getFromAttrList otherspace $attrList]"
      set ans [$_exitDialog draw]
      if {$ans == 1} {return}
      set attrList [list]
      insertOrReplaceInAttrList name "[$e_nameLE cget -text]" attrList
      set descr "[$e_descrLE cget -text]"
      insertOrReplaceInAttrList X "[$e_xLE cget -text]" attrList
      insertOrReplaceInAttrList Y "[$e_yLE cget -text]" attrList
      set needmediatreeupdated no
      insertOrReplaceInAttrList imfile [$self _filewidgethandler "[$e_imfileFE cget -text]"] attrList
      insertOrReplaceInAttrList sheet [$self _filewidgethandler "[$e_sheetFileFE cget -text]"] attrList
      set spacelevel [$self _checkIsLevel "[$e_otherSpaceLevelFE cget -text]"]
      if {"$spacelevel" eq ""} {return}
      insertOrReplaceInAttrList otherlevel "$spacelevel" attrList
      set spacename [$self _checkIsSpaceOnLevel "[$e_otherSpaceSpaceFE cget -text]" "$spacelevel"]
      if {"$spacename" eq ""} {return}
      insertOrReplaceInAttrList otherspace "$spacename" attrList
      puts stderr "*** $self _editexit: attrList = $attrList"
      $exitlist insert end #auto -text "$descr" -data "$attrList"
      $self redrawspace
      set isdirty yes
      if {$needmediatreeupdated} {$options(-leveleditor) updatemediatree}
    }
    method _deleteexit {} {
      set exitlist [$spaceframe getElementWidgetById exitlist]
      set selection [$exitlistt selection get]
      if {[llength $selection] < 1} {return}
      set index [lindex $selection 0]
      set attrList [$exitlist itemcget $index -data]
      set descr    [$exitlist itemcget $index -text]
      if {[tk_messageBox -type yesno -default no -icon question -parent $win \
      			 -message "Really delete $descr?"]} {
	$exitlist delete $index
	set isdirty yes
	$self redrawspace
      }
    }
  }
}

package provide RPGMapLevelSpace 1.0
