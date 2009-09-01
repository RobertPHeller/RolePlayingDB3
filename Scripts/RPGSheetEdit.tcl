#* 
#* ------------------------------------------------------------------
#* Role PlayingDB V3.0 by Deepwoods Software
#* ------------------------------------------------------------------
#* RPGSheetEdit.tcl - Generic Sheet Editor
#* Created by Robert Heller on Mon Aug 31 18:13:04 2009
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
package require xml
package require BWLabelComboBox
package require BWLabelSpinBox
package require BWFileEntry

namespace eval RolePlayingDB3 {
  snit::widgetadaptor LabeledScrolledText {
    component scroll
    component text
    delegate option * to text except {-xscrollcommand -yscrollcommand}
    delegate method * to text except {cget configure xview yview}
    delegate option -label to hull as -text
#    delegate option -labelfont to hull as -font
#    delegate option -labelside to hull as -side
#    delegate option -labelbaseline to hull as -baseline
    option  -auto -readonly yes -default both \
		  -type {snit::enum -values {none both vertical horizontal}}
    option  -scrollbar -readonly yes -default both \
		  -type {snit::enum -values {none both vertical horizontal}}
    option -text -cgetmethod gettext -configuremethod puttext
    method gettext {option} {
      return "[$text get 1.0 end-1c]"
    }
    method puttext {option value} {
      $text get delete 1.0 end
      $text insert end "$value"
    }
    method bind {args} {
      eval [list ::bind $text] $args
    }
    constructor {args} {
      set auto [from args -auto]
      set scrollbar [from args -scrollbar]
      installhull using TitleFrame
      install scroll using ScrolledWindow [$hull getframe].scroll \
					-auto $auto -scrollbar $scrollbar
      pack $scroll -fill both -expand yes
      install text using text [$scroll getframe].text
      pack $text -fill both -expand yes
      $scroll setwidget $text
      $self configurelist  $args
    }
  }
  snit::widget SheetEdit {
    option -template -readonly yes -default {}
    option {-sheetclass sheetClass SheetClass} \
		-readonly yes -default Character \
		-type { snit::enum -values {Character Dressing Monster Spell 
					    Treasure TrickTrap} }
    option {-openfilename openFilename OpenFilename} \
		-readonly yes -default {} -validatemethod validateinputfile
    method validateinputfile {option value} {
      if {"$value" eq ""} {return "$value"}
      if {[file exists $value] && 
	  [file readable $value] && 
	  [file isfile $value]} {
	return $value
      } else {
	error "Expected a valid file for $option, but got $value"
      }
    }
    typevariable defaultfilename "sheet.rpg"
    variable currentFilename
    variable currentBaseFilename
    variable isdirty no
    method setdirty {} {set isdirty yes}
    variable path
    variable tempfile
    variable sheetClass
    variable nodeStack
    variable nodeTree -array {}
    variable parseMode
    variable fileWidgets -array {}
    
    component banner
    component toolbar
    component sheetsw
    component   sheetframe

    typevariable filetypes {
      {{Sheet Files} {.rpg}		}
      {{All Files}        *             }
    }
    typemethod myfiletypes {} {return $filetypes}
    typecomponent _editDialog
    typecomponent  sheetTemplateFE
    typevariable bannerImage -array {}
    typevariable dialogIcon  -array {}
    typevariable bannerBackgrounds -array {
	Character #e52b2a
	Dressing  #91938e
	Monster   #85c152
	Spell     #a2dd84
	Treasure  #e1eb8f
	TrickTrap #95d9d7
    }
    typevariable editDialogIcon {}
    typeconstructor {
      foreach theclass {Character Dressing Monster Spell Treasure TrickTrap} {
	set bannerImage($theclass) [image create photo \
					-file [file join \
						$::RolePlayingDB3::ImageDir \
						${theclass}Banner.png]]
	set dialogIcon($theclass) [image create photo \
					-file [file join \
						$::RolePlayingDB3::ImageDir \
						${theclass}DialogIcon.png]]
	set editDialogIcon [image create photo \
					-file [file join \
						$::RolePlayingDB3::ImageDir \
						SheetEditDialogIcon.png]]
      }
      set _editDialog {}
    }
    typemethod _createEditDialog {} {
      if {"$_editDialog" ne "" && [winfo exists "$_editDialog"]} {return}
      set _editDialog [Dialog .editSheetEditDialog -image $editDialogIcon \
					-cancel 2 -default 0 -modal local \
					-parent . -side bottom \
					-title "Edit Sheet" \
					-transient yes]
      $_editDialog add -name new    -text {Create}
      $_editDialog add -name open   -text {Open}  
      $_editDialog add -name cancel -text {Cancel}
      pack [message [$_editDialog getframe].message \
			-text "Create a new Sheet file or\nopen an existing Sheet file?" \
			-aspect 500] -fill both
      set sheetTemplateFE [FileEntry \
				[$_editDialog getframe].sheetTemplateFE \
				-filedialog open \
				-filetypes { {{Template XML}   {.xml} TEXT}
					     {{All Text Files}     *  TEXT} } \
				-defaultextension .xml \
				-title {Template XML file}]
      pack $sheetTemplateFE -fill x
    }
    typemethod edit {args} {
      set sheetclass [from args -sheetclass]
      $type _createEditDialog
      set templateFile [::RolePlayingDB3::Configuration getoption Template]
      set mp [file rootname [file tail $templateFile]]
      vfs::zip::Mount $templateFile $mp
      set t1 [lindex [lsort -dictionary [glob -nocomplain \
						[file join $mp $sheetclass \
							*.xml]]] 0]
      $sheetTemplateFE configure -text "$t1"
      set answer [$_editDialog draw]
      switch $answer {
	0 {
	    set templateFile [$sheetTemplateFE cget -text]
	    if {[catch {open $templateFile r} tfp]} {
	      tk_messageBox -type ok -icon error -message "Could not open $templateFile: $tfp"
	      vfs::unmount $mp
	      return
	    }
	    set templateXML [read $tfp]
	    close $tfp
	    vfs::unmount $mp
	    $type new -template $templateXML -sheetclass $sheetclass
	}
	1 {
	    vfs::unmount $mp
	    $type open -sheetclass $sheetclass
	}
	2 {return}
      }
    }
    method getfile {} {return "$currentFilename"}
    typemethod new {args} {
      set templateXML [from args -template]
      set sheetclass [from args -sheetclass]
      set parent [from args -parent .]

      if {"$templateXML" eq ""} {
        set templateFile [::RolePlayingDB3::Configuration getoption Template]
        set mp [file rootname [file tail $templateFile]]
        vfs::zip::Mount $templateFile $mp
	set template [tk_getOpenFile -defaultextension .xml \
				     -filetypes { {{Template XML}   {.xml} TEXT}
						  {{All Text Files}     *  TEXT} } \
				     -initialdir [file join $mp $sheetclass] \
				     -parent $parent \
				     -title "Template for new $sheetclass"]
	if {"$template" eq ""} {return}
	if {[catch {open $template r} tfp]} {
	  tk_messageBox -type ok -icon error -message "Could not open $template: $tfp"
	  vfs::unmount $mp
	  return
	}
	set templateXML [read $tfp]
	close $tfp
	vfs::unmount $mp
      }
      set newTop [RolePlayingDB3::RPGToplevel \
			.[string tolower $sheetclass]%AUTO% \
			-mainframeconstructor $type \
			-mainframetemplate $templateXML \
			-sheetclass $sheetclass]
    }
    method opennew {} {
      set currentFilename {}
      set currentBaseFilename *noname*
      set path [$type genname $options(-sheetclass)]
      set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      while {[file exists $tempfile]} {
	set path [$type genname $options(-sheetclass)]
	set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      }
      vfs::mk4::Mount $tempfile $path
      file mkdir [file join $path media]
      close [open [file join $path media flag] w]
      file mkdir [file join $path xml]
      close [open [file join $path xml flag] w]
      [winfo toplevel $win] configure -title "$options(-sheetclass) Edit: $currentBaseFilename"
    }
    method new {} {
      $type new -parent $win -sheetclass $options(-sheetclass) \
		-template $options(-template)
    }
    typevariable genindex 0
    typemethod genname {class} {
      incr genindex
      return [format {%s%05d} [string toupper $class] $genindex]
    }
    method open {} {
      $type open -parent $win -sheetclass $options(-sheetclass)\
		 -like $currentFilename
    }
    typemethod open {args} {
      set sheetclass [from args -sheetclass]
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
			.[string tolower $sheetclass]%AUTO% \
			-mainframeconstructor $type \
			-mainframetemplate {} \
			-sheetclass $sheetclass \
			-openfilename $currentFilename]
    }
    method openold {_filename} {
      set path [$type genname $options(-sheetclass)]
      set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      while {[file exists $tempfile]} {
	set path [$type genname $options(-sheetclass)]
	set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      }
      vfs::mk4::Mount $tempfile $path
      set currentFilename $_filename 
      set currentBaseFilename [file tail $currentFilename]
      set inpath [$type genname $options(-sheetclass)]
      vfs::zip::Mount $currentFilename $inpath
      if {[catch {file copy [file join $inpath media] $path}]} {
	file mkdir [file join $path media]
	close [open [file join $path media flag] w]
      }
      if {[catch {file copy [file join $inpath xml] $path}]} {
	file mkdir [file join $path xml]
      }
      vfs::unmount $inpath
      [winfo toplevel $win] configure -title "$options(-sheetclass) $currentBaseFilename"
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
      if {$isdirty} {$self recreateXML}
      ::ZipArchive createZipFromDirtree $_filename $path
      set isdirty no
      if {"$currentFilename" ne "$_filename"} {
	set currentFilename $_filename
	set currentBaseFilename [file tail $currentFilename]
	[winfo toplevel $win] configure -title "$options(-sheetclass) Edit: $currentBaseFilename]"
      }
    }
    method print {} {
    }
    method close {} {
      if {$isdirty} {
	set ans [tk_messageBox -type yesnocancel -icon question \
				-message "Save data before closing window?"]
	switch $ans {
	  yes {$self save}
	  cancel {return}
	  no {}
        }
      }
      vfs::unmount $path
      file delete $tempfile
      destroy [winfo toplevel $win]
    }
    constructor {args} {
      set options(-template) [from args -template]
      set options(-sheetclass) [from args -sheetclass]
      set options(-openfilename) [from args -openfilename]
      if {"$options(-openfilename)" eq "" && "$options(-template)" ne ""} {
	$self opennew
	set parseMode {template}
	set XML $options(-template)
      } elseif {"$options(-openfilename)" ne ""} {
	$self openold $options(-openfilename)
	set parseMode {file}
	if {[catch {open [file join $path xml sheet.xml] r} shfp]} {
	  error "Illformed sheet bundle: sheet.xml cannot be opened: $shfp"
	}
	set XML [read $shfp]
	close $shfp
      } else {
	error "Neither -template nor -openfilename was passed!"
      }
      install banner using Label $win.banner \
			-image $bannerImage($options(-sheetclass)) -anchor w \
			-background $bannerBackgrounds($options(-sheetclass))
      pack $banner -fill x
      install toolbar using ButtonBox $win.toolbar -orient horizontal \
						-homogeneous no
      pack $toolbar -fill x 
      ## Tools??? ##
      install sheetsw using ScrolledWindow $win.sheetsw \
				-scrollbar vertical -auto vertical
      pack $sheetsw -fill both -expand yes
      install sheetframe using ScrollableFrame [$sheetsw getframe].sheetframe \
					-constrainedwidth yes
      pack $sheetframe -fill both -expand yes
      $sheetsw setwidget $sheetframe
      #### Process openfile (open) or template (new)
      #### if open, re-generate template in archive
      set p [xml::parser -elementstartcommand [mymethod _elementstart] \
			 -elementendcommand   [mymethod _elementend] \
			 -characterdatacommand [mymethod _characterdata]]
      set nodeStack [list $sheetframe]
      array unset nodeTree
      if {"$parseMode" eq "file"} {
	set options(-template) {<?xml version="1.0" ?>}
	append options(-template) "\n"
      }
      $p parse $XML
      $p free
      if {"$parseMode" ne "file"} {$self recreateXML}
      $self configurelist $args
#      puts stderr "*** $type $self: parseMode = $parseMode"
#      if {"$parseMode" eq "file"} {
#	puts stderr "*** $type $self: options(-template) is:\n$options(-template)"
#      }
    }
    proc makeattrlist {attrlist} {
      set result {}
      foreach {n v} $attrlist {
	append result "$n=\"$v\" "
      }
      return $result
    }
    variable needNamelist yes
    method _elementstart {name attrlist args} {
      set nodename $name
      set widget none
      set bindscript {}
      set label  {}
      set widgetopts [list]
      if {"$name" eq "Field"} {
	array unset attrlist_array
	array set attrlist_array $attrlist
	if {[catch {set attrlist_array(name)}]} {
	  set attrlist_array(name) "Unamed Field"
	}
	if {[catch {set attrlist_array(type)}]} {
	  set attrlist_array(type) {Word / Short Phrase}
	}
	if {[catch {set attrlist_array(generator)}]} {
	  set attrlist_array(generator) {}
	}
	if {[catch {set attrlist_array(updatable)}]} {
	  set attrlist_array(updatable) yes
	}
	set nodename "Field:$attrlist_array(name)"
	set label $attrlist_array(name)
	lappend widgetopts -label "$label:"
	switch -exact -- "$attrlist_array(type)" {
	  {Whole Number} {
	    if {!$attrlist_array(updatable) && "$parseMode" eq "file"} {
	      set widget LabelEntry
	      lappend widgetopts -editable no
	    } else {
	      set widget LabelSpinBox
	      lappend widgetopts -modifycmd [mymethod setdirty]
	      set bindscript [list bind <KeyPress> [mymethod setdirty]]
	      if {[regexp {^([[:digit:]]*)[dD]([[:digit:]]*)$} \
			"$attrlist_array(generator)" -> num sides] > 0} {
		lappend widgetopts -range [list $num [expr {$sides * $num}] 1]
	      } elseif {[regexp {^[dD]%$} "$attrlist_array(generator)"] > 0} {
		lappend widgetopts -range {1 100 1}
	      }
	    }
       	  }
	  {Word / Short Phrase} {
	    set widget LabelEntry
	    if {!$attrlist_array(updatable) && "$parseMode" eq "file"} {
	      lappend widgetopts -editable no
	    } else {
	      set bindscript [list bind <KeyPress> [mymethod setdirty]]
	    }

	  }
	  {Long Text} {
	    set widget ::RolePlayingDB3::LabeledScrolledText
	    lappend widgetopts -auto vertical -scrollbar vertical \
			       -wrap word -width 70 -height 10 -relief sunken
	    set bindscript [list bind <KeyPress> [mymethod setdirty]]
	  }
	  Graphic {
	    set widget FileEntry
	    lappend widgetopts -modifycmd [mymethod setdirty]
	    set bindscript [list bind <KeyPress> [mymethod setdirty]]
	    lappend widgetopts -filetypes { 
				{"BMP Files"        {.bmp}              }
				{"GIF Files"        {.gif .GIF}     GIFf}
				{"JPEG Files"       {.jpeg .jpg}    JPEG}
				{"PNG Files"        {.png}          PNGF}
				{"TIFF Files"       {.tiff .tif}    TIFF}
				{"XBM Files"        {.xbm}              }
				{"XPM Files"        {.xpm}              }
				{"Postscript Files" {.ps .eps}          } 
				{"All Files"        *                   } }
	  }
	  Document {
	    set widget FileEntry
	    set bindscript [list bind <KeyPress> [mymethod setdirty]]
	    lappend widgetopts -modifycmd [mymethod setdirty]
	    lappend widgetopts -filetypes { 
				{"All Files"        *                   } }
	  }
	}
      } else {
	set widget TitleFrame
	lappend widgetopts -text $nodename
      }
      regsub -all {[[:space:]]} $nodename {} wname
      set curroot [[lindex $nodeStack end] getframe]
      set wname $curroot.[string tolower $wname]
      set bwname $wname
      set i 0
      while {[winfo exists $wname]} {
	incr i
	set wname $bwname$i
      }
      eval [list $widget $wname] $widgetopts
      if {"$widget" eq "FileEntry"} {set fileWidgets($wname) true}
      if {"$name" ne "Field"} {
	pack $wname -fill both -expand yes
      } else {
	pack $wname -fill x
      }
      if {"$bindscript" ne ""} {eval $wname $bindscript}
      lappend nodeStack $wname
      set nodeTree($wname) [list $name $attrlist $args]
      if {"$parseMode" eq "file"} {
	set indent [string repeat {  } [llength $nodeStack]]
	append options(-template) "$indent<rpgv3:$name "
	if {$needNamelist} {
	  append options(-template) "xmlns:rpgv3=\""
	  set namespace [lindex $args [expr {[lsearch -exact $args -namespace] + 1}]]
	  append options(-template) "$namespace"
	  append options(-template) "\" "
	  set needNamelist no
	}	    
	append options(-template) "[makeattrlist $attrlist]"
        if {"$name" ne "Field"} {
	  append options(-template) ">\n"
	} else {
	  append options(-template) "/>\n"
	}
      }
    }
    method _elementend {name args} {
      if {"$parseMode" eq "file"} {
	if {"$name" != "Field"} {
	  set indent [string repeat {  } [llength $nodeStack]]
	  append options(-template) "</$name >\n"
	}
      }
      set nodeStack [lrange $nodeStack 0 [expr {[llength $nodeStack] - 2}]]
    }
    method _characterdata {data} {
#      puts stderr "*** $self _characterdata $data"
      set curnode [lindex $nodeStack end]
      set data [string trim "$data"]
#      puts stderr "*** $self _characterdata: curnode = $curnode"
#      puts stderr "*** $self _characterdata: data = '$data'"
      if {"$data" ne ""} {$curnode configure -text "$data"}
      if {"$parseMode" eq "file"} {
	if {[winfo class $curnode] eq "TitleFrame"} {
	  append options(-template) "$data\n"
	}
      }      
    }
    method recreateXML {} {
      if {[catch {open [file join $path xml sheet.xml] w} shfp]} {
	tk_messageBox -type ok -icon error -message "Internal error: cannot create sheet.xml: $shfp"
	return
      }
      puts $shfp {<?xml version="1.0" ?>}
      $self _recreateXML_processNodesAt $sheetframe $shfp
      close $shfp
    }
    method childrenofgetframe {w} {
      if {[catch {winfo children [$w getframe]} children]} {
	return {}
      } else {
	set result [list]
	foreach c $children {
	  if {[catch {set nodeTree($c)}]} {continue}
	  lappend result $c
	}
      }
      return $result
    }
    method _recreateXML_processNodesAt {node fp {needxmlns yes} {indent {}}} {
      foreach n [$self childrenofgetframe $node] {
	if {[catch {set nodeTree($n)} data]} {continue}
	foreach {name attrlist args} $data {break}
	puts -nonewline $fp "$indent<rpgv3:$name "
	if {$needxmlns} {
	  puts -nonewline $fp "xmlns:rpgv3=\""
	  set namespace [lindex $args [expr {[lsearch -exact $args -namespace] + 1}]]
	  puts -nonewline $fp "$namespace"
	  puts -nonewline $fp "\" "
	  set needxmlns no
	}
	puts -nonewline $fp [makeattrlist $attrlist]
	if {"$name" ne "Field" &&
	    ([llength [$self childrenofgetframe $n]] > 0 ||
	     [llength $attrlist] == 0 ||
	     ([llength $attrlist] == 2 && [lindex $attrlist 0] eq "name"))} {
	  puts -nonewline $fp ">"
	  puts $fp "[$n cget -text]"
	  $self _recreateXML_processNodesAt $n $fp $needxmlns "$indent  "
	  puts $fp "$indent</rpgv3:$name>"
	} else {
	  puts -nonewline $fp ">"
	  if {[catch {set fileWidgets($n)} fileflag]} {
	    puts -nonewline $fp "[$n cget -text]"
	  } else {
	    set curfile "[$n cget -text]"
	    if {"$curfile" ne ""} {
	      if {[file pathtype "$curfile"] eq "relative" &&
		  "media" eq [lindex [file split $curfile] 0]} {
		puts -nonewline $fp "$curfile"
	      } else {
		file copy "$curfile" [file join $path media [file tail $curfile]]
		puts -nonewline $fp "[file join media [file tail $curfile]]"
	      }
	    }
	  }
	  puts $fp "</rpgv3:$name>"
	}
      }
    }
  }
}



package provide RPGSheetEdit 1.0
