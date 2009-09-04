#* 
#* ------------------------------------------------------------------
#* Role PlayingDB V3.0 by Deepwoods Software
#* ------------------------------------------------------------------
#* RPGTemplate.tcl - Template Editor code
#* Created by Robert Heller on Sun Aug 30 18:14:10 2009
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
package require RPGUtilities

namespace eval RolePlayingDB3 {
  snit::widget Template {
    option -template -readonly yes -default {}	  ;# Not used
    typevariable defaultfilename "template.rpgtmpl"
    variable currentFilename
    variable currentBaseFilename
    variable isdirty no
    variable path
    variable tempfile
    variable currentTemplateName
    component banner
    component toolbar
    component panes
    component  sbpane
    component   sidebartree
    component  tmppane
    component   templatename
    component   templatesw
    component     templatetree

    component _addNewTemplateDialog
    component   newTemplateName
    component   newTemplateClass

    component _addNewFieldDialog
    component   newFieldName
    component   newFieldType
    component   newFieldGenerator
    component   newFieldUpdatable

    component _editFieldDialog
    component   theFieldName
    component   theFieldType
    component   theFieldGenerator
    component   theFieldUpdatable

    component _editContainerTextDialog
    component   containerText

    typevariable filetypes {
      {{Tempate Files} {.rpgtmpl}       }
      {{All Files}        *             }
    }
    typemethod myfiletypes {} {return $filetypes}
    typecomponent _editDialog
    typevariable bannerImage
    typevariable bannerBackground #eadc9b
    typevariable templateMonster
    typeconstructor {
      set bannerImage [image create photo \
				-file [file join $::RolePlayingDB3::ImageDir \
						 TemplateBanner.png]]
      set templateMonster [image create photo \
					-file [file join $::RolePlayingDB3::ImageDir \
						SmallTemplateMonster.png]]
      set _editDialog {}
    }
    typemethod _createEditDialog {} {
      if {"$_editDialog" ne "" && [winfo exists "$_editDialog"]} {return}
      set _editDialog [Dialog .editTemplateDialog -image $templateMonster \
					-cancel 2 -default 0 -modal local \
					-parent . -side bottom \
					-title "Edit Template" \
					-transient yes]
      $_editDialog add -name new    -text {Create}
      $_editDialog add -name open   -text {Open}
      $_editDialog add -name cancel -text {Cancel}
      pack [message [$_editDialog getframe].message \
			-text "Create a new Template file or\nopen an existing Template file?" \
			-aspect 500] -fill both
    }
    typemethod edit {} {
      $type _createEditDialog
      set answer [$_editDialog draw]
      switch $answer {
	0 {$type new}
	1 {$type open}
	2 {return}
      }      
    }
    method getfile {} {return "$currentFilename"}
    typemethod new  {args} {
      set like [from args -like {}]
      if {"$like" ne "" && [winfo exists "$like"]} {
	set like_filename [$like getfile]
      } else {
	set like_filename $defaultfilename
      }
      set newTop [RolePlayingDB3::RPGToplevel .template%AUTO% \
			-mainframeconstructor $type -mainframetemplate {} \
			-class TemplateEditor]
      $newTop opennew
    }
    method opennew {} {
      set currentFilename {}
      set currentBaseFilename *noname*
      $sidebartree configure -label "$currentBaseFilename"
      set path [$type genname]
      set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      while {[file exists $tempfile]} {
	set path [$type genname]
	set tempfile [file join $::RolePlayingDB3::TmpDir $path]
      }
      vfs::mk4::Mount $tempfile /$path
      foreach theclass {Character Dressing Monster Spell Treasure TrickTrap} {
	file mkdir [file join /$path $theclass]
        close [open [file join /$path $theclass flag] w]
      }
      $sidebartree configure -directory [file join /$path]
      [winfo toplevel $win] configure -title "Template Edit: $currentBaseFilename"
    }
    method new {} {$type new -like $win}
    typevariable genindex 0
    typemethod genname {} {
      incr genindex
      return [format {TEMPLATE%05d} $genindex]
    }
    typemethod open {args} {
      set like [from args -like {}]
      if {"$like" ne "" && [winfo exists "$like"]} {
	set like_filename [$like getfile]
	set w $like
      } else {
	set like_filename $defaultfilename
	set w .
      }
      set currentFilename [tk_getOpenFile -defaultextension .rpgtmpl \
				   -filetypes $filetypes \
				   -initialdir [file dirname $like_filename] \
				   -initialfile $like_filename \
				   -parent $w \
				   -title "File to open"]
      if {"$currentFilename" eq ""} {return}
      set newTop [RolePlayingDB3::RPGToplevel .template%AUTO% \
			-mainframeconstructor $type -mainframetemplate {} \
			-class TemplateEditor]
      $newTop openold "$currentFilename"
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
      $sidebartree configure -label "$currentBaseFilename"
      set inpath [$type genname]
      vfs::zip::Mount $currentFilename $inpath
      foreach classDir {Character Dressing Monster Spell Treasure TrickTrap} {
        if {[catch {file copy [file join $inpath $classDir] /$path}]} {
	  file mkdir /$path $classDir
	  close [open [file join /$path $classDir flag] w]
	}
      }

#      puts stderr "*** $self openold: files in $currentFilename ($inpath): [glob -nocomplain $inpath/*]"
#      puts stderr "*** $self openold: files in $tempfile (/$path): [glob -nocomplain /$path/*]"
      vfs::unmount $inpath
      $sidebartree configure -directory [file join /$path]
      [winfo toplevel $win] configure -title "Template Edit: $currentBaseFilename"
    }
    method rescantemplates {} {
      $sidebartree redrawdirtree
    }
    method open {args} {eval [list $type open -like $win] $args}
    method save {} {
#      puts stderr "*** $self save: currentFilename = $currentFilename"
      $self saveas $currentFilename
    }
    method saveas {{_filename {}}} {
      if {"$_filename" eq {}} {
	set _filename [tk_getSaveFile -defaultextension .rpgtmpl \
				      -filetypes $filetypes \
				      -initialdir [file dirname $currentFilename] \
				      -initialfile $currentFilename \
				      -parent $win \
				      -title "Save As File"]
      }
      if {"$_filename" eq {}} {return}
      ::ZipArchive createZipFromDirtree $_filename /$path \
				-comment "RPGV3 Template Bundle"
      set isdirty no
      if {"$currentFilename" ne "$_filename"} {
	set currentFilename $_filename
	set currentBaseFilename [file tail $currentFilename]
	$sidebartree configure -label "$currentBaseFilename"
	[winfo toplevel $win] configure -title "Template Edit: [file tail $currentFilename]"
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
      vfs::unmount /$path
      file delete $tempfile
      destroy [winfo toplevel $win]
    }
    constructor {args} {
      install banner using Label $win.banner -image $bannerImage -anchor w \
				-background $bannerBackground
      pack $banner -fill x
      install toolbar using ButtonBox $win.toolbar -orient horizontal \
						   -homogeneous no
      pack $toolbar -fill x
      $toolbar add -name addtemp -text {Add Template} \
				 -command [mymethod _addtemplate]
      $toolbar add -name deltemp -text {Delete Template}  \
				 -command [mymethod _deletetemplate]
      $toolbar add -name addfield -text {Add Field or Container} \
				  -command [mymethod _addfield]
      $toolbar add -name edittext -text {Edit Container Text} \
				  -command [mymethod _edittext]
      $toolbar add -name delfield -text {Delete Field or Container} \
				  -command [mymethod _deletefield]
      $toolbar add -name editfield -text {Edit Field} \
				   -command [mymethod _editfield]
      install panes using PanedWindow $win.panes -side top
      pack $panes -fill both -expand yes
      set sbpane [$panes add -weight 1]
      install sidebartree using ::RolePlayingDB3::LabeledDirTree $sbpane.sidebarname \
				-showextension no -filepattern *.xml
      pack $sidebartree -fill both -expand yes
      $sidebartree bindText <Double-Button-1> [mymethod _openTemplate]
      set tmppane [$panes add -weight 5]
      install templatename using LabelEntry $tmppane.templatename \
				-editable no -label "Open Template:" \
				-textvariable [myvar currentTemplateName]
      pack $templatename -fill x
      install templatesw using ScrolledWindow $tmppane.templatesw \
						-scrollbar both -auto both
      pack $templatesw -fill both -expand yes
      install templatetree using Tree [$templatesw getframe].templatetree
      pack $templatetree -fill both -expand yes
      $templatesw setwidget $templatetree
      $templatetree bindText <ButtonRelease-3> [mymethod _postItemMenu]
      $templatetree bindText <ButtonPress-2>   [mymethod _startMove %x %y]
      $templatetree bindText <ButtonRelease-2> [mymethod _endMove %x %y]
      $self configurelist $args

      update
      install _addNewTemplateDialog using Dialog [winfo toplevel $win].addNewTemplateDialog \
					-image $templateMonster \
					-cancel 1 -default 0 -modal local \
					-parent $win -side bottom \
					-title "Add New Template" \
					-transient yes
      $_addNewTemplateDialog add -name add    -text {Add}
      $_addNewTemplateDialog add -name cancel -text {Cancel}
      set dframe [$_addNewTemplateDialog getframe]
      install newTemplateName using LabelEntry $dframe.newTemplateName \
					-label "Name:" -labelwidth 6
      pack $newTemplateName -fill x
      install newTemplateClass using LabelComboBox $dframe.newTemplateClass \
					-label "Class:" -labelwidth 6 \
					-values {Character Monster Spell 
					         Treasure TrickTrap Dressing} \
					-editable no
      pack $newTemplateClass -fill x
      $newTemplateClass setvalue first

      #update
      install _addNewFieldDialog using Dialog [winfo toplevel $win].addNewFieldDialog \
					-image $templateMonster \
					-cancel 1 -default 0 -modal local \
					-parent $win -side bottom \
					-title "Add New Field" \
					-transient yes
      $_addNewFieldDialog add -name add    -text {Add}
      $_addNewFieldDialog add -name cancel -text {Cancel}
      set dframe [$_addNewFieldDialog getframe]
      install newFieldName using LabelEntry $dframe.newFieldName \
				-label "Name:" -labelwidth 10
      pack $newFieldName -fill x
      install newFieldType using LabelComboBox $dframe.newFieldType \
				-label "Type:" -labelwidth 10 \
				-editable no \
				-values {{Whole Number} {Word / Short Phrase}
					 {Long Text} {Graphic} {Document}
					 {Container}}
      pack $newFieldType -fill x
      $newFieldType setvalue first
      install newFieldGenerator using LabelComboBox $dframe.newFieldGenerator \
				-label "Generator:" -labelwidth 10 \
				-editable yes \
				-values {{} d% 1d4 2d4 3d4 5d4 1d6 2d6 3d6 4d6
					 1d8 2d8 3d8 4d8 5d8 1d10 2d10 3d10
					 1d12 2d12 3d12 1d20 2d20}
      pack $newFieldGenerator -fill x
      $newFieldGenerator setvalue first
      install newFieldUpdatable using LabelComboBox $dframe.newFieldUpdatable \
				-label "Updatable:" -labelwidth 10 \
				-editable no \
				-values {yes no}
      pack $newFieldUpdatable -fill x
      $newFieldUpdatable setvalue first

      install _editFieldDialog using Dialog [winfo toplevel $win].editFieldDialog \
					-image $templateMonster \
					-cancel 1 -default 0 -modal local \
					-parent $win -side bottom \
					-title "Edit Field" \
					-transient yes
      $_editFieldDialog add -name add    -text {Update}
      $_editFieldDialog add -name cancel -text {Cancel}
      set dframe [$_editFieldDialog getframe]
      install theFieldName using LabelEntry $dframe.theFieldName \
				-label "Name:" -labelwidth 10 \
				-editable no
      pack $theFieldName -fill x
      install theFieldType using LabelComboBox $dframe.theFieldType \
				-label "Type:" -labelwidth 10 \
				-editable no \
				-values {{Whole Number} {Word / Short Phrase}
					 {Long Text} {Graphic} {Document}}
      pack $theFieldType -fill x
      $theFieldType setvalue first
      install theFieldGenerator using LabelComboBox $dframe.theFieldGenerator \
				-label "Generator:" -labelwidth 10 \
				-editable yes \
				-values {{} d% 1d4 2d4 3d4 5d4 1d6 2d6 3d6 4d6
					 1d8 2d8 3d8 4d8 5d8 1d10 2d10 3d10
					 1d12 2d12 3d12 1d20 2d20}
      pack $theFieldGenerator -fill x
      $theFieldGenerator setvalue first
      install theFieldUpdatable using LabelComboBox $dframe.theFieldUpdatable \
				-label "Updatable:" -labelwidth 10 \
				-editable no \
				-values {yes no}
      pack $theFieldUpdatable -fill x
      $theFieldUpdatable setvalue first

      #update
      install _editContainerTextDialog using \
				Dialog [winfo toplevel $win].editContainerTextDialog \
				-image $templateMonster \
				-cancel 1 -default 0 -modal local \
				-parent $win -side bottom \
				-title "Edit container text" \
				-transient yes
     $_editContainerTextDialog add -name update -text {Update}
     $_editContainerTextDialog add -name cancel -text {Cancel}
     set dframe [$_editContainerTextDialog getframe]
     install containerText using LabelEntry $dframe.containerText \
				-label "Text:" -labelwidth 5
     pack $containerText -fill x
     update idle
    }
    method _addtemplate {} {
#      puts stderr "*** $self _addtemplate"
#      puts stderr "*** $self _addtemplate: _addNewTemplateDialog = $_addNewTemplateDialog"
      switch [$_addNewTemplateDialog draw] {
	0 {
	  set name [$newTemplateName cget -text]
	  set class [$newTemplateClass cget -text]
	  if {[file extension $name] eq ""} {append name ".xml"}
	  set filename [file join /$path $class $name]
	  if {[file exists $filename]} {
	    tk_messageBox -type ok -icon warning -parent $win \
			  -message "A Template named $name already exists! Please select a different name or delete the existing template."
	    return
	  }
          set fp [open "$filename" w]
	  puts $fp {<?xml version="1.0" ?>}
	  puts $fp "<rpgv3:$class name=\"$class\" xmlns:rpgv3=\"http://www.deepsoft.com/roleplayingdb/v3xmlns\">"
	  puts $fp "</rpgv3:$class>"
          close $fp
	  $self rescantemplates
	  set isdirty yes
	}
	1 {return}
      }
    }
    method _deletetemplate {} {
      set selected [$sidebartree selection get]
      switch -- [llength $selected] {
	0 {
	  tk_messageBox -parent $win -type ok -icon info -message "Please select a template to delete!"
	  return
	}
	1 {
	  set thetemplate "[lindex $selected 0]"
	  set ans [tk_messageBox -parent $win -type yesno -icon question -message "Are you sure you want to delete $thetemplate?"]
	  if {"$ans" ne "yes"} {return}
	  file delete [$sidebartree itemcget $thetemplate -fullpath]
	  $self rescantemplates
	  set isdirty yes
	}
        default {
	  tk_messageBox -parent $win -type ok -icon info -message "Please select only one template to delete!"
	}
      }
    }
    variable templateFile {}
    variable nodeStack {}
    method _openTemplate {thetemplate} {
      set templateFile [$sidebartree itemcget $thetemplate -fullpath]
      regsub "/$path/" [file rootname "$templateFile"] {} currentTemplateName
      if {[catch {open $templateFile r} fp]} {
	error "_openTemplate: open $templateFile r: $fp"
	return
      }
      set xml [read $fp]
      close $fp
      $templatetree delete [$templatetree nodes root]
      set p [xml::parser -elementstartcommand [mymethod _elementstart] \
			 -elementendcommand   [mymethod _elementend] \
			 -characterdatacommand [mymethod _characterdata]]
      set nodeStack {root}
      $p parse $xml
      $p free
    }
    method _elementstart {name attlist args} {
      set nodename $name
      if {"$name" eq "Field"} {
	foreach {n v} $attlist {
	  if {"$n" eq "name"} {set nodename "Field:$v"}
	}
	set open no
      } else {
	set open yes
      }
      set text "$nodename"
      foreach {n v} $attlist {
	if {"$n" ne "name" && "$v" ne ""} {
	  append text " $n=\"$v\""
	}
      }
      regsub -all {[[:space:]]} "$nodename" {_} nodename
      set item [$templatetree insert end "[lindex $nodeStack end]" \
			"${nodename}#auto" -text $text \
			-data [list $name $attlist $args] \
			-open $open]
      lappend nodeStack $item
    }
    method _elementend {name args} {
      set nodeStack [lrange $nodeStack 0 [expr {[llength $nodeStack] - 2}]]
    }
    method _characterdata {data} {
      set data [string trim "$data"]
      if {"$data" ne ""} {
	$templatetree insert 0 "[lindex $nodeStack end]" #auto \
			-text "$data" -data "$data"
      }
    }
    method _addfield {{item {}} {menu {}}} {
      if {"$item" ne ""} {
	set selected [list $item]
      } else {
	set selected [$templatetree selection get]
      }
      if {"$menu" ne ""} {
	catch {$menu unpost}
	catch {$menu destroy}
      }
      switch -- [llength $selected] {
	0 {
	  tk_messageBox -parent $win -type ok -icon info -message "Please select a parent field to add a child field to!"
	  return
	}
	1 {
	  set parent [lindex $selected 0]
	  foreach {name attrlist args} [$templatetree itemcget "$parent" -data] {break}
	  if {"$name" eq "Field"} {
	    tk_messageBox -parent $win -type ok -icon info -message "Fields cannot be used as Containers!"
	    return
	  }
	  if {[llength $attrlist] > 2} {
	    tk_messageBox -parent $win -type ok -icon info -message "Parent is not a properly formed container, it has too many attributes!"
	    return
	  }
	  if {[llength $attrlist] > 0 && [lindex $attrlist 0] ne "name"} {
	    tk_messageBox -parent $win -type ok -icon info -message "Parent does not have a name attribute!"
	    return
	  }
	  set ans [$_addNewFieldDialog draw]
	  switch -- $ans {
	    0 {
	      set name [$newFieldName cget -text]
	      set ftype [$newFieldType cget -text]
	      set generator [$newFieldGenerator cget -text]
	      set updatable [$newFieldUpdatable cget -text]
	      if {!$updatable && 
		  [lsearch -exact {{Whole Number} {Word / Short Phrase}} "$ftype"] < 0} {
		tk_messageBox -type ok -icon warning -message "Only Whole Numbers and Word / Short Phrase type fields can be locked!"
		set updatable yes
	      }
	      if {"$ftype" eq "Container" && "$name" ne "Field"} {
		set attrlist [list name $name]
		regsub -all {[[:space:]]} "$name" {_} tag
		regsub -all {:} $tag {-} tag
		set tag [string totitle $tag]
		set nodename $tag
	      } else {
		set attrlist [list name $name type $ftype \
				        generator $generator \
					updatable $updatable]
		set tag Field
		set nodename "Field:$name"
	      }
	      set text "$nodename"
	      foreach {n v} $attrlist {
		if {"$n" ne "name" && "$v" ne ""} {
		  append text " $n=\"$v\""
		}
	      }
	      regsub -all {[[:space:]]} "$nodename" {_} nodename
	      $templatetree insert end "$parent" "${nodename}#auto" -text $text \
		-data [list $tag  $attrlist \
			    [list -namespace \
				  http://www.deepsoft.com/roleplayingdb/v3xmlns]] \
		-open yes
	      $templatetree see "${nodename}#auto"
	      $self _regenerateXMLFromTree
	    }
	    1 {return}
	  }
	}
	default {
	  tk_messageBox -parent $win -type ok -icon info -message "Please select only one parent container to add a child field to!"
	  return
	}
      }
    }
    method _regenerateXMLFromTree {} {
      if {[catch {open $templateFile w} fp]} {
	error "_regenerateXMLFromTree: open $templateFile w: $fp"
	return
      }
      puts $fp {<?xml version="1.0" ?>}
      $self _processNodesAt root $fp
      close $fp
      set isdirty yes
    }
    method _processNodesAt {node fp {needxmlns yes} {indent {}}} {
      foreach n [$templatetree nodes "$node"] {
	if {[string is integer -strict $n]} {
	  puts -nonewline $fp "[$templatetree itemcget "$n" -data]"
	} else {
	  foreach {name attrlist args} [$templatetree itemcget "$n" -data] {break}
	  puts -nonewline $fp "$indent<rpgv3:$name "
          if {$needxmlns} {
	    puts -nonewline $fp "xmlns:rpgv3=\""
	    set namespace [lindex $args [expr {[lsearch -exact $args -namespace] + 1}]]
	    puts -nonewline $fp "$namespace"
	    puts -nonewline $fp "\" "
	    set needxmlns no
	  }
	  foreach {nn vv} $attrlist {
	    if {"$vv" eq ""} {continue}
	    puts -nonewline $fp "$nn=\"$vv\" "
	  }
	  if {"$name" ne "Field" && 
	      ([llength [$templatetree nodes "$n"]] > 0 || 
	       [llength $attrlist] == 0 ||
	       ([llength $attrlist] == 2 && [lindex $attrlist 0] eq "name"))} {
	    puts $fp ">"
	    $self _processNodesAt $n $fp $needxmlns "$indent  "
	    puts $fp "$indent</rpgv3:$name>"
	  } else {
	    puts $fp "/>"
	  }
	}
      }
    }	  
    method _deletefield {{item {}} {menu {}}} {
      if {"$item" ne ""} {
	set selected [list $item]
      } else {
	set selected [$templatetree selection get]
      }
      if {"$menu" ne ""} {
	catch {$menu unpost}
	catch {$menu destroy}
      }
      switch -- [llength $selected] {
	0 {
	  tk_messageBox -parent $win -type ok -icon info -message "Please select a field or container to delete!"
	  return
	}
	1 {
	  set thefield "[lindex $selected 0]"
	  if {[$templatetree parent "$thefield"] eq "root"} {
	    tk_messageBox -parent $win -type ok -icon warning -message "This is the toplevel container and cannot be deleted!"
	    return
	  }
	  if {[llength [$templatetree nodes "$thefield"]] > 0} {
	    tk_messageBox -parent $win -type ok -icon info -message "This container is not empty, please be very sure you want to delete it!"
	  }
	  set ans [tk_messageBox -parent $win -type yesno -icon question -message "Are you sure you want to delete $thefield?"]
	  if {"$ans" ne "yes"} {return}
	    
	  $templatetree delete "$thefield"
	  $self _regenerateXMLFromTree
	}
        default {
	  tk_messageBox -parent $win -type ok -icon info -message "Please select only one field or container to delete!"
	}
      }
    }
    method _edittext {{item {}} {menu {}}} {
      if {"$item" ne ""} {
	set selected [list $item]
      } else {
	set selected [$templatetree selection get]
      }
      if {"$menu" ne ""} {
	catch {$menu unpost}
	catch {$menu destroy}
      }
      switch -- [llength $selected] {
	0 {
	  tk_messageBox -parent $win -type ok -icon info -message "Please select a container!"
	  return
	}
	1 {
	  set thecontainer "[lindex $selected 0]"
	  set t [$templatetree itemcget "$thecontainer" -text]
	  set d [$templatetree itemcget "$thecontainer" -data]
	  if {[string is integer -strict $thecontainer] && "$t" eq "$d"} {
	    tk_messageBox -parent $win -type ok -icon info -message "Please select a container.  Text cannot be added to text!"
	    return
	  }
	  foreach {name attrlist args} $d {break}
	  if {"$name" eq "Field"} {
	    tk_messageBox -parent $win -type ok -icon info -message "Please select a container.  Text cannot be added a Field!"
	    return
	  }
	  set children [$templatetree nodes "$thecontainer"]
	  set oldtext {}
	  set oldnode {}
	  foreach child $children {
	    if {[string is integer -strict $child]} {
	      set oldtext [$templatetree itemcget "$child" -data]
	      set oldnode $child
	      break
	    }
	  }
	  $containerText configure -text [string trim "$oldtext"]
	  set ans [$_editContainerTextDialog draw]
	  switch -exact $ans {
	    0 {
	      set newtext [string trim [$containerText cget -text]]
	      if {"$oldnode" ne ""} {
		if {"$newtext" eq ""} {
		  $templatetree delete "$oldnode"
		} else {
		  $templatetree itemconfigure "$oldnode" -text "$newtext" \
						       -data "$newtext"
		}
	      } elseif {"$newtext" ne ""} {
		$templatetree insert 0 "$thecontainer" #auto -text "$newtext" \
							   -data "$newtext"
	      }
	    }
	    1 {
	    }
	  }
	  $self _regenerateXMLFromTree
	}
        default {
	  tk_messageBox -parent $win -type ok -icon info -message "Please select only one container to text edit!"
	}
      }
    }
    method _editfield {{item {}} {menu {}}} {
      if {"$item" ne ""} {
	set selected [list $item]
      } else {
	set selected [$templatetree selection get]
      }
      if {"$menu" ne ""} {
	catch {$menu unpost}
	catch {$menu destroy}
      }
      switch -- [llength $selected] {
	0 {
	  tk_messageBox -parent $win -type ok -icon info -message "Please select a field!"
	  return
	}
	1 {
	  set thefield "[lindex $selected 0]"
	  set t [$templatetree itemcget "$thefield" -text]
	  set d [$templatetree itemcget "$thefield" -data]
	  if {[string is integer -strict "$thefield"] && "$t" eq "$d"} {
	    tk_messageBox -parent $win -type ok -icon info -message "Please select a field.  Text cannot be directly edited!"
	    return
	  }
	  foreach {name attrlist args} $d {break}
	  if {"$name" ne "Field"} {
	    tk_messageBox -parent $win -type ok -icon info -message "Please select a field.  Only fields can be edited!"
	    return
	  }
	  $theFieldGenerator configure -text {}
	  foreach {n v} $attrlist {
	    switch $n {
	      name {$theFieldName configure -text "$v"}
	      type {$theFieldType configure -text "$v"}
	      generator {$theFieldGenerator configure -text "$v"}
	      updatable {$theFieldUpdatable configure -text "$v"}
	    }
	  }
	  set ans [$_editFieldDialog draw]
	  switch $ans {
	    1 {return}
	    0 {
	      set name [$theFieldName cget -text]
	      set ftype [$theFieldType cget -text]
	      set generator [$theFieldGenerator cget -text]
	      set updatable [$theFieldUpdatable cget -text]
	      if {!$updatable && [lsearch -exact {{Whole Number} {Word / Short Phrase}} "$ftype"] < 0} {
		tk_messageBox -type ok -icon warning -message "Only Whole Numbers and Word / Short Phrase type fields can be locked!"
		set updatable yes
	      }
	      set attrlist [list name $name  \
				 type $ftype \
                                 generator $generator \
                                 updatable $updatable]
	      set text "Field:[$theFieldName cget -text]"
	      foreach {n v} $attrlist {
		if {"$n" ne "name" && "$v" ne ""} {
		  append text " $n=\"$v\""
		}
	      }
	      $templatetree itemconfigure "$thefield" -text $text -data [list $name  $attrlist $args]
	      $self _regenerateXMLFromTree
	    }
	  }
	}
        default {
	  tk_messageBox -parent $win -type ok -icon info -message "Please select only one field!"
	}
      }
    }
    method _postItemMenu {item} {
      set postX [winfo pointerx $templatetree]
      set postY [winfo pointery $templatetree]
      regsub -all {[[:space:].]} "$item" {} mpath
      set mpath [string tolower $mpath]
      set mpath "$templatetree.$mpath"
      set basepath $mpath
      set count 0
      while {[winfo exists $mpath]} {
	incr count
	set mpath "$basepath$count"
      }
      set t [$templatetree itemcget "$item" -text]
      set d [$templatetree itemcget "$item" -data]
#      puts stderr "*** $self _postItemMenu: item = $item, t = $t, d = $d"
      if {[string is integer -strict "$item"] && "$t" eq "$d"} {return};# Text
      foreach {name attrlist args} $d {break}
      menu $mpath -tearoff no
      set iscontainer [expr {"$name" ne "Field"}]
#      puts stderr "*** $self _postItemMenu: iscontainer = $iscontainer, name = $name"
      if {$iscontainer} {
	$mpath add command -label "Add Field" -command [mymethod _addfield $item $mpath]
	$mpath add command -label "Edit Text" -command [mymethod _edittext $item $mpath]
      } else {
	$mpath add command -label "Edit Field" -command [mymethod _editfield $item $mpath]
      }
      $mpath add command -label "Delete"  -command [mymethod _deletefield $item $mpath]
      $mpath add command -label "Cancel"  -command [mymethod _dismisMenu $mpath]
      $mpath post $postX $postY
    }
    method _dismisMenu {menu} {
      catch {$menu unpost}
      catch {$menu destroy}
    }
    variable fromX 
    variable fromY
    variable oldCursor
    method _startMove {mx my item} {
      set fromX $mx
      set fromY $my
      set oldCursor [$templatetree cget -cursor]
      $templatetree configure -cursor crosshair
    }
    method _endMove {mx my item} {
      if {"$item" eq ""} {
	return
      }	
      if {[string is integer -strict "$item"]} {
	tk_messageBox -type ok -icon info \
		-message "Cannot move caintainer text"
	return
      }
      set toX $mx
      set toY $my
      $templatetree configure -cursor $oldCursor
      if {$toY != $fromY} {
#	puts stderr "*** $self _endMove: toY = $toY, $fromY = $fromY"
	set which [$templatetree find @$toX,$toY]
	if {"$which" eq ""} {
	  tk_messageBox -type ok -icon info \
		-message "Cannot move to nowhere"
	  return
	}
#	puts stderr "*** $self _endMove: which = $which, item = $item"
	if {$which eq $item} {return}
	set myparent [$templatetree parent $item]
	if {"$myparent" eq "root"} {
	  tk_messageBox -type ok -icon info \
		     -message "Cannot move sheet class container"
	  return
        }

	set hisparent [$templatetree parent $which]
#	puts stderr "*** $self _endMove: myparent = $myparent, hisparent = $hisparent"
	if {"$hisparent" eq "root"} {
	  tk_messageBox -type ok -icon info \
		     -message "Cannot move fields or containers above sheet class container"
	  return
        }
	if {$myparent eq $hisparent} {
	  set currentOrder [$templatetree nodes $hisparent]
	  set itemIndex [lsearch -exact $currentOrder $item]
	  set newOrder [lreplace $currentOrder $itemIndex $itemIndex]
	  set whichIndex [lsearch -exact $newOrder $which]
	  set newOrder [linsert $newOrder $whichIndex $item]
	  $templatetree reorder $hisparent $newOrder
	} else {
	  $templatetree move $hisparent $item [$templatetree index $which]
	}
	$self _regenerateXMLFromTree
      }
    }
  }
}

package provide RPGTemplate 1.0
